protocol WholeFunction : Equatable, Hashable {
    var findex: Int32 { get }
    var memory: any MemoryAddress { get }
    var type: Resolvable<HLType> { get }

    // simply convenience for testing/doublechecking what's here
    var wholeFunctionDebugDescription: String { get } 
}

enum WholeFunctionsTableError: Error, Equatable {
    case functionsNotCompiled(String)
    case tableNotReadyWhenRequired
    case invalidUnderlyingData(String)
}

class WholeFunctionsTable {
    let natives: TableResolver<HLNative>
    let compiledFunctions: TableResolver<HLCompiledFunction> 
    let jitBase: SharedStorage<UnsafeMutableRawPointer?>
    
    // convenience for tests
    convenience init(nnatives: Int32, nfunctions: Int32, jitBase: SharedStorage<UnsafeMutableRawPointer?>) {
        let natives: TableResolver<HLNative> = TableResolver(table: SharedStorage(wrappedValue: []), count: nnatives)
        let funcs: TableResolver<HLCompiledFunction> = TableResolver(table: SharedStorage(wrappedValue: []), count: nfunctions)
        self.init(natives: natives, compiledFunctions: funcs, jitBase: jitBase)
    }

    init(
        natives: TableResolver<HLNative>,
        compiledFunctions: TableResolver<HLCompiledFunction>,
        jitBase: SharedStorage<UnsafeMutableRawPointer?>) 
    {
        self.jitBase = jitBase
        self.natives = natives 
        guard natives.count == natives.table.count, compiledFunctions.count == compiledFunctions.table.count else {
            // This is the normal case. Placing a fatalError in case something changes, because
            // the address store/lookup is not considered for this scenario.
            fatalError("Both function tables must already be fully populated (but addresses can be deferred)")
        }

        self.compiledFunctions = compiledFunctions
    } 

    func requireReady() throws {
        guard ready else {
            throw WholeFunctionsTableError.tableNotReadyWhenRequired
        }

        guard table[table.count - 1].findex == table.count - 1 else {
            throw WholeFunctionsTableError.invalidUnderlyingData("Function indexes have a gap")
        }

        guard table.count == Set(table.map { $0.findex }).count else {
            throw WholeFunctionsTableError.invalidUnderlyingData("Some function indexes are duplicated")
        }
    }

    func getAddr(_ ix: Int) -> any DeferredMemoryAddress {
        do {
            let mem = try get(ix).memory
            guard let res = mem as? any DeferredMemoryAddress else {
                fatalError("Could not cast \(mem) as DeferredMemoryAddress")
            }
            return res
        } catch {
            fatalError("getAddr failed: \(error)")
        }
    }

    func get(_ ix: Int) throws -> any WholeFunction {
        try requireReady()
        return self.table![ix] 
    }

    var ready: Bool { table != nil && _cachedTable != nil }

    var _cachedTable: [any WholeFunction]?
    var table: [any WholeFunction]! {
        guard _cachedTable == nil else { 
            return _cachedTable 
        }

        let castNatives: [any WholeFunction] = self.natives.table
        let castFuncs: [any WholeFunction] = self.compiledFunctions.table
        let result = (castNatives + castFuncs).sorted(by: { $0.findex < $1.findex })

        guard result.count == natives.count + compiledFunctions.count else {
            return nil
        }

        _cachedTable = result
        return _cachedTable
    }
}