struct HLCompiledFunction : Equatable, WholeFunction, CustomDebugStringConvertible {
    let function: HLFunction
    let memory: any MemoryAddress

    var type: Resolvable<HLType> { function.type }
    var findex: Int32 { function.findex }

    var debugDescription: String {
        function.debugDescription
    }

    var wholeFunctionDebugDescription: String {
        "compiled/ops:\(function.ops.count)/regs:\(function.regs.count)/\(findex)"
    }

    static func == (lhs: Self, rhs: Self) -> Bool {
        return lhs.function == rhs.function  &&
            lhs.memory.isEqual(rhs.memory) && 
            lhs.type == rhs.type && 
            lhs.findex == rhs.findex 
    }
}

protocol WholeFunction : Equatable {
    var findex: Int32 { get }
    var memory: any MemoryAddress { get }
    var type: Resolvable<HLType> { get }

    // simply convenience for testing/doublechecking what's here
    var wholeFunctionDebugDescription: String { get } 
}

enum WholeFunctionsTableError: Error, Equatable {
    case tableNotReadyWhenRequired
    case invalidUnderlyingData(String)
}

class WholeFunctionsTable {
    let natives: TableResolver<HLNative>
    let functions: TableResolver<HLCompiledFunction> 
    
    // For compiler to be able to refer to addresses ahead of them being known
    // These can be relative (for HLCompiledFunction) or absolute (for HLNative)
    //
    // These need to be separate from functions, and have to be merged once all data is available,
    // for example in this scenario:
    //   - start compiling function #0
    //   - function #0 needs to call function #1
    //   - we need a deferred address, but function #1 has not been compiled yet
    let addresses: [any DeferredMemoryAddress] 

    // convenience for tests
    convenience init(nnatives: Int32, nfunctions: Int32, jitBase: SharedStorage<UnsafeMutableRawPointer?>) {
        let natives: TableResolver<HLNative> = TableResolver(table: SharedStorage(wrappedValue: []), count: nnatives)
        let funcs: TableResolver<HLCompiledFunction> = TableResolver(table: SharedStorage(wrappedValue: []), count: nfunctions)
        self.init(natives: natives, functions: funcs, jitBase: jitBase)
    }

    init(
        natives: TableResolver<HLNative>,
        functions: TableResolver<HLCompiledFunction>,
        jitBase: SharedStorage<UnsafeMutableRawPointer?>) 
    {
        self.natives = natives 
        guard natives.count == natives.table.count else {
            // This is the normal case. Placing a fatalError in case something changes, because
            // the address store/lookup is not considered for this scenario.
            fatalError("Natives must already be fully populated")
        }

        self.functions = functions
        self.addresses = (0..<(natives.count + functions.count)).map { (ix: Int32)->any DeferredMemoryAddress in
            // assuming natives are always available with a set absolute address
            if let nat = natives.table.first { $0.findex == ix } {
                return nat.memory.value
            } 
            
            return FullyDeferredRelativeAddress(jitBase: jitBase)
        }
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
        self.addresses[ix]
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
        let castFuncs: [any WholeFunction] = self.functions.table
        let result = (castNatives + castFuncs).sorted(by: { $0.findex < $1.findex })

        guard result.count == natives.count + functions.count else {
            print("\(result.count) != \(natives.count) + \(functions.count)")
            return nil
        }

        _cachedTable = result
        return _cachedTable
    }
}