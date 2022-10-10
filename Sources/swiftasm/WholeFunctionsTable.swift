struct HLCompiledFunction : WholeFunction, CustomDebugStringConvertible {
    let function: HLFunction
    let memory: UnsafeMutableRawPointer

    var type: Resolvable<HLType> { function.type }
    var findex: Int32 { function.findex }

    var debugDescription: String {
        function.debugDescription
    }

    var wholeFunctionDebugDescription: String {
        "compiled/ops:\(function.ops.count)/regs:\(function.regs.count)/\(findex)"
    }
}

protocol WholeFunction {
    var findex: Int32 { get }
    var memory: UnsafeMutableRawPointer { get }
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
    
    init(
        natives: TableResolver<HLNative>,
        functions: TableResolver<HLCompiledFunction>) 
    {
        self.natives = natives 
        self.functions = functions
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

    func get(_ ix: Int) throws -> WholeFunction {
        try requireReady()
        return self.table![ix] 
    }

    var ready: Bool { table != nil && _cachedTable != nil }

    var _cachedTable: [WholeFunction]?
    var table: [WholeFunction]! {
        guard _cachedTable == nil else { 
            return _cachedTable 
        }

        let castNatives: [any WholeFunction] = self.natives.table
        let castFuncs: [any WholeFunction] = self.functions.table
        let result = (castNatives + castFuncs).sorted(by: { $0.findex < $1.findex })

        guard result.count == natives.count + functions.count else {
            return nil
        }

        _cachedTable = result
        return _cachedTable
    }
}