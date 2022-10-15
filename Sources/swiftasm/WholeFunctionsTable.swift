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
    let addresses: [DeferredAbsoluteAddress] 

    init(
        natives: TableResolver<HLNative>,
        functions: TableResolver<HLCompiledFunction>) 
    {
        self.natives = natives 
        self.functions = functions
        self.addresses = (0..<(natives.count + functions.count)).map { ix in
            if let nat = natives.table.first { $0.findex == ix } {
                return DeferredAbsoluteAddress(wrappedValue: nat.memory.value)
            } 
            
            return DeferredAbsoluteAddress(wrappedValue: nil)
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

    func getAddr(_ ix: Int) -> DeferredAbsoluteAddress {
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