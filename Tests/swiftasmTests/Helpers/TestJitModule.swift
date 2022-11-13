@testable import swiftasm


class TestJitModule : JitContext2 {
    var ntypes: UInt32 { UInt32(types.count) }
    var nints: UInt32 { UInt32(ints.count) }
    var nfunctions: UInt32 { UInt32(compilables.count) }
    var nnatives: UInt32 { UInt32(natives.count) }
    
    func getType(_ ix: Int) throws -> any HLTypeProvider {
        //        let setIx = types.index(types.startIndex, offsetBy: ix)
        //        return types[setIx]
        orderedTypes_slow[ix]
    }
    
    func getInt(_ ix: Int) throws -> Int32 {
        ints[ix]
    }
    
    let compilables: [any Compilable2]
    let natives: [any NativeCallable2]
    let types: Set<AnyHLTypeProvider>
    let ints: [Int32]
    
    func getOrderedCompilablesByRealIx__slow() -> [any Compilable2] { compilables }
    func getOrderedNativesByRealIx__slow() -> [any NativeCallable2] { natives }
    
    var orderedTypes_slow: [AnyHLTypeProvider] {
        let res = Array(types)
            .sorted(by: { $0.kind.rawValue > $1.kind.rawValue })
            .sorted(by: { a, _ in a.kind == .bool ? true : false })
            .sorted(by: { a, _ in a.kind == .i32 ? true : false })
            .sorted(by: { a, _ in a.kind == .void ? true : false })
            

        assert(res[0].kind == .void)
        assert(res[1].kind == .i32)
        assert(res[2].kind == .bool)
        return res
    }
    
    // MARK: JitContext2 properties
    let funcTracker = FunctionTracker()
    
    // MARK: Initializers
    init(_ compilables: [any Compilable2], natives: [any NativeCallable2] = [], ints: [Int32] = []) {
        self.compilables = compilables
        self.natives = natives
        
        let allCallables: [any Callable2] = compilables + natives
        let allTypes = allCallables.flatMap {
            guard let funType = $0.typeProvider.funProvider else {
                fatalError("Compilable must have funType")
            }
            
            let regsProvider: [any HLTypeProvider]
            if let compilable = $0 as? Compilable2 {
                regsProvider = compilable.regsProvider
            } else {
                regsProvider = []
            }
            return regsProvider + [$0.typeProvider] + funType.argsProvider + [funType.retProvider]
        }
        
        let allExpanded = allTypes.flatMap({ t in Self.expand(t) })
        
        self.types = Set(allExpanded.map({ AnyHLTypeProvider($0) }) + [
            // Some types must be always present because we need to
            // guarantee their ordering for tests
            AnyHLTypeProvider(HLTypeKind.void as! (any HLTypeProvider)),
            AnyHLTypeProvider(HLTypeKind.i32 as! (any HLTypeProvider)),
            AnyHLTypeProvider(HLTypeKind.bool as! (any HLTypeProvider))])
        print("Serializing types", self.types)
        
        self.ints = ints
    }
    
    init(types: [any HLTypeProvider]) {
        self.types = Set(types.map { AnyHLTypeProvider($0) })
        self.compilables = []
        self.ints = []
        self.natives = []
    }
    
    static func expand(_ t: any HLTypeProvider) -> [any HLTypeProvider] {
        var res: [any HLTypeProvider] = []
        
        if let fp = t.funProvider {
            res += fp.argsProvider + [fp.retProvider]
        }
        
        if let fp = t.objProvider {
            res += fp.fieldsProvider.map { $0.typeProvider }
            if let superType = fp.superTypeProvider {
                res += expand(superType) + [superType]
            }
        }
        
        if let fp = t.tparamProvider {
            res += [fp]
        }
                
        return res + [t]
    }
    
    // MARK: JitContext2 methods
    func getCompilable(findex fix: RefFun) throws -> (any Compilable2)? {
        self.compilables.first(where: { $0.findex == fix })
    }
}
