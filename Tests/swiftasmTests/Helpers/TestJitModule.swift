@testable import swiftasm


class TestJitModule : JitContext2 {
    
    func getBytes(_ ix: Int) throws -> BytesProvider {
        bytes[ix]
    }
    
    func getAllBytes_forWriters() throws -> ([UInt8], [Int32]) {
        let left = self.bytes.flatMap { $0 }
        let right = self.bytes.map({ $0.count }).reduce([Int32(0)]) { (res, curC) -> [Int32] in
            let resLast = res.last ?? 0
            return res + [resLast + Int32(curC)]
        }.dropLast()
        
        return (left, Array(right))
    }
    
    var nbytes: UInt32 { UInt32(bytes.count) }
    var ntypes: UInt32 { UInt32(types.count) }
    var nints: UInt32 { UInt32(ints.count) }
    var nfunctions: UInt32 { UInt32(compilables.count) }
    var nnatives: UInt32 { UInt32(natives.count) }
    var nstrings: UInt32 { UInt32(strings.count) }
    var nglobals: UInt32 { UInt32(globals.count) }
    var nfloats: UInt32 { UInt32(floats.count) }
    
    let versionHint: Int?
    
    func getType(_ ix: Int) throws -> any HLTypeProvider {
        //        let setIx = types.index(types.startIndex, offsetBy: ix)
        //        return types[setIx]
        orderedTypes_slow[ix]
    }
    
    func getGlobal(_ globalRef: Ref) throws -> (any HLTypeProvider)? {
        globals[globalRef]
    }
    
    func getInt(_ ix: Int) throws -> Int32 {
        ints[ix]
    }
    
    func getFloat(_ ix: Int) throws -> Float64 {
        floats[ix]
    }
    
    func getString(_ ix: Int) throws -> any StringProvider {
        strings[ix]
    }
    
    let compilables: [any Compilable2]
    let natives: [any NativeCallable2]
    let types: Set<AnyHLTypeProvider>
    let ints: [Int32]
    let floats: [Float64]
    let strings: [String]
    let bytes: [[UInt8]]
    let globals: [any HLTypeProvider]
    
    func getOrderedCompilablesByRealIx__slow() -> [any Compilable2] { compilables }
    func getOrderedNativesByRealIx__slow() -> [any NativeCallable2] { natives }
    
    var orderedTypes_slow: [AnyHLTypeProvider] {
        let res = Array(types)
            .sorted(by: { $0.kind.rawValue > $1.kind.rawValue })
            .sorted(by: { a, _ in a.kind == .i64 ? true : false })
            .sorted(by: { a, _ in a.kind == .bool ? true : false })
            .sorted(by: { a, _ in a.kind == .i32 ? true : false })
            .sorted(by: { a, _ in a.kind == .void ? true : false })
            
        // These are the type indexes you can rely on in tests
        assert(res[0].kind == .void)
        assert(res[1].kind == .i32)
        assert(res[2].kind == .bool)
        assert(res[3].kind == .i64)
        return res
    }
    
    // MARK: JitContext2 properties
    let funcTracker = FunctionTracker()
    
    // MARK: Initializers
    init(_ compilables: [any Compilable2], natives: [any NativeCallable2] = [], ints: [Int32] = [], strings: [String] = [], bytes: [[UInt8]] = [], globals: [any HLTypeProvider] = [], floats: [Float64] = [], v versionHint: Int? = nil) {
        self.compilables = compilables
        self.natives = natives
        self.globals = globals
        self.floats = floats
        self.versionHint = versionHint
        self.bytes = bytes
        
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
            AnyHLTypeProvider(HLTypeKind.void as (any HLTypeProvider)),
            AnyHLTypeProvider(HLTypeKind.i32 as (any HLTypeProvider)),
            AnyHLTypeProvider(HLTypeKind.i64 as (any HLTypeProvider)),
            AnyHLTypeProvider(HLTypeKind.bool as (any HLTypeProvider))])
        print("Serializing types", self.types.map({ $0._overrideDebugDescription }).joined(separator: "\n --"))
        
        self.ints = ints
        self.strings = strings
    }
    
    init(types: [any HLTypeProvider]) {
        self.types = Set(types.map { AnyHLTypeProvider($0) })
        self.compilables = []
        self.ints = []
        self.strings = []
        self.natives = []
        self.versionHint = nil
        self.bytes = []
        self.globals = []
        self.floats = []
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
            print("tparam: \(fp) from \(t)")
            res += [fp]
        }
                
        return res + [t]
    }
    
    // MARK: JitContext2 methods
    func getCompilable(findex fix: RefFun) throws -> (any Compilable2)? {
        self.compilables.first(where: { $0.findex == fix })
    }
}
