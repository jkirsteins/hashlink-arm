class FunctionTracker {
    var refs: Set<RefFun> = Set()
    var comps: Set<RefFun> = Set()

    func referenced(_ entry: FunctionAddresses.Entry) {
        guard case .compilable(let compilable) = entry else {
            return
        }
        
        refs.insert(compilable.getFindex())
    }
    
    func referenced2(_ call: Callable2) {
        refs.insert(call.findex)
    }
    
    func compiled(_ ix: RefFun) { comps.insert(ix) }
}

protocol HLTypeFunProvider: CustomDebugStringConvertible, Equatable, Hashable  {
    var argsProvider: [any HLTypeProvider] { get }
    var retProvider: any HLTypeProvider { get }
}

protocol StringProvider {
    var stringValue: String { get }
}

extension UnsafePointer<CChar16> : StringProvider {
    var stringValue: String { String._wrapUtf16(from: self) }
}

extension String : StringProvider {
    var stringValue: String { self }
}

protocol HLObjFieldProvider {
    var nameProvider: any StringProvider { get }
    var typeProvider: any HLTypeProvider { get }
}

extension UnsafePointer<HLObjField_CCompat> : HLObjFieldProvider {
    var nameProvider: any StringProvider { self.pointee.nameProvider }
    var typeProvider: any HLTypeProvider { self.pointee.typeProvider }
}

extension HLObjField_CCompat : HLObjFieldProvider {
    var nameProvider: any StringProvider { self.namePtr }
    var typeProvider: any HLTypeProvider { self.tPtr }
}

protocol HLTypeObjProvider: CustomDebugStringConvertible, Equatable, Hashable  {
    var fieldsProvider: [any HLObjFieldProvider] { get }
}

extension UnsafePointer<HLTypeObj_CCompat> : HLTypeObjProvider {
    var fieldsProvider: [HLObjFieldProvider] {
        Array(UnsafeBufferPointer(start: self.pointee.fieldsPtr, count: Int(self.pointee.nfields)))
    }
}

extension HLTypeObjProvider {
    /// Meant for use in v simple tests, doesn't need to cover every case.
    func isEquivalent(_ other: any HLTypeObjProvider) -> Bool {
        
        return true
    }
}

protocol HLTypeKindProvider : Equatable, Hashable, CustomDebugStringConvertible {
    var kind: HLTypeKind { get }
    var hlRegSize: ByteCount { get }
}

extension HLTypeKindProvider {
    var debugDescription: String {
        self.kind.debugDescription
    }
}
 
extension HLTypeKind : HLTypeKindProvider {
    var kind: HLTypeKind { self }
}

protocol HLTypeProvider: HLTypeKindProvider, CustomDebugStringConvertible, Equatable, Hashable {
    var kind: HLTypeKind { get }
    var funProvider: (any HLTypeFunProvider)? { get }
    var objProvider: (any HLTypeObjProvider)? { get }
    
    var ccompatAddress: UnsafeRawPointer { get }
}

struct Test_HLTypeFun : HLTypeProvider, HLTypeFunProvider, Equatable, Hashable, CustomDebugStringConvertible {
    var hlRegSize: ByteCount { self.kind.hlRegSize }
    
    static func == (lhs: Test_HLTypeFun, rhs: Test_HLTypeFun) -> Bool {
        fatalError("wip")
    }
    
    func hash(into hasher: inout Hasher) {
        fatalError("hash")
    }
    
    var kind: HLTypeKind { .fun }
    var ccompatAddress: UnsafeRawPointer { fatalError("Don't use this outside of tests.") }
    var argsProvider: [any HLTypeProvider]
    var retProvider: any HLTypeProvider
    
    var funProvider: (any HLTypeFunProvider)? { self }
    var objProvider: (any HLTypeObjProvider)? { nil }
    
    var debugDescription: String {
        "HLTypeFun(\(argsProvider.map { $0.debugDescription }.joined(separator: ", "))) -> (\(retProvider.debugDescription))"
    }
}

extension HLTypeFunProvider {
    /// Meant for use in v simple tests, doesn't need to cover every case.
    func isEquivalent(_ other: any HLTypeFunProvider) -> Bool {
        guard self.retProvider.isEquivalent(other.retProvider) else { return false }
        guard self.argsProvider.count == other.argsProvider.count else { return false }
        guard self.argsProvider.enumerated().allSatisfy({ (ix, lhsI) in
            lhsI.isEquivalent(other.argsProvider[ix])
        }) else {
            return false
        }
        
        return true
    }
}

extension HLTypeProvider {
    /// Meant for use in v simple tests, doesn't need to cover every case.
    func isEquivalent(_ other: any HLTypeProvider) -> Bool {
        guard self.kind == other.kind else { return false }
        switch(self.kind) {
        case .i32, .u8, .u16, .i64:
            break
        case .fun:
            guard let lhs = self.funProvider, let rhs = other.funProvider else {
                fatalError("fun type must have funProvider set")
            }
            return lhs.isEquivalent(rhs)
        default:
            fatalError("HLTypeProvider.isEquivalent not implemented for \(self.kind)")
        }
        
        return true
    }
}

protocol HLTypeListProvider {
    var ntypes: UInt32 { get }
    var nfunctions: UInt32 { get }
    
    /// Get type from index.
    func getType(_ ix: Int) throws -> any HLTypeProvider
}

protocol Callable2 {
    var findex: RefFun { get }
    var address: any MemoryAddress { get }
    var retProvider: any HLTypeProvider { get }
    var argsProvider: [any HLTypeProvider] { get }
}

struct NativeCallable2 : Callable2 {
    let native: UnsafePointer<HLNative_CCompat>
    let address: any MemoryAddress
    
    var findex: RefFun { RefFun(native.pointee.findex) }
    var retProvider: any HLTypeProvider { native.pointee.typePtr.pointee.fun.pointee.retProvider }
    var argsProvider: [any HLTypeProvider] { native.pointee.typePtr.pointee.fun.pointee.argsProvider }
}

struct FunctionCallable2 : Callable2 {
    let function: UnsafePointer<HLFunction_CCompat>
    let address: any MemoryAddress
    
    var findex: RefFun { RefFun(function.pointee.findex) }
    var retProvider: any HLTypeProvider { function.pointee.typePtr!.pointee.fun.pointee.retProvider }
    var argsProvider: [any HLTypeProvider] { function.pointee.typePtr!.pointee.fun.pointee.argsProvider }
}

protocol Compilable2 {
    var findex: RefFun { get }
    var ops: [HLOpCode] { get }
    var address: any LinkableAddress { get }
    var regs: [any HLTypeProvider] { get }
    var args: [any HLTypeProvider] { get }
    var type: any HLTypeProvider { get }
}

protocol LinkableAddress : MemoryAddress {
    func setOffset(_ offset: ByteCount)
    var hasOffset: Bool { get }
}

/// Like `FullyDeferredRelativeAddress` that also updates the address in CCompat `function_ptrs`.
struct CCompatUpdatingLinkableAddress : LinkableAddress {
    var hasOffset: Bool { self.wrapped.hasOffset }
    
    var value: UnsafeMutableRawPointer { self.wrapped.value }
    
    func isEqual(_ to: any MemoryAddress) -> Bool {
        self.wrapped.isEqual(to)
    }
    
    let wrapped: FullyDeferredRelativeAddress
    let ccompat: UnsafeMutablePointer<UnsafeRawPointer?>
 
    init(jitBase: JitBase, ccompat: UnsafeMutablePointer<UnsafeRawPointer?>) {
        self.wrapped = FullyDeferredRelativeAddress(jitBase: jitBase)
        self.ccompat = ccompat
    }
    
    func setOffset(_ offset: ByteCount) {
        self.wrapped.setOffset(offset)
    }
    
    
}

struct PointerCompilable : Compilable2 {
    
    let findex: RefFun
    let ops: [HLOpCode]
    let address: any LinkableAddress
    
    let regs: [any HLTypeProvider]
    
    var args: [any HLTypeProvider]
    var ret: any HLTypeProvider
    var type: any HLTypeProvider
    
//
//    var entrypoint: MemoryAddress
//
//    var ret: Resolvable<HLType>
//
//    var args: [Resolvable<HLType>]
}

extension UnsafePointer<HLType_CCompat> : HLTypeProvider, HLTypeKindProvider {
    var ccompatAddress: UnsafeRawPointer { .init(self) }
    var kind: HLTypeKind { self.pointee.kind }
    var hlRegSize: ByteCount { kind.hlRegSize }
    var funProvider: (any HLTypeFunProvider)? {
        switch(self.kind) {
        case .fun:
            return self.pointee.fun
        default:
            return nil
        }
    }
    var objProvider: (any HLTypeObjProvider)? {
        switch(self.kind) {
        case .obj:
            return self.pointee.obj
        default:
            return nil
        }
    }
}

extension UnsafePointer<HLTypeFun_CCompat> : HLTypeFunProvider {
    var argsProvider: [any HLTypeProvider] { self.pointee.argsProvider }
    var retProvider: any HLTypeProvider { self.pointee.retProvider }
}

protocol JitContext2 : HLTypeListProvider {
    var funcTracker: FunctionTracker { get }
    
    /// Used in tests
    func getOrderedCompilablesByRealIx__slow() throws -> [any Compilable2]
    
    func getCompilable(findex fix: RefFun) throws -> (any Compilable2)?
}


class AnyHLTypeProvider : Equatable, Hashable, CustomDebugStringConvertible, HLTypeProvider {
    static func == (lhs: AnyHLTypeProvider, rhs: AnyHLTypeProvider) -> Bool {
        lhs.isEquivalent(rhs)
    }
    
    func hash(into hasher: inout Hasher) {
        hasher.combine(kind)
        hasher.combine(_funProvider)
    }
    
    var kind: HLTypeKind { _kind() }
    var hlRegSize: ByteCount { _hlRegSize() }
    var debugDescription: String { _debugDescription() }
    var ccompatAddress: UnsafeRawPointer { _ccompatAddress() }
    var funProvider: (any HLTypeFunProvider)? { _funProvider }
    var objProvider: (any HLTypeObjProvider)? { _objProvider }
    
    let _ccompatAddress: ()->UnsafeRawPointer
    let _kind: ()->HLTypeKind
    let _hlRegSize: ()->ByteCount
    let _debugDescription: ()->String
    let _funProvider: AnyHLTypeFunProvider?
    let _objProvider: AnyHLTypeObjProvider?
    
    init(_ wrapped: any HLTypeProvider) {
        self._kind = { wrapped.kind }
        self._hlRegSize = { wrapped.hlRegSize }
        self._debugDescription = { wrapped.debugDescription }
        self._ccompatAddress = { wrapped.ccompatAddress }
        
        if let fp = wrapped.funProvider {
            self._funProvider = AnyHLTypeFunProvider(fp)
        } else {
            self._funProvider = nil
        }
        
        if let op = wrapped.objProvider {
            self._objProvider = AnyHLTypeObjProvider(op)
        } else {
            self._objProvider = nil
        }
    }
}

class AnyHLTypeObjProvider : Equatable, Hashable, CustomDebugStringConvertible, HLTypeObjProvider {
    var debugDescription: String { _debugDescription() }
    var fieldsProvider: [any HLObjFieldProvider] { _fieldsProvider() }
    
    var _fieldsProvider: ()->[any HLObjFieldProvider]
    let _debugDescription: ()->String

    static func == (lhs: AnyHLTypeObjProvider, rhs: AnyHLTypeObjProvider) -> Bool {
        lhs.isEquivalent(rhs)
    }
    
    func hash(into hasher: inout Hasher) {
        hasher.combine(1)
    }
    
    init(_ wrapped: any HLTypeObjProvider) {
        self._debugDescription = { wrapped.debugDescription }
        self._fieldsProvider = { wrapped.fieldsProvider }
    }
}

class AnyHLTypeFunProvider : Equatable, Hashable, CustomDebugStringConvertible, HLTypeFunProvider {
    static func == (lhs: AnyHLTypeFunProvider, rhs: AnyHLTypeFunProvider) -> Bool {
        lhs.isEquivalent(rhs)
    }
    
    func hash(into hasher: inout Hasher) {
        hasher.combine(_args)
        hasher.combine(_ret)
    }
    
    let _args: [AnyHLTypeProvider]
    var argsProvider: [any HLTypeProvider] { _args }
    
    let _ret: AnyHLTypeProvider
    var retProvider: any HLTypeProvider { _ret }
    
    let debugDescription: String
    
    init(_ wrapped: any HLTypeFunProvider) {
        self.debugDescription = wrapped.debugDescription
        self._args = wrapped.argsProvider.map { AnyHLTypeProvider($0) }
        self._ret = AnyHLTypeProvider(wrapped.retProvider)
    }
    
}

class TestJitModule : JitContext2 {
    var ntypes: UInt32 { UInt32(types.count) }
    var nfunctions: UInt32 { UInt32(compilables.count) }
    
    func getType(_ ix: Int) throws -> any HLTypeProvider {
//        let setIx = types.index(types.startIndex, offsetBy: ix)
//        return types[setIx]
        orderedTypes_slow[ix]
    }
    
    let compilables: [any Compilable2]
    let types: Set<AnyHLTypeProvider>
    
    func getOrderedCompilablesByRealIx__slow() -> [any Compilable2] { compilables }
    
    var orderedTypes_slow: [AnyHLTypeProvider] {
        Array(types).sorted(by: { $0.kind.rawValue > $1.kind.rawValue })
    }
    
    // MARK: JitContext2 properties
    let funcTracker = FunctionTracker()
    
    // MARK: Initializers
    init(_ compilables: [any Compilable2]) {
        self.compilables = compilables
        
        let allTypes = compilables.flatMap {
            guard let funType = $0.type.funProvider else {
                fatalError("Compilable must have funType")
            }
            return $0.regs + [$0.type] + funType.argsProvider + [funType.retProvider]
        }
        
        self.types = Set(allTypes.map { AnyHLTypeProvider($0) })
    }
    
    // MARK: JitContext2 methods
    func getCompilable(findex fix: RefFun) throws -> (any Compilable2)? {
        self.compilables.first(where: { $0.findex == fix })
    }
}

class CCompatJitContext : JitContext2 {
    let jitBase: JitBase
    let mainContext: UnsafeMutablePointer<MainContext_CCompat>
    let funcTracker = FunctionTracker()
    
    // function index (not findex) to linkable addr
    let linkableAddresses: Dictionary<Int, any LinkableAddress>
    
    /// The writer is only needed if we are initializing the memory to match the CCompat layout
    /// (this is meant for tests. The aim is not to have a fully fledged, and compatible serializer)
    let _writer: CCompatWriter_MainContext?
    
    /// This will not initialize the memory. Use this with `Bootstrap.start`.
    init(_ code: UnsafePointer<HLCode_CCompat>) {
        let jitBase = JitBase(wrappedValue: nil)
        
        self.jitBase = jitBase
        self.mainContext = .allocate(capacity: 1)
        self.mainContext.initialize(to: MainContext_CCompat())
        self._writer = nil
        
        
        self.linkableAddresses = .init((0..<code.pointee.nfunctions).map { realIx in
            (Int(realIx), FullyDeferredRelativeAddress(jitBase: jitBase))
        }, uniquingKeysWith: { _, _ in fatalError("No duplicate keys allowed") })
    }
     
    /// This will initialize the memory. Use this for tests.
    init(ctx: any JitContext2) throws {
        let writer = try CCompatWriter_MainContext(ctx, file: #file)
        
        let jitBase = JitBase(wrappedValue: nil)
        self.jitBase = jitBase
        self.mainContext = .allocate(capacity: 1)
        self._writer = writer
        
        self.linkableAddresses = .init((0..<ctx.nfunctions).map { realIx in
            (Int(realIx), FullyDeferredRelativeAddress(jitBase: jitBase))
        }, uniquingKeysWith: { _, _ in fatalError("No duplicate keys allowed") })
        
        try writer.initialize(target: self.mainContext)
    }
    
    deinit {
        mainContext.deinitialize(count: 1)
        mainContext.deallocate()
    }
    
    private func withModule<T>(_ callback: (UnsafePointer<HLModule_CCompat>)throws->T) throws -> T {
        guard let m = mainContext.pointee.m else {
            throw GlobalError.unexpected("Module data not available")
        }
        return try callback(m)
    }
    
    var ntypes: UInt32  {
        try! withModule {
            $0.pointee.code.pointee.ntypes
        }
    }
    
    var nfunctions: UInt32  {
        try! withModule {
            $0.pointee.code.pointee.nfunctions
        }
    }
    
    func getType(_ ix: Int) throws -> any HLTypeProvider {
        try withModule {
            return $0.pointee.code.pointee.getType(ix)
        }
    }
    
    func getFunctionTableIndex(findex fix: RefFun) throws -> Int32 {
        try withModule { m in
            let realIx = m.pointee.functions_indexes.advanced(by: fix).pointee
            guard realIx >= 0 && realIx < m.pointee.code.pointee.nfunctions else {
                throw GlobalError.unexpected("Real index \(realIx) is not valid for findex \(fix)")
            }
            return realIx
        }
    }
    
    func getOrderedCompilablesByRealIx__slow() throws -> [any Compilable2] {
        try withModule { m in
            let bufPointer: UnsafeBufferPointer<HLFunction_CCompat> = .init(
                start: m.pointee.code.pointee.functions,
                count: Int(m.pointee.code.pointee.nfunctions))
            return try bufPointer.map {
                guard let c = try getCompilable(findex: RefFun($0.findex)) else {
                    fatalError("Could not fetch compilable (findex=\($0.findex)) which should exist.")
                }
                return c
            }
        }
    }
    
    func requireInt(_ ix: Ref) throws -> Int32 {
        guard let result = try getInt(ix) else {
            throw GlobalError.invalidOperation("Required int (ix==\(ix)) not found.")
        }
        return result
    }
    
    func getInt(_ ix: Ref) throws -> Int32? {
        try withModule { m in
            m.pointee.code.pointee.getInt(ix)
        }
    }
    
    func requireGlobal(_ globalRef: Ref) throws -> UnsafePointer<HLType_CCompat> {
        guard let result = try getGlobal(globalRef) else {
            throw GlobalError.invalidOperation("Required global (ix==\(globalRef)) not found.")
        }
        return result
    }
    
    func getGlobal(_ globalRef: Ref) throws -> UnsafePointer<HLType_CCompat>? {
        try withModule { m in
            m.pointee.code.pointee.getGlobal(globalRef)
        }
    }
    
    func requireCallable(findex fix: RefFun) throws -> (any Callable2) {
        guard let result = try getCallable(findex: fix) else {
            throw GlobalError.invalidOperation("Required callable (fix==\(fix)) not found.")
        }
        return result
    }
    
    func getCallable(findex fix: RefFun) throws -> (any Callable2)? {
        try withModule {
            (m)->(any Callable2)? in
            
            let isNative = fix >= m.pointee.code.pointee.nfunctions
            
            let realIx = try getFunctionTableIndex(findex: fix)
            print("Real ix: \(realIx)")
            
            if isNative {
                guard let nat = self.mainContext.pointee.code?.pointee.getNative(Int(realIx)) else {
                    return nil
                }
                guard let addr = self.mainContext.pointee.m?.pointee.functions_ptrs.advanced(by: fix).pointee else {
                    fatalError("Native (fix==\(fix)) not resolved.")
                }
                
                return NativeCallable2(native: nat, address: addr)
            } else {
                guard let fun = self.mainContext.pointee.code?.pointee.getFunction(Int(realIx)) else {
                    return nil
                }
                
                guard let addr = self.linkableAddresses[Int(realIx)] else {
                    fatalError("Function (fix==\(fix)) has no address.")
                }
                
                return FunctionCallable2(function: fun, address: addr)
            }
        }
        
    }
    
    func getCompilable(findex fix: RefFun) throws -> (any Compilable2)? {
        try withModule { (m)->(PointerCompilable?) in
            let addr = m.pointee.functions_ptrs.advanced(by: fix)
            
            if addr.pointee != nil {
                // already compiled
                return nil
            }
            
            let realIx = try getFunctionTableIndex(findex: fix)
            print("Real ix: \(realIx)")
            
            guard let fun = self.mainContext.pointee.code?.pointee.getFunction(Int(realIx)) else {
                return nil
            }
            
            // TODO: remove
            for ix in (0..<self.mainContext.pointee.code!.pointee.nfunctions) {
                let f = self.mainContext.pointee.code!.pointee.functions.advanced(by: Int(ix)).pointee
                print("Found f2 \(fun.pointee)")
            }
            // TODO: remove
            
            
            guard let funTypePtr = fun.pointee.typePtr else {
                throw GlobalError.unexpected("Function type found but it lacks function data (findex=\(fix))")
            }
            
            let funData = funTypePtr.pointee.fun
            
            let regs = UnsafeBufferPointer(start: fun.pointee.regsPtr, count: Int(fun.pointee.nregs))
            
            guard let laddr = linkableAddresses[Int(realIx)] else {
                fatalError("Linkable addresses not available (real index \(realIx))")
            }
            
            return PointerCompilable(
                findex: fix,
                ops: fun.pointee.ops,
                address: laddr,
                regs: Array(regs),
                args: fun.pointee.cType.fun.argsProvider,
                ret: funData.pointee.retPtr,
                type: funTypePtr)
        }
    }
}


class JitContext {

    let jitBase: JitBase

    // v0
    private let wft: WholeFunctionsTable
    
    let storage: ModuleStorage
    @SharedStorage var compiledFunctions: [HLCompiledFunction]
    let compiledFunctionResolver: TableResolver<HLCompiledFunction>

    // v1
    var hlMainContext = MainContext_CCompat()
    var hlcode: UnsafePointer<HLCode_CCompat>? { hlMainContext.code }
    var callTargets: FunctionAddresses
    let funcTracker = FunctionTracker()
    
    convenience init(module: Module, hlcode: UnsafePointer<HLCode_CCompat>) {
        self.init(storage: module.storage, hlcode: hlcode)
    }

    /// Useful for testing (e.g. initialize context from ModuleStorage, but
    /// otherwise not used.
    convenience init(storage: ModuleStorage) {
        self.init(storage: storage, hlcode: nil)
    }

    init(storage: ModuleStorage, hlcode: UnsafePointer<HLCode_CCompat>?) {
        hlMainContext.code = hlcode 

        let jitBase: SharedStorage<UnsafeMutableRawPointer?> = SharedStorage(
            wrappedValue: nil
        )
        
        if let hlcode = hlcode {
            self.callTargets = FunctionAddresses(hlcode.pointee, jitBase: jitBase)
        } else {
            self.callTargets = FunctionAddresses(storage, jitBase: jitBase)
        }
        
        self._compiledFunctions = SharedStorage(
            wrappedValue: storage.functionResolver.table.map {
                HLCompiledFunction(
                    function: $0,
                    memory: FullyDeferredRelativeAddress(jitBase: jitBase)
                )
            }
        )
        let compiledFunctionResolver = TableResolver(
            table: self._compiledFunctions,
            count: Int32(self._compiledFunctions.wrappedValue.count)
        )
        self.wft = WholeFunctionsTable(
            natives: storage.nativeResolver,
            compiledFunctions: compiledFunctionResolver,
            jitBase: jitBase
        )
        self.jitBase = jitBase
        self.storage = storage
        self.compiledFunctionResolver = compiledFunctionResolver
    }
}
