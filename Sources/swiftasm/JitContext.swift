class FunctionTracker {
    var refs: Set<RefFun> = Set()
    var comps: Set<RefFun> = Set()

    func referenced(_ entry: FunctionAddresses.Entry) {
        guard case .compilable(let compilable) = entry else {
            return
        }
        
        refs.insert(compilable.getFindex())
    }
    func compiled(_ ix: RefFun) { comps.insert(ix) }
}

protocol HLTypeFunProvider: CustomDebugStringConvertible, Equatable, Hashable  {
    var argsProvider: [any HLTypeProvider] { get }
    var retProvider: any HLTypeProvider { get }
}

protocol HLTypeProvider: CustomDebugStringConvertible, Equatable, Hashable {
    var kind: HLTypeKind { get }
    var funProvider: (any HLTypeFunProvider)? { get }
}

struct HLTypeFun : HLTypeProvider, HLTypeFunProvider, Equatable, Hashable, CustomDebugStringConvertible {
    static func == (lhs: HLTypeFun, rhs: HLTypeFun) -> Bool {
        fatalError("wip")
    }
    
    func hash(into hasher: inout Hasher) {
        fatalError("hash")
    }
    
    var kind: HLTypeKind { .fun }
     
    var argsProvider: [any HLTypeProvider]
    var retProvider: any HLTypeProvider
    
    var funProvider: (any HLTypeFunProvider)? { self }
    
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

extension HLTypeKind : HLTypeProvider {
    var kind: HLTypeKind { self }
    
    var funProvider: (any HLTypeFunProvider)? {
        switch(self) {
        case .fun:
            fatalError("HLTypeKind can not be used as a function type")
        default:
            return nil
        }
        
    }
    
    
}

extension HLTypeFun_CCompat : HLTypeFunProvider {
    var argsProvider: [any HLTypeProvider] {
        Array(UnsafeBufferPointer(start: self.argsPtr, count: Int(nargs)))
    }
    
    var retProvider: any HLTypeProvider {
        self.retPtr
    }
    
    var debugDescription: String {
        "HLTypeFun_CCompat"
    }
}

extension HLType_CCompat : HLTypeProvider {
    var funProvider: (any HLTypeFunProvider)? {
        switch(self.kind) {
        case .fun:
            return self.fun
        default:
            return nil
        }
    }
}
 
protocol Compilable2 {
    var findex: RefFun { get }
    var ops: [HLOpCode] { get }
    var address: any AddressHolder { get }
    var regs: [any HLTypeProvider] { get }
    var type: any HLTypeProvider { get }
}

protocol AddressHolder {
    var address: UnsafeRawPointer? { get }
    func setAddress(_ addr: UnsafeRawPointer)
}

// Usable for tests
class RefAddressHolder : AddressHolder {
    var address: UnsafeRawPointer? = nil
    
    func setAddress(_ addr: UnsafeRawPointer) {
        guard address == nil else { fatalError("Can't set address twice") }
        self.address = addr
    }
}

class OffsetAddressHolder : AddressHolder {
    let jitBase: JitBase
    let ccompat: UnsafePointer<UnsafeRawPointer?>
    var offset: ByteCount?
    
    init(jitBase: JitBase, ccompat: UnsafePointer<UnsafeRawPointer?>) {
        self.jitBase = jitBase
        self.ccompat = ccompat
    }
    
    var address: UnsafeRawPointer? {
        guard let base = jitBase.wrappedValue, let offset = offset else { return nil }
        return .init(base.advanced(by: Int(offset)))
    }
    
    func setAddress(_ addr: UnsafeRawPointer) {
        fatalError("Can't set address for OffsetAddressHolder. Set it via offset, and it'll be combined with jitBase.")
    }
    
    func setRelativeOffset(_ offset: ByteCount) {
        guard address == nil else { fatalError("Can't set offset twice") }
        self.offset = offset
    }
}


// Updates the address in CCompat
extension UnsafePointer<UnsafeRawPointer?> : AddressHolder {
    var address: UnsafeRawPointer? { self.pointee }
    
    func setAddress(_ addr: UnsafeRawPointer) {
        let mutating = UnsafeMutablePointer(mutating: self)
        mutating.pointee = addr
    }
}

struct PointerCompilable : Compilable2 {
    
    let findex: RefFun
    let ops: [HLOpCode]
    let address: any AddressHolder
    
    let regs: [any HLTypeProvider]
    var ret: any HLTypeProvider
    
    var type: any HLTypeProvider
    
//
//    var entrypoint: MemoryAddress
//
//    var ret: Resolvable<HLType>
//
//    var args: [Resolvable<HLType>]
}

extension UnsafePointer<HLType_CCompat> : HLTypeProvider {
    var kind: HLTypeKind { self.pointee.kind }
    var funProvider: (any HLTypeFunProvider)? { self.pointee.funProvider }
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
    
    let kind: HLTypeKind
    let debugDescription: String
    let _funProvider: AnyHLTypeFunProvider?
    
    var funProvider: (any HLTypeFunProvider)? { _funProvider }
    
    init(_ wrapped: any HLTypeProvider) {
        self.kind = wrapped.kind
        self.debugDescription = wrapped.debugDescription
        
        if let fp = wrapped.funProvider {
            self._funProvider = AnyHLTypeFunProvider(fp)
        } else {
            self._funProvider = nil
        }
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
    
    /// The writer is only needed if we are initializing the memory to match the CCompat layout
    /// (this is meant for tests. The aim is not to have a fully fledged, and compatible serializer)
    let _writer: CCompatWriter_MainContext?
    
    /// This will not initialize the memory. Use this with `Bootstrap.start`.
    init() {
        self.jitBase = JitBase(wrappedValue: nil)
        self.mainContext = .allocate(capacity: 1)
        self.mainContext.initialize(to: MainContext_CCompat())
        self._writer = nil
    }
    
    /// This will initialize the memory. Use this for tests.
    init(ctx: any JitContext2) throws {
        let writer = try CCompatWriter_MainContext(ctx, file: #file)
        
        self.jitBase = JitBase(wrappedValue: nil)
        self.mainContext = .allocate(capacity: 1)
        self._writer = writer
        
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
            
            return PointerCompilable(
                findex: fix,
                ops: fun.pointee.ops,
                address: addr,
                regs: Array(regs),
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
