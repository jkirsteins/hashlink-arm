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

struct JitContext {

    let jitBase: JitBase

    // v0
    private let wft: WholeFunctionsTable
    
    let storage: ModuleStorage
    @SharedStorage var compiledFunctions: [HLCompiledFunction]
    let compiledFunctionResolver: TableResolver<HLCompiledFunction>

    // v1
    let hlcode: UnsafePointer<HLCode_CCompat>?
    let callTargets: FunctionAddresses
    let funcTracker = FunctionTracker()
    
    init(module: Module, hlcode: UnsafePointer<HLCode_CCompat>) {
        self.init(storage: module.storage, hlcode: hlcode)
    }

    /// Useful for testing (e.g. initialize context from ModuleStorage, but
    /// otherwise not used.
    init(storage: ModuleStorage) {
        self.init(storage: storage, hlcode: nil)
    }

    init(storage: ModuleStorage, hlcode: UnsafePointer<HLCode_CCompat>?) {
        self.hlcode = hlcode 

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
