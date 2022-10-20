struct JitContext {

    let jitBase: JitBase

    // v0
    let wft: WholeFunctionsTable
    
    let storage: ModuleStorage
    @SharedStorage var compiledFunctions: [HLCompiledFunction]
    let compiledFunctionResolver: TableResolver<HLCompiledFunction>

    // v1
    let hlcode: UnsafePointer<HLCode_CCompat>
    let addresses: FunctionAddresses

    init(module: Module, hlcode: UnsafePointer<HLCode_CCompat>) {
        self.init(storage: module.storage, hlcode: hlcode)
    }

    init(storage: ModuleStorage) {
        let unsafeCompat: UnsafeMutablePointer<HLCode_CCompat> = UnsafeMutablePointer.allocate(capacity: 1)
        unsafeCompat.initialize(to: HLCode_CCompat(
            version: 4, 
            nints: 0, 
            nfloats: 0, 
            nstrings: 0, 
            nbytes: 0, 
            ntypes: 0, 
            nglobals: 0, 
            nnatives: 0, 
            nfunctions: 0, 
            nconstants: 0, 
            entrypoint: 0, 
            ndebugfiles: 0, 
            hasdebug: false, 
            ints: UnsafeMutablePointer.allocate(capacity: 0),
            floats: UnsafeMutablePointer.allocate(capacity: 0), 
            strings: UnsafeMutablePointer.allocate(capacity: 0), 
            string_lens: UnsafeMutablePointer.allocate(capacity: 0), 
            bytes: UnsafeMutablePointer.allocate(capacity: 0), 
            bytes_pos: UnsafeMutablePointer.allocate(capacity: 0), 
            debugfiles: UnsafeMutablePointer.allocate(capacity: 0), 
            debuffiles_lens: UnsafeMutablePointer.allocate(capacity: 0), 
            ustrings: UnsafeMutablePointer.allocate(capacity: 0), 
            types: UnsafeMutablePointer.allocate(capacity: 0), 
            globals: UnsafeMutablePointer.allocate(capacity: 0), 
            natives: UnsafeMutablePointer.allocate(capacity: 0), 
            functions: UnsafeMutablePointer.allocate(capacity: 0), 
            constants: UnsafeMutablePointer.allocate(capacity: 0), 
            alloc: 0, 
            falloc: 0))
        self.init(storage: storage, hlcode: unsafeCompat)
    }

    init(storage: ModuleStorage, hlcode: UnsafePointer<HLCode_CCompat>) {
        self.hlcode = hlcode 

        let jitBase: SharedStorage<UnsafeMutableRawPointer?> = SharedStorage(
            wrappedValue: nil
        )
        
        printerr("Creating FunAddr \(hlcode)")
        self.addresses = FunctionAddresses(hlcode.pointee, jitBase: jitBase)
        printerr("Creating FunAddr2")
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
