struct JitContext {
    let wft: WholeFunctionsTable
    let jitBase: SharedStorage<UnsafeMutableRawPointer?>

    let storage: ModuleStorage
    @SharedStorage var compiledFunctions: [HLCompiledFunction]
    let compiledFunctionResolver: TableResolver<HLCompiledFunction>

    let hlcode: UnsafePointer<HLCode_CCompat>

    init(module: Module, hlcode: UnsafePointer<HLCode_CCompat>) {
        self.init(storage: module.storage, hlcode: hlcode)
    }

    init(storage: ModuleStorage) {
        self.init(storage: storage, hlcode: UnsafePointer(bitPattern: 0xDEAD)!)
    }

    init(storage: ModuleStorage, hlcode: UnsafePointer<HLCode_CCompat>) {
        self.hlcode = hlcode 

        let jitBase: SharedStorage<UnsafeMutableRawPointer?> = SharedStorage(
            wrappedValue: nil
        )
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
