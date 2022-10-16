struct JitContext {
    let wft: WholeFunctionsTable
    let jitBase: SharedStorage<UnsafeMutableRawPointer?>

    let storage: ModuleStorage

    init(storage: ModuleStorage) {
        let jitBase: SharedStorage<UnsafeMutableRawPointer?> = SharedStorage(wrappedValue: nil)
        self.wft = WholeFunctionsTable(
            natives: storage.nativeResolver, 
            functions: storage.compiledFunctionResolver,
            jitBase: jitBase)
        self.jitBase = jitBase 
        self.storage = storage
    }
}