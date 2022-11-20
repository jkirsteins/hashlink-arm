class CCompatWriter_MainContext {
    let ctx: any JitContext2
    let code: UnsafeMutablePointer<HLCode_CCompat> = .allocate(capacity: 1)
    let mPtr: UnsafeMutablePointer<UnsafeMutablePointer<HLModule_CCompat>> = .allocate(capacity: 1)
    let file: String
    
    let codeWriter: CCompatWriter_HLCode
    
    let logger = LoggerFactory.create(CCompatWriter_MainContext.self)
    
    /// Use this writer to initialize CCompat memory from an existing context.
    ///
    /// Use this for tests, but not for loading a proper module from disk.
    init(_ ctx: any JitContext2, file: String) throws {
        self.ctx = ctx
        self.file = file
        self.codeWriter = try CCompatWriter_HLCode(ctx)
    }
    
    deinit {
        mPtr.deinitialize(count: 1)
        mPtr.deallocate()
        
        code.deinitialize(count: 1)
        code.deallocate()
    }
    
    func initialize(target: UnsafeMutablePointer<MainContext_CCompat>) throws {
        guard target.pointee.code == nil else {
            fatal("Target already written: \(String(describing: target.pointee.code))", logger)
        }
        
        try codeWriter.initialize(target: self.code)
        mPtr.pointee = .init(mutating: LibHl.hl_module_alloc(.init(self.code)))
        
        // TODO: can this be nil?
//        assert(mPtr.pointee.pointee.globals_indexes == nil)
//        assert(mPtr.pointee.pointee.globals_data == nil)
        
        file.withCString { fileCstr in
            target.initialize(to: MainContext_CCompat(
                code: self.code,
                m: mPtr.pointee,
                ret: nil,
                file: fileCstr,
                file_time: 0
            ))
        }
        
        // This initializes the function indexes etc.
        guard LibHl.hl_module_init(mPtr.pointee, false) == 1 else {
            throw GlobalError.unexpected("Failed to initialize the hl_module* after writing hl_code*")
        }
        
        assert(mPtr.pointee.pointee.globals_indexes != nil)
        assert(mPtr.pointee.pointee.globals_data != nil)
        
        // If we want to "mock" hl_natives, we have to pass them in with known address and lib "?". In this case
        // the memory pointer is uninitialized after LibHl.hl_module_init, so we need to overwrite those
        for native in try ctx.getOrderedNativesByRealIx__slow() {
            let realIx = self.mPtr.pointee.pointee.functions_indexes.advanced(by: native.findex).pointee
            let mutating: UnsafeMutablePointer = .init(mutating: self.mPtr.pointee.pointee.functions_ptrs.advanced(by: Int(realIx)))
            mutating.pointee = .init(native.address.value)
            
            logger.debug("Mutated address for fix=\(native.findex); realIx=\(realIx); setting address to \(String(describing: mutating.pointee))")
        }
        
        logger.debug("Finished initializing")
    }
}
