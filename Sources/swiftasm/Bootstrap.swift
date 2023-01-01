import Darwin

actor Bootstrap
{
    private(set) static var _file: ContiguousArray<CChar> = []
    private(set) static var canStart = true
    
    static func start2(_ file: String, args: [String]) throws -> CCompatJitContext {
        guard canStart else {
            fatalError("Can't call start twice")
        }
        canStart = false
        
        let _hlc_resolve_symbol: (@convention(c) (OpaquePointer, OpaquePointer, OpaquePointer)->()) = { _, _, _ in
            
            print("TODO: hlc_resolve_symbol does nothing")
        }
        
        let _hlc_capture_stack: (@convention(c) (OpaquePointer, OpaquePointer)->()) = { _, _ in
            
            print("TODO: hlc_capture_stack does nothing")
        }
        
        LibHl.hl_global_init()
        LibHl._hl_setup_exception(
            unsafeBitCast(_hlc_resolve_symbol, to: OpaquePointer.self),
            unsafeBitCast(_hlc_capture_stack, to: OpaquePointer.self));
        LibHl.hl_sys_init(args: args, file: file)
        
        let hsc = unsafeBitCast(M1Compiler2.hlc_static_call, to: OpaquePointer.self)
        let hgw = unsafeBitCast(M1Compiler2.hlc_get_wrapper, to: OpaquePointer.self)
        LibHl._hl_setup_callbacks(hsc, hgw)
        
        let ctx = try CCompatJitContext(file)
        LibHl.hl_register_thread(ctx: ctx.mainContext)
        
        return ctx
    }
    
    static func wrap_entrypoint(ctx: CCompatJitContext, fix: RefFun) throws {
        var voidType: UnsafePointer<HLType_CCompat>? = nil
        for t in 0..<ctx.ntypes {
            let voidCandidate = try ctx.getType(Int(t))
            if voidCandidate.kind == .void {
                voidType = .init(OpaquePointer(voidCandidate.ccompatAddress))
                break
            }
        }
        
        guard let voidType = voidType else {
            fatalError("Failed to find .void type in the module")
        }
        
        let tf: UnsafeMutablePointer<HLTypeFun_CCompat> = .allocate(capacity: 1)
        let tfbytes: UnsafeMutableBufferPointer<UInt8> = .init(start: .init(OpaquePointer(tf)), count: MemoryLayout<HLTypeFun_CCompat>.stride)
        tfbytes.initialize(repeating: 0)
        defer { tf.deallocate() }
        
        let clt: UnsafeMutablePointer<HLType_CCompat> = .allocate(capacity: 1)
        let cltbytes: UnsafeMutableBufferPointer<UInt8> = .init(start: .init(OpaquePointer(clt)), count: MemoryLayout<HLType_CCompat>.stride)
        cltbytes.initialize(repeating: 0)
        defer { clt.deallocate() }
        
        let cl: UnsafeMutablePointer<vclosure> = .allocate(capacity: 1)
        let clbytes: UnsafeMutableBufferPointer<UInt8> = .init(start: .init(OpaquePointer(cl)), count: MemoryLayout<vclosure>.stride)
        clbytes.initialize(repeating: 0)
        defer { cl.deallocate() }
        
        // tf.ret = &hlt_void;
        tf.pointee.retPtr = voidType
        
        // clt.kind = HFUN;
        clt.pointee.kind = .fun
        
        // clt.fun = &tf;
        clt.pointee.union = .init(tf);
        
        // cl.t = &clt;
        cl.pointee.t = .init(clt)
        
        // cl.fun = hl_entry_point;
        guard let hl_entry_point = try ctx.getCallable(findex: fix) else {
            fatalError("Couldn't find entrypoint address for fun@\(fix)")
        }
        cl.pointee.fun = .init(hl_entry_point.address.value)
        
        //
        let isExc: UnsafeMutablePointer<Bool> = .allocate(capacity: 1)
        defer { isExc.deallocate() }
                
        let ret = LibHl.hl_dyn_call_safe(cl, nil, 0, isExc)
        if( isExc.pointee ) {
            let a = LibHl.hl_exception_stack()
            let msg = LibHl.hl_to_string(ret)
            print("Uncaught exception: \(msg.stringValue)")
            
            for i in (0..<a.pointee.size) {
//                uprintf(USTR("Called from %s\n"), hl_aptr(a, uchar*)[i]);
                print("TODO: \(i): print stack trace")
            }
            exit(1)
        }
    }
    
    static func stop(ctx: CCompatJitContext) {
        guard !canStart else {
            fatalError("Can't call stop when not started")
        }
        guard let mToRemove = ctx.mainContext.pointee.m else {
            fatalError("Can't call stop when no module is allocated")
        }
        canStart = true
        
        ctx.mainContext.pointee.m = nil
        LibHl._hl_module_free(mToRemove)
        
        let allocPtr: UnsafeRawPointer = UnsafeRawPointer(ctx.mainContext.pointee.code!).advanced(
            by: 161 // offset to alloc
        )
        
        // TODO: LibHl._hl_free(allocPtr)
        LibHl.hl_unregister_thread()
        LibHl.hl_global_free()
    }
    
    static func start(_ ctx: inout MainContext_CCompat, _ file: String, args: [String]) {
        guard canStart else {
            fatalError("Can't call start twice")
        }
        canStart = false
        
        LibHl.hl_global_init()
        LibHl.hl_sys_init(args: args, file: file)
        LibHl.hl_register_thread(ctx: &ctx)
        
        let fileChars = file.utf8CString
        let filePtr: UnsafeMutableBufferPointer<CChar> = .allocate(capacity: fileChars.count)
        _ = filePtr.initialize(from: fileChars)
        ctx.file = UnsafePointer(filePtr.baseAddress)
        
        let _code = UnsafePointer(LibHl.load_code(file))
        ctx.code = _code
        
        let module = LibHl.hl_module_alloc(_code)
        ctx.m = module
        
        let res = LibHl.hl_module_init(module, false)
        guard res == 1 else {
            fatalError("Failed to init module (got \(res))")
            // exit 3
        }
    }
    
    static func __tmp() {
//        let res = LibHl.hl_module_init(module, false)
//        guard res == 1 else {
//            fatalError("Failed to init module (got \(res))")
//            // exit 3
//        }
//
//        LibHl.hl_code_free(_code)
//        return _code
        
        // cl.t = ctx.code->functions[ctx.m->functions_indexes[ctx.m->code->entrypoint]].type;
        // cl.fun = ctx.m->functions_ptrs[ctx.m->code->entrypoint];
        // cl.hasValue = 0;
        // setup_handler();
        // hl_profile_setup(profile_count);
        // ctx.ret = hl_dyn_call_safe(&cl,NULL,0,&isExc);
        // hl_profile_end();
        // if( isExc ) {
        //     varray *a = hl_exception_stack();
        //     int i;
        //     uprintf(USTR("Uncaught exception: %s\n"), hl_to_string(ctx.ret));
        //     for(i=0;i<a->size;i++)
        //         uprintf(USTR("Called from %s\n"), hl_aptr(a,uchar*)[i]);
        //     hl_debug_break();
        //     hl_global_free();
        //     return 1;
        // }
        // hl_module_free(ctx.m);
        // hl_free(&ctx.code->alloc);
        // hl_unregister_thread();
        // hl_global_free();
        // return 0;
    }
}
