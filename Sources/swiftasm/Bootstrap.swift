actor Bootstrap
{
//    private(set) static var ctx = MainContext_CCompat()
    private(set) static var _file: ContiguousArray<CChar> = []
    private(set) static var canStart = true
    
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
