import Darwin

struct LibHl {
    enum Source: String {
        case dylib = "libhl.dylib"
        case bin = "/usr/local/bin/hl"

        var handle: UnsafeMutableRawPointer {
            guard let handle = (self == .dylib ? LibHl.dylibHandle : LibHl.binHandle) else {
                fatalError("Could not load \(self.rawValue)")
            } 
            return handle
        }
    }
    static let dylibHandle = dlopen(Source.dylib.rawValue, RTLD_NOW)
    static let binHandle = dlopen(Source.bin.rawValue, RTLD_NOW)
    
    static func load<T>(_ name: String, from src: Source = .dylib) -> T {
        guard 
            let nat: UnsafeMutableRawPointer = dlsym(src.handle, name) else {
            fatalError("Could not load \(name)")
        }

        return unsafeBitCast(nat, to: T.self)
    }

    static let hl_global_free: (@convention(c) () -> ()) = { load("hl_global_free") }()
    static let hl_global_init: (@convention(c) () -> ()) = { load("hl_global_init") }()
    
    static let _hl_sys_init: (@convention(c) (UnsafePointer<UnsafePointer<CChar>?>?, Int32, UnsafeRawPointer) -> ()) = { load("hl_sys_init") }()
    
    static func hl_sys_init(args: [String], file: String) {
        withUnsafePointer(to: file) { filePtr in
            // TODO: fix CStringArray to support empty arrays
            if args.count > 0 {
                let argsCcompat = CStringArray(args)
                LibHl._hl_sys_init(
                    argsCcompat.constPointer,
                    Int32(args.count),
                    filePtr)
            } else {
                LibHl._hl_sys_init(
                    nil,
                    0,
                    filePtr)
            }
        }
    }
    
    static let hl_utf8_length: (@convention(c) (UnsafePointer<Int8>?, Int) -> (Int)) = { load("hl_utf8_length") }()
    static func hl_utf8_length(_ val: String, pos: Int = 0) -> Int {
        return val.withCString {
            return hl_utf8_length($0, pos)
        }
    }

    /// hl_code *hl_code_read( const unsigned char *data, int size, char **error_msg );
    
    /// const pchar *file, char **error_msg, bool print_errors
    static let _load_code: (@convention(c) (UnsafePointer<Int8>?, UnsafeMutablePointer<UnsafeMutablePointer<CChar>>, Bool) -> (UnsafeMutableRawPointer?)) = { load("load_code", from: .bin) }()
    static func load_code(_ val: String) -> UnsafeMutablePointer<HLCode_CCompat> {
        
        var err: UnsafeMutablePointer<CChar> = .allocate(capacity: 1)
        defer { err.deallocate() }
        
        var res: UnsafeMutableRawPointer? = nil
        
        withUnsafeMutablePointer(to: &err) {
            errPtr in
        
            res = val.withCString {
                charPtr in
                print("charPtr: \(charPtr)")
                return _load_code(charPtr, errPtr, true)
            }
        }
            
        guard let res = res?.bindMemory(to: HLCode_CCompat.self, capacity: 1) else {
            // TODO: print error
            let str = String.wrapUtf8(from: err)
            fatalError("Failed to load code: \(str)")
        }
        return res
    }

    /// hl_ucs2length
    static let hl_ucs2length: (@convention(c) (UnsafePointer<UniChar>?, Int) -> Int) = { load("hl_ucs2length") }()
    
    /// hl_to_utf16
    static let hl_to_utf16: (@convention(c) (UnsafePointer<Int8>?) -> UnsafeMutablePointer<UniChar>) = { load("hl_to_utf16") }()
    static func hl_to_utf16(_ val: String) -> UnsafeMutablePointer<UniChar> {
        return val.withCString {
            charPtr in 

            return hl_to_utf16(charPtr)
        }
    }

    // HL_API vdynobj *hl_alloc_dynobj( void );
    static let _hl_alloc_dynobj: (@convention(c) () -> UnsafeRawPointer) = { load("hl_alloc_dynobj") }()
    static func hl_alloc_dynobj() -> UnsafeRawPointer { _hl_alloc_dynobj() }
    
    // HL_API void hl_register_thread( void *stack_top ) 
    static let _hl_register_thread: (@convention(c) (UnsafeRawPointer) -> ()) = { load("hl_register_thread") }()
    static func hl_register_thread(ctx: inout MainContext_CCompat) {
        withUnsafeMutablePointer(to: &ctx) {
            ctxPtr in
            Self._hl_register_thread(ctxPtr)
        }
    }
    static func hl_register_thread(ctx ctxPtr: UnsafePointer<MainContext_CCompat>) {
        Self._hl_register_thread(ctxPtr)
    }
    
    // hl_module *hl_module_alloc( hl_code *code );
    static let _hl_module_alloc: (@convention(c) (UnsafeRawPointer) -> UnsafeRawPointer) = { load("hl_module_alloc", from: .bin) }()
    static func hl_module_alloc(_ code: UnsafePointer<HLCode_CCompat>) -> UnsafePointer<HLModule_CCompat> {
        UnsafePointer(OpaquePointer(_hl_module_alloc(code)))
    }
    
    // int hl_module_init( hl_module *m, h_bool hot_reload );
    static let hl_module_init: (@convention(c) (UnsafeRawPointer, Bool) -> Int32) = { load("hl_module_init", from: .bin) }()

    // HL_API vvirtual *hl_alloc_obj( hl_type *t );
    static let _hl_alloc_obj: (@convention(c) (UnsafeRawPointer) -> UnsafeRawPointer) = { load("hl_alloc_obj") }()
    static func hl_alloc_obj(_ hltype: UnsafePointer<HLType_CCompat>) -> UnsafeRawPointer {
        _hl_alloc_obj(hltype)
    }

    // HL_API vvirtual *hl_alloc_virtual( hl_type *t );
    static let _hl_alloc_virtual: (@convention(c) (UnsafeRawPointer) -> UnsafeRawPointer) = { load("hl_alloc_virtual") }()
    static func hl_alloc_virtual(_ hltype: UnsafePointer<HLType_CCompat>) -> UnsafeRawPointer {
        _hl_alloc_virtual(hltype)
    }
    
    // HL_PRIM hl_runtime_obj *hl_get_obj_rt( hl_type *ot ) {
    static let _hl_get_obj_rt: (@convention(c) (UnsafeRawPointer) -> UnsafeRawPointer) = { load("hl_get_obj_rt") }()
    static func hl_get_obj_rt(_ hltype: UnsafePointer<HLType_CCompat>) -> UnsafePointer<HLRuntimeObj_CCompat> {
        UnsafePointer(OpaquePointer(_hl_get_obj_rt(hltype)))
    }
    
    // HL_PRIM varray *hl_alloc_array( hl_type *at, int size ) {
    static let _hl_alloc_array: (@convention(c) (UnsafeRawPointer, Int32) -> UnsafeRawPointer) = { load("hl_alloc_array") }()
    static func hl_alloc_array(_ hltype: UnsafePointer<HLType_CCompat>, _ size: Int32) -> UnsafePointer<varray> {
        UnsafePointer(OpaquePointer(_hl_alloc_array(hltype, size)))
    }
    
    // void hl_code_free( hl_code *c );
    static let _hl_code_free: (@convention(c) (UnsafeRawPointer) -> ()) = { load("hl_code_free", from: .bin) }()
    static func hl_code_free(_ hlcode: UnsafePointer<HLCode_CCompat>) -> () {
        _hl_code_free(UnsafeRawPointer(hlcode))
    }
    
    // HL_PRIM int hl_hash_gen( const uchar *name, bool cache_name ) {
    static let hl_hash_gen: (@convention(c) (UnsafeRawPointer, Bool) -> Int32) = { load("hl_hash_gen") }()
    
    // hl_module_free( hl_module *m );
    static let _hl_module_free: (@convention(c) (UnsafeRawPointer) -> ()) = { load("hl_module_free", from: .bin) }()
    
    // void hl_free( hl_alloc *a ) {
    static let _hl_free: (@convention(c) (UnsafeRawPointer) -> ()) = { load("hl_free") }()
    
    // HL_API void hl_unregister_thread() {
    static let hl_unregister_thread: (@convention(c) () -> ()) = { load("hl_unregister_thread") }()
    
    // vdynamic *hl_alloc_dynamic( hl_type *t )
    static let _hl_alloc_dynamic: (@convention(c) (UnsafeRawPointer) -> (UnsafeRawPointer)) = { load("hl_alloc_dynamic") }()
    static func hl_alloc_dynamic(_ type: UnsafePointer<HLType_CCompat>) -> (UnsafePointer<vdynamic>) {
        .init(OpaquePointer(_hl_alloc_dynamic(.init(type))))
    }
    
    static let hl_null_access: (@convention(c) () -> ()) = { load("hl_null_access") }()
    
    // HL_PRIM venum *hl_alloc_enum( hl_type *t, int index ) {
    static let _hl_alloc_enum: (@convention(c) (UnsafeRawPointer, Int32) -> (UnsafeRawPointer)) = { load("hl_alloc_enum") }()
    static func hl_alloc_enum(_ type: UnsafePointer<HLType_CCompat>, _ index: Int32) -> (UnsafePointer<venum>) {
        .init(OpaquePointer(_hl_alloc_enum(.init(type), index)))
    }
}
