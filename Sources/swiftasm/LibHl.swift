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
    
    static let _hl_sys_init: (@convention(c) (UnsafePointer<UnsafeRawPointer>?, Int32, UnsafeRawPointer) -> ()) = { load("hl_sys_init") }()
    
    static let hl_utf8_length: (@convention(c) (UnsafePointer<Int8>?, Int) -> (Int)) = { load("hl_utf8_length") }()
    static func hl_utf8_length(_ val: String, pos: Int = 0) -> Int {
        return val.withCString {
            return hl_utf8_length($0, pos)
        }
    }

    /// hl_code *hl_code_read( const unsigned char *data, int size, char **error_msg );
    
    /// const pchar *file, char **error_msg, bool print_errors
    static let load_code: (@convention(c) (UnsafePointer<Int8>?, UnsafePointer<UnsafePointer<Int8>?>?, Bool) -> (UnsafeMutableRawPointer?)) = { load("load_code", from: .bin) }()
    static func load_code(_ val: String) -> UnsafeMutablePointer<HLCode_CCompat> {
        let res = val.withCString {
            charPtr in 
            print("charPtr: \(charPtr)")
            return load_code(charPtr, nil, true)
        }
        print("res: \(res)")
        guard let res = res?.bindMemory(to: HLCode_CCompat.self, capacity: 1) else {
            fatalError("Failed to load code")
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
    static let hl_register_thread: (@convention(c) (UnsafeMutableRawPointer) -> ()) = { load("hl_register_thread") }()
    
    // hl_module *hl_module_alloc( hl_code *code );
    static let hl_module_alloc: (@convention(c) (UnsafeRawPointer) -> UnsafeRawPointer) = { load("hl_module_alloc", from: .bin) }()
    
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
}
