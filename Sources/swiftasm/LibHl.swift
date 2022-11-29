import Darwin

struct LibSystem {
    enum Source: String {
        // /usr/lib/system/libsystem_platform.dylib
        case libsystem = "libsystem_platform.dylib"
        
        var handle: UnsafeMutableRawPointer {
            guard let handle = (self == .libsystem ? LibHl.dylibHandle : LibHl.dylibHandle) else {
                fatalError("Could not load \(self.rawValue)")
            }
            return handle
        }
    }
    static let libsystemHandle = dlopen(Source.libsystem.rawValue, RTLD_NOW)
    
    static func load<T>(_ name: String, from src: Source = .libsystem) -> T {
        guard
            let nat: UnsafeMutableRawPointer = dlsym(src.handle, name) else {
            fatalError("Could not load \(name)")
        }
        
        return unsafeBitCast(nat, to: T.self)
    }
    
    // NOTE: do NOT wrap this in a calling function, as
    // that kills the magic
    static let setjmp: (@convention(c) (OpaquePointer) -> (Int32)) = { load("_setjmp") }()
    
    static let longjmp: (@convention(c) (OpaquePointer, Int32) -> ()) = { load("longjmp") }()
    
    
//    // void longjmp(jmp_buf env, int val);
//    static let _longjmp: (@convention(c) (OpaquePointer, Int32) -> (Int32)) = { load("setjmp") }()
//    static func setjmp(_ buf: UnsafeMutablePointer<Int32>) -> Int32 {
//        return _setjmp(.init(buf))
//    }
}

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
    
    // void *hl_malloc( hl_alloc *a, int size ) {
    static let _hl_malloc: (@convention(c) (OpaquePointer, Int32) -> (OpaquePointer)) = { load("hl_malloc", from: .bin) }()
    static func hl_malloc(_ hlalloc: UnsafePointer<HLAlloc_CCompat>, _ size: Int32) -> (OpaquePointer) {
        _hl_malloc(.init(hlalloc), size)
    }
    
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
    
    // HL_PRIM vclosure *hl_alloc_closure_ptr( hl_type *fullt, void *fvalue, void *v ) {
    static let _hl_alloc_closure_ptr: (@convention(c) (OpaquePointer, OpaquePointer, OpaquePointer) -> (OpaquePointer)) = { load("hl_alloc_closure_ptr") }()
    static func hl_alloc_closure_ptr(
        _ type: UnsafePointer<HLType_CCompat>,
        _ fvalue: OpaquePointer,
        _ v: OpaquePointer) -> (UnsafePointer<vclosure>) {
        .init(_hl_alloc_closure_ptr(.init(type), fvalue, v))
    }
    
    // HL_PRIM float hl_dyn_castf( void *data, hl_type *t ) {
    static let _hl_dyn_castf: (@convention(c) (OpaquePointer, OpaquePointer) -> (Float32)) = { load("hl_dyn_castf") }()
    static func hl_dyn_castf(
        _ data: OpaquePointer,
        _ type: UnsafePointer<HLType_CCompat>) -> (Float32) {
            _hl_dyn_castf(data, .init(type))
    }
    
    // HL_PRIM double hl_dyn_castd( void *data, hl_type *t ) {
    static let _hl_dyn_castd: (@convention(c) (OpaquePointer, OpaquePointer) -> (Float64)) = { load("hl_dyn_castd") }()
    static func hl_dyn_castd(
        _ data: OpaquePointer,
        _ type: UnsafePointer<HLType_CCompat>) -> (Float64) {
            _hl_dyn_castd(data, .init(type))
    }
    
    // HL_PRIM int hl_dyn_casti( void *data, hl_type *t, hl_type *to ) {
    static let _hl_dyn_casti: (@convention(c) (OpaquePointer, OpaquePointer, OpaquePointer) -> (Int32)) = { load("hl_dyn_casti") }()
    static func hl_dyn_casti(
        _ data: OpaquePointer,
        _ type: UnsafePointer<HLType_CCompat>,
        _ to: UnsafePointer<HLType_CCompat>) -> (Int32) {
            _hl_dyn_casti(data, .init(type), .init(to))
    }
    
    // HL_PRIM void *hl_dyn_castp( void *data, hl_type *t, hl_type *to ) {
    static let _hl_dyn_castp: (@convention(c) (OpaquePointer, OpaquePointer, OpaquePointer) -> (OpaquePointer)) = { load("hl_dyn_castp") }()
    static func hl_dyn_castp(
        _ data: OpaquePointer,
        _ type: UnsafePointer<HLType_CCompat>,
        _ to: UnsafePointer<HLType_CCompat>) -> (OpaquePointer) {
            _hl_dyn_castp(data, .init(type), .init(to))
    }
    
    // MARK: hl_dyn_set
    
    // HL_PRIM float hl_dyn_getf( vdynamic *d, int hfield )
    static let _hl_dyn_getf: (@convention(c) (OpaquePointer, Int32) -> (Float32)) = { load("hl_dyn_getf") }()
    static func hl_dyn_getf(
        _ d: UnsafePointer<vdynamic>,
        _ hfield: Int32) -> (Float32) {
            hl_dyn_getf(.init(d), hfield)
    }
    
    // HL_PRIM double hl_dyn_getd( vdynamic *d, int hfield )
    static let _hl_dyn_getd: (@convention(c) (OpaquePointer, Int32) -> (Float64)) = { load("hl_dyn_getd") }()
    static func hl_dyn_getd(
        _ d: UnsafePointer<vdynamic>,
        _ hfield: Int32) -> (Float64) {
            hl_dyn_getd(.init(d), hfield)
    }
    
    // HL_PRIM int hl_dyn_geti( vdynamic *d, int hfield, hl_type *t ) {
    static let _hl_dyn_geti: (@convention(c) (OpaquePointer, Int32, OpaquePointer) -> (Int32)) = { load("hl_dyn_geti") }()
    static func hl_dyn_geti(
        _ d: UnsafePointer<vdynamic>,
        _ hfield: Int32,
        _ t: UnsafePointer<HLType_CCompat>) -> (Int32) {
            hl_dyn_geti(.init(d), hfield, .init(t))
    }
    
    // HL_PRIM void *hl_dyn_getp( vdynamic *d, int hfield, hl_type *t ) {
    static let _hl_dyn_getp: (@convention(c) (OpaquePointer, Int32, OpaquePointer) -> (OpaquePointer)) = { load("hl_dyn_getp") }()
    static func hl_dyn_getp(
        _ d: UnsafePointer<vdynamic>,
        _ hfield: Int32,
        _ t: UnsafePointer<HLType_CCompat>) -> (OpaquePointer) {
            hl_dyn_getp(.init(d), hfield, .init(t))
    }
    
    
    // MARK: hl_dyn_get
    
    // HL_PRIM void hl_dyn_setf( vdynamic *d, int hfield, float value )
    static let _hl_dyn_setf: (@convention(c) (OpaquePointer, Int32, Float32) -> ()) = { load("hl_dyn_setf") }()
    static func hl_dyn_setf(
        _ d: UnsafePointer<vdynamic>,
        _ hfield: Int32,
        _ t: UnsafePointer<HLType_CCompat>,
        _ value: Float32) -> () {
            hl_dyn_setf(.init(d), hfield, .init(t), value)
    }
    
    // HL_PRIM void hl_dyn_setd( vdynamic *d, int hfield, double value )
    static let _hl_dyn_setd: (@convention(c) (OpaquePointer, Int32, Float64) -> ()) = { load("hl_dyn_setd") }()
    static func hl_dyn_setd(
        _ d: UnsafePointer<vdynamic>,
        _ hfield: Int32,
        _ t: UnsafePointer<HLType_CCompat>,
        _ value: Float64) -> () {
            hl_dyn_setd(.init(d), hfield, .init(t), value)
    }
    
    // HL_PRIM void hl_dyn_seti( vdynamic *d, int hfield, hl_type *t, int value )
    static let _hl_dyn_seti: (@convention(c) (OpaquePointer, Int32, Int32) -> ()) = { load("hl_dyn_seti") }()
    static func hl_dyn_seti(
        _ d: UnsafePointer<vdynamic>,
        _ hfield: Int32,
        _ t: UnsafePointer<HLType_CCompat>,
        _ value: Int32) -> () {
            hl_dyn_seti(.init(d), hfield, .init(t), value)
    }
    
    // HL_PRIM void hl_dyn_setp( vdynamic *d, int hfield, hl_type *t, void *value )
    static let _hl_dyn_setp: (@convention(c) (OpaquePointer, Int32, OpaquePointer) -> ()) = { load("hl_dyn_setp") }()
    static func hl_dyn_setp(
        _ d: UnsafePointer<vdynamic>,
        _ hfield: Int32,
        _ t: UnsafePointer<HLType_CCompat>,
        _ value: OpaquePointer) -> () {
            hl_dyn_setp(.init(d), hfield, .init(t), value)
    }
    
    // HL_API hl_thread_info *hl_get_thread();
    static let _hl_get_thread: (@convention(c) () -> (OpaquePointer)) = { load("hl_get_thread") }()
    static func hl_get_thread() -> (UnsafePointer<HLThreadInfo_CCompat>) {
        .init(_hl_get_thread())
    }
    
    // HL_PRIM void hl_throw( vdynamic *v ) {
    static let _hl_throw: (@convention(c) (OpaquePointer)->()) = { load("hl_throw") }()
    static func hl_throw(_ p: UnsafePointer<vdynamic>) -> () {
        _hl_throw(.init(p))
    }
    
    // HL_PRIM void hl_rethrow( vdynamic *v ) {
    static let _hl_rethrow: (@convention(c) (OpaquePointer)->()) = { load("hl_rethrow") }()
    static func hl_rethrow(_ p: UnsafePointer<vdynamic>) -> () {
        _hl_rethrow(.init(p))
    }
    
    // HL_PRIM hl_obj_field *hl_obj_field_fetch( hl_type *t, int fid ) {
    static let _hl_obj_field_fetch: (@convention(c) (OpaquePointer, Int32)->(OpaquePointer)) = { load("hl_obj_field_fetch") }()
    static func hl_obj_field_fetch(_ t: UnsafePointer<HLType_CCompat>, _ fid: Int32) -> (UnsafePointer<HLObjField_CCompat>) {
        .init(_hl_obj_field_fetch(.init(t), fid))
    }
    
    // vvirtual *hl_to_virtual( hl_type *vt, vdynamic *obj ) {
    static let _hl_to_virtual: (@convention(c) (OpaquePointer, OpaquePointer)->(OpaquePointer)) = { load("hl_to_virtual") }()
    
    // MARK: code.c
    
    // const uchar *hl_get_ustring( hl_code *code, int index ) {
    static let _hl_get_ustring: (@convention(c) (OpaquePointer, Int32)->(OpaquePointer)) = { load("hl_get_ustring", from: .bin) }()
    static func hl_get_ustring(_ code: UnsafePointer<HLCode_CCompat>, _ index: Int32) -> (UnsafePointer<CChar16>) {
        .init(_hl_get_ustring(.init(code), index))
    }
}
