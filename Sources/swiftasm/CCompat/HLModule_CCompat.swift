/**
 typedef struct {
     hl_code *code;
     int codesize;
     int globals_size;
     int *globals_indexes;
     unsigned char *globals_data;
     void **functions_ptrs;
     int *functions_indexes;
     void *_jit_code;
     hl_code_hash *hash;
     void *_jit_debug;     //hl_debug_infos *jit_debug;
     void *_jit_ctx;     //jit_ctx *jit_ctx;
     hl_module_context ctx;
 } hl_module;
 */
struct HLModule_CCompat : Equatable, Hashable {
    let code: UnsafePointer<HLCode_CCompat>
    let codesize: Int32
    let globals_size: Int32
    let globals_indexes: UnsafePointer<Int32>?
    
    /// In reality this is `UnsafePointer<UnsafePointer<vdynamic>?>?` but
    /// the `globals_indexes` contains a byte-count offset not vdynamic-sized offsets.
    let globals_data: UnsafeRawPointer?
    
    let functions_ptrs: UnsafePointer<UnsafeRawPointer?>
    let functions_indexes: UnsafePointer<Int32>
    let _jit_code: UnsafeRawPointer
    let hash: UnsafeRawPointer
    let _jit_debug: UnsafeRawPointer
    let _jit_ctx: UnsafeRawPointer
    let ctx: HLModuleContext_CCompat
    
    func getFunctionType(_ fix: RefFun) -> UnsafePointer<HLType_CCompat> {
        let ix = self.functions_indexes.advanced(by: fix).pointee
        return self.ctx.functions_types.advanced(by: Int(ix)).pointee
    }
}
