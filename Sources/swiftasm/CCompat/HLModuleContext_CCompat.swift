/**
 typedef struct {
     hl_alloc alloc;
     void **functions_ptrs;
     hl_type **functions_types;
 } hl_module_context;
 */
struct HLModuleContext_CCompat: Equatable, Hashable {
    let alloc: HLAlloc_CCompat
    let functions_ptrs: UnsafePointer<UnsafeRawPointer>
    let functions_types: UnsafePointer<UnsafePointer<HLType_CCompat>>
}
