/**
 Runtime binding
 
        typedef struct {
            void *ptr;
            hl_type *closure;
            int fid;
        } hl_runtime_binding;
 */

struct HLRuntimeBinding_CCompat: Equatable, Hashable {
    let ptr: OpaquePointer
    let closure: UnsafePointer<HLType_CCompat>
    let fid: Int32
}
