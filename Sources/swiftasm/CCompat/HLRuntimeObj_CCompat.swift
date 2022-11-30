/**
 struct hl_runtime_obj {
     hl_type *t;
     // absolute
     int nfields;
     int nproto;
     int size;
     int nmethods;
     int nbindings;
     bool hasPtr;
     void **methods;
     int *fields_indexes;
     hl_runtime_binding *bindings;
     hl_runtime_obj *parent;
     const uchar *(*toStringFun)( vdynamic *obj );
     int (*compareFun)( vdynamic *a, vdynamic *b );
     vdynamic *(*castFun)( vdynamic *obj, hl_type *t );
     vdynamic *(*getFieldFun)( vdynamic *obj, int hfield );
     // relative
     int nlookup;
     int ninterfaces;
     hl_field_lookup *lookup;
     int *interfaces;
 };
 */
struct HLRuntimeObj_CCompat {
    let t: UnsafePointer<HLType_CCompat>
    let nfields: Int32
    let nproto: Int32
    let size: Int32
    let nmethods: Int32
    let nbindings: Int32
    let hasPtr: Bool
    let methods: UnsafeMutableRawPointer
    let fields_indexes: UnsafePointer<Int32>
    let bindings: UnsafePointer<HLRuntimeBinding_CCompat>
    let parent: UnsafeRawPointer
    let toStringFunc: (@convention(c) (UnsafeRawPointer) -> UnsafePointer<CChar16>)
    let compareFun: (@convention(c) (UnsafeRawPointer, UnsafeRawPointer) -> UnsafePointer<Int32>)
    let castFun: (@convention(c) (UnsafeRawPointer, UnsafeRawPointer) -> UnsafeRawPointer)
    let getFieldFun: (@convention(c) (UnsafeRawPointer, Int32) -> UnsafeRawPointer)
    let nlookup: Int32
    let ninterfaces: Int32
    let lookup: UnsafeRawPointer
    let interfaces: UnsafePointer<Int32>
    
}
