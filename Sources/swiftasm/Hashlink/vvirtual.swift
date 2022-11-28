/**
  Virtual
 
        struct _vvirtual {
            hl_type *t;
            vdynamic *value;
            vvirtual *next;
        };
*/
struct vvirtual {
    let t: UnsafePointer<HLType_CCompat>
    let value: UnsafePointer<vdynamic>
    let next: UnsafePointer<vvirtual>
}
