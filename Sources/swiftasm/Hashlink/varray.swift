/**
 typedef struct {
     hl_type *t;
     hl_type *at;
     int size;
     int __pad; // force align on 16 bytes for double
 } varray;
 */

struct varray {
    let t: UnsafePointer<HLType_CCompat>    // master type (.array)
    let at: UnsafePointer<HLType_CCompat>   // stored type
    let size: UInt32
    let __pad: UInt32
}
