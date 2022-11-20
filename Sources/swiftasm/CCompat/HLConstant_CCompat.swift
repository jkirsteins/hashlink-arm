/**
 typedef struct {
     int global;
     int nfields;
     int *fields;
 } hl_constant;
 */

struct HLConstant_CCompat : Equatable, Hashable {
    let global: Int32
    let nfields: Int32
    let fields: UnsafePointer<Int32>
}
