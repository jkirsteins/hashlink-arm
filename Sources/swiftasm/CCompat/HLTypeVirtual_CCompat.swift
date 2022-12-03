struct HLTypeVirtual_CCompat : Equatable, Hashable {
    let fields: UnsafePointer<HLObjField_CCompat>
    let nfields: Int32
        
    // runtime
    let dataSize: Int32
    let indexes: UnsafePointer<Int32>
    let lookup: UnsafePointer<HLFieldLookup_CCompat>
}
