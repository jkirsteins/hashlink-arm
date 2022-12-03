struct HLFieldLookup_CCompat : Equatable, Hashable {
    let t: UnsafePointer<HLType_CCompat>
    let hashed_name: Int32
    let field_index: Int32
}
