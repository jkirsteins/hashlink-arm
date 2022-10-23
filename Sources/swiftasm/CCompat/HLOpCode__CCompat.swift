struct HLOpCode_CCompat : Equatable, Hashable {
    let op: Int32
    let p1: Int32
    let p2: Int32
    let p3: Int32
    let extra: UnsafePointer<Int32>?
}
