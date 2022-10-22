struct HLObjField_CCompat : Equatable, Hashable, CustomDebugStringConvertible {
    // const uchar *name;
    let namePtr: UnsafePointer<CChar16>
    let tPtr: UnsafePointer<HLType_CCompat>
    let hashedName: Int64   // should be int, but we need to match mem size?
    
    var t: HLType_CCompat { tPtr.pointee }
    var name: String { .wrapUtf16(from: namePtr) }

    var debugDescription: String {
        ".objField(\(name)"
    }
}
