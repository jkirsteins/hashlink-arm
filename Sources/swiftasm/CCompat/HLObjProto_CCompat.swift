struct HLObjProto_CCompat: Equatable, CustomDebugStringConvertible, Hashable {
    let namePtr: UnsafePointer<CChar16>
    let findex: Int32
    let pindex: Int32
    let hashed_name: Int32
    
    let _dummy: Int32 // to get the right size, should match C 24 bytes for entire struct
    
    var name: String { .wrapUtf16(from: namePtr) }
    
    init(namePtr: UnsafePointer<CChar16>, findex: Int32, pindex: Int32, hashed_name: Int32) {
        self.namePtr = namePtr
        self.findex = findex
        self.pindex = pindex
        self.hashed_name = hashed_name
        self._dummy = 0
    }
    
    var debugDescription: String {
        "\(name): <fun>@\(findex) (\(pindex))"
    }
}

protocol HLObjProtoProvider {
    var nameProvider: any StringProvider { get }
    var findex: Int32 { get }
    var pindex: Int32 { get }
    var hashedName: Int32 { get }
}

extension HLObjProto_CCompat: HLObjProtoProvider {
    var nameProvider: any StringProvider {
        self.namePtr
    }
    
    var hashedName: Int32 { self.hashed_name }
}

extension UnsafePointer<HLObjProto_CCompat> : HLObjProtoProvider {
    var nameProvider: any StringProvider { self.pointee.nameProvider }
    var findex: Int32 { self.pointee.findex }
    var pindex: Int32 { self.pointee.pindex }
    var hashedName: Int32 { self.pointee.hashedName }
}
