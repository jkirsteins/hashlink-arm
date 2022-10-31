/**
 typedef struct {
     const uchar *name;
     int findex;
     int pindex;
     int hashed_name;
 } hl_obj_proto;
 */
struct HLObjProto: Equatable, CustomDebugStringConvertible, Hashable {
    let name: Resolvable<String>
    let functionIx: Int32
    let pIx: Int32

    var debugDescription: String { "\(name.value): <fun>@\(functionIx) (\(pIx))" }
    
    init(name: Resolvable<String>, functionIx: Int32, pIx: Int32) {
        self.name = name
        self.functionIx = functionIx
        self.pIx = pIx
    }
    
    init(_ ccompat: UnsafePointer<HLObjProto_CCompat>) {
        self.name = Resolvable(ccompat.pointee.name, memory: ccompat.pointee.namePtr)
        self.functionIx = ccompat.pointee.findex
        self.pIx = ccompat.pointee.pindex
    }
}

