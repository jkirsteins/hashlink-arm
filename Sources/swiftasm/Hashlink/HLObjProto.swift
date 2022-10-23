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
    
    init(_ ccompat: HLObjProto_CCompat) {
        self.name = Resolvable(ccompat.name, memory: ccompat.namePtr)
        self.functionIx = ccompat.findex
        self.pIx = ccompat.pindex
    }
}

