fileprivate var _obj2: UnsafePointer<HLTypeObj_CCompat>? = nil

struct HLType_CCompat : Equatable, Hashable, CustomDebugStringConvertible {
    // hl_type_kind kind
    var kind: HLTypeKind

    /*
    union {
		const uchar *abs_name;
		hl_type_fun *fun;
		hl_type_obj *obj;
		hl_type_enum *tenum;
		hl_type_virtual *virt;
		hl_type	*tparam;
	};
    */
    var union: UnsafeMutableRawPointer?
    
    /// NOTENOTE: should not be an optional! Testing....
    ///
    ///
    /// NOTE: very important that the underlying `union` pointer is not optional,
    /// otherwise the pointee down the chain can be set to nil at unexpected times, causing
    /// weird bugs. See `HLType_CCompatTests`.
    ///
    /// I suspect it's because there's no strong reference to the unsafe pointer we initialize based on
    /// the optional, but not completely clear to me what's happening there.
    func getUnion<T>() -> UnsafePointer<T> {
        return UnsafePointer(union!.bindMemory(to: T.self, capacity: 1))
    }
    var obj: UnsafePointer<HLTypeObj_CCompat> { getUnion() }
    var virt: UnsafePointer<HLTypeVirtual_CCompat> { getUnion() }
    var fun: UnsafePointer<HLTypeFun_CCompat> { getUnion() }
    var tparam: UnsafePointer<HLType_CCompat> { getUnion() }
    var tenum: UnsafePointer<HLTypeEnum_CCompat> { getUnion() }
    
    // void **vobj_proto
    let vobjProto: UnsafeMutableRawPointer?

    // unsigned int *mark_bits
    let markBits: UnsafeMutableRawPointer?
    
    init(kind: HLTypeKind, union: UnsafeMutableRawPointer?, vobjProto: UnsafeMutableRawPointer?, markBits: UnsafeMutableRawPointer?) {
        self.kind = kind
        self.union = union
        self.vobjProto = vobjProto
        self.markBits = markBits
    }
    
    init(kind: HLTypeKind) {
        self.kind = kind
        self.union = .none
        self.vobjProto = .none
        self.markBits = .none
    }

    var debugDescription: String {
        switch self.kind {
        case .void: return "void"
        case .u8: return "u8"
        case .u16: return "u16"
        case .i32: return "i32"
        case .i64: return "i64"
        case .f32: return "f32"
        case .f64: return "f64"
        case .bool: return "bool"
        case .bytes: return "bytes"
        case .dyn: return "dynamic"
        case .fun: return "fun"
        case .obj: return "obj(\(obj.pointee.name))"
        case .array: return "array"
        case .type: return "type"
        case .ref: return "ref"
        case .virtual: return "virtual"
        case .dynobj: return "dynobj"
        case .abstract: return "abs(<wip>)"
        case .`enum`: return "enum(<wip>)"
        case .null: return "null(<wip>)"
        case .method: return "method"
        case .`struct`: return "struct(\(obj.pointee.name))"
        default: fatalError("Unknown kind \(self.kind.rawValue)")
        }
    }
}
