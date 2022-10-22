struct HLType_CCompat : Equatable, Hashable, CustomDebugStringConvertible {
    // hl_type_kind kind
    let kind: HLTypeKind

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
    let union: UnsafeMutableRawPointer?
    // case absName(UnsafeMutableRawPointer)
    // case fun(UnsafeMutableRawPointer)
    func getUnion<T>() -> T? {
        union?.bindMemory(to: T.self, capacity: 1).pointee
    }
    var obj: HLType_CCompat_Obj { getUnion()! }
    var fun: HLType_CCompat_Fun { getUnion()! }

    // void **vobj_proto
    let vobjProto: UnsafeMutableRawPointer?

    // unsigned int *mark_bits
    let markBits: UnsafeMutableRawPointer

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
        case .obj: return "obj(\(obj.name))"
        case .array: return "array"
        case .type: return "type"
        case .ref: return "ref"
        case .virtual: return "virtual"
        case .dynobj: return "dynobj"
        case .abstract: return "abs(<wip>)"
        case .`enum`: return "enum(<wip>)"
        case .null: return "null(<wip>)"
        case .method: return "method"
        case .`struct`: return "struct(\(obj.name))"
        default: fatalError("Unknown kind \(self.kind.rawValue)")
        }
    }
}
