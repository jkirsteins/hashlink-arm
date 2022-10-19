struct HLType_CCompat : Equatable, Hashable {
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
    let union: UnsafeMutableRawPointer
    // case absName(UnsafeMutableRawPointer)
    // case fun(UnsafeMutableRawPointer)
    func getUnion<T>() -> T {
        union.bindMemory(to: T.self, capacity: 1).pointee
    }
    var obj: HLType_CCompat_Obj { getUnion() }
    var fun: HLType_CCompat_Fun { getUnion() }

    // void **vobj_proto
    let vobjProto: UnsafeMutableRawPointer

    // unsigned int *mark_bits
    let markBits: UnsafeMutableRawPointer
}