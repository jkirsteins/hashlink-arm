/*
    int findex;
	int nregs;
	int nops;
	int ref;
	hl_type *type;
	hl_type **regs;
	hl_opcode *ops;
	int *debug;

	hl_type_obj *obj;
	union {
		const uchar *name;
		hl_function *ref; // obj = NULL
	} field;
*/



struct HLFunction_CCompat : Equatable, Hashable {
    // stored 

    let findex: Int32
    let nregs: Int32
    let nops: Int32 
    let ref: Int32 
    let typePtr: UnsafePointer<HLType_CCompat>?
    let regsPtr: UnsafePointer<UnsafePointer<HLType_CCompat>>?
    let opsPtr: UnsafePointer<HLOpCode_CCompat>?
    let debug: UnsafePointer<UInt8>?
    let objPtr: UnsafePointer<HLType_CCompat>?
    let unionPtr: UnsafeRawPointer?

    // following computed 

    var cOps: [HLOpCode_CCompat] {
        let buf = UnsafeBufferPointer(start: opsPtr, count: Int(nops))
        return Array(buf)
    } 

    var cType: HLType_CCompat { typePtr!.pointee }
    var cRegs: [HLType_CCompat] {
        regsPtr?.getArray(count: nregs) ?? []
    }
    var cObj: HLType_CCompat? { objPtr?.pointee }

    // union
    var field__name: String? {
        guard let unionPtr = unionPtr else { return nil }
        return .wrapUtf16(from: unionPtr)
    }
    var field__ref: HLFunction_CCompat? {
        guard let unionPtr = unionPtr else { return nil }
        return unionPtr.boundPointee()
    }
} 
