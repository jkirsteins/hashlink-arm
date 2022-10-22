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

struct HLOpCode__CCompat : Equatable, Hashable {
    let op: Int32
	let p1: Int32
	let p2: Int32
	let p3: Int32
	let extra: UnsafePointer<Int32>?
}

struct HLFunction_CCompat : Equatable, Hashable {
    // stored 

    let findex: Int32
    let nregs: Int32
    let nops: Int32 
    let ref: Int32 
    let typePtr: UnsafePointer<HLType_CCompat>?
    let regsPtr: UnsafePointer<UnsafePointer<HLType_CCompat>>?
    let opsPtr: UnsafePointer<HLOpCode__CCompat>?
    let debug: UnsafePointer<UInt8>?
    let objPtr: UnsafePointer<HLType_CCompat>?
    let unionPtr: UnsafeRawPointer?

    // following computed 

    var ops: [HLOpCode__CCompat] {
        let buf = UnsafeBufferPointer(start: opsPtr, count: Int(nops))
        return Array(buf)
    } 

    var type: HLType_CCompat { typePtr!.pointee }
    var regs: [HLType_CCompat] { 
        let startPtr = regsPtr!.pointee
        let buf = UnsafeBufferPointer(start: startPtr, count: Int(nregs))
        return Array(buf)
    }
    var obj: HLType_CCompat { objPtr!.pointee }

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
