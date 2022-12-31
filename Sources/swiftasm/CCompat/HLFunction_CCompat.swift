/**
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
    
    var fieldName: UnsafePointer<CChar16>? {
        .init(OpaquePointer(unionPtr))
    }

    var cOps: [HLOpCode_CCompat] {
        let buf = UnsafeBufferPointer(start: opsPtr, count: Int(nops))
        return Array(buf)
    } 

    var cType: HLType_CCompat { typePtr!.pointee }
} 

extension HLFunction_CCompat : Compilable {
    func getFindex() -> Int { Int(self.findex) }
    
    var regs: [Resolvable<HLType>] {
        (0..<nregs).map { ix in
            let ptrPtr = self.regsPtr!.advanced(by: Int(ix))
            print("Function \(getFindex()) loading reg \(ix)")
            return .type(fromUnsafe: ptrPtr.pointee)
        }
    }
    
    var entrypoint: any MemoryAddress {
        fatalError("No memory, need to wrap this in HLFunction_CCompat__WithMemory")
    }
    
    var args: [Resolvable<HLType>] {
        let res = (0..<cType.fun.pointee.nargs).map { ix in
            let ptr: UnsafePointer<HLType_CCompat> = self.cType.fun.pointee.argsPtr.advanced(by: Int(ix)).pointee
            return Resolvable.type(fromUnsafe: ptr)
        }
        
        return res
    }
    var ret: Resolvable<HLType> {
        .type(fromUnsafe: self.cType.fun.pointee.retPtr)
    }
    
    var ops: [HLOpCode] {
        let bufPtr = UnsafeBufferPointer(start: self.opsPtr!, count: Int(nops))
        let res = bufPtr.map { HLOpCode.parseCCompat($0) }
        if self.findex == 29 {
            print(res, "for", self.findex)
        }
        return res
//        let res = (0..<nops).map { ix in
//            let ptr: UnsafePointer<HLOpCode_CCompat> = self.opsPtr!.advanced(by: Int(ix))
//            print("Parsing from \(ptr)")
//            return HLOpCode.parseCCompat(ptr.pointee)
//        }
        
//        return res
//        return cOps.map { HLOpCode.parseCCompat($0) }
    }
}
