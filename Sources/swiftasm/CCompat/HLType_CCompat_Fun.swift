/*
typedef struct {
	hl_type **args;
	hl_type *ret;
	int nargs;
	// storage for closure
	hl_type *parent;
	struct {
		hl_type_kind kind;
		void *p;
	} closure_type;
	struct {
		hl_type **args;
		hl_type *ret;
		int nargs;
		hl_type *parent;
	} closure;
} hl_type_fun; */

struct HLType_CCompat_Fun_ClosureType : Equatable, Hashable {
    let kind: HLTypeKind
    let p: UnsafeRawPointer?
}

struct HLType_CCompat_Fun_Closure : Equatable, Hashable {
    let argsPtr: UnsafePointer<UnsafePointer<HLType_CCompat>>?
    let ret: UnsafePointer<HLType_CCompat>?
    let nargs: UInt32
    let parent: UnsafePointer<HLType_CCompat>?

	var args: [HLType_CCompat] {
	    guard let startPtr = self.argsPtr?.pointee else {
			fatalError("argsPtr empty")
		}
        let buf = UnsafeBufferPointer(start: startPtr, count: Int(nargs))
        return Array(buf)
    }
}

struct HLType_CCompat_Fun : Equatable, Hashable {
    let argsPtr: UnsafePointer<UnsafePointer<HLType_CCompat>>
    let retPtr: UnsafePointer<HLType_CCompat>
    let nargs: UInt32
    let parent: UnsafePointer<HLType_CCompat>
    let closure_type: HLType_CCompat_Fun_ClosureType
    let closure: HLType_CCompat_Fun_Closure

	var ret: HLType_CCompat {
//		guard let retPtr = self.retPtr else {
//			fatalError("retPtr is nil")
//		}

		return retPtr.pointee
	}
	var args: [HLType_CCompat] {
//	    guard let startPtr = self.argsPtr?.pointee else {
//			fatalError("argsPtr empty")
//		}
        let buf = UnsafeBufferPointer(start: self.argsPtr.pointee, count: Int(nargs))
        return Array(buf)
    }
}
