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

    var args: [HLType_CCompat] { self.argsPtr?.getArray(count: Int32(nargs)) ?? [] }
}

struct HLTypeFun_CCompat : Equatable, Hashable {
    let argsPtr: UnsafePointer<UnsafePointer<HLType_CCompat>>
    let retPtr: UnsafePointer<HLType_CCompat>
    let nargs: UInt32
    let parent: UnsafePointer<HLType_CCompat>?
    let closure_type: HLType_CCompat_Fun_ClosureType
    let closure: HLType_CCompat_Fun_Closure

	var ret: HLType_CCompat { return retPtr.pointee }
	var args: [HLType_CCompat] { self.argsPtr.getArray(count: Int32(nargs)) }
}

extension HLTypeFun_CCompat : HLTypeFunProvider {
    var argsProvider: [any HLTypeProvider] {
        Array(UnsafeBufferPointer(start: self.argsPtr, count: Int(nargs)))
    }
    
    var retProvider: any HLTypeProvider {
        print("Returning ret provider \(self.retPtr) \(self.retPtr.kind)")
        return self.retPtr
    }
    
    var debugDescription: String {
        "HLTypeFun_CCompat"
    }
}


extension HLTypeFun_Depr {
    init(_ ccompat: HLTypeFun_CCompat) {
        self.args = ccompat.args.enumerated().map { ix, item in
            let ptr = ccompat.argsPtr.advanced(by: ix)
            return .type(fromUnsafe: ptr.pointee)
        }
        self.ret = .type(fromUnsafe: ccompat.retPtr)
    }
}
