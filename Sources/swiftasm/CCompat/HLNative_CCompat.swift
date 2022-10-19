/*
typedef struct {
	const char *lib;
	const char *name;
	hl_type *t;
	int findex;
} hl_native;
*/
struct HLNative_CCompat : Equatable, Hashable {
    let libPtr: UnsafePointer<CChar>
    let namePtr: UnsafePointer<CChar>
    let typePtr: UnsafePointer<HLType_CCompat>
    let findex: Int64

    var lib: String { .wrapUtf8(from: libPtr) }
    var name: String { .wrapUtf8(from: namePtr) }
    var type: HLType_CCompat { typePtr.pointee }
}