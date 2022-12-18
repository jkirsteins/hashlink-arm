@testable import swiftasm

struct _String {
    let t: UnsafePointer<HLType_CCompat>
    let bytes: UnsafePointer<CChar16>
    let length: Int32
}

let _StringType = Test_HLTypeObj(
    fieldsProvider: [
        Test_HLObjField(nameProvider: "bytes", typeProvider: HLTypeKind.bytes),
        Test_HLObjField(nameProvider: "length", typeProvider: HLTypeKind.i32)
    ],

    // don't change this as we need this type to compare (==) with the real type
    nameProvider: "String"
)
