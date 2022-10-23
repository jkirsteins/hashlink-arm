import XCTest

@testable import swiftasm

final class HLType_CCompatTests: XCTestCase {
    static var code: UnsafePointer<HLCode_CCompat>?
    
    override class func setUp() {
        LibHl.hl_global_init()
        
        let mod1 = Bundle.module.url(forResource: "mod1", withExtension: "hl")!.path
        self.code = UnsafePointer(LibHl.load_code(mod1))
    }

    override class func tearDown() {
        LibHl.hl_global_free()
    }
    
    var code: UnsafePointer<HLCode_CCompat> { Self.code! }
    
    func testHLTypeObj__String() throws {
        let type = code.pointee.getType(13)
        let native = HLType(type.pointee)
        
        XCTAssertEqual(native.kind, .obj)
        XCTAssertEqual(native.objData!.name.value, "String")
        XCTAssertEqual(native.objData!.superType?.value, nil)
        XCTAssertEqual(native.objData!.global, 2)
        
        XCTAssertEqual(native.objData!.fields, Resolvable.array([
            HLObjField(name: Resolvable("bytes"), type: Resolvable(.bytes)),
            HLObjField(name: Resolvable("length"), type: Resolvable(.i32))
        ]))
        
        XCTAssertEqual(native.objData!.bindings, [])
    }
    
    func test(_ val: HLType_CCompat?) {
        print("Got val \(val)")
    }

    func testHLTypeObj__$SysError() throws {
        let type = code.pointee.getType(44)
        let native = HLType(type.pointee)
        
        
        XCTAssertEqual(native.kind, .obj)
        XCTAssertEqual(native.objData!.name.value, "$SysError")

        XCTAssertEqual(native.objData!.superType?.value.kind, .obj)
        XCTAssertNotNil(native.objData!.superType?.value.objData?.superType?.value)
        XCTAssertEqual(native.objData!.superType?.value.objData?.bindings, [])
        XCTAssertEqual(native.objData!.superType?.value.objData?.proto, [])
        XCTAssertEqual(native.objData!.superType?.value.objData?.name, Resolvable("hl.Class"))



        XCTAssertEqual(
            native.objData!.superType!.value.objData!.fields[0].value.name.value,
            "__name__")
        XCTAssertEqual(
            native.objData!.superType!.value.objData!.fields[1].value.name.value,
            "__constructor__")
        XCTAssertEqual(
            native.objData!.superType!.value.objData!.fields[1].value.type.value,
            .dyn)

        XCTAssertEqual(native.objData!.global, nil)

        XCTAssertEqual(native.objData!.fields, [])
        XCTAssertEqual(native.objData!.bindings, [
            HLTypeBinding(fieldRefIx: 4, functionIx: 285)
        ])
    }
    
}
