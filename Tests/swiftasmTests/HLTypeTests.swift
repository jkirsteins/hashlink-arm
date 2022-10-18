import XCTest

@testable import swiftasm

final class HLTypeTests: XCTestCase {
    func testCCompat() throws {
        // XCTAssertEqual(MemoryLayout<HLType_CCompat_Obj>.size, 80)
        // XCTAssertEqual(MemoryLayout<HLTypeObjData>.size, 80)

        // XCTAssertEqual(MemoryLayout<HLType_CCompat_BodyUnion>.size, 8)
        // XCTAssertEqual(MemoryLayout<HLTypeKind>.size, 4)
        // XCTAssertEqual(MemoryLayout<HLType_CCompat>.size, 32)

        let x = allocCCompat(
            hltype: HLType.obj(
                HLTypeObjData(
                    name: Resolvable("Class"),
                    superType: nil,
                    global: nil,
                    fields: [],
                    protos: [],
                    bindings: []
                )
            )
        )
    }
}
