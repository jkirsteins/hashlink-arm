import XCTest
@testable import swiftasm

final class CCompatTests: XCTestCase {

    func testSizes() throws {
        XCTAssertEqual(MemoryLayout<HLTypeKind>.size, 4)
        XCTAssertEqual(MemoryLayout<HLType_CCompat>.size, 32)
        XCTAssertEqual(MemoryLayout<HLTypeObj_CCompat>.size, 80)
        XCTAssertEqual(MemoryLayout<HLObjField_CCompat>.size, 24)
        XCTAssertEqual(MemoryLayout<HLTypeFun_CCompat>.size, 80)
        XCTAssertEqual(MemoryLayout<HLType_CCompat_Fun_Closure>.size, 32)
        XCTAssertEqual(MemoryLayout<HLType_CCompat_Fun_ClosureType>.size, 16)
        XCTAssertEqual(MemoryLayout<HLObjProto_CCompat>.size, 24)
        XCTAssertEqual(MemoryLayout<HLRuntimeObj_CCompat>.size, 120)
        XCTAssertEqual(MemoryLayout<HLRuntimeObj_CCompat>.size, 120)
        XCTAssertEqual(MemoryLayout<HLCode_CCompat>.size, 184)
        XCTAssertEqual(MemoryLayout<HLFunction_CCompat>.size, 64)
        XCTAssertEqual(MemoryLayout<HLNative_CCompat>.size, 32)
        XCTAssertEqual(MemoryLayout<vdynamic>.size, 16)
    }
}

