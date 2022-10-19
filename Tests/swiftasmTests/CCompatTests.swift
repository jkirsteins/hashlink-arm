import XCTest
@testable import swiftasm

final class CCompatTests: XCTestCase {
    

    func testHLType_sizes() throws {
        XCTAssertEqual(MemoryLayout<HLTypeKind>.size, 4)
        XCTAssertEqual(MemoryLayout<HLType_CCompat>.size, 32)

        // union sizes
        // obj
        XCTAssertEqual(MemoryLayout<HLType_CCompat_Obj>.size, 80)
        // fun
        XCTAssertEqual(MemoryLayout<HLType_CCompat_Fun>.size, 80)
        XCTAssertEqual(MemoryLayout<HLType_CCompat_Fun_Closure>.size, 32)
        XCTAssertEqual(MemoryLayout<HLType_CCompat_Fun_ClosureType>.size, 16)
    } 

    func testHLCode_sizes() throws {
        XCTAssertEqual(MemoryLayout<HLCode_CCompat>.size, 184)
    }

    func testHLFunction_sizes() throws {
        XCTAssertEqual(MemoryLayout<HLFunction_CCompat>.size, 64)
    }

    func testHLNative_sizes() throws {
        XCTAssertEqual(MemoryLayout<HLNative_CCompat>.size, 32)
    }
}

