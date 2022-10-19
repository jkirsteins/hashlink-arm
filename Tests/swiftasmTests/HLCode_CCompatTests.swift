import XCTest

@testable import swiftasm

final class HLCode_CCompatTests: XCTestCase {
    func testSize() throws {
        XCTAssertEqual(MemoryLayout<HLCode_CCompat>.size, 184)
    }
}