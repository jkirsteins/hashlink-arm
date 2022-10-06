import XCTest
@testable import swiftasm

final class HLOpCodeIdTests: XCTestCase {
    func testRawValue() throws {
        XCTAssertEqual(HLOpCodeId.OMov.rawValue, 0)
        XCTAssertEqual(HLOpCodeId.OOr.rawValue, 18)
        XCTAssertEqual(HLOpCodeId.ONeg.rawValue, 20)
        XCTAssertEqual(HLOpCodeId.OJSGt.rawValue, 50)
        XCTAssertEqual(HLOpCodeId.OLabel.rawValue, 66)
        XCTAssertEqual(HLOpCodeId.OAssert.rawValue, 95)
        XCTAssertEqual(HLOpCodeId.ONop.rawValue, 98)
    }
}
