import XCTest
@testable import swiftasm

final class vdynamicTests: XCTestCase {

    func testSet() throws {
        var x = vdynamic(t: .init(bitPattern: 123)!, union: nil)
        
        x.set(d: 123.0)
        XCTAssertEqual(x.d, 123.0)
        XCTAssertNotEqual(x.f, 123.0)
        
        x.set(f: 0.123)
        XCTAssertNotEqual(x.d, 123.0)
        XCTAssertEqual(x.f, 0.123)
    }
}

