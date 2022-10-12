import XCTest

@testable import swiftasm

final class RelativeOffsetTests: XCTestCase {
    func testDeferred() throws {
        let sut = RelativeDeferredOffset(wrappedValue: 0) 
        let off: any RelativeOffset = sut 
        sut.wrappedValue = 561
        XCTAssertEqual(
            off.value,
            561
        )
    }
}
