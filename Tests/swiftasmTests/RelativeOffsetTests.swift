import XCTest

@testable import swiftasm

final class RelativeOffsetTests: XCTestCase {
    func testDeferred() throws {
        let sut = RelativeDeferredOffset() 
        let off: any RelativeOffset = sut 
        sut.storage.wrappedValue = 561
        XCTAssertEqual(
            off.value,
            561
        )
    }
}
