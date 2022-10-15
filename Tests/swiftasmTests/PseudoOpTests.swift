import XCTest
@testable import swiftasm

final class PseudoOpTests: XCTestCase {
    

    func testMov() throws {
        let op = PseudoOp.mov(.x0, 1229801703818430600)
        let res = try op.emit()

        XCTAssertEqual(
            res,
            [0x00, 0x11, 0x91, 0xd2,
            0x80, 0x88, 0xa8, 0xf2,
            0x40, 0x44, 0xc4, 0xf2,
            0x20, 0x22, 0xe2, 0xf2]
        )
    }

    func testMov_DeferredAddressImmediate() throws {
        let deferred = DeferredAddressImmediate()
        let op = PseudoOp.mov(.x0, deferred)

        XCTAssertThrowsError(try op.emit())
        
        deferred.finalize(try AbsoluteAddressImmediate(1229801703818430600, bits: 64))
        let res = try op.emit()

        XCTAssertEqual(
            res,
            [0x00, 0x11, 0x91, 0xd2,
            0x80, 0x88, 0xa8, 0xf2,
            0x40, 0x44, 0xc4, 0xf2,
            0x20, 0x22, 0xe2, 0xf2]
        )
    }

    func testMov_DeferredAddressImmediate_withLiteral() throws {
        let deferred = DeferredAddressImmediate()
        let op = PseudoOp.mov(.x0, deferred)

        XCTAssertThrowsError(try op.emit())
        
        deferred.finalize(1229801703818430600)
        let res = try op.emit()

        XCTAssertEqual(
            res,
            [0x00, 0x11, 0x91, 0xd2,
            0x80, 0x88, 0xa8, 0xf2,
            0x40, 0x44, 0xc4, 0xf2,
            0x20, 0x22, 0xe2, 0xf2]
        )
    }
}
