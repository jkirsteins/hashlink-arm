import XCTest

@testable import swiftasm

final class ImmediateTests: XCTestCase {
    func testInit() throws {
        let x: Immediate12 = -14
        // value will not match -14 cause truncated
        XCTAssertEqual(x.immediate, 4082)   
        // but if we flip, it should come back to the right one
        XCTAssertEqual(x.flippedSign.immediate, 14) 

        XCTAssert(x.isNegative) 
        XCTAssert(x.flippedSign.isPositive) 
        XCTAssertFalse(x.flippedSign.isNegative) 
        XCTAssertFalse(x.isPositive) 
    }

    func testAbsoluteAddressImmediate() throws {
        var test = 1
        var ptr = UnsafeMutableRawPointer(mutating: &test)
        let x = AbsoluteAddressImmediate(ptr)

        XCTAssertEqual(x.immediate, Int64(Int(bitPattern: ptr)))
    }

    func testDeferredAbsoluteAddressImmediate() throws {
        var test = 1
        var ptr = UnsafeMutableRawPointer(mutating: &test)
        let sut: DeferredImmediate<AbsoluteAddressImmediate> = DeferredImmediate()

        XCTAssertThrowsError(try sut.get())

        let x = AbsoluteAddressImmediate(ptr)
        sut.finalize(x)

        XCTAssertEqual(try sut.get(), x)
        XCTAssertEqual(sut.immediate, x.immediate)
    }
}
