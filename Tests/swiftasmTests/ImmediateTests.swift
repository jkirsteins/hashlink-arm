import XCTest

@testable import swiftasm

final class ImmediateTests: XCTestCase {
    func testImm12Lsl12() throws {
        // fits, so default to lsl 0
        let x1: Imm12Lsl12 = 51
        let x2 = try Imm12Lsl12(51, bits: 12)
        XCTAssertEqual(x1.immediate, 51)
        XCTAssertEqual(x1.lsl, ._0)
        XCTAssertEqual(x2.immediate, 51)
        XCTAssertEqual(x2.lsl, ._0)

        // infer 4096 doesn't fit, so apply lsl 12
        let y1: Imm12Lsl12 = 4096
        let y2 = try Imm12Lsl12(4096, bits: 12)
        XCTAssertEqual(y1.immediate, 1)
        XCTAssertEqual(y1.lsl, ._12)
        XCTAssertEqual(y2.immediate, 1)
        XCTAssertEqual(y2.lsl, ._12)
    }

    func testImmediate12() throws {
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

    // func testAbsoluteAddressImmediate() throws {
    //     var test = 1
    //     var ptr = UnsafeMutableRawPointer(mutating: &test)
    //     let x = AbsoluteAddressImmediate(ptr)

    //     XCTAssertEqual(x.immediate, Int64(Int(bitPattern: ptr)))
    // }

    // func testDeferredAbsoluteAddressImmediate() throws {
    //     var test = 1
    //     var ptr = UnsafeMutableRawPointer(mutating: &test)
    //     let sut: DeferredImmediate<AbsoluteAddressImmediate> = DeferredImmediate()

    //     XCTAssertThrowsError(try sut.get())

    //     let x = AbsoluteAddressImmediate(ptr)
    //     sut.finalize(x)

    //     XCTAssertEqual(try sut.get(), x)
    //     XCTAssertEqual(sut.immediate, x.immediate)
    // }
}
