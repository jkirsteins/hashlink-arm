import XCTest

@testable import swiftasm

final class ImmediateTests: XCTestCase {
    // -520 can't fit in 7 bits even when divided by 8 (for stp 64-bit)
    func testVariableImmediate__regression1() throws {
        XCTAssertThrowsError(
            try VariableImmediate(-520, bits: 7, divisor: 8)
        )
    }
    
    // 0 is border case that shouldn't ever raise an exception
    // about fitting
    func testVariableImmediate__regression2() throws {
        _ = try VariableImmediate(0, bits: 19, divisor: 1)
    }
    
    func testImmediate19() throws {
        let x1: Immediate19 = -4
        XCTAssertEqual(x1.immediate, 0b1111111111111111100)
    }
    
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
}
