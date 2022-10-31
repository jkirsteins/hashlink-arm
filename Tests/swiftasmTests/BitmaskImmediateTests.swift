import XCTest
@testable import swiftasm

final class BitmaskImmediateTests: XCTestCase {
    func testBinaryExtensions() throws {
        let sut64: UInt64 = 18374686479671623680
        let sut32: UInt32 = 4093640704
        XCTAssertEqual(sut64.leadingOneBitCount, 8)
        XCTAssertEqual(sut32.leadingOneBitCount, 4)
        XCTAssertEqual(63.trailingOneBitCount, 6)
    }

    func testInit_throws() throws {
        XCTAssertThrowsError(try BitmaskImmediate(0))
        XCTAssertThrowsError(try BitmaskImmediate( 18446744073709551615))
    }
    
    func testInit_size16_minimum() throws {
        let sut = try BitmaskImmediate(0x0001000100010001)
        XCTAssertEqual(sut,  BitmaskImmediate(
            n: 0,
            immr: 0b000000,
            imms: 0b100000
        ))
    }
    
    func testInit_size16_rotated() throws {
        let sut = try BitmaskImmediate(0xff8fff8fff8fff8f)
        XCTAssertEqual(sut,  BitmaskImmediate(
            n: 0,
            immr: 0b001001,
            imms: 0b101100
        ))
    }
    
    func testInit_size16_maximum() throws {
        let sut = try BitmaskImmediate(0xfffefffefffefffe)
        XCTAssertEqual(sut,  BitmaskImmediate(
            n: 0,
            immr: 0b001111,
            imms: 0b101110
        ))
    }
    
    func testInit_32() throws {
        let sut = try BitmaskImmediate(31)
        XCTAssertEqual(sut,  BitmaskImmediate(
            n: 1,
            immr: 0b000000,
            imms: 0b000100
        ))
    }
}

