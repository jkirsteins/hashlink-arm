import XCTest
@testable import swiftasm

final class EmitterM1Tests: XCTestCase {
    func testRet() throws {
        XCTAssertEqual(
            try! EmitterM1.emit(for: .ret), 
            [0xc0, 0x03, 0x5f, 0xd6]
        )
    }

    func testNop() throws {
        XCTAssertEqual(
            try! EmitterM1.emit(for: .nop), 
            [0x1f, 0x20, 0x03, 0xd5]
        )
    }

    func testLdr_base_unsignedImmediate() throws {
        XCTAssertEqual(
            try! EmitterM1.emit(for: .ldr32(.w2, .x1)), 
            [0x22, 0x00, 0x40, 0xb9]
        )
        
        XCTAssertEqual(
            try! EmitterM1.emit(for: .ldr64(.x2, .x1)), 
            [0x22, 0x00, 0x40, 0xf9]
        )
    }

    func testMovk64() throws {
        XCTAssertEqual(
            try! EmitterM1.emit(for: .movk64(.x0, 1, nil)), 
            [0x20, 0x00, 0x80, 0xf2]
        )
        XCTAssertEqual(
            try! EmitterM1.emit(for: .movk64(.x1, 2, ._0)), 
            [0x41, 0x00, 0x80, 0xf2]
        )
        XCTAssertEqual(
            try! EmitterM1.emit(for: .movk64(.x2, 2, ._16)), 
            [0x42, 0x00, 0xa0, 0xf2]
        )
    }

    func testMovz64() throws {
        XCTAssertEqual(
            try! EmitterM1.emit(for: .movz64(.x0, 1, ._0)), 
            [0x20, 0x00, 0x80, 0xd2]
        )

        XCTAssertEqual(
            try! EmitterM1.emit(for: .movz64(.x0, 0, ._0)), 
            [0x00, 0x00, 0x80, 0xd2] 
        )

        XCTAssertEqual(
            try! EmitterM1.emit(for: .movz64(.x15, 65535, ._0)), 
            [0xef, 0xff, 0x9f, 0xd2]
        )

        XCTAssertEqual(
            try! EmitterM1.emit(for: .movz64(.x0, 0, ._16)), 
            [0x00, 0x00, 0xa0, 0xd2]
        )

        XCTAssertEqual(
            try! EmitterM1.emit(for: .movz64(.x1, 1, ._32)), 
            [0x21, 0x00, 0xc0, 0xd2]
        )

        XCTAssertEqual(
            try! EmitterM1.emit(for: .movz64(.x2, 2, ._48)), 
            [0x42, 0x00, 0xe0, 0xd2]
        )
    }
}

