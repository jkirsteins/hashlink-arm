import XCTest

@testable import swiftasm

final class EmitterM1Tests: XCTestCase {
    func testSvc() throws {
        XCTAssertEqual(
            try EmitterM1.emit(for: .svc(51)),
            [0x61, 0x06, 0x00, 0xd4]
        )
        XCTAssertEqual(
            try EmitterM1.emit(for: .svc(65053)),
            [0xa1, 0xc3, 0x1f, 0xd4]
        )
    }
    func testAdr() throws {
        XCTAssertEqual(
            try EmitterM1.emit(for: .adr64(.x1, 16)),
            [0x81, 0x00, 0x00, 0x10]
        )
        XCTAssertEqual(
            try EmitterM1.emit(for: .adr64(.x1, 153)),
            [0xc1, 0x04, 0x00, 0x30]
        )
        XCTAssertEqual(
            try EmitterM1.emit(for: .adr64(.x21, -5426)),
            [0x75, 0x56, 0xff, 0x50]
        )
    }
    func testLdp() throws {
        XCTAssertEqual(
            try EmitterM1.emit(for: .ldp((.x29_fp, .x30_lr), .reg64offset(.sp, 16, .post))),
            [0xfd, 0x7b, 0xc1, 0xa8]
        )
        XCTAssertEqual(
            try EmitterM1.emit(for: .ldp((.x0, .x1), .reg64offset(.sp, 16, .post))),
            [0xe0, 0x07, 0xc1, 0xa8]
        )
        XCTAssertEqual(
            try EmitterM1.emit(for: .ldp((.x0, .x1), .reg64offset(.sp, 16, nil))),
            [0xe0, 0x07, 0x41, 0xa9]
        )
        XCTAssertEqual(
            try EmitterM1.emit(for: .ldp((.x0, .x1), .reg64offset(.sp, 16, .pre))),
            [0xe0, 0x07, 0xc1, 0xa9]
        )
    }

    func testStp() throws {
        XCTAssertThrowsError(try EmitterM1.emit(for: .stp((.x0, .x1), .immediate(10))))
        { error in
            XCTAssertEqual(
                error as! EmitterM1Error,
                EmitterM1Error.invalidOffset("STP can only have .reg64offset offset")
            )
        }

        XCTAssertThrowsError(
            try EmitterM1.emit(for: .stp((.x0, .x1), .reg64offset(.sp, -520, nil)))
        ) { error in
            XCTAssertEqual(
                error as! EmitterM1Error,
                EmitterM1Error.invalidOffset("Offset immediate -520 must fit in 7 bits")
            )
        }

        XCTAssertEqual(
            try EmitterM1.emit(for: .stp((.x10, .x12), .reg64offset(.sp, 16, .pre))),
            [0xea, 0x33, 0x81, 0xa9]
        )

        XCTAssertEqual(
            try EmitterM1.emit(for: .stp((.x10, .x12), .reg64offset(.sp, 16, .post))),
            [0xea, 0x33, 0x81, 0xa8]
        )

        XCTAssertEqual(
            try EmitterM1.emit(for: .stp((.x10, .x12), .reg64offset(.sp, 16, nil))),
            [0xea, 0x33, 0x01, 0xa9]
        )

        XCTAssertEqual(
            try EmitterM1.emit(
                for: .stp((.x29_fp, .x30_lr), .reg64offset(.sp, -512, .pre))
            ),
            [0xfd, 0x7b, 0xa0, 0xa9]
        )

        XCTAssertEqual(
            try EmitterM1.emit(
                for: .stp((.x29_fp, .x30_lr), .reg64offset(.sp, -16, .pre))
            ),
            [0xfd, 0x7b, 0xbf, 0xa9]
        )
    }

    //        0: fd 7b bf a9  	stp	x29, x30, [sp, #-16]!
    //    4: fd 03 00 91  	mov	x29, sp

    func testBl() throws {
        XCTAssertThrowsError(try EmitterM1.emit(for: .bl(0x3FFFFFF + 1))) { error in
            XCTAssertEqual(
                error as! EmitterM1Error,
                EmitterM1Error.invalidValue(
                    "BL requires the immediate to fit in 26 bits"
                )
            )
        }

        XCTAssertThrowsError(try EmitterM1.emit(for: .bl(141))) { error in
            XCTAssertEqual(
                error as! EmitterM1Error,
                EmitterM1Error.invalidValue(
                    "BL requires the immediate to be a multiple of 4"
                )
            )
        }

        XCTAssertEqual(try EmitterM1.emit(for: .bl(140)), [0x23, 0x00, 0x00, 0x94])
    }

    func testBlr() throws {
        XCTAssertEqual(try EmitterM1.emit(for: .blr(.x0)), [0x00, 0x00, 0x3f, 0xd6])

        XCTAssertEqual(try EmitterM1.emit(for: .blr(.x8)), [0x00, 0x01, 0x3f, 0xd6])
    }
    func testRet() throws {
        XCTAssertEqual(try EmitterM1.emit(for: .ret), [0xc0, 0x03, 0x5f, 0xd6])
    }

    func testNop() throws {
        XCTAssertEqual(try EmitterM1.emit(for: .nop), [0x1f, 0x20, 0x03, 0xd5])
    }

    func testLdr_base_noOffset() throws {
        XCTAssertEqual(
            try EmitterM1.emit(for: .ldr(._32(.w2, .x1, nil))),
            [0x22, 0x00, 0x40, 0xb9]
        )
        XCTAssertEqual(
            try EmitterM1.emit(for: .ldr(._64(.x2, .x1, nil))),
            [0x22, 0x00, 0x40, 0xf9]
        )
    }

    func testLdr_invalidOffset() throws {
        /*
        Invalid offsets are:
        - When using pre/post indexing mode, the offset we can use must be in the range -256 to 255.
        - The offset may range from -256 to 255 for 32-bit and 64-bit accesses.
        - Larger offsets are a bit more limited: for 32-bit it must be multiple of 4 in the range 0 to 16380,
          for 64-bit it must be a multiple of 8 in the range of 0 to 32760.
        */
        XCTAssertThrowsError(
            try EmitterM1.emit(for: .ldr(._64(.x2, .x1, .immediate(257))))
        ) { error in
            XCTAssertEqual(
                error as! EmitterM1Error,
                EmitterM1Error.invalidOffset(
                    "Offset in 64-bit mode must be a multiple of 8 in range 0...32760"
                )
            )
        }
        XCTAssertThrowsError(
            try EmitterM1.emit(for: .ldr(._32(.w2, .x1, .immediate(257))))
        ) { error in
            XCTAssertEqual(
                error as! EmitterM1Error,
                EmitterM1Error.invalidOffset(
                    "Offset in 32-bit mode must be a multiple of 4 in range 0...16380"
                )
            )
        }
        XCTAssertThrowsError(
            try EmitterM1.emit(for: .ldr(._64(.x2, .x1, .immediate(-257))))
        ) { error in
            XCTAssertEqual(
                error as! EmitterM1Error,
                EmitterM1Error.invalidOffset("Offset can\'t be less than -256")
            )
        }
        XCTAssertThrowsError(
            try EmitterM1.emit(for: .ldr(._64(.x2, .x1, .immediate(404))))
        ) { error in
            XCTAssertEqual(
                error as! EmitterM1Error,
                EmitterM1Error.invalidOffset(
                    "Offset in 64-bit mode must be a multiple of 8 in range 0...32760"
                )
            )
        }
    }

    func testMovk64() throws {
        XCTAssertEqual(
            try EmitterM1.emit(for: .movk64(.x0, 1, nil)),
            [0x20, 0x00, 0x80, 0xf2]
        )
        XCTAssertEqual(
            try EmitterM1.emit(for: .movk64(.x1, 2, ._0)),
            [0x41, 0x00, 0x80, 0xf2]
        )
        XCTAssertEqual(
            try EmitterM1.emit(for: .movk64(.x2, 2, ._16)),
            [0x42, 0x00, 0xa0, 0xf2]
        )
    }

    func testMovr64() throws {
        
        // test values
        XCTAssertEqual(
            try EmitterM1.emit(for: .movr64(.x0, .x2)),
            [0xe0, 0x03, 0x02, 0xaa]
        )
        XCTAssertEqual(
            try EmitterM1.emit(for: .movr64(.x2, .x3)),
            [0xe2, 0x03, 0x03, 0xaa]
        )
        XCTAssertEqual(
            try EmitterM1.emit(for: .movr64(.x1, .sp)),
            [0xe1, 0x03, 0x00, 0x91]
        )
        XCTAssertEqual(
            try EmitterM1.emit(for: .movr64(.sp, .x2)),
            [0x5f, 0x00, 0x00, 0x91]
        )
    }

    func testMovz64() throws {
        XCTAssertEqual(
            try EmitterM1.emit(for: .movz64(.x0, 1, ._0)),
            [0x20, 0x00, 0x80, 0xd2]
        )

        XCTAssertEqual(
            try EmitterM1.emit(for: .movz64(.x0, 0, ._0)),
            [0x00, 0x00, 0x80, 0xd2]
        )

        XCTAssertEqual(
            try EmitterM1.emit(for: .movz64(.x15, 65535, ._0)),
            [0xef, 0xff, 0x9f, 0xd2]
        )

        XCTAssertEqual(
            try EmitterM1.emit(for: .movz64(.x0, 0, ._16)),
            [0x00, 0x00, 0xa0, 0xd2]
        )

        XCTAssertEqual(
            try EmitterM1.emit(for: .movz64(.x1, 1, ._32)),
            [0x21, 0x00, 0xc0, 0xd2]
        )

        XCTAssertEqual(
            try EmitterM1.emit(for: .movz64(.x2, 2, ._48)),
            [0x42, 0x00, 0xe0, 0xd2]
        )
    }
}
