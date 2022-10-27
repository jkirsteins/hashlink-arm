import XCTest

@testable import swiftasm

final class EmitterM1Tests: XCTestCase {
    func testB_lt() throws {
        XCTAssertEqual(
            "b.lt #16",
            M1Op.b_lt(16).debugDescription
        )
        XCTAssertEqual(
            "b.lt #-4",
            M1Op.b_lt(-4).debugDescription
        )

        XCTAssertEqual(
            try EmitterM1.emit(for: .b_lt(0x10)),
            [0x8b, 0x00, 0x00, 0x54]
        )

        XCTAssertEqual(
            try EmitterM1.emit(for: .b_lt(-4)),
            [0xeb, 0xff, 0xff, 0x54]
        )
        
        XCTAssertEqual(
            try EmitterM1.emit(for: .b_lt(-16)),
            [0x8b, 0xff, 0xff, 0x54]
        )

        XCTAssertEqual(
            try EmitterM1.emit(for: .b_lt(0)),
            [0x0b, 0x00, 0x00, 0x54]
        )
    }
    func testCmp() throws {
        XCTAssertEqual(
            "cmp x0, x1",
            M1Op.cmp(X.x0, X.x1).debugDescription
        )
        
        XCTAssertEqual(
            try EmitterM1.emit(for: .cmp(X.x0, X.x1)),
            [0x1f, 0x00, 0x01, 0xeb]
        )
    }
    func testSub() throws {
        XCTAssertEqual("sub sp, sp, #16", M1Op.sub(Register64.sp, Register64.sp, 16).debugDescription)
        XCTAssertEqual("sub sp, sp, #16",
                       M1Op.sub(Register64.sp, Register64.sp,
                                Imm12Lsl12(16)).debugDescription)
        XCTAssertEqual("sub sp, sp, #16, lsl 12", M1Op.sub(Register64.sp, Register64.sp, try Imm12Lsl12(16, lsl: ._12)).debugDescription)
        XCTAssertEqual("add sp, sp, #16", M1Op.sub(Register64.sp, Register64.sp, -16).debugDescription)
        XCTAssertEqual(
            try EmitterM1.emit(for: .sub(X.sp, X.sp, 16)),
            [0xff, 0x43, 0x00, 0xd1]
        )
        XCTAssertEqual(
            try EmitterM1.emit(for: .sub(W.w0, W.w0, 16)),
            [0x00, 0x40, 0x00, 0x51]
        )
        XCTAssertEqual(
            try EmitterM1.emit(for: .sub(X.sp, X.sp, -16)),
            [0xff, 0x43, 0x00, 0x91]
        )
        XCTAssertEqual(
            try EmitterM1.emit(for: .sub(X.sp, X.sp, Imm12Lsl12(16, lsl: ._12))),
            [0xff, 0x43, 0x40, 0xd1]
        )
    }

    func testAdd() throws {
        XCTAssertEqual("add sp, sp, #16", M1Op.add(X.sp, X.sp, 16).debugDescription)
        XCTAssertEqual("add sp, sp, #16, lsl 12", M1Op.add(X.sp, X.sp, try Imm12Lsl12(16, lsl: ._12)).debugDescription)
        XCTAssertEqual("sub sp, sp, #16", M1Op.add(X.sp, X.sp, -16).debugDescription)
        XCTAssertEqual(
            try EmitterM1.emit(for: .add(X.sp, X.sp, -16)),
            [0xff, 0x43, 0x00, 0xd1]
        )
        XCTAssertEqual(
            try EmitterM1.emit(for: .add(X.sp, X.sp, 16)),
            [0xff, 0x43, 0x00, 0x91]
        )
        XCTAssertEqual(
            try EmitterM1.emit(for: .add(X.sp, X.sp, Imm12Lsl12(16, lsl: ._12))),
            [0xff, 0x43, 0x40, 0x91]
        )
    }

    func testStur() throws {
        XCTAssertEqual(
            try EmitterM1.emit(for: .stur(Register64.x0, .sp, -16)),
            [0xe0, 0x03, 0x1f, 0xf8]
        )
        XCTAssertEqual(
            try EmitterM1.emit(for: .stur(Register32.w0, .sp, -16)),
            [0xe0, 0x03, 0x1f, 0xb8]
        )
    }

    func testB() throws {
        XCTAssertEqual(
            try EmitterM1.emit(for: .b(RelativeLiteralOffset(8))),
            [0x02, 0x00, 0x00, 0x14]
        )
        XCTAssertEqual(
            try EmitterM1.emit(for: .b(RelativeLiteralOffset(-16))),
            [0xfc, 0xff, 0xff, 0x17]
        )
        let x = RelativeDeferredOffset()
        x.storage.wrappedValue = 24
        XCTAssertEqual(
            try EmitterM1.emit(for: .b(x)),
            [0x06, 0x00, 0x00, 0x14]
        )
    }

    func testStr() throws {
        // should match stur
        XCTAssertEqual(
            try EmitterM1.emit(for: .str(Register64.x0, .reg64offset(.sp, -16, nil))),
            [0xe0, 0x03, 0x1f, 0xf8]
        )
        XCTAssertEqual(
            try EmitterM1.emit(for: .str(Register32.w0, .reg64offset(.sp, -16, nil))),
            [0xe0, 0x03, 0x1f, 0xb8]
        )

        XCTAssertEqual(
            try EmitterM1.emit(for: .str(Register64.x0, .reg64offset(.sp, -16, .pre))),
            [0xe0, 0x0f, 0x1f, 0xf8]
        )
        XCTAssertEqual(
            try EmitterM1.emit(for: .str(Register64.x0, .reg64offset(.sp, -16, .post))),
            [0xe0, 0x07, 0x1f, 0xf8]
        )
        
        // stur should not be used for 0
        // same for ldur
        // TODO: 
        // XCTAssertEqual(
        //     try EmitterM1.emit(for: .str(Register64.x0, .reg64offset(.sp, 0, nil))),
        //     [0xe1, 0x03, 0x00, 0xf9]
        // )
        // XCTAssertNotEqual(
        //     try EmitterM1.emit(for: .str(Register64.x0, .reg64offset(.sp, 0, nil))),
        //     try EmitterM1.emit(for: .stur(Register64.x0, .sp, 0))
        // )
    }

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
            try EmitterM1.emit(for: .adr64(.x1, Int64(16))),
            [0x81, 0x00, 0x00, 0x10]
        )
        XCTAssertEqual(
            try EmitterM1.emit(for: .adr64(.x1, Int64(153))),
            [0xc1, 0x04, 0x00, 0x30]
        )
        XCTAssertEqual(
            try EmitterM1.emit(for: .adr64(.x21, Int64(-5426))),
            [0x75, 0x56, 0xff, 0x50]
        )

        // quick doublecheck for deferred offsets
        let offset = RelativeDeferredOffset()
        offset.storage.wrappedValue = -5426
        XCTAssertEqual(
            try EmitterM1.emit(for: .adr64(.x21, offset)),
            [0x75, 0x56, 0xff, 0x50]
        )
        offset.storage.wrappedValue = 153
        XCTAssertEqual(
            try EmitterM1.emit(for: .adr64(.x1, offset)),
            [0xc1, 0x04, 0x00, 0x30]
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
        XCTAssertThrowsError(try EmitterM1.emit(for: .bl(try Immediate26(0x3FFFFFF + 1)))) { error in
            XCTAssertEqual(
                error as! EmitterM1Error,
                EmitterM1Error.invalidValue(
                    "Immediate 67108864 must fit in 26 bits"
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

    func testLdr() throws {
        XCTAssertEqual(
            try EmitterM1.emit(for: .ldr(Register32.w2, .reg64offset(.x1, 0, nil))),
            [0x22, 0x00, 0x40, 0xb9]
        )
        XCTAssertEqual(
            try EmitterM1.emit(for: .ldr(Register64.x2, .reg64offset(.x1, 0, nil))),
            [0x22, 0x00, 0x40, 0xf9]
        )
        XCTAssertEqual(
            try EmitterM1.emit(for: .ldr(Register64.x0, .reg64offset(.sp, 16, .post))),
            [0xe0, 0x07, 0x41, 0xf8]
        )
        XCTAssertEqual(
            try EmitterM1.emit(for: .ldr(Register64.x0, .reg64offset(.sp, 16, .pre))),
            [0xe0, 0x0f, 0x41, 0xf8]
        )
        XCTAssertEqual(
            try EmitterM1.emit(for: .ldr(Register64.x0, .reg64offset(.sp, 16, nil))),
            [0xe0, 0x0b, 0x40, 0xf9]
        )

        // divisible by 8 -> ldr, otherwise -> ldur
        XCTAssertEqual(
            try EmitterM1.emit(for: .ldr(Register64.x1, .reg64offset(.sp, 1, nil))),
            try EmitterM1.emit(for: .ldur(Register64.x1, .sp, 1))
        )
        XCTAssertEqual(
            try EmitterM1.emit(for: .ldr(Register64.x1, .reg64offset(.sp, 4, nil))),
            try EmitterM1.emit(for: .ldur(Register64.x1, .sp, 4))
        )
        XCTAssertEqual(
            try EmitterM1.emit(for: .ldr(Register64.x1, .reg64offset(.sp, 8, nil))),
            [0xe1, 0x07, 0x40, 0xf9]
        )

        // for 0 ldur should not be the fallback
        // TODO
        XCTAssertNotEqual(
            try EmitterM1.emit(for: .ldr(Register64.x1, .reg64offset(.sp, 0, nil))),
            try EmitterM1.emit(for: .ldur(Register64.x1, .sp, 0))
        )
    }

    func testLdur() throws {
        XCTAssertEqual(
            try EmitterM1.emit(for: .ldur(Register64.x1, .sp, 1)),
            [0xe1, 0x13, 0x40, 0xf8]
        )
        XCTAssertEqual(
            try EmitterM1.emit(for: .ldur(Register64.x1, .sp, 4)),
            [0xe1, 0x43, 0x40, 0xf8]
        )
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
