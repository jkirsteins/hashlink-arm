import XCTest

@testable import swiftasm

extension String {
    func chunked(into c: Int) -> String {
        Array(self.utf8).chunked(into: c).map {
            String(bytes: $0, encoding: .utf8)!
        }.joined(separator: "_")
    }
}

extension [UInt8] {
    var b16: String {
        let c = String(u32, radix: 16).leftPadding(toLength: 8, withPad: "0")
        return "0x\(c.chunked(into: 2))"
    }
    
    var b2: String {
        let c = String(u32, radix: 2).leftPadding(toLength: 32, withPad: "0")
        return "0b\(c.chunked(into: 4))"
    }
    
    var u32: UInt32 {
        UInt32(self[0]) | (UInt32(self[1]) << 8) | (UInt32(self[2]) << 16) | (UInt32(self[3]) << 24)
    }
}

extension XCTestCase {
    func XCTAssertM1OpDesc(_ op: M1Op, _ dd: String) {
        if op.debugDescription != dd {
            print("\(op) description: \n  expected: \(dd)\n  actual:   \(op.debugDescription)")
        }
        XCTAssertEqual(dd, op.debugDescription)
    }
    func XCTAssertM1OpBytes(_ op: M1Op, _ expected: UInt8...) {
        XCTAssertM1OpBytes(op, expected)
    }
    func XCTAssertM1OpBytes(_ op: M1Op, _ expected: [UInt8]) {
        let actual = try! EmitterM1.emit(for: op)
        
        if actual != expected {
            print("\(op.debugDescription): \n  expected: \(expected.b16)\t\(expected.b2)\n  actual:   \(actual.b16)\t\(actual.b2)")
        }
        XCTAssertEqual(actual, expected)
    }
    func XCTAssertM1Op(_ op: M1Op, _ desc: String, _ expected: UInt8...) {
        XCTAssertM1OpDesc(op, desc)
        XCTAssertM1OpBytes(op, expected)
    }
}

final class EmitterM1Tests: XCTestCase {
    func testSxtw() throws {
        XCTAssertM1Op(
            M1Op.sxtw(.x2, .w3),
            "sxtw x2, w3",
            0x62, 0x7c, 0x40, 0x93
        )
    }
    
    func testSxth() throws {
        XCTAssertM1Op(
            M1Op.sxth(.x2, .w3),
            "sxth x2, w3",
            0x62, 0x3c, 0x40, 0x93
        )
    }
    
    func testSxtb() throws {
        XCTAssertM1Op(
            M1Op.sxtb(.x2, .w3),
            "sxtb x2, w3",
            0x40, 0x1c, 0x62
        )
    }
    
    func testUxth() throws {
        XCTAssertM1Op(
            M1Op.uxth(.w1, .w6),
            "uxth w1, w6",
            0xc1, 0x3c, 0x00, 0x53
        )
    }
    
    func testAnd() throws {
        XCTAssertM1Op(
            M1Op.and(X.x1, X.x3, .imm(0x1f, nil)),
            "and x1, x3, #31",
            0x61, 0x10, 0x40, 0x92
        )
        XCTAssertM1Op(
            M1Op.and(X.x1, X.x3, .r64shift(X.x2, .lsl(0))),
            "and x1, x3, x2",
            0x61, 0x00, 0x02, 0x8a
        )
    }
    
    func testLdrh() throws {
        
        XCTAssertM1Op(
            M1Op.ldrh(.w0, .reg64(X.sp, .r32ext(.w1, .uxtw(0)))),
            "ldrh w0, [sp, w1, uxtw #0]",
            0xe0, 0x4b, 0x61, 0x78
        )
        XCTAssertM1Op(
            M1Op.ldrh(.w0, .reg64(X.sp, .r32ext(.w1, .uxtw(1)))),
            "ldrh w0, [sp, w1, uxtw #1]",
            0xe0, 0x5b, 0x61, 0x78
        )
        XCTAssertM1Op(
            M1Op.ldrh(.w0, .reg64(X.sp, .r32ext(.w1, .sxtw(0)))),
            "ldrh w0, [sp, w1, sxtw #0]",
            0xe0, 0xcb, 0x61, 0x78
        )
        XCTAssertM1Op(
            M1Op.ldrh(.w0, .reg64(X.sp, .r32ext(.w1, .sxtw(1)))),
            "ldrh w0, [sp, w1, sxtw #1]",
            0xe0, 0xdb, 0x61, 0x78
        )
        XCTAssertM1Op(
            M1Op.ldrh(.w0, .reg64(X.sp, .r64ext(.x1, .sxtx(0)))),
            "ldrh w0, [sp, x1, sxtx #0]",
            0xe0, 0xeb, 0x61, 0x78
        )
        XCTAssertM1Op(
            M1Op.ldrh(.w0, .reg64(X.sp, .r64ext(.x1, .sxtx(1)))),
            "ldrh w0, [sp, x1, sxtx #1]",
            0xe0, 0xfb, 0x61, 0x78
        )
        XCTAssertM1Op(
            M1Op.ldrh(.w0, .reg64(X.sp, .r64shift(.x1, .lsl(0)))),
            "ldrh w0, [sp, x1]",
            0xe0, 0x6b, 0x61, 0x78
        )
        XCTAssertM1Op(
            M1Op.ldrh(.w0, .reg64(X.sp, .r64shift(.x1, .lsl(1)))),
            "ldrh w0, [sp, x1, lsl #1]",
            0xe0, 0x7b, 0x61, 0x78
        )
        XCTAssertM1Op(
            M1Op.ldrh(.w1, .reg64(X.sp, nil)),
            "ldrh w1, [sp]",
            0xe1, 0x03, 0x40, 0x79
        )
        XCTAssertM1Op(
            M1Op.ldrh(.w2, .reg64(X.x1, nil)),
            "ldrh w2, [x1]",
            0x22, 0x00, 0x40, 0x79
        )
        XCTAssertM1Op(
            M1Op.ldrh(.w0, .imm64(X.sp, 24, .post)),
            "ldrh w0, [sp], #24",
            0xe0, 0x87, 0x41, 0x78
        )
        XCTAssertM1Op(
            M1Op.ldrh(.w0, .imm64(X.sp, 24, nil)),
            "ldrh w0, [sp, #24]",
            0xe0, 0x33, 0x40, 0x79
        )
        XCTAssertM1Op(
            M1Op.ldrh(.w0, .imm64(X.sp, 24, .pre)),
            "ldrh w0, [sp, #24]!",
            0xe0, 0x8f, 0x41, 0x78
        )
    }
    
    func testLdrb() throws {
        XCTAssertM1Op(
            M1Op.ldrb(.w0, .reg64(X.sp, .r32ext(.w1, .uxtw(0)))),
            "ldrb w0, [sp, w1, uxtw #0]",
            0xe0, 0x5b, 0x61, 0x38
        )
        XCTAssertM1Op(
            M1Op.ldrb(.w0, .reg64(X.sp, .r64ext(.x1, .sxtx(0)))),
            "ldrb w0, [sp, x1, sxtx #0]",
            0xe0, 0xfb, 0x61, 0x38
        )
        XCTAssertM1Op(
            M1Op.ldrb(.w0, .reg64(X.sp, .r64shift(.x1, .lsl(0)))),
            "ldrb w0, [sp, x1]",
            0xe0, 0x6b, 0x61, 0x38
        )
        XCTAssertM1Op(
            M1Op.ldrb(.w1, .reg64(X.sp, nil)),
            "ldrb w1, [sp]",
            0xe1, 0x03, 0x40, 0x39
        )
        XCTAssertM1Op(
            M1Op.ldrb(.w0, .imm64(X.sp, 24, nil)),
            "ldrb w0, [sp, #24]",
            0xe0, 0x63, 0x40, 0x39
        )
        XCTAssertM1Op(
            M1Op.ldrb(.w0, .imm64(X.sp, 24, .post)),
            "ldrb w0, [sp], #24",
            0xe0, 0x87, 0x41, 0x38
        )
        XCTAssertM1Op(
            M1Op.ldrb(.w0, .imm64(X.sp, 24, .pre)),
            "ldrb w0, [sp, #24]!",
            0xe0, 0x8f, 0x41, 0x38
        )
    }
    
    func testLsl_i() throws {
        XCTAssertM1OpDesc(M1Op.lsl_i(X.x0, X.x1, 1), "lsl x0, x1, #1")
        XCTAssertM1OpDesc(M1Op.lsl_i(X.x0, X.x1, 40), "lsl x0, x1, #40")
        XCTAssertM1OpDesc(M1Op.lsl_i(X.x0, X.x1, 63), "lsl x0, x1, #63")
        XCTAssertM1OpDesc(M1Op.lsl_i(W.w0, W.w1, 31), "lsl w0, w1, #31")
        
        XCTAssertM1OpBytes(M1Op.lsl_i(X.x0, X.x1, 1), 0x20, 0xf8, 0x7f, 0xd3)
        XCTAssertM1OpBytes(M1Op.lsl_i(X.x0, X.x1, 40), 0x20, 0x5c, 0x58, 0xd3)
        XCTAssertM1OpBytes(M1Op.lsl_i(X.x0, X.x1, 63), 0x20, 0x00, 0x41, 0xd3)
        XCTAssertM1OpBytes(M1Op.lsl_i(W.w0, W.w1, 31), 0x20, 0x00, 0x01, 0x53)
    }
    
    func testLsl_r() throws {
        XCTAssertM1OpDesc(M1Op.lsl_r(X.x0, X.x1, X.x2), "lsl x0, x1, x2")
        XCTAssertM1OpDesc(M1Op.lsl_r(W.w1, W.w2, W.w3), "lsl w1, w2, w3")
        
        XCTAssertM1OpBytes(M1Op.lsl_r(X.x0, X.x1, X.x2), 0x20, 0x20, 0xc2, 0x9a)
        XCTAssertM1OpBytes(M1Op.lsl_r(W.w1, W.w2, W.w3), 0x41, 0x20, 0xc3, 0x1a)
    }
    
    func testUbfm() throws {
        XCTAssertM1OpDesc(M1Op.ubfm(X.x0, X.x2, 3, 1), "ubfx x0, x2, #3, #1")
        XCTAssertM1OpDesc(M1Op.ubfm(W.w0, W.w2, 3, 1), "ubfx w0, w2, #3, #1")
        
        // TODO
//        XCTAssertM1OpBytes(M1Op.ubfm(X.x0, X.x2, 3, 1), 0x40, 0x0c, 0x43, 0xd3)
//        XCTAssertM1OpBytes(M1Op.ubfm(W.w0, W.w2, 3, 1), 0x40, 0x0c, 0x03, 0x53)
    }
    
    func testB_ne() throws {
        XCTAssertM1OpDesc(M1Op.b_ne(4), "b.ne #4")
        XCTAssertM1OpDesc(M1Op.b_ne(-4), "b.ne #-4")
        
        XCTAssertM1OpBytes(.b_ne(4), 0x21, 0x00, 0x00, 0x54)
        XCTAssertM1OpBytes(.b_ne(-4), 0xe1, 0xff, 0xff, 0x54)
    }
    
    func testB_le() throws {
        XCTAssertM1OpDesc(M1Op.b_le(4), "b.le #4")
        XCTAssertM1OpDesc(M1Op.b_le(-4), "b.le #-4")
        XCTAssertM1OpBytes(.b_le(4), 0x2d, 0x00, 0x00, 0x54)
        XCTAssertM1OpBytes(.b_le(-4), 0xed, 0xff, 0xff, 0x54)
    }
    
    func testB_ge() throws {
        XCTAssertM1OpDesc(M1Op.b_ge(4), "b.ge #4")
        XCTAssertM1OpDesc(M1Op.b_ge(-4), "b.ge #-4")
        XCTAssertM1OpBytes(.b_ge(4), 0x2a, 0x00, 0x00, 0x54)
        XCTAssertM1OpBytes(.b_ge(-4), 0xea, 0xff, 0xff, 0x54)
    }
    
    func testB_eq() throws {
        XCTAssertM1OpDesc(M1Op.b_eq(16), "b.eq #16")
        XCTAssertM1OpDesc(M1Op.b_eq(-4), "b.eq #-4")
        
        XCTAssertM1OpBytes(.b_eq(16), 0x80, 0x00, 0x00, 0x54)
        XCTAssertM1OpBytes(.b_eq(-4), 0xe0, 0xff, 0xff, 0x54)
        XCTAssertM1OpBytes(.b_eq(-8), 0xc0, 0xff, 0xff, 0x54)
        XCTAssertM1OpBytes(.b_eq(-12), 0xa0, 0xff, 0xff, 0x54)
        XCTAssertM1OpBytes(.b_eq(-16), 0x80, 0xff, 0xff, 0x54)
        XCTAssertM1OpBytes(.b_eq(0), 0x00, 0x00, 0x00, 0x54)
    }
    
    func testB_gt() throws {
        XCTAssertM1OpDesc(M1Op.b_gt(16), "b.gt #16")
        XCTAssertM1OpDesc(M1Op.b_gt(-4), "b.gt #-4")
        
        XCTAssertM1OpBytes(.b_gt(16), 0x8c, 0x00, 0x00, 0x54)
        XCTAssertM1OpBytes(.b_gt(-4), 0xec, 0xff, 0xff, 0x54)
        XCTAssertM1OpBytes(.b_gt(-8), 0xcc, 0xff, 0xff, 0x54)
        XCTAssertM1OpBytes(.b_gt(-12), 0xac, 0xff, 0xff, 0x54)
        XCTAssertM1OpBytes(.b_gt(-16), 0x8c, 0xff, 0xff, 0x54)
        XCTAssertM1OpBytes(.b_gt(0), 0x0c, 0x00, 0x00, 0x54)
    }
    
    func testB_lt() throws {
        XCTAssertM1OpDesc(M1Op.b_lt(16), "b.lt #16")
        XCTAssertM1OpDesc(M1Op.b_lt(-4), "b.lt #-4")
        XCTAssertM1OpBytes(.b_lt(0x10), 0x8b, 0x00, 0x00, 0x54)
        XCTAssertM1OpBytes(.b_lt(-4), 0xeb, 0xff, 0xff, 0x54)
        XCTAssertM1OpBytes(.b_lt(-16), 0x8b, 0xff, 0xff, 0x54)
        XCTAssertM1OpBytes(.b_lt(0), 0x0b, 0x00, 0x00, 0x54)
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
