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
        guard self.count > 0 else { return "" }
        let c = String(u32, radix: 16).leftPadding(toLength: 8, withPad: "0")
        return "0x\(c.chunked(into: 2))"
    }
    
    var b2: String {
        guard self.count > 0 else { return "" }
        let c = String(u32, radix: 2).leftPadding(toLength: 32, withPad: "0")
        return "0b\(c.chunked(into: 4))"
    }
    
    var u32: UInt32 {
        guard self.count > 0 else { return 0 }
        return UInt32(self[0]) | (UInt32(self[1]) << 8) | (UInt32(self[2]) << 16) | (UInt32(self[3]) << 24)
    }
}

extension XCTestCase {
    func XCTAssertM1OpDesc(_ op: M1Op, _ dd: String, file: StaticString = #filePath,
                           line: UInt = #line) {
        if op.asmDescription != dd {
            print("\(op) description: \n  expected: \(dd)\n  actual:   \(op.asmDescription)")
        }
        XCTAssertEqual(dd, op.asmDescription, file: file, line: line)
    }
    func XCTAssertM1OpBytes(_ op: M1Op, _ expected: UInt8..., file: StaticString = #filePath,
                            line: UInt = #line) throws {
        try XCTAssertM1OpBytes(op, expected, file: file, line: line)
    }
    func XCTAssertM1OpBytes(_ op: M1Op, _ expected: [UInt8], file: StaticString = #filePath,
                            line: UInt = #line) throws {
        let actual = try EmitterM1.emit(for: op)
        
        if actual != expected {
            print("\(op.asmDescription): \n  expected: \(expected.b16)\t\(expected.b2)\n  actual:   \(actual.b16)\t\(actual.b2)")
        }
        XCTAssertEqual(actual, expected, file: file, line: line)
    }
    func XCTAssertM1Op(_ op: M1Op, _ desc: String, _ expected: UInt8..., file: StaticString = #filePath,
                       line: UInt = #line) throws {
        XCTAssertM1OpDesc(op, desc, file: file, line: line)
        try XCTAssertM1OpBytes(op, expected, file: file, line: line)
    }
}

final class EmitterM1Tests: XCTestCase {
    
    func testDiv() throws {
        // throw cause sizes should be the same
        XCTAssertThrowsError(
            try XCTAssertM1OpBytes(M1Op.sdiv(X.x1, W.w2, X.x3)))
        XCTAssertThrowsError(
            try XCTAssertM1OpBytes(M1Op.udiv(X.x1, W.w2, X.x3)))
        
        try XCTAssertM1Op(
            M1Op.sdiv(X.x1, X.x2, X.x3),
            "sdiv x1, x2, x3",
            0x41, 0x0c, 0xc3, 0x9a
        )
        try XCTAssertM1Op(
            M1Op.sdiv(W.w1, W.w2, W.w3),
            "sdiv w1, w2, w3",
            0x41, 0x0c, 0xc3, 0x1a
        )
        try XCTAssertM1Op(
            M1Op.udiv(X.x1, X.x2, X.x3),
            "udiv x1, x2, x3",
            0x41, 0x08, 0xc3, 0x9a
        )
        try XCTAssertM1Op(
            M1Op.udiv(W.w1, W.w2, W.w3),
            "udiv w1, w2, w3",
            0x41, 0x08, 0xc3, 0x1a
        )
        try XCTAssertM1Op(
            M1Op.fdiv(D.d1, D.d2, D.d3),
            "fdiv d1, d2, d3",
            0x41, 0x18, 0x63, 0x1e
        )
        try XCTAssertM1Op(
            M1Op.fdiv(S.s1, S.s2, S.s3),
            "fdiv s1, s2, s3",
            0x41, 0x18, 0x23, 0x1e
        )
    }
    
    func testFcvtzs() throws {
        try XCTAssertM1Op(
            M1Op.fcvtzs(W.w8, D.d0),
            "fcvtzs w8, d0",
            0x08, 0x00, 0x78, 0x1e
        )
        try XCTAssertM1Op(
            M1Op.fcvtzs(X.x8, D.d0),
            "fcvtzs x8, d0",
            0x08, 0x00, 0x78, 0x9e
        )
        try XCTAssertM1Op(
            M1Op.fcvtzs(W.w8, S.s0),
            "fcvtzs w8, s0",
            0x08, 0x00, 0x38, 0x1e
        )
        try XCTAssertM1Op(
            M1Op.fcvtzs(X.x8, S.s0),
            "fcvtzs x8, s0",
            0x08, 0x00, 0x38, 0x9e
        )
    }
    
    func testScvtf() throws {
        try XCTAssertM1Op(
            M1Op.scvtf(D.d0, X.x8),
            "scvtf d0, x8",
            0x00, 0x01, 0x62, 0x9e
        )
        try XCTAssertM1Op(
            M1Op.scvtf(D.d0, W.w8),
            "scvtf d0, w8",
            0x00, 0x01, 0x62, 0x1e
        )
        try XCTAssertM1Op(
            M1Op.scvtf(S.s0, X.x8),
            "scvtf s0, x8",
            0x00, 0x01, 0x22, 0x9e
        )
        try XCTAssertM1Op(
            M1Op.scvtf(S.s0, W.w8),
            "scvtf s0, w8",
            0x00, 0x01, 0x22, 0x1e
        )
    }
    
    func testUcvtf() throws {
        try XCTAssertM1Op(
            M1Op.ucvtf(D.d0, X.x8),
            "ucvtf d0, x8",
            0x00, 0x01, 0x63, 0x9e
        )
        try XCTAssertM1Op(
            M1Op.ucvtf(D.d0, W.w8),
            "ucvtf d0, w8",
            0x00, 0x01, 0x63, 0x1e
        )
    }
    
    func testOrr() throws {
        try XCTAssertM1Op(
            M1Op.orr(X.x0, X.x1, X.x2, nil),
            "orr x0, x1, x2",
            0x20, 0x00, 0x02, 0xaa
        )
        try XCTAssertM1Op(
            M1Op.orr(W.w1, W.w2, W.w3, nil),
            "orr w1, w2, w3",
            0x41, 0x00, 0x03, 0x2a
        )
    }
    
    func testLsr() throws {
        try XCTAssertM1Op(
            M1Op.lsr(X.x1, X.x2, .uimmediate6(3)),
            "lsr x1, x2, #3",
            0x41, 0xfc, 0x43, 0xd3
        )
        try XCTAssertM1Op(
            M1Op.lsr(W.w1, W.w2, .uimmediate6(3)),
            "lsr w1, w2, #3",
            0x41, 0x7c, 0x03, 0x53
        )
        try XCTAssertM1Op(
            M1Op.lsr(W.w1, W.w2, .reg(W.w3, nil)),
            "lsr w1, w2, w3",
            0x41, 0x24, 0xc3, 0x1a
        )
    }
    
    func testAsr() throws {
        try XCTAssertM1Op(
            M1Op.asr(X.x1, X.x2, .uimmediate6(3)),
            "asr x1, x2, #3",
            0x41, 0xfc, 0x43, 0x93
        )
        try XCTAssertM1Op(
            M1Op.asr(W.w1, W.w2, .uimmediate6(3)),
            "asr w1, w2, #3",
            0x41, 0x7c, 0x03, 0x13
        )
        try XCTAssertM1Op(
            M1Op.asr(W.w1, W.w2, .reg(W.w3, nil)),
            "asr w1, w2, w3",
            0x41, 0x28, 0xc3, 0x1a
        )
    }
    
    func testAddSub_imm() throws {
        try XCTAssertM1Op(
            M1Op.add(W.w1, W.w2, .imm(1, nil)),
            "add w1, w2, #1",
            0x41, 0x04, 0x00, 0x11
        )
        try XCTAssertM1Op(
            M1Op.add(W.w1, W.w2, .imm(-1, nil)),
            "add w1, w2, #-1",
            0x41, 0x04, 0x00, 0x51
        )
        try XCTAssertM1Op(
            M1Op.sub(W.w1, W.w2, .imm(1, nil)),
            "sub w1, w2, #1",
            0x41, 0x04, 0x00, 0x51
        )
        try XCTAssertM1Op(
            M1Op.sub(W.w1, W.w2, .imm(-1, nil)),
            "sub w1, w2, #-1",
            0x41, 0x04, 0x00, 0x11
        )
    }
    
    func testAdd() throws {
        try XCTAssertM1Op(
            M1Op.add(W.w1, W.w2, .r64shift(W.w3, .lsl(0))),
            "add w1, w2, w3",
            0x41, 0x00, 0x03, 0x0b
        )
        try XCTAssertM1Op(
            M1Op.add(X.x1, X.x2, .r64shift(X.x3, .lsl(0))),
            "add x1, x2, x3",
            0x41, 0x00, 0x03, 0x8b
        )
        try XCTAssertM1Op(
            M1Op.add(X.sp, X.sp, .imm(16, nil)),
            "add sp, sp, #16",
            0xff, 0x43, 0x00, 0x91
        )
    }
    
    func testFadd() throws {
        try XCTAssertM1Op(
            M1Op.fadd(S.s0, S.s0, S.s1),
            "fadd s0, s0, s1",
            0x00, 0x28, 0x21, 0x1e
        )
        try XCTAssertM1Op(
            M1Op.fadd(D.d0, D.d0, D.d1),
            "fadd d0, d0, d1",
            0x00, 0x28, 0x61, 0x1e
        )
        
    }
        
    func testStrb() throws {
        try XCTAssertM1Op(
            M1Op.strb(.w0, .reg(X.sp, .imm(0, nil))),
            "strb w0, [sp]",
            0xe0, 0x03, 0x00, 0x39
        )
        try XCTAssertM1Op(
            M1Op.strb(.w0, .imm64(.sp, 0, nil)),
            "strb w0, [sp]",
            0xe0, 0x03, 0x00, 0x39
        )
        try XCTAssertM1Op(
            M1Op.strb(.w0, .imm64(.sp, 4, .pre)),
            "strb w0, [sp, #4]!",
            0xe0, 0x4f, 0x00, 0x38
        )
        try XCTAssertM1Op(
            M1Op.strb(.w0, .imm64(.sp, 4, .post)),
            "strb w0, [sp], #4",
            0xe0, 0x47, 0x00, 0x38
        )
        try XCTAssertM1Op(
            M1Op.strb(.w2, .reg(X.x0, .r64shift(X.x1, .lsl(0)))),
            "strb w2, [x0, x1]",
            0x02, 0x68, 0x21, 0x38
        )
    }
    
    func testStr_regression() throws {
        try XCTAssertM1Op(
            M1Op.strb(.w0, .reg(X.sp, .imm(0, nil))),
            "strb w0, [sp]",
            0xe0, 0x03, 0x00, 0x39
        )
        try XCTAssertM1Op(
            M1Op.strh(.w0, .imm64(X.sp, 1, nil)),
            "sturh w0, [sp, #1]",
            0xe0, 0x13, 0x00, 0x78
        )
        try XCTAssertM1Op(
            M1Op.str(W.w0, .reg64offset(X.sp, 3, nil)),
            "str w0, [sp, #3]",
            0xe0, 0x33, 0x00, 0xb8
        )
        
        try XCTAssertM1Op(
            M1Op.ldrb(.w0, .reg(X.sp, .imm(0, nil))),
            "ldrb w0, [sp]",
            0xe0, 0x03, 0x40, 0x39
        )
        try XCTAssertM1Op(
            M1Op.ldrh(.w0, .imm64(.sp, 1, nil)),
            "ldurh w0, [sp, #1]",
            0xe0, 0x13, 0x40, 0x78
        )
        try XCTAssertM1Op(
            M1Op.ldr(W.w0, .reg64offset(.sp, 3, nil)),
            "ldr w0, [sp, #3]",
            0xe0, 0x33, 0x40, 0xb8
        )
    }
    
    func testStrh() throws {
        try XCTAssertM1Op(
            M1Op.strh(.w0, .reg(X.sp, .imm(0, nil))),
            "strh w0, [sp]",
            0xe0, 0x03, 0x00, 0x79
        )
        try XCTAssertM1Op(
            M1Op.strh(.w0, .imm64(.sp, 0, nil)),
            "strh w0, [sp]",
            0xe0, 0x03, 0x00, 0x79
        )
        try XCTAssertM1Op(
            M1Op.strh(.w0, .imm64(.sp, 4, .pre)),
            "strh w0, [sp, #4]!",
            0xe0, 0x4f, 0x00, 0x78
        )
        try XCTAssertM1Op(
            M1Op.strh(.w0, .imm64(.sp, 4, .post)),
            "strh w0, [sp], #4",
            0xe0, 0x47, 0x00, 0x78
        )
        try XCTAssertM1Op(
            M1Op.strh(.w2, .reg(X.x0, .r64shift(X.x1, .lsl(1)))),
            "strh w2, [x0, x1, lsl #1]",
            0x02, 0x78, 0x21, 0x78
        )
    }
    
    func testSxtw() throws {
        try XCTAssertM1Op(
            M1Op.sxtw(.x2, .w3),
            "sxtw x2, w3",
            0x62, 0x7c, 0x40, 0x93
        )
    }
    
    func testSxth() throws {
        try XCTAssertM1Op(
            M1Op.sxth(.x2, .w3),
            "sxth x2, w3",
            0x62, 0x3c, 0x40, 0x93
        )
    }
    
    func testSxtb() throws {
        try XCTAssertM1Op(
            M1Op.sxtb(.x2, .w3),
            "sxtb x2, w3",
            0x62, 0x1c, 0x40, 0x93
        )
    }
    
    func testUxth() throws {
        try XCTAssertM1Op(
            M1Op.uxth(.w1, .w6),
            "uxth w1, w6",
            0xc1, 0x3c, 0x00, 0x53
        )
    }
    
    func testUxtb() throws {
        try XCTAssertM1Op(
            M1Op.uxtb(.w1, .w2),
            "uxtb w1, w2",
            0x41, 0x1c, 0x00, 0x53
        )
    }
    
    func testUxtw() throws {
        try XCTAssertM1Op(
            M1Op.uxtw(.x1, .w2),
            "uxtw x1, w2",
            0x41, 0x7c, 0x40, 0xd3
        )
    }
    
    func testAnd() throws {
        try XCTAssertM1Op(
            M1Op.and(X.x1, X.x3, .imm(0x1f, nil)),
            "and x1, x3, #31",
            0x61, 0x10, 0x40, 0x92
        )
        try XCTAssertM1Op(
            M1Op.and(X.x1, X.x3, .r64shift(X.x2, .lsl(0))),
            "and x1, x3, x2",
            0x61, 0x00, 0x02, 0x8a
        )
    }
    
    func testLdrh() throws {
        
        try XCTAssertM1Op(
            M1Op.ldrh(.w0, .reg(X.sp, .r32ext(.w1, .uxtw(0)))),
            "ldrh w0, [sp, w1, uxtw #0]",
            0xe0, 0x4b, 0x61, 0x78
        )
        try XCTAssertM1Op(
            M1Op.ldrh(.w0, .reg(X.sp, .r32ext(.w1, .uxtw(1)))),
            "ldrh w0, [sp, w1, uxtw #1]",
            0xe0, 0x5b, 0x61, 0x78
        )
        try XCTAssertM1Op(
            M1Op.ldrh(.w0, .reg(X.sp, .r32ext(.w1, .sxtw(0)))),
            "ldrh w0, [sp, w1, sxtw #0]",
            0xe0, 0xcb, 0x61, 0x78
        )
        try XCTAssertM1Op(
            M1Op.ldrh(.w0, .reg(X.sp, .r32ext(.w1, .sxtw(1)))),
            "ldrh w0, [sp, w1, sxtw #1]",
            0xe0, 0xdb, 0x61, 0x78
        )
        try XCTAssertM1Op(
            M1Op.ldrh(.w0, .reg(X.sp, .r64ext(.x1, .sxtx(0)))),
            "ldrh w0, [sp, x1, sxtx #0]",
            0xe0, 0xeb, 0x61, 0x78
        )
        try XCTAssertM1Op(
            M1Op.ldrh(.w0, .reg(X.sp, .r64ext(.x1, .sxtx(1)))),
            "ldrh w0, [sp, x1, sxtx #1]",
            0xe0, 0xfb, 0x61, 0x78
        )
        try XCTAssertM1Op(
            M1Op.ldrh(.w0, .reg(X.sp, .r64shift(X.x1, .lsl(0)))),
            "ldrh w0, [sp, x1]",
            0xe0, 0x6b, 0x61, 0x78
        )
        try XCTAssertM1Op(
            M1Op.ldrh(.w0, .reg(X.sp, .r64shift(X.x1, .lsl(1)))),
            "ldrh w0, [sp, x1, lsl #1]",
            0xe0, 0x7b, 0x61, 0x78
        )
        try XCTAssertM1Op(
            M1Op.ldrh(.w1, .reg(X.sp, nil)),
            "ldrh w1, [sp]",
            0xe1, 0x03, 0x40, 0x79
        )
        try XCTAssertM1Op(
            M1Op.ldrh(.w2, .reg(X.x1, nil)),
            "ldrh w2, [x1]",
            0x22, 0x00, 0x40, 0x79
        )
        try XCTAssertM1Op(
            M1Op.ldrh(.w0, .imm64(X.sp, 24, .post)),
            "ldrh w0, [sp], #24",
            0xe0, 0x87, 0x41, 0x78
        )
        try XCTAssertM1Op(
            M1Op.ldrh(.w0, .imm64(X.sp, 24, nil)),
            "ldrh w0, [sp, #24]",
            0xe0, 0x33, 0x40, 0x79
        )
        try XCTAssertM1Op(
            M1Op.ldrh(.w0, .imm64(X.sp, 24, .pre)),
            "ldrh w0, [sp, #24]!",
            0xe0, 0x8f, 0x41, 0x78
        )
    }
    
    func testLdrb() throws {
        try XCTAssertM1Op(
            M1Op.ldrb(.w0, .reg(X.sp, .r32ext(.w1, .uxtw(0)))),
            "ldrb w0, [sp, w1, uxtw #0]",
            0xe0, 0x5b, 0x61, 0x38
        )
        try XCTAssertM1Op(
            M1Op.ldrb(.w0, .reg(X.sp, .r64ext(.x1, .sxtx(0)))),
            "ldrb w0, [sp, x1, sxtx #0]",
            0xe0, 0xfb, 0x61, 0x38
        )
        try XCTAssertM1Op(
            M1Op.ldrb(.w0, .reg(X.sp, .r64shift(X.x1, .lsl(0)))),
            "ldrb w0, [sp, x1]",
            0xe0, 0x6b, 0x61, 0x38
        )
        try XCTAssertM1Op(
            M1Op.ldrb(.w1, .reg(X.sp, nil)),
            "ldrb w1, [sp]",
            0xe1, 0x03, 0x40, 0x39
        )
        try XCTAssertM1Op(
            M1Op.ldrb(.w0, .imm64(X.sp, 24, nil)),
            "ldrb w0, [sp, #24]",
            0xe0, 0x63, 0x40, 0x39
        )
        try XCTAssertM1Op(
            M1Op.ldrb(.w0, .imm64(X.sp, 24, .post)),
            "ldrb w0, [sp], #24",
            0xe0, 0x87, 0x41, 0x38
        )
        try XCTAssertM1Op(
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
        
        try try XCTAssertM1OpBytes(M1Op.lsl_i(X.x0, X.x1, 1), 0x20, 0xf8, 0x7f, 0xd3)
        try try XCTAssertM1OpBytes(M1Op.lsl_i(X.x0, X.x1, 40), 0x20, 0x5c, 0x58, 0xd3)
        try try XCTAssertM1OpBytes(M1Op.lsl_i(X.x0, X.x1, 63), 0x20, 0x00, 0x41, 0xd3)
        try try XCTAssertM1OpBytes(M1Op.lsl_i(W.w0, W.w1, 31), 0x20, 0x00, 0x01, 0x53)
    }
    
    func testLsl_r() throws {
        XCTAssertM1OpDesc(M1Op.lsl_r(X.x0, X.x1, X.x2), "lsl x0, x1, x2")
        XCTAssertM1OpDesc(M1Op.lsl_r(W.w1, W.w2, W.w3), "lsl w1, w2, w3")
        
        try XCTAssertM1OpBytes(M1Op.lsl_r(X.x0, X.x1, X.x2), 0x20, 0x20, 0xc2, 0x9a)
        try XCTAssertM1OpBytes(M1Op.lsl_r(W.w1, W.w2, W.w3), 0x41, 0x20, 0xc3, 0x1a)
    }
    
    func testUbfm() throws {
        XCTAssertM1OpDesc(M1Op.ubfm(X.x0, X.x2, 3, 1), "ubfx x0, x2, #3, #1")
        XCTAssertM1OpDesc(M1Op.ubfm(W.w0, W.w2, 3, 1), "ubfx w0, w2, #3, #1")
        
        // TODO
//        try XCTAssertM1OpBytes(M1Op.ubfm(X.x0, X.x2, 3, 1), 0x40, 0x0c, 0x43, 0xd3)
//        try XCTAssertM1OpBytes(M1Op.ubfm(W.w0, W.w2, 3, 1), 0x40, 0x0c, 0x03, 0x53)
    }
    
    func testB_ne() throws {
        XCTAssertM1OpDesc(M1Op.b_ne(4), "b.ne #4")
        XCTAssertM1OpDesc(M1Op.b_ne(-4), "b.ne #-4")
        
        try XCTAssertM1OpBytes(.b_ne(4), 0x21, 0x00, 0x00, 0x54)
        try XCTAssertM1OpBytes(.b_ne(-4), 0xe1, 0xff, 0xff, 0x54)
    }
    
    func testB_le() throws {
        XCTAssertM1OpDesc(M1Op.b_le(4), "b.le #4")
        XCTAssertM1OpDesc(M1Op.b_le(-4), "b.le #-4")
        try XCTAssertM1OpBytes(.b_le(4), 0x2d, 0x00, 0x00, 0x54)
        try XCTAssertM1OpBytes(.b_le(-4), 0xed, 0xff, 0xff, 0x54)
    }
    
    func testB_ge() throws {
        XCTAssertM1OpDesc(M1Op.b_ge(4), "b.ge #4")
        XCTAssertM1OpDesc(M1Op.b_ge(-4), "b.ge #-4")
        try XCTAssertM1OpBytes(.b_ge(4), 0x2a, 0x00, 0x00, 0x54)
        try XCTAssertM1OpBytes(.b_ge(-4), 0xea, 0xff, 0xff, 0x54)
    }
    
    func testB_eq() throws {
        XCTAssertM1OpDesc(M1Op.b_eq(16), "b.eq #16")
        XCTAssertM1OpDesc(M1Op.b_eq(-4), "b.eq #-4")
        
        try XCTAssertM1OpBytes(.b_eq(16), 0x80, 0x00, 0x00, 0x54)
        try XCTAssertM1OpBytes(.b_eq(-4), 0xe0, 0xff, 0xff, 0x54)
        try XCTAssertM1OpBytes(.b_eq(-8), 0xc0, 0xff, 0xff, 0x54)
        try XCTAssertM1OpBytes(.b_eq(-12), 0xa0, 0xff, 0xff, 0x54)
        try XCTAssertM1OpBytes(.b_eq(-16), 0x80, 0xff, 0xff, 0x54)
        XCTAssertThrowsError(try M1Op.b_eq(0).emit())
    }
    
    func testB_gt() throws {
        try XCTAssertM1Op(M1Op.b_gt(262424), "b.gt #262424", 0xcc, 0x08, 0x20, 0x54)
        XCTAssertM1OpDesc(M1Op.b_gt(16), "b.gt #16")
        XCTAssertM1OpDesc(M1Op.b_gt(-4), "b.gt #-4")

        try XCTAssertM1OpBytes(.b_gt(16), 0x8c, 0x00, 0x00, 0x54)
        try XCTAssertM1OpBytes(.b_gt(-4), 0xec, 0xff, 0xff, 0x54)
        try XCTAssertM1OpBytes(.b_gt(-8), 0xcc, 0xff, 0xff, 0x54)
        try XCTAssertM1OpBytes(.b_gt(-12), 0xac, 0xff, 0xff, 0x54)
        try XCTAssertM1OpBytes(.b_gt(-16), 0x8c, 0xff, 0xff, 0x54)
        
        XCTAssertThrowsError(try M1Op.b_gt(0).emit())
    }
    
    func testB_lt() throws {
        XCTAssertM1OpDesc(M1Op.b_lt(16), "b.lt #16")
        XCTAssertM1OpDesc(M1Op.b_lt(-4), "b.lt #-4")
        try XCTAssertM1OpBytes(.b_lt(0x10), 0x8b, 0x00, 0x00, 0x54)
        try XCTAssertM1OpBytes(.b_lt(-4), 0xeb, 0xff, 0xff, 0x54)
        try XCTAssertM1OpBytes(.b_lt(-16), 0x8b, 0xff, 0xff, 0x54)
        XCTAssertThrowsError(try M1Op.b_lt(0).emit())
    }
    func testCmp() throws {
        try XCTAssertM1Op(
            M1Op.cmp(X.x0, X.x1),
            "cmp x0, x1",
            0x1f, 0x00, 0x01, 0xeb
        )
    }
    
    func testSub() throws {
        try XCTAssertM1Op(.sub(X.x0, X.x1, .r64shift(X.x2, .lsl(0))),
                      "sub x0, x1, x2",
                      0x20, 0x00, 0x02, 0xcb)
        try XCTAssertM1Op(.sub(X.x1, X.x2, .r64shift(X.x3, .lsl(12))),
                      "sub x1, x2, x3, lsl #12",
                      0x41, 0x30, 0x03, 0xcb)
        try XCTAssertM1Op(.sub(W.w1, W.w2, .r64shift(W.w3, .lsl(12))),
                      "sub w1, w2, w3, lsl #12",
                      0x41, 0x30, 0x03, 0x4b)
    }
    
    func testFrintz() throws {
        try XCTAssertM1Op(.frintz(D.d0, D.d0),
                          "frintz d0, d0",
                          0x00, 0xc0, 0x65, 0x1e)
        XCTAssertThrowsError(
            try M1Op.frintz(S.s0, D.d0).emit()
        )
    }
    
    func testFsub() throws {
        try XCTAssertM1Op(.fsub(D.d0, D.d1, D.d2),
                          "fsub d0, d1, d2",
                          0x20, 0x38, 0x62, 0x1e)
        try XCTAssertM1Op(.fsub(S.s0, S.s1, S.s2),
                          "fsub s0, s1, s2",
                          0x20, 0x38, 0x22, 0x1e)
        XCTAssertThrowsError(
            try M1Op.fsub(D.d0, S.s1, S.s2).emit()
        )
    }
    
    func testSubImm12() throws {
        XCTAssertEqual("sub sp, sp, #16", M1Op.subImm12(Register64.sp, Register64.sp, 16).asmDescription)
        XCTAssertEqual("sub sp, sp, #16",
                       M1Op.subImm12(Register64.sp, Register64.sp,
                                Imm12Lsl12(16)).asmDescription)
        XCTAssertEqual("sub sp, sp, #16, lsl 12", M1Op.subImm12(Register64.sp, Register64.sp, try Imm12Lsl12(16, lsl: ._12)).asmDescription)
        XCTAssertEqual("add sp, sp, #16", M1Op.subImm12(Register64.sp, Register64.sp, -16).asmDescription)
        XCTAssertEqual(
            try EmitterM1.emit(for: .subImm12(X.sp, X.sp, 16)),
            [0xff, 0x43, 0x00, 0xd1]
        )
        XCTAssertEqual(
            try EmitterM1.emit(for: .subImm12(W.w0, W.w0, 16)),
            [0x00, 0x40, 0x00, 0x51]
        )
        XCTAssertEqual(
            try EmitterM1.emit(for: .subImm12(X.sp, X.sp, -16)),
            [0xff, 0x43, 0x00, 0x91]
        )
        XCTAssertEqual(
            try EmitterM1.emit(for: .subImm12(X.sp, X.sp, Imm12Lsl12(16, lsl: ._12))),
            [0xff, 0x43, 0x40, 0xd1]
        )
    }
    
    func testAddImm12() throws {
        XCTAssertEqual("add sp, sp, #16", M1Op.addImm12(X.sp, X.sp, 16).asmDescription)
        XCTAssertEqual("add sp, sp, #16, lsl 12", M1Op.addImm12(X.sp, X.sp, try Imm12Lsl12(16, lsl: ._12)).asmDescription)
        XCTAssertEqual("sub sp, sp, #16", M1Op.addImm12(X.sp, X.sp, -16).asmDescription)
        XCTAssertEqual(
            try EmitterM1.emit(for: .addImm12(X.sp, X.sp, -16)),
            [0xff, 0x43, 0x00, 0xd1]
        )
        XCTAssertEqual(
            try EmitterM1.emit(for: .addImm12(X.sp, X.sp, 16)),
            [0xff, 0x43, 0x00, 0x91]
        )
        XCTAssertEqual(
            try EmitterM1.emit(for: .addImm12(X.sp, X.sp, Imm12Lsl12(16, lsl: ._12))),
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
    
    func testLdrStr_regression() throws {
        try XCTAssertM1Op(
            .ldr(X.x2, .reg64offset(X.x9, 24, nil)),
            "ldr x2, [x9, #24]",
            0x22, 0x0d, 0x40, 0xf9
        )
        try XCTAssertM1Op(
            .str(X.x2, .reg64offset(X.sp, 36 + 192, nil)),
            "str x2, [sp, #228]",
            0xe2, 0x43, 0x0e, 0xf8
        )
        try XCTAssertM1Op(
            .ldr(X.x2, .reg64offset(X.x9, 304, nil)),
            "ldr x2, [x9, #304]",
            0x22, 0x99, 0x40, 0xf9
        )
        try XCTAssertM1Op(
            .ldr(W.w1, .reg64offset(X.sp, 340, nil)),
            "ldr w1, [sp, #340]",
            0xe1, 0x57, 0x41, 0xb9
        )
        try XCTAssertM1Op(
            .str(X.x2, .reg64offset(X.sp, 304, nil)),
            "str x2, [sp, #304]",
            0xe2, 0x9b, 0x00, 0xf9
        )
        // invalid immediates (not divisible by 8 and not [-256; 255]
        XCTAssertThrowsError(
            try EmitterM1.emit(for: M1Op.ldr(X.x2, .reg64offset(X.x9, 303, nil)))
        )
        XCTAssertThrowsError(
            try EmitterM1.emit(for: M1Op.str(X.x2, .reg64offset(X.sp, 303, nil)))
        )
    }
    
    func testLdrStr_fp() throws {
        try XCTAssertM1Op(
            .str(D.d0, .reg(X.sp, .r64ext(X.x15, .sxtx(0)))),
            "str d0, [sp, x15, sxtx #0]",
            0xe0, 0xeb, 0x2f, 0xfc
        )
        try XCTAssertM1Op(
            .str(S.s0, .reg(X.sp, .r64ext(X.x15, .sxtx(0)))),
            "str s0, [sp, x15, sxtx #0]",
            0xe0, 0xeb, 0x2f, 0xbc
        )
        try XCTAssertM1Op(
            .ldr(D.d0, .reg(X.sp, .r64ext(X.x15, .sxtx(0)))),
            "ldr d0, [sp, x15, sxtx #0]",
            0xe0, 0xeb, 0x6f, 0xfc
        )
        try XCTAssertM1Op(
            .ldr(S.s0, .reg(X.sp, .r64ext(X.x15, .sxtx(0)))),
            "ldr s0, [sp, x15, sxtx #0]",
            0xe0, 0xeb, 0x6f, 0xbc
        )
    }
        
    func testB() throws {
        try XCTAssertM1Op(M1Op.b(RelativeLiteralOffset(262424)), "b #262424", 0x46, 0x00, 0x01, 0x14)
        
        try XCTAssertM1Op(
            .b(RelativeLiteralOffset(-264)),
            "b #-264",
            0xbe, 0xff, 0xff, 0x17
        )
        try XCTAssertM1Op(
            .b(RelativeLiteralOffset(84)),
            "b #84",
            0x15, 0x00, 0x00, 0x14
        )

        try XCTAssertM1Op(
            .b_v2(Immediate26(-264)),
            "b #-264",
            0xbe, 0xff, 0xff, 0x17
        )
        try XCTAssertM1Op(
            .b_v2(Immediate26(84)),
            "b #84",
            0x15, 0x00, 0x00, 0x14
        )

        // doesn't fit//
        XCTAssertThrowsError(try M1Op.b(RelativeLiteralOffset(134217728)).emit())
        
        try XCTAssertM1Op(
            .b(RelativeLiteralOffset(67108864)),
            "b #67108864",
            0x00, 0x00, 0x00, 0x15)
        try XCTAssertM1Op(
            .b(RelativeLiteralOffset(-67108864)),
            "b #-67108864",
            0x00, 0x00,0x00, 0x17)
        try XCTAssertM1Op(
            .b(RelativeLiteralOffset(-134217728)),
            "b #-134217728",
            0x00, 0x00,0x00, 0x16)
        
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
    
    func testLdr_fp() throws {
        try XCTAssertM1Op(
            M1Op.ldr(D.d0, .reg64offset(.sp, 4, nil)),
            "ldr d0, [sp, #4]",
            0xe0, 0x43, 0x40, 0xfc
        )
        try XCTAssertM1Op(
            M1Op.ldr(D.d0, .reg64offset(.sp, 8, nil)),
            "ldr d0, [sp, #8]",
            0xe0, 0x07, 0x40, 0xfd
        )
        try XCTAssertM1Op(
            M1Op.ldr(D.d0, .reg64offset(.sp, 8, .pre)),
            "ldr d0, [sp, #8]!",
            0xe0, 0x8f, 0x40, 0xfc
        )
        try XCTAssertM1Op(
            M1Op.ldr(D.d0, .reg64offset(.sp, 8, .post)),
            "ldr d0, [sp], #8",
            0xe0, 0x87, 0x40, 0xfc
        )
        try XCTAssertM1Op(
            M1Op.ldr(S.s0, .reg64offset(.sp, 4, nil)),
            "ldr s0, [sp, #4]",
            0xe0, 0x07, 0x40, 0xbd
        )
        try XCTAssertM1Op(
            M1Op.ldr(S.s0, .reg64offset(.sp, 2, nil)),
            "ldr s0, [sp, #2]",
            0xe0, 0x23, 0x40, 0xbc
        )
    }
    
    func testStr_fp() throws {
        try XCTAssertM1Op(
            M1Op.str(D.d0, .reg64offset(.sp, 4, nil)),
            "str d0, [sp, #4]",
            0xe0, 0x43, 0x00, 0xfc
        )
        try XCTAssertM1Op(
            M1Op.str(D.d0, .reg64offset(.sp, 8, nil)),
            "str d0, [sp, #8]",
            0xe0, 0x07, 0x00, 0xfd
        )
        try XCTAssertM1Op(
            M1Op.str(D.d0, .reg64offset(.sp, 8, .pre)),
            "str d0, [sp, #8]!",
            0xe0, 0x8f, 0x00, 0xfc
        )
        try XCTAssertM1Op(
            M1Op.str(D.d0, .reg64offset(.sp, 8, .post)),
            "str d0, [sp], #8",
            0xe0, 0x87, 0x00, 0xfc
        )
        try XCTAssertM1Op(
            M1Op.str(S.s0, .reg64offset(.sp, 4, .post)),
            "str s0, [sp], #4",
            0xe0, 0x47, 0x00, 0xbc
        )
        try XCTAssertM1Op(
            M1Op.str(S.s0, .reg64offset(.sp, 2, .post)),
            "str s0, [sp], #2",
            0xe0, 0x27, 0x00, 0xbc
        )
        try XCTAssertM1Op(
            M1Op.str(S.s0, .reg64offset(.sp, 2, nil)),
            "str s0, [sp, #2]",
            0xe0, 0x23, 0x00, 0xbc
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
        
        // str not stur follows
        XCTAssertEqual(
            try EmitterM1.emit(for: .str(Register64.x0, .reg64offset(.sp, -16, .pre))),
            [0xe0, 0x0f, 0x1f, 0xf8]
        )
        XCTAssertEqual(
            try EmitterM1.emit(for: .str(Register64.x0, .reg64offset(.sp, -16, .post))),
            [0xe0, 0x07, 0x1f, 0xf8]
        )
    }
    
    func testStrLdr_3regs_regression() throws {
        try XCTAssertM1Op(
            M1Op.str(X.x3, .reg(X.x2, .r64ext(X.x1, .sxtx(0)))),
                                   "str x3, [x2, x1, sxtx #0]",
                                   0x43, 0xe8, 0x21, 0xf8)
        try XCTAssertM1Op(
            M1Op.ldr(X.x3, .reg(X.x2, .r64ext(X.x1, .sxtx(0)))),
                                   "ldr x3, [x2, x1, sxtx #0]",
                                   0x43, 0xe8, 0x61, 0xf8)
        try XCTAssertM1Op(
            M1Op.str(X.x3, .reg(X.x2, .r64ext(X.x1, .sxtx(3)))),
                                   "str x3, [x2, x1, sxtx #3]",
                                   0x43, 0xf8, 0x21, 0xf8)
        try XCTAssertM1Op(
            M1Op.ldr(X.x3, .reg(X.x2, .r64ext(X.x1, .sxtx(3)))),
                                   "ldr x3, [x2, x1, sxtx #3]",
                                   0x43, 0xf8, 0x61, 0xf8)
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
            try EmitterM1.emit(for: .ldp((X.x29_fp, X.x30_lr), .reg64offset(.sp, 16, .post))),
            [0xfd, 0x7b, 0xc1, 0xa8]
        )
        XCTAssertEqual(
            try EmitterM1.emit(for: .ldp((X.x0, X.x1), .reg64offset(.sp, 16, .post))),
            [0xe0, 0x07, 0xc1, 0xa8]
        )
        XCTAssertEqual(
            try EmitterM1.emit(for: .ldp((X.x0, X.x1), .reg64offset(.sp, 16, nil))),
            [0xe0, 0x07, 0x41, 0xa9]
        )
        XCTAssertEqual(
            try EmitterM1.emit(for: .ldp((X.x0, X.x1), .reg64offset(.sp, 16, .pre))),
            [0xe0, 0x07, 0xc1, 0xa9]
        )
    }
    
    func testFcvt() throws {
        try XCTAssertM1Op(
            M1Op.fcvt(D.d1, S.s2),
            "fcvt d1, s2",
            0x41, 0xc0, 0x22, 0x1e
        )
        try XCTAssertM1Op(
            M1Op.fcvt(S.s3, D.d4),
            "fcvt s3, d4",
            0x83, 0x40, 0x62, 0x1e
        )
        try XCTAssertM1Op(
            M1Op.fcvt(H.h3, D.d4),
            "fcvt h3, d4",
            0x83, 0xc0, 0x63, 0x1e
        )
        try XCTAssertM1Op(
            M1Op.fcvt(D.d3, H.h4),
            "fcvt d3, h4",
            0x83, 0xc0, 0xe2, 0x1e
        )
        try XCTAssertM1Op(
            M1Op.fcvt(S.s0, D.d0),
            "fcvt s0, d0",
            0x00, 0x40, 0x62, 0x1e
        )
    }
    
    func testLdp_fp() throws {
        try XCTAssertM1Op(
            M1Op.ldp((D.d0, D.d1), .reg64offset(.sp, 8, nil)),
            "ldp d0, d1, [sp, #8]",
            0xe0, 0x87, 0x40, 0x6d
        )
        try XCTAssertM1Op(
            M1Op.ldp((D.d0, D.d1), .reg64offset(.sp, 8, .pre)),
            "ldp d0, d1, [sp, #8]!",
            0xe0, 0x87, 0xc0, 0x6d
        )
        try XCTAssertM1Op(
            M1Op.ldp((D.d0, D.d1), .reg64offset(.sp, 8, .post)),
            "ldp d0, d1, [sp], #8",
            0xe0, 0x87, 0xc0, 0x6c
        )
        try XCTAssertM1Op(
            M1Op.ldp((S.s0, S.s1), .reg64offset(.sp, 8, .post)),
            "ldp s0, s1, [sp], #8",
            0xe0, 0x07, 0xc1, 0x2c
        )
    }
    
    func testStp_fp() throws {
        try XCTAssertM1Op(
            M1Op.stp((D.d0, D.d1), .reg64offset(.sp, 8, nil)),
            "stp d0, d1, [sp, #8]",
            0xe0, 0x87, 0x00, 0x6d
        )
        try XCTAssertM1Op(
            M1Op.stp((D.d0, D.d1), .reg64offset(.sp, 8, .pre)),
            "stp d0, d1, [sp, #8]!",
            0xe0, 0x87, 0x80, 0x6d
        )
        try XCTAssertM1Op(
            M1Op.stp((D.d0, D.d1), .reg64offset(.sp, 8, .post)),
            "stp d0, d1, [sp], #8",
            0xe0, 0x87, 0x80, 0x6c
        )
        try XCTAssertM1Op(
            M1Op.stp((S.s0, S.s1), .reg64offset(.sp, 8, .post)),
            "stp s0, s1, [sp], #8",
            0xe0, 0x07, 0x81, 0x2c
        )
    }
    
    func testStp() throws {
        XCTAssertThrowsError(try EmitterM1.emit(for: .stp((X.x0, X.x1), .immediate_depr(10))))
        { error in
            XCTAssertEqual(
                error as! EmitterM1Error,
                EmitterM1Error.invalidOffset("STP can only have .reg64offset offset")
            )
        }
        
        XCTAssertThrowsError(
            try EmitterM1.emit(for: .stp((X.x0, X.x1), .reg64offset(.sp, -520, nil)))
        ) { error in
            XCTAssertEqual(
                error as! EmitterM1Error,
                EmitterM1Error.invalidValue("Immediate -520 must fit in 7 bits")
            )
        }
        
        XCTAssertEqual(
            try EmitterM1.emit(for: .stp((X.x10, X.x12), .reg64offset(.sp, 16, .pre))),
            [0xea, 0x33, 0x81, 0xa9]
        )
        
        XCTAssertEqual(
            try EmitterM1.emit(for: .stp((X.x10, X.x12), .reg64offset(.sp, 16, .post))),
            [0xea, 0x33, 0x81, 0xa8]
        )
        
        XCTAssertEqual(
            try EmitterM1.emit(for: .stp((X.x10, X.x12), .reg64offset(.sp, 16, nil))),
            [0xea, 0x33, 0x01, 0xa9]
        )
        
        XCTAssertEqual(
            try EmitterM1.emit(
                for: .stp((X.x29_fp, X.x30_lr), .reg64offset(.sp, -512, .pre))
            ),
            [0xfd, 0x7b, 0xa0, 0xa9]
        )
        
        XCTAssertEqual(
            try EmitterM1.emit(
                for: .stp((X.x29_fp, X.x30_lr), .reg64offset(.sp, -16, .pre))
            ),
            [0xfd, 0x7b, 0xbf, 0xa9]
        )
    }
    
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
    
    func testBr() throws {
        try XCTAssertM1Op(.br(.x0), "br x0", 0x00, 0x00, 0x1f, 0xd6)
        try XCTAssertM1Op(.br(.x8), "br x8", 0x00, 0x01, 0x1f, 0xd6)
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
        try XCTAssertM1Op(
            M1Op.ldr(W.w0, .reg(X.x0, .r64shift(X.x1, .lsl(0)))),
            "ldr w0, [x0, x1]",
            0x00, 0x68, 0x61, 0xb8)
        
        try XCTAssertM1Op(
            M1Op.ldr(X.x1, .reg64offset(.x0, 56, nil)),
            "ldr x1, [x0, #56]",
            0x01, 0x1c, 0x40, 0xf9)
        
        try XCTAssertM1Op(
            M1Op.ldr(X.x1, .reg(X.x0, .imm(56, nil))),
            "ldr x1, [x0, #56]",
            0x01, 0x1c, 0x40, 0xf9)
        
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
    
    func testFmov() throws {
        try XCTAssertM1Op(
            .fmov(D.d2, D.d3),
            "fmov d2, d3",
            0x62, 0x40, 0x60, 0x1e
        )
        try XCTAssertM1Op(
            .fmov(S.s2, S.s3),
            "fmov s2, s3",
            0x62, 0x40, 0x20, 0x1e
        )
        try XCTAssertM1Op(
            .fmov(H.h2, H.h3),
            "fmov h2, h3",
            0x62, 0x40, 0xe0, 0x1e
        )
        try XCTAssertM1Op(
            .fmov(D.d1, X.x3),
            "fmov d1, x3",
            0x61, 0x00, 0x67, 0x9e
        )
        try XCTAssertM1Op(
            .fmov(X.x1, D.d3),
            "fmov x1, d3",
            0x61, 0x00, 0x66, 0x9e
        )
        try XCTAssertM1Op(
            .fmov(S.s1, W.w3),
            "fmov s1, w3",
            0x61, 0x00, 0x27, 0x1e
        )
        try XCTAssertM1Op(
            .fmov(W.w1, S.s3),
            "fmov w1, s3",
            0x61, 0x00, 0x26, 0x1e
        )
        try XCTAssertM1Op(
            .fmov(H.h1, W.w3),
            "fmov h1, w3",
            0x61, 0x00, 0xe7, 0x1e
        )
        try XCTAssertM1Op(
            .fmov(W.w1, H.h3),
            "fmov w1, h3",
            0x61, 0x00, 0xe6, 0x1e
        )
        try XCTAssertM1Op(
            .fmov(H.h1, X.x3),
            "fmov h1, x3",
            0x61, 0x00, 0xe7, 0x9e
        )
        try XCTAssertM1Op(
            .fmov(X.x1, H.h3),
            "fmov x1, h3",
            0x61, 0x00, 0xe6, 0x9e
        )
        
        // sizes should be the same
        XCTAssertThrowsError(
            try M1Op.fmov(H.h2, D.d3).emit()
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
    
    func testEor_r() throws {
        try XCTAssertM1Op(
            .eor_r(W.w1, W.w2, W.w3, nil),
            "eor w1, w2, w3",
            0x41, 0x00, 0x03, 0x4a
        )
        try XCTAssertM1Op(
            .eor_r(X.x1, X.x2, X.x3, nil),
            "eor x1, x2, x3",
            0x41, 0x00, 0x03, 0xca
        )
        try XCTAssertM1Op(
            .eor_r(X.x1, X.x2, X.x3, .lsl(4)),
            "eor x1, x2, x3, lsl #4",
            0x41, 0x10, 0x03, 0xca
        )
    }
    
    func testMul() throws {
        try XCTAssertM1Op(
            .mul(W.w1, W.w2, W.w3),
            "mul w1, w2, w3",
            0x41, 0x7c, 0x03, 0x1b
        )
        try XCTAssertM1Op(
            .mul(X.x1, X.x2, X.x3),
            "mul x1, x2, x3",
            0x41, 0x7c, 0x03, 0x9b
        )
    }
    
    func testFmul() throws {
        try XCTAssertM1Op(
            .fmul(S.s1, S.s2, S.s3),
            "fmul s1, s2, s3",
            0x41, 0x08, 0x23, 0x1e
        )
        try XCTAssertM1Op(
            .fmul(D.d1, D.d2, D.d3),
            "fmul d1, d2, d3",
            0x41, 0x08, 0x63, 0x1e
        )
        try XCTAssertM1Op(
            .fmul(H.h1, H.h2, H.h3),
            "fmul h1, h2, h3",
            0x41, 0x08, 0xe3, 0x1e
        )
    }
    
    func testMadd() throws {
        try XCTAssertM1Op(
            .madd(X.x1, X.x2, X.x3, X.x4),
            "madd x1, x2, x3, x4",
            0x41, 0x10, 0x03, 0x9b
        )
        try XCTAssertM1Op(
            .madd(W.w1, W.w2, W.w3, W.wZR),
            "mul w1, w2, w3",
            0x41, 0x7c, 0x03, 0x1b
        )
        try XCTAssertM1Op(
            .madd(X.x1, X.x2, X.x3, X.xZR),
            "mul x1, x2, x3",
            0x41, 0x7c, 0x03, 0x9b
        )
    }
}
