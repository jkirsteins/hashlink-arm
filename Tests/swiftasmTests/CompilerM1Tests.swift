import XCTest

@testable import swiftasm

extension Sequence where Iterator.Element: Hashable {
    func unique() -> [Iterator.Element] {
        var seen: [Iterator.Element: Bool] = [:]
        return self.filter { seen.updateValue(true, forKey: $0) == nil }
    }
}

func sut() -> M1Compiler {
    M1Compiler(stripDebugMessages: true)
}

/* Helper method for compiling a HLFunction and inspecting results */
func compileHLFunction(
    ctx: JitContext,
    findex: Int32,
    regs: [HLType],
    ret: HLType,
    args: [HLType],
    into mem: OpBuilder,
    ops: [HLOpCode]) throws -> HLCompiledFunction
{
    let sut = sut()

    let funcType = HLType.fun(HLTypeFunData(args: Resolvable.array(args), ret: Resolvable(ret)))

    let f = HLFunction(type: Resolvable(funcType),
        findex: findex,
        regs: Resolvable.array(regs),
        ops: ops,
        assigns: []
    )

    let compiled = try sut.compile(native: f, into: mem, ctx: ctx)

    ctx.storage.functionTable.wrappedValue.insert(f, at: Int(findex))
    ctx.storage.compiledFunctionTable.wrappedValue.insert(compiled, at: Int(findex))

    return compiled
}

final class CompilerM1Tests: XCTestCase {
    /* Test handling deferred addresses.

    Not testing output on purpose here (in favor
    of smaller, less brittle tests targetting specific compilation
    outputs). */
    func testCompile_addressesAreAvailable() throws {

        let storage = ModuleStorage(
            nstrings: 0, 
            ntypes: 2, 
            nglobals: 0, 
            nnatives: 0, 
            nfunctions: 2, 
            nconstants: 0)
        
        storage.typeTable.wrappedValue = [
            .void,
            HLType.fun(HLTypeFunData(args: [], ret: Resolvable(.void)))
        ]

        let sut = sut()

        let f1 = HLFunction(
            type: storage.typeResolver.getResolvable(1),  // ()->void,
            findex: 0,
            regs: [Resolvable(.void)],
            ops: [.OCall0(dst: 0 /*reg*/, fun: 1 /*findex*/), .ORet(ret: 0)],
            assigns: []
        )
        let f2 = HLFunction(
            type: storage.typeResolver.getResolvable(1),  // ()->void,
            findex: 1,
            regs: [Resolvable(.void)],
            ops: [
                .OCall0(dst: 0 /*reg*/, fun: 1 /*findex*/)  // .ORet(ret: 0)
            ],
            assigns: []
        )

        let opb = OpBuilder()
        let ctx = JitContext(storage: storage)

        let compiled1 = try sut.compile(native: f1, into: opb, ctx: ctx)
        let compiled2 = try sut.compile(native: f2, into: opb, ctx: ctx)

        // finalize
        // compiledTable.wrappedValue += [compiled2, compiled1]
        ctx.jitBase.wrappedValue = UnsafeMutableRawPointer(bitPattern: 1337)

        try ctx.wft.requireReady()
        let rawData = opb.build()

        let f1addr = ctx.wft.getAddr(0)  // should map w jitBase
        let f2addr = ctx.wft.getAddr(1)  // should be jitBase + len(f1)

        XCTAssertEqual(1337, (try ctx.wft.get(0)).memory.immediate)
        XCTAssertGreaterThan((try ctx.wft.get(1)).memory.immediate, 1337)
        XCTAssertEqual(f1addr.immediate, (try ctx.wft.get(0)).memory.immediate)
        XCTAssertEqual(f2addr.immediate, (try ctx.wft.get(1)).memory.immediate)
    }

    func testCompile_emptyFunction() throws {
        let storage = ModuleStorage(nfunctions: 1) 
        let ctx = JitContext(storage: storage)
        let mem = OpBuilder(ctx: ctx)
        let f = try compileHLFunction(ctx: ctx, findex: 0, regs: [.void], ret: .void, args: [], into: mem, ops: [
            .ORet(ret: 0)
        ])

        // mem.hexPrint()

        XCTAssertEqual(mem.build(), [
            0xfd, 0x7b, 0xbf, 0xa9, // stp x29, x30, [sp, #-16]!
            0xfd, 0x03, 0x00, 0x91, // movr x29, sp
            0xe0, 0x03, 0x40, 0xf9, // ldr x0, [sp, #0]
            0x01, 0x00, 0x00, 0x14, // b #4
            0xfd, 0x7b, 0xc1, 0xa8, // ldp x29, x30, [sp], #16
            0xc0, 0x03, 0x5f, 0xd6, // ret
        ])

        // run the entrypoint and ensure it works
        let entrypoint: JitVoid = mem.buildEntrypoint(f)
        entrypoint()
    }

    func testCompile_simpleReturn() throws {
        let storage = ModuleStorage(nfunctions: 1, ints: [165]) 
        let ctx = JitContext(storage: storage)
        let mem = OpBuilder(ctx: ctx)
        let f = try compileHLFunction(ctx: ctx, findex: 0, regs: [.void, .i32], ret: .i32, args: [], into: mem, ops: [
            .OInt(dst: 1, ptr: 0),
            .ORet(ret: 1)
        ])
        
        // mem.hexPrint()
        
        XCTAssertEqual(mem.build(), [
            0xfd, 0x7b, 0xbf, 0xa9, // stp x29, x30, [sp, #-16]!
            0xfd, 0x03, 0x00, 0x91, // movr x29, sp
            0xff, 0x43, 0x00, 0xd1, // sub sp, sp, #16
            0xe0, 0x03, 0x00, 0xf8, // str x0, [sp, #0]
            0xa0, 0x14, 0x80, 0xd2, // .mov x0, #165
            0x00, 0x00, 0xa0, 0xf2, // 
            0x00, 0x00, 0xc0, 0xf2, // 
            0x00, 0x00, 0xe0, 0xf2, // 
            0xe0, 0x03, 0x00, 0xf8, // str x0, [sp, #0]
            0xe0, 0x03, 0x40, 0xf9, // ldr x0, [sp, #0]
            0x01, 0x00, 0x00, 0x14, // b #4
            0xff, 0x43, 0x00, 0x91, // add sp, sp, #16
            0xfd, 0x7b, 0xc1, 0xa8, // ldp x29, x30, [sp], #16
            0xc0, 0x03, 0x5f, 0xd6, // ret
        ])

        let entrypoint: JitInt64 = mem.buildEntrypoint(f)
        var res: Int64 = entrypoint()
        XCTAssertEqual(165, res)
    }

    func testCalcStackArgReq() throws {
        let sut = sut()

        // test different size combinations (ensure aligned to 16 bytes)
        var (size, _) = sut.calcStackArgReq(regs: [.array, .array], args: [])
        XCTAssertEqual(16, size)

        (size, _) = sut.calcStackArgReq(regs: [.array, .array, .i32], args: [])
        XCTAssertEqual(32, size)

        (size, _) = sut.calcStackArgReq(regs: [.array, .array, .i32, .dyn, .dynobj], args: [])
        XCTAssertEqual(48, size)

        (size, _) = sut.calcStackArgReq(
            regs: [.array, .array, .i32, .dyn, .dynobj], 
            args: [.array, .array, .i32, .dyn, .dynobj])
        XCTAssertEqual(48, size)

        // args exceeding first 8 should not allocate extra space (as it
        // should already be allocated due to calling convention)
        (size, _) = sut.calcStackArgReq(
            regs: Array(repeating: .dyn, count: 16), 
            args: Array(repeating: .dyn, count: 16))
        XCTAssertEqual(64, size)

        // non args should take space
        (size, _) = sut.calcStackArgReq(regs: [.i32], args: [])
        XCTAssertEqual(16, size)

        // 4 regs (all except 1st) and 1 arg should contribute to size here
        (size, _) = sut.calcStackArgReq(
            regs: [.array] + Array(repeating: .i32, count: 4), 
            args: [.array])
        XCTAssertEqual(32, size)

        // first 8 args should take space
        (size, _) = sut.calcStackArgReq(
            regs: Array(repeating: .i32, count: 8), 
            args: Array(repeating: .i32, count: 8))
        XCTAssertEqual(32, size)

        // void should be ignored
        (size, _) = sut.calcStackArgReq(
            regs: Array(repeating: .void, count: 8) + Array(repeating: .i32, count: 8), 
            args: Array(repeating: .void, count: 8) + Array(repeating: .i32, count: 8))
        XCTAssertEqual(32, size)
    }

    func testAppendPrologue() throws {
        let mem = OpBuilder()
        sut().appendPrologue(builder: mem)

        XCTAssertEqual(mem.build(), [0xfd, 0x7b, 0xbf, 0xa9, 0xfd, 0x03, 0x00, 0x91])
    }

    func testAppendEpilogue() throws {
        let mem = OpBuilder()
        sut().appendEpilogue(builder: mem)

        XCTAssertEqual(mem.build(), [0xfd, 0x7b, 0xc1, 0xa8])
    }

    func testGetRegStackOffset_min16() throws {
        let regs = [HLType.i32, HLType.void, HLType.void, HLType.i32, HLType.void]
        // we need 4 bytes but stack for x0 has moved 16 bytes
        let args = [HLType.i32]
        let sut = sut()

        // First reg is in args, so offset 0
        XCTAssertEqual(0, sut.getRegStackOffset(regs, args: args, reg: 0))

        // Second reg is in stack after the register area,
        // which is aligned to 16 bytes
        XCTAssertEqual(16, sut.getRegStackOffset(regs, args: args, reg: 3))
    }

    func testGetRegStackOffset_noMin_funcArgs() throws {
        let regs = [HLType.i32, HLType.void, HLType.i32]
        // we need 4 bytes but stack for x0 has moved 16 bytes
        let args = [HLType.i32, HLType.void, HLType.i32]

        // Second reg is in stack after the register area,
        // which is aligned to 16 bytes
        XCTAssertEqual(4, sut().getRegStackOffset(regs, args: args, reg: 1))
    }

    func testGetRegStackOffset_void() throws {
        let regs = [HLType.void]
        // we need 4 bytes but stack for x0 has moved 16 bytes
        let args = [HLType.void]
        XCTAssertEqual(0, sut().getRegStackOffset(regs, args: args, reg: 0))
    }

    func testAppendStackInit_skipVoid() throws {
        let mem = OpBuilder()
        try sut().appendStackInit([.void], args: [.void], builder: mem)
        XCTAssertEqual([], mem.build())
    }

    func testAppendStackInit_min16() throws {
        let _1_need16 = Array(repeating: HLType.i32, count: 1)
        let _4_need16 = Array(repeating: HLType.i32, count: 4)
        let _5_need32 = Array(repeating: HLType.i32, count: 5)
        let sut = sut()
        // 4 byte requirement should still be aligned to 16 byte boundary
        let mem1 = OpBuilder()
        try sut.appendStackInit(_1_need16, args: _1_need16, builder: mem1)
        XCTAssertEqual(
            [
                0xff, 0x43, 0x00, 0xd1,  // sub sp, sp, #16
                0xe0, 0x03, 0x00, 0xf8,  // str x0, [sp, #0]
            ],
            mem1.build()
        )

        // 16 byte requirement should not round to 32
        let mem2 = OpBuilder()
        try sut.appendStackInit(_4_need16, args: _4_need16, builder: mem2)
        XCTAssertEqual(
            [
                0xff, 0x43, 0x00, 0xd1,  // sub sp, sp, #16
                0xe0, 0x03, 0x00, 0xf8,  // str x0, [sp, #0]
                0xe1, 0x43, 0x00, 0xf8,  // str x1, [sp, #4]
                0xe2, 0x83, 0x00, 0xf8,  // str x2, [sp, #8]
                0xe3, 0xc3, 0x00, 0xf8,  // str x3, [sp, #12]
            ],
            mem2.build()
        )
        // 20 byte requirement should round to 32
        let mem3 = OpBuilder()
        try sut.appendStackInit(_5_need32, args: _5_need32, builder: mem3)
        XCTAssertEqual(
            [
                0xff, 0x83, 0x00, 0xd1,  // sub sp, sp, #32
                0xe0, 0x03, 0x00, 0xf8,  // str x0, [sp, #0]
                0xe1, 0x43, 0x00, 0xf8,  // str x1, [sp, #4]
                0xe2, 0x83, 0x00, 0xf8,  // str x2, [sp, #8]
                0xe3, 0xc3, 0x00, 0xf8,  // str x3, [sp, #12]
                0xe4, 0x03, 0x01, 0xf8,  // str x4, [sp, #16]
            ],
            mem3.build()
        )
    }

    func testAppendStackInit_multiple() throws {
        let mem = OpBuilder()
        let sut = sut()
        try sut.appendStackInit(
            [.void, .i32, .i64],
            args: [.void, .i32, .i64],
            builder: mem
        )
        XCTAssertEqual(
            [
                0xff, 0x43, 0x00, 0xd1,  // sub sp, sp, #16
                0xe0, 0x03, 0x00, 0xf8,  // str x0, [sp, #0]
                0xe1, 0x43, 0x00, 0xf8,  // str x1, [sp, #4]
            ],
            mem.build()
        )
    }

    func testAppendStackInit_moreThan8Args() throws {
        let mem = OpBuilder()
        try sut().appendStackInit(
            Array(repeating: HLType.i32, count: 12),
            args: Array(repeating: HLType.i32, count: 12),
            builder: mem
        )
        
        XCTAssertEqual(
            [
                0xff, 0x83, 0x00, 0xd1,  // sub sp, sp, #32
                0xe0, 0x03, 0x00, 0xf8,  // str x0, [sp, #0]
                0xe1, 0x43, 0x00, 0xf8,  // str x1, [sp, #4]
                0xe2, 0x83, 0x00, 0xf8,  // str x2, [sp, #8]
                0xe3, 0xc3, 0x00, 0xf8,  // str x3, [sp, #12]
                0xe4, 0x03, 0x01, 0xf8,  // str x4, [sp, #16]
                0xe5, 0x43, 0x01, 0xf8,  // str x5, [sp, #20]
                0xe6, 0x83, 0x01, 0xf8,  // str x6, [sp, #24]
                0xe7, 0xc3, 0x01, 0xf8,  // str x7, [sp, #28]
            ],
            mem.build()
        )
    }

    func testAppendStackInit_mismatchedRegs() throws {
        let mem = OpBuilder()

        XCTAssertThrowsError(
            try sut().appendStackInit([.i32], args: [.void], builder: mem)
        )
    }

    func testAppendDebugPrintAligned4() throws {
        let memWith = OpBuilder()
        let memWithout = OpBuilder()
        M1Compiler(stripDebugMessages: false).appendDebugPrintAligned4("Hello World", builder: memWith)
        M1Compiler(stripDebugMessages: true).appendDebugPrintAligned4("Hello World", builder: memWithout)

        XCTAssertEqual(
            memWithout.build(),
            []
        )

        XCTAssertEqual(
            memWith.build(),
            [
                // Printing debug message: Hello World
                0xe0, 0x0f, 0x1e, 0xf8, // str x0, [sp, #-32]!
                0xe1, 0x83, 0x00, 0xf8, // str x1, [sp, #8]
                0xe2, 0x03, 0x01, 0xf8, // str x2, [sp, #16]
                0xf0, 0x83, 0x01, 0xf8, // str x16, [sp, #24]
                0x20, 0x00, 0x80, 0xd2, // movz x0, #1
                0x21, 0x01, 0x00, 0x10, // adr x1, #36
                0xe2, 0x02, 0x80, 0xd2, // movz x2, #23
                0x90, 0x00, 0x80, 0xd2, // movz x16, #4
                0x01, 0x10, 0x00, 0xd4, // svc 0x0080
                0xf0, 0x0f, 0x40, 0xf9, // ldr x16, [sp, #24]
                0xe2, 0x0b, 0x40, 0xf9, // ldr x2, [sp, #16]
                0xe1, 0x07, 0x40, 0xf9, // ldr x1, [sp, #8]
                0xe0, 0x07, 0x42, 0xf8, // ldr x0, [sp], #32
                0x07, 0x00, 0x00, 0x14, // b #28
                0x5b, 0x6a, 0x69, 0x74, // [jit
                0x64, 0x65, 0x62, 0x75, // debu
                0x67, 0x5d, 0x20, 0x48, // g].H
                0x65, 0x6c, 0x6c, 0x6f, // ello
                0x20, 0x57, 0x6f, 0x72, // .Wor
                0x6c, 0x64, 0x0a, // ld\n
                0x00, // .zero
            ]
        )
    }
}
