import XCTest

@testable import swiftasm

final class CompilerM1Tests: XCTestCase {
    
    /* Test handling deferred addresses.
    
    Not testing output on purpose here (in favor
    of smaller, less brittle tests targetting specific compilation
    outputs). */ 
    func testCompile_addressesAreAvailable() throws {
        let typeTable = SharedStorage(wrappedValue: [
            HLType.void
        ])

        let compiledTable: SharedStorage<[HLCompiledFunction]> = SharedStorage(wrappedValue: [])
        let nativeTable: SharedStorage<[HLNative]> = SharedStorage(wrappedValue: [])
        
        
        let types = TableResolver(table: typeTable, count: 2)
        let compiledFunctions = TableResolver(table: compiledTable, count: 2)
        let natives = TableResolver(table: nativeTable, count: 0)

        typeTable.wrappedValue.append(HLType.fun(HLTypeFunData(args: [], ret: types.getResolvable(0 /*void*/))))
        

        let sut = M1Compiler()

        let f1 = HLFunction(
            type: types.getResolvable(1),   // ()->void,
            findex: 0,
            regs: [types.getResolvable(0) /*void is always present*/],
            ops: [
                .OCall0(dst: 0 /*reg*/, fun: 1 /*findex*/),
                .ORet(ret: 0)
            ],
            assigns: []
        )
        let f2 = HLFunction(
            type: types.getResolvable(1),   // ()->void,
            findex: 1,
            regs: [types.getResolvable(0) /*void is always present*/],
            ops: [
                .OCall0(dst: 0 /*reg*/, fun: 1 /*findex*/),
                // .ORet(ret: 0)
            ],
            assigns: []
        )

        let jitBase: SharedStorage<UnsafeMutableRawPointer?> = SharedStorage(wrappedValue: nil)
        let wft = WholeFunctionsTable(natives: natives, functions: compiledFunctions, jitBase: jitBase)
        let opb = OpBuilder()
        let ctx = JitContext(wft: wft)

        let compiled1 = try sut.compile(native: f1, into: opb, ctx: ctx)
        let compiled2 = try sut.compile(native: f2, into: opb, ctx: ctx)

        // finalize
        // compiledTable.wrappedValue += [compiled2, compiled1]
        jitBase.wrappedValue = UnsafeMutableRawPointer(bitPattern: 1337)

        try wft.requireReady()        
        let rawData = opb.build()

        let f1addr = wft.getAddr(0) // should map w jitBase
        let f2addr = wft.getAddr(1) // should be jitBase + len(f1)

        XCTAssertEqual(1337, (try wft.get(0)).memory.immediate)
        XCTAssertGreaterThan((try wft.get(1)).memory.immediate, 1337)
        XCTAssertEqual(f1addr.immediate, (try wft.get(0)).memory.immediate)
        XCTAssertEqual(f2addr.immediate, (try wft.get(1)).memory.immediate)
    }

    func testAppendPrologue() throws {
        let sut = OpBuilder()
        appendPrologue(builder: sut)

        XCTAssertEqual(
            sut.build(),
            [0xfd, 0x7b, 0xbf, 0xa9,
             0xfd, 0x03, 0x00, 0x91]
        )
    }

    func testAppendEpilogue() throws {
        let sut = OpBuilder()
        appendEpilogue(builder: sut)

        XCTAssertEqual(
            sut.build(),
            [0xfd, 0x7b, 0xc1, 0xa8]
        )
    }

    func testGetRegStackOffset_min16() throws {
        let regs = [HLType.i32, HLType.void, HLType.void, HLType.i32, HLType.void]
        // we need 4 bytes but stack for x0 has moved 16 bytes
        let args = [HLType.i32]

        // First reg is in args, so offset 0
        XCTAssertEqual(
            0,
            getRegStackOffset(regs, args: args, reg: 0)
        )

        // Second reg is in stack after the register area,
        // which is aligned to 16 bytes
        XCTAssertEqual(
            16,
            getRegStackOffset(regs, args: args, reg: 3)
        )
    }

    func testGetRegStackOffset_noMin_funcArgs() throws {
        let regs = [HLType.i32, HLType.void, HLType.i32]
        // we need 4 bytes but stack for x0 has moved 16 bytes
        let args = [HLType.i32, HLType.void, HLType.i32]

        // Second reg is in stack after the register area,
        // which is aligned to 16 bytes
        XCTAssertEqual(
            4,
            getRegStackOffset(regs, args: args, reg: 1)
        )
    }

    func testGetRegStackOffset_void() throws {
        let regs = [HLType.void]
        // we need 4 bytes but stack for x0 has moved 16 bytes
        let args = [HLType.void]
        XCTAssertEqual(
            0,
            getRegStackOffset(regs, args: args, reg: 0)
        )
    }

    func testAppendStackInit_skipVoid() throws {
        let mem = OpBuilder()
        try appendStackInit([.void], args: [.void], builder: mem)
        
        XCTAssertEqual([], mem.build())
    }

    func testAppendStackInit_min16() throws {
        let _1_need16 = Array(repeating: HLType.i32, count: 1)
        let _4_need16 = Array(repeating: HLType.i32, count: 4)
        let _5_need32 = Array(repeating: HLType.i32, count: 5)
        
        // 4 byte requirement should still be aligned to 16 byte boundary
        let mem1 = OpBuilder()
        try appendStackInit(_1_need16, args: _1_need16, builder: mem1)
        XCTAssertEqual([
            0xff, 0x43, 0x00, 0xd1, // sub sp, sp, #16
            0xe0, 0x03, 0x00, 0xf8, // str x0, [sp, #0]
        ], mem1.build())

        // 16 byte requirement should not round to 32
        let mem2 = OpBuilder()
        try appendStackInit(_4_need16, args: _4_need16, builder: mem2)
        XCTAssertEqual([
            0xff, 0x43, 0x00, 0xd1, // sub sp, sp, #16
            0xe0, 0x03, 0x00, 0xf8, // str x0, [sp, #0]
            0xe1, 0x43, 0x00, 0xf8, // str x1, [sp, #4]
            0xe2, 0x83, 0x00, 0xf8, // str x2, [sp, #8]
            0xe3, 0xc3, 0x00, 0xf8, // str x3, [sp, #12]
        ], mem2.build())
        
        // 20 byte requirement should round to 32
            let mem3 = OpBuilder()
        try appendStackInit(_5_need32, args: _5_need32, builder: mem3)
            
        XCTAssertEqual([
            0xff, 0x83, 0x00, 0xd1, // sub sp, sp, #32
            0xe0, 0x03, 0x00, 0xf8, // str x0, [sp, #0]
            0xe1, 0x43, 0x00, 0xf8, // str x1, [sp, #4]
            0xe2, 0x83, 0x00, 0xf8, // str x2, [sp, #8]
            0xe3, 0xc3, 0x00, 0xf8, // str x3, [sp, #12]
            0xe4, 0x03, 0x01, 0xf8, // str x4, [sp, #16]
        ], mem3.build())
    }

    func testAppendStackInit_multiple() throws {
        let mem = OpBuilder()
        try appendStackInit([.void, .i32, .i64], args: [.void, .i32, .i64], builder: mem)
        
        XCTAssertEqual([
            0xff, 0x43, 0x00, 0xd1, // sub sp, sp, #16
            0xe0, 0x03, 0x00, 0xf8, // str x0, [sp, #0]
            0xe1, 0x43, 0x00, 0xf8, // str x1, [sp, #4]
        ], mem.build())
    }

    func testAppendStackInit_moreThan8Args() throws {
        let mem = OpBuilder()
        try appendStackInit(
            Array(repeating: HLType.i32, count: 12), 
            args: Array(repeating: HLType.i32, count: 12), 
            builder: mem)
        
        mem.hexPrint()

        XCTAssertEqual([
            0xff, 0x83, 0x00, 0xd1, // sub sp, sp, #32
            0xe0, 0x03, 0x00, 0xf8, // str x0, [sp, #0]
            0xe1, 0x43, 0x00, 0xf8, // str x1, [sp, #4]
            0xe2, 0x83, 0x00, 0xf8, // str x2, [sp, #8]
            0xe3, 0xc3, 0x00, 0xf8, // str x3, [sp, #12]
            0xe4, 0x03, 0x01, 0xf8, // str x4, [sp, #16]
            0xe5, 0x43, 0x01, 0xf8, // str x5, [sp, #20]
            0xe6, 0x83, 0x01, 0xf8, // str x6, [sp, #24]
            0xe7, 0xc3, 0x01, 0xf8, // str x7, [sp, #28]
        ], mem.build())
    }

    func testAppendStackInit_mismatchedRegs() throws {
        let mem = OpBuilder()

        XCTAssertThrowsError(
            try appendStackInit([.i32], args: [.void], builder: mem)
        )
    }

    func testAppendDebugPrintAligned4() throws {
        let sut = OpBuilder()
        appendDebugPrintAligned4("Hello World", builder: sut)

        XCTAssertEqual(
            sut.build(), 
            [
                0xe0, 0x0f, 0x1e, 0xf8, //; str x0, [sp, #-32]!
                0xe1, 0x83, 0x00, 0xf8, //; str x1, [sp, #8]
                0xe2, 0x03, 0x01, 0xf8, //; str x2, [sp, #16]
                0xf0, 0x83, 0x01, 0xf8, //; str x16, [sp, #24]
                0x20, 0x00, 0x80, 0xd2, //; movz x0, #1
                0x21, 0x01, 0x00, 0x10, //; adr x1, #36
                0x62, 0x01, 0x80, 0xd2, //; movz x2, #11
                0x90, 0x00, 0x80, 0xd2, //; movz x16, #4
                0x01, 0x10, 0x00, 0xd4, //; svc 0x0080
                0xf0, 0x0f, 0x40, 0xf9, //; ldr x16, [sp, #24]
                0xe2, 0x0b, 0x40, 0xf9, //; ldr x2, [sp, #16]
                0xe1, 0x07, 0x40, 0xf9, //; ldr x1, [sp, #8]
                0xe0, 0x07, 0x42, 0xf8, //; ldr x0, [sp], #32
                0x04, 0x00, 0x00, 0x14, //; b #16
                0x48, 0x65, 0x6c, 0x6c, //; Hell
                0x6f, 0x20, 0x57, 0x6f, //; o.Wo
                0x72, 0x6c, 0x64      , //; rld
                0x00                  , //; .zero
            ])
    }
}
