import XCTest

@testable import swiftasm

extension Sequence where Iterator.Element: Hashable {
    func unique() -> [Iterator.Element] {
        var seen: [Iterator.Element: Bool] = [:]
        return self.filter { seen.updateValue(true, forKey: $0) == nil }
    }
}

func sut() -> M1Compiler { M1Compiler(stripDebugMessages: true) }

func builder() -> OpBuilder {
    printerr("Builder1")
    let storage = ModuleStorage()
    printerr("Builder2")
    let ctx = JitContext(storage: storage)
    printerr("Builder3")
    return OpBuilder(ctx: ctx)
}

func prepareFunction(
    retType: HLType,
    findex: Int32,
    regs: [HLType],
    args: [HLType],
    ops: [HLOpCode]
) -> HLFunction {
    let funcType = HLType.fun(
        HLTypeFunData(args: Resolvable.array(args), ret: Resolvable(retType))
    )
    return HLFunction(
        type: Resolvable(funcType),
        findex: findex,
        regs: Resolvable.array(regs),
        ops: ops,
        assigns: []
    )
}

final class CompilerM1Tests: XCTestCase {
    func testCompile_OGetThis() throws {
        let sut = sut()
        struct _TestMemory {
            var hl_type_addr: Int64 = 0xDEAD
            var field: Int32 = 0xBEEF
        }

        XCTAssertEqual(MemoryLayout<_TestMemory>.size, 12)

        let structType = HLType.obj(
            HLTypeObjData(
                name: Resolvable("_TestMemory"), 
                superType: nil, 
                global: 0, 
                fields: [
                    HLTypeField(
                        name: Resolvable("field"), 
                        type: Resolvable(.i32))], 
                protos: [], 
                bindings: []))

        let storage = ModuleStorage(
            types: [structType], 
            functions: [ 
                prepareFunction(
                    retType: .i32,
                    findex: 0,
                    regs: [structType, .i32],
                    args: [structType],
                    ops: [ .OGetThis(dst: 1, field: 0), .ORet(ret: 1) ]
                )
        ])
        let ctx = JitContext(storage: storage)
        let mem = OpBuilder(ctx: ctx)
        
        try sut.compile(findex: 0, into: mem)
        
        mem.hexPrint()
        // return
        var obj = _TestMemory()

        try withUnsafeMutableBytes(of: &obj) {  
            guard let ptr = $0.baseAddress else { fatalError("Couldn't get ptr") }
            let objAddress = Int64(Int(bitPattern: ptr))

            // run the entrypoint and ensure it works
            typealias _JitFunc = (@convention(c) (Int64) -> Int32)
            let entrypoint: _JitFunc = try mem.buildEntrypoint(0) 
            let result = entrypoint(objAddress)
            XCTAssertEqual(result, 0xBEEF) 
        }
    }

    func testCompile_emptyFunction() throws {
        let storage = ModuleStorage(functions: [
            prepareFunction(
                retType: .void,
                findex: 0,
                regs: [.void],
                args: [],
                ops: [.ORet(ret: 0)]
            )
        ])
        let ctx = JitContext(storage: storage)
        let mem = OpBuilder(ctx: ctx)
        let sut = sut()
        try sut.compile(findex: 0, into: mem)

        // mem.hexPrint()

        XCTAssertEqual(
            mem.lockAddressesAndBuild(),
            [
                0xfd, 0x7b, 0xbf, 0xa9,  // stp x29, x30, [sp, #-16]!
                0xfd, 0x03, 0x00, 0x91,  // movr x29, sp
                0xe0, 0x03, 0x40, 0xf9,  // ldr x0, [sp, #0]
                0x01, 0x00, 0x00, 0x14,  // b #4
                0xfd, 0x7b, 0xc1, 0xa8,  // ldp x29, x30, [sp], #16
                0xc0, 0x03, 0x5f, 0xd6,  // ret
            ]
        )

        // run the entrypoint and ensure it works
        let entrypoint: JitVoid = try mem.buildEntrypoint(0)
        entrypoint()
    }

    func testCompile__OCall3() throws {
        // Prepare function we'll call from JIT
        typealias _JitFunc = (@convention(c) (UInt8, UInt16, Int32) -> Int32)
        let swiftFunc: _JitFunc = { (_ a: UInt8, _ b: UInt16, _ c: Int32) in
            return Int32(bitPattern: UInt32(UInt16(a) | b)) | c
        }
        let swiftFuncPtr = unsafeBitCast(swiftFunc, to: UnsafeMutableRawPointer.self)
        
        let f = prepareFunction(
            retType: .i32,
            findex: 0,
            regs: [.u8, .u16, .i32],
            args: [.u8, .u16, .i32],
            ops: [
                .OCall3(dst: 2, fun: 1, arg0: 0, arg1: 1, arg2: 2), 
                .ORet(ret: 2),
            ]
        )

        // Misc. JIT stuff
        let storage = ModuleStorage(
            functions: [f],
            natives: [
                HLNative(
                    lib: Resolvable("builtin"),
                    name: Resolvable("swiftFunc"),
                    type: Resolvable(
                        .fun(
                            HLTypeFunData(
                                args: Resolvable.array([.u8, .u16, .i32]),
                                ret: Resolvable(.i32)
                            )
                        )
                    ),
                    findex: 1,
                    memory: swiftFuncPtr
                )
            ]
        )
        let ctx = JitContext(storage: storage)
        let mem = OpBuilder(ctx: ctx)
        // Compile HL function with function index 0 (from the whole functions table)
        let sut = sut()
        try sut.compile(findex: 0, into: mem)
        // Print debug output of the generated Aarch64 bytecode
        mem.hexPrint()
        // Now place the compiled bytecode in executable memory and get a pointer to it in
        // form of function (JitInt64 means (void)->Int64)
        let entrypoint: _JitFunc = try mem.buildEntrypoint(0)
        let res: Int32 = entrypoint(1, 2, 4)

        // HL called Swift func. Swift func returned 145. HL returned the result it received.
        XCTAssertEqual(0b111, res)
    }

    func testCompile_callAndReturn_callNative() throws {
        // Prepare function we'll call from JIT
        let swiftFunc: JitInt64 = { return 145 }
        let swiftFuncPtr = unsafeBitCast(swiftFunc, to: UnsafeMutableRawPointer.self)
        //       ^ pointer to the `swiftFunc` closure

        // Function that consists of Hashlink ops:
        //   - call function with index 1, and store result in HL register 0
        //   - return value in register 0
        let f = prepareFunction(
            retType: .i32,
            findex: 0,
            regs: [.i32],
            args: [],
            ops: [
                //                                         ^ this fuction will be registered in the whole
                //                                           functions table with function-index 0
                .OCall0(dst: 0, fun: 1), .ORet(ret: 0),
            ]
        )

        // Misc. JIT stuff
        let storage = ModuleStorage(
            functions: [f],
            natives: [
                HLNative(
                    lib: Resolvable("builtin"),
                    name: Resolvable("swiftFunc"),
                    type: Resolvable(
                        .fun(
                            HLTypeFunData(
                                args: Resolvable.array([]),
                                ret: Resolvable(.i32)
                            )
                        )
                    ),
                    findex: 1,
                    memory: swiftFuncPtr
                )
            ]
        )
        let ctx = JitContext(storage: storage)
        let mem = OpBuilder(ctx: ctx)
        // Compile HL function with function index 0 (from the whole functions table)
        let sut = sut()
        try sut.compile(findex: 0, into: mem)
        // Print debug output of the generated Aarch64 bytecode
        mem.hexPrint()
        // Now place the compiled bytecode in executable memory and get a pointer to it in
        // form of function (JitInt64 means (void)->Int64)
        let entrypoint: JitInt64 = try mem.buildEntrypoint(0)
        let res: Int64 = entrypoint()

        // HL called Swift func. Swift func returned 145. HL returned the result it received.
        XCTAssertEqual(145, res)
    }

    func testCompile_callAndReturn_callCompiled() throws {
        // Misc. JIT stuff
        let storage = ModuleStorage(
            functions: [
                prepareFunction(
                    retType: .i32,
                    findex: 0,
                    regs: [.i32],
                    args: [],
                    ops: [.OCall0(dst: 0, fun: 1), .ORet(ret: 0)]
                ),
                prepareFunction(
                    retType: .i32,
                    findex: 1,
                    regs: [.void, .i32],
                    args: [],
                    ops: [.OInt(dst: 1, ptr: 0), .ORet(ret: 1)]
                ),
            ],
            ints: [152]
        )
        let ctx = JitContext(storage: storage)
        let mem = OpBuilder(ctx: ctx)
        let sut = sut()
        try sut.compile(findex: 0, into: mem)
        try sut.compile(findex: 1, into: mem)

        // Print debug output of the generated Aarch64 bytecode
        mem.hexPrint()
        let entrypoint: JitInt64 = try mem.buildEntrypoint(0)
        let entrypoint2: JitInt64 = try mem.buildEntrypoint(1)
        let res1: Int64 = entrypoint()
        let res2: Int64 = entrypoint2()

        XCTAssertEqual(152, res1)
        XCTAssertEqual(res1, res2)
    }

    func testCompile_simpleReturn() throws {
        let storage = ModuleStorage(
            functions: [
                prepareFunction(
                    retType: .i32,
                    findex: 0,
                    regs: [.void, .i32],
                    args: [],
                    ops: [.OInt(dst: 1, ptr: 0), .ORet(ret: 1)]
                )
            ],
            ints: [165]
        )
        let ctx = JitContext(storage: storage)
        let mem = OpBuilder(ctx: ctx)
        let sut = sut()
        try sut.compile(findex: 0, into: mem)
        // mem.hexPrint()

        XCTAssertEqual(
            mem.lockAddressesAndBuild(),
            [
                0xfd, 0x7b, 0xbf, 0xa9,  // stp x29, x30, [sp, #-16]!
                0xfd, 0x03, 0x00, 0x91,  // movr x29, sp
                0xff, 0x43, 0x00, 0xd1,  // sub sp, sp, #16
                0xe0, 0x03, 0x00, 0xf8,  // str x0, [sp, #0]
                0xa0, 0x14, 0x80, 0xd2,  // .mov x0, #165
                0x00, 0x00, 0xa0, 0xf2,  //
                0x00, 0x00, 0xc0, 0xf2,  //
                0x00, 0x00, 0xe0, 0xf2,  //
                0xe0, 0x03, 0x00, 0xf8,  // str x0, [sp, #0]
                0xe0, 0x03, 0x40, 0xf9,  // ldr x0, [sp, #0]
                0x01, 0x00, 0x00, 0x14,  // b #4
                0xff, 0x43, 0x00, 0x91,  // add sp, sp, #16
                0xfd, 0x7b, 0xc1, 0xa8,  // ldp x29, x30, [sp], #16
                0xc0, 0x03, 0x5f, 0xd6,  // ret
            ]
        )

        let entrypoint: JitInt64 = try mem.buildEntrypoint(0)
        let res: Int64 = entrypoint()
        XCTAssertEqual(165, res)
    }

    func testCalcStackArgReq() throws {
        let sut = sut()

        // test different size combinations (ensure aligned to 16 bytes)
        var (size, _) = sut.calcStackArgReq(regs: [.array, .array], args: [])
        XCTAssertEqual(16, size)

        (size, _) = sut.calcStackArgReq(regs: [.array, .array, .i32], args: [])
        XCTAssertEqual(32, size)

        (size, _) = sut.calcStackArgReq(
            regs: [.array, .array, .i32, .dyn, .dynobj],
            args: []
        )
        XCTAssertEqual(48, size)

        (size, _) = sut.calcStackArgReq(
            regs: [.array, .array, .i32, .dyn, .dynobj],
            args: [.array, .array, .i32, .dyn, .dynobj]
        )
        XCTAssertEqual(48, size)

        // args exceeding first 8 should not allocate extra space (as it
        // should already be allocated due to calling convention)
        (size, _) = sut.calcStackArgReq(
            regs: Array(repeating: .dyn, count: 16),
            args: Array(repeating: .dyn, count: 16)
        )
        XCTAssertEqual(64, size)

        // non args should take space
        (size, _) = sut.calcStackArgReq(regs: [.i32], args: [])
        XCTAssertEqual(16, size)

        // 4 regs (all except 1st) and 1 arg should contribute to size here
        (size, _) = sut.calcStackArgReq(
            regs: [.array] + Array(repeating: .i32, count: 4),
            args: [.array]
        )
        XCTAssertEqual(32, size)

        // first 8 args should take space
        (size, _) = sut.calcStackArgReq(
            regs: Array(repeating: .i32, count: 8),
            args: Array(repeating: .i32, count: 8)
        )
        XCTAssertEqual(32, size)

        // void should be ignored
        (size, _) = sut.calcStackArgReq(
            regs: Array(repeating: .void, count: 8) + Array(repeating: .i32, count: 8),
            args: Array(repeating: .void, count: 8) + Array(repeating: .i32, count: 8)
        )
        XCTAssertEqual(32, size)
    }

    func testAppendPrologue() throws {
        let mem = builder()
        sut().appendPrologue(builder: mem)

        XCTAssertEqual(
             mem.lockAddressesAndBuild(),
            [0xfd, 0x7b, 0xbf, 0xa9, 0xfd, 0x03, 0x00, 0x91]
        )
    }

    func testAppendEpilogue() throws {
        let mem = builder()
        sut().appendEpilogue(builder: mem)

        XCTAssertEqual(mem.lockAddressesAndBuild(), [0xfd, 0x7b, 0xc1, 0xa8])
    }

    func testGetRegStackOffset_min16() throws {
        let regs: HLTypeKinds = [.i32, .void, .void, .i32, .void]
        // we need 4 bytes but stack for x0 has moved 16 bytes
        let args: HLTypeKinds = [.i32]
        let sut = sut()

        // First reg is in args, so offset 0
        XCTAssertEqual(0, sut.getRegStackOffset(regs, args: args, reg: 0))

        // Second reg is in stack after the register area,
        // which is aligned to 16 bytes
        XCTAssertEqual(16, sut.getRegStackOffset(regs, args: args, reg: 3))
    }

    func testGetRegStackOffset_noMin_funcArgs() throws {
        let regs: HLTypeKinds = [.i32, .void, .i32]
        // we need 4 bytes but stack for x0 has moved 16 bytes
        let args: HLTypeKinds = [.i32, .void, .i32]

        // Second reg is in stack after the register area,
        // which is aligned to 16 bytes
        XCTAssertEqual(4, sut().getRegStackOffset(regs, args: args, reg: 1))
    }

    func testGetRegStackOffset_void() throws {
        let regs: HLTypeKinds = [.void]
        // we need 4 bytes but stack for x0 has moved 16 bytes
        let args: HLTypeKinds = [.void]
        XCTAssertEqual(0, sut().getRegStackOffset(regs, args: args, reg: 0))
    }

    func testAppendStackInit_skipVoid() throws {
        let mem = builder()
        try sut().appendStackInit([.void], args: [.void], builder: mem)
        XCTAssertEqual([], mem.lockAddressesAndBuild())
    }

    func testAppendStackInit_min16() throws {
        let _1_need16 = Array(repeating: HLTypeKind.i32, count: 1)
        let _4_need16 = Array(repeating: HLTypeKind.i32, count: 4)
        let _5_need32 = Array(repeating: HLTypeKind.i32, count: 5)
        let sut = sut()
        // 4 byte requirement should still be aligned to 16 byte boundary
        let mem1 = builder()
        try sut.appendStackInit(_1_need16, args: _1_need16, builder: mem1)
        XCTAssertEqual(
            [
                0xff, 0x43, 0x00, 0xd1,  // sub sp, sp, #16
                0xe0, 0x03, 0x00, 0xf8,  // str x0, [sp, #0]
            ],
            mem1.lockAddressesAndBuild()
        )

        // 16 byte requirement should not round to 32
        let mem2 = builder()
        try sut.appendStackInit(_4_need16, args: _4_need16, builder: mem2)
        XCTAssertEqual(
            [
                0xff, 0x43, 0x00, 0xd1,  // sub sp, sp, #16
                0xe0, 0x03, 0x00, 0xf8,  // str x0, [sp, #0]
                0xe1, 0x43, 0x00, 0xf8,  // str x1, [sp, #4]
                0xe2, 0x83, 0x00, 0xf8,  // str x2, [sp, #8]
                0xe3, 0xc3, 0x00, 0xf8,  // str x3, [sp, #12]
            ],
            mem2.lockAddressesAndBuild()
        )
        // 20 byte requirement should round to 32
        let mem3 = builder()
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
            mem3.lockAddressesAndBuild()
        )
    }

    func testAppendStackInit_multiple() throws {
        let mem = builder()
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
            mem.lockAddressesAndBuild()
        )
    }

    func testAppendStackInit_moreThan8Args() throws {
        let mem = builder()
        try sut().appendStackInit(
            Array(repeating: HLTypeKind.i32, count: 12),
            args: Array(repeating: HLTypeKind.i32, count: 12),
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
            mem.lockAddressesAndBuild()
        )
    }

    func testAppendStackInit_mismatchedRegs() throws {
        let mem = builder()

        XCTAssertThrowsError(
            try sut().appendStackInit([.i32], args: [.void], builder: mem)
        )
    }

    func testAppendDebugPrintAligned4() throws {
        let memWith = builder()
        let memWithout = builder()
        M1Compiler(stripDebugMessages: false).appendDebugPrintAligned4(
            "Hello World",
            builder: memWith
        )
        M1Compiler(stripDebugMessages: true).appendDebugPrintAligned4(
            "Hello World",
            builder: memWithout
        )

        XCTAssertEqual(memWithout.lockAddressesAndBuild(), [])

        XCTAssertEqual(
            memWith.lockAddressesAndBuild(),
            [
                // Printing debug message: Hello World
                0xe0, 0x0f, 0x1e, 0xf8,  // str x0, [sp, #-32]!
                0xe1, 0x83, 0x00, 0xf8,  // str x1, [sp, #8]
                0xe2, 0x03, 0x01, 0xf8,  // str x2, [sp, #16]
                0xf0, 0x83, 0x01, 0xf8,  // str x16, [sp, #24]
                0x20, 0x00, 0x80, 0xd2,  // movz x0, #1
                0x21, 0x01, 0x00, 0x10,  // adr x1, #36
                0xe2, 0x02, 0x80, 0xd2,  // movz x2, #23
                0x90, 0x00, 0x80, 0xd2,  // movz x16, #4
                0x01, 0x10, 0x00, 0xd4,  // svc 0x0080
                0xf0, 0x0f, 0x40, 0xf9,  // ldr x16, [sp, #24]
                0xe2, 0x0b, 0x40, 0xf9,  // ldr x2, [sp, #16]
                0xe1, 0x07, 0x40, 0xf9,  // ldr x1, [sp, #8]
                0xe0, 0x07, 0x42, 0xf8,  // ldr x0, [sp], #32
                0x07, 0x00, 0x00, 0x14,  // b #28
                0x5b, 0x6a, 0x69, 0x74,  // [jit
                0x64, 0x65, 0x62, 0x75,  // debu
                0x67, 0x5d, 0x20, 0x48,  // g].H
                0x65, 0x6c, 0x6c, 0x6f,  // ello
                0x20, 0x57, 0x6f, 0x72,  // .Wor
                0x6c, 0x64, 0x0a,  // ld\n
                0x00,  // .zero
            ]
        )
    }
}
