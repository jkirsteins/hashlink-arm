import XCTest

@testable import swiftasm

fileprivate func prepareFunction(
    retType: any HLTypeProvider,
    findex: RefFun,
    regs: [any HLTypeProvider],
    args: [any HLTypeProvider],
    ops: [HLOpCode]
) -> any Compilable2 {
    let funType = Test_HLTypeFun(argsProvider: args, retProvider: retType)
    return PointerCompilable(
        findex: findex,
        ops: ops,
        linkableAddress: TestDummyLinkableAddress(),    // dummy address, won't be used as CCompatJitContext replaces these
        regsProvider: regs,
        typeProvider: funType)
}

fileprivate func prepareContext(compilables: [any Compilable2], natives: [any NativeCallable2] = [], ints: [Int32] = []) throws -> CCompatJitContext {
    let tm = TestJitModule(compilables, natives: natives, ints: ints)
    assert(tm.ntypes > 0)
    return try CCompatJitContext(ctx: tm)
}

fileprivate func compileAndLink(ctx: CCompatJitContext, _ fix: Int..., callback: (UnsafeMutableRawPointer)throws->()) throws {
    let mem = CpuOpBuffer()
    let sut = M1Compiler2(ctx: ctx, stripDebugMessages: false)
    
    try fix.forEach { try sut.compile(findex: $0, into: mem) }

    let mapper = BufferMapper(ctx: ctx, buffer: mem)
    let mappedMem = try mapper.getMemory()
    
    try callback(mappedMem)
    
//    try mapper.freeMemory()
}

final class CompilerM1v2Tests: CCompatTestCase {
    
    func sut(ctx: CCompatJitContext, strip: Bool = true) -> M1Compiler2 { M1Compiler2(ctx: ctx, stripDebugMessages: strip) }
    
    func testCompile_OGetThis() throws {
        
        let structType = Test_HLTypeObj(fieldsProvider: [
            Test_HLObjField(nameProvider: "field1", typeProvider: HLTypeKind.u8),
            Test_HLObjField(nameProvider: "field2", typeProvider: HLTypeKind.i32),
            Test_HLObjField(nameProvider: "field3", typeProvider: HLTypeKind.u16)
        ], nameProvider: "testObj")
        
        let ctx = try prepareContext(compilables: [
            prepareFunction(
                retType: HLTypeKind.u8,
                findex: 0,
                regs: [structType, HLTypeKind.u8],
                args: [structType],
                ops: [
                    .OGetThis(dst: 1, field: 0),
                    .ORet(ret: 1)
                ])
        ])
        
        struct _TestMemory {
            var hl_type_addr: Int64 = 0x0000
            var field1: UInt8
            var field2: Int32
            var field3: UInt16
        }
        let obj = _TestMemory(field1: 0b10000001, field2: Int32.max, field3: UInt16.max)
        
        let mem = CpuOpBuffer()
        let sut = M1Compiler2(ctx: ctx, stripDebugMessages: false)
        try sut.compile(findex: 0, into: mem)

        //
        let mapper = BufferMapper(ctx: ctx, buffer: mem)
        let mappedMem = try mapper.getMemory()
        
        try withUnsafePointer(to: obj) { ptr in
            XCTAssertEqual(0b10000001, UInt8(try mappedMem.calljit(ctx: ctx, fix: 0, arg0: ptr)))
        }
    }

    func testCompile_OJNotNull() throws {
        let objType = Test_HLTypeObj(fieldsProvider: [], nameProvider: "testObject")
        
        // constants
        let constI_3 = 1 // constant value 3
        let constI_57005 = 2 // constant value 57005

        let ctx = try prepareContext(compilables: [
            prepareFunction(
                retType: HLTypeKind.u8,
                findex: 0,
                regs: [objType, HLTypeKind.i32],
                args: [objType],
                ops: [
                    // if first arg < second arg, skip 2 following ops
                    .OJNotNull(reg: 0, offset: 2),

                    // return 3
                    .OInt(dst: 1, ptr: constI_3),
                    .ORet(ret: 1),

                    // return 57005
                    .OInt(dst: 1, ptr: constI_57005),
                    .ORet(ret: 1)
                ])
        ], ints: [0, 3, 57005])

        let mem = CpuOpBuffer()
        let sut = M1Compiler2(ctx: ctx, stripDebugMessages: false)
        try sut.compile(findex: 0, into: mem)

        //
        let mapper = BufferMapper(ctx: ctx, buffer: mem)
        let mappedMem = try mapper.getMemory()
        
        // jump
        XCTAssertEqual(57005, try mappedMem.calljit(ctx: ctx, fix: 0, arg0: UnsafeRawPointer(bitPattern: 123)))

        // no jump
        XCTAssertEqual(3, try mappedMem.calljit(ctx: ctx, fix: 0, arg0: nil))
    }

    func testCompile_OJFalse() throws {
        // constants
        let constI_3 = 1 // constant value 3
        let constI_57005 = 2 // constant value 57005

        let ctx = try prepareContext(compilables: [
            prepareFunction(
                retType: HLTypeKind.u8,
                findex: 0,
                regs: [HLTypeKind.bool, HLTypeKind.i32],
                args: [HLTypeKind.bool],
                ops: [
                    .OJFalse(cond: 0, offset: 2),

                    // return 3
                    .OInt(dst: 1, ptr: constI_3),
                    .ORet(ret: 1),

                    // return 57005
                    .OInt(dst: 1, ptr: constI_57005),
                    .ORet(ret: 1)
                ])
        ], ints: [0, 3, 57005])

        try compileAndLink(ctx: ctx, 0) {
            mappedMem in
            
            XCTAssertEqual(3, try mappedMem.calljit(ctx: ctx, fix: 0, arg0: UInt8(1)))
            XCTAssertEqual(57005, try mappedMem.calljit(ctx: ctx, fix: 0, arg0: UInt8(0)))
        }
    }

    func testCompile_OJAlways() throws {
        let ctx = try prepareContext(compilables: [
            prepareFunction(
                retType: HLTypeKind.i32,
                findex: 0,
                regs: [HLTypeKind.i32, HLTypeKind.i32],
                args: [HLTypeKind.i32, HLTypeKind.i32],
                ops: [
                    .OJAlways(offset: 1),
                    .ORet(ret: 0),
                    .ORet(ret: 1)
                ])
        ])
        
        
        try compileAndLink(ctx: ctx, 0) {
            mappedMem in
            
            XCTAssertEqual(456, try mappedMem.calljit(ctx: ctx, fix: 0, arg0: 123, arg1: 456))
        }
    }

    func testCompile_negativeJumps_regression() throws {
        let ctx = try prepareContext(compilables: [
            prepareFunction(
                retType: HLTypeKind.u8,
                findex: 0,
                regs: [HLTypeKind.u8],
                args: [HLTypeKind.u8],
                ops: [
                    .OJAlways(offset: 14),
                    .OIncr(dst: 0),
                    .OIncr(dst: 0),
                    .OIncr(dst: 0),
                    .OIncr(dst: 0),
                    .OIncr(dst: 0),
                    .OIncr(dst: 0),
                    .OIncr(dst: 0),
                    .OIncr(dst: 0),
                    .OIncr(dst: 0),
                    .ORet(ret: 0),
                    .ONop,
                    .ONop,
                    .ONop,
                    .ONop,
                    .ONop,
                    .ONop,
                    .ONop,
                    .ONop,
                    .OJAlways(offset: -12),      // jump to Ret-2 (so should incr by 2 before returning)
                ])
        ])
        
        try compileAndLink(ctx: ctx, 0) {
            mem in
            
            XCTAssertEqual(127, try mem.calljit(ctx: ctx, fix: 0, arg0: UInt8(125)))
        }
    }

    func testCompile_OJNull() throws {
        let objType = Test_HLTypeObj(fieldsProvider: [])
        
        // constants
        let constI_3 = 1 // constant value 3
        let constI_57005 = 2 // constant value 57005

        let ctx = try prepareContext(compilables: [
            prepareFunction(
                retType: HLTypeKind.i32,
                findex: 0,
                regs: [objType, HLTypeKind.i32],
                args: [objType],
                ops: [
                    // if first arg < second arg, skip 2 following ops
                    .OJNull(reg: 0, offset: 2),

                    // return 3
                    .OInt(dst: 1, ptr: constI_3),
                    .ORet(ret: 1),

                    // return 57005
                    .OInt(dst: 1, ptr: constI_57005),
                    .ORet(ret: 1)
                ])
        ], ints: [0, 3, 57005])

        try compileAndLink(ctx: ctx, 0) {
            mappedMem in
            
            XCTAssertEqual(57005, try mappedMem.calljit(ctx: ctx, fix: 0, arg0: nil))
            XCTAssertEqual(3, try mappedMem.calljit(ctx: ctx, fix: 0, arg0: UnsafeRawPointer(bitPattern: 123)))
        }
    }

    func testCompile_OJULt() throws {
        // constants
        let constI_3 = 1 // constant value 3
        let constI_57005 = 2 // constant value 57005

        let ctx = try prepareContext(compilables: [
            prepareFunction(
                retType: HLTypeKind.i32,
                findex: 0,
                regs: [HLTypeKind.i32, HLTypeKind.i32],
                args: [HLTypeKind.i32],
                ops: [
                    // if first arg < second arg, skip 2 following ops
                    .OJULt(a: 0, b: 1, offset: 2),

                    // return 3
                    .OInt(dst: 0, ptr: constI_3),
                    .ORet(ret: 0),

                    // return 57005
                    .OInt(dst: 0, ptr: constI_57005),
                    .ORet(ret: 0)
                ])
        ], ints: [0, 3, 57005])

        try compileAndLink(ctx: ctx, 0) {
            mappedMem in
            
            XCTAssertEqual(57005, try mappedMem.calljit(ctx: ctx, fix: 0, arg0: 2, arg1: 3))
            XCTAssertEqual(3, try mappedMem.calljit(ctx: ctx, fix: 0, arg0: 3, arg1: 3))
            XCTAssertEqual(3, try mappedMem.calljit(ctx: ctx, fix: 0, arg0: 4, arg1: 3))
        }
    }

    func testCompile_OJSLt() throws {
        // constants
        let constI_3 = 1 // constant value 3
        let constI_57005 = 2 // constant value 57005

        let ctx = try prepareContext(compilables: [
            prepareFunction(
                retType: HLTypeKind.i32,
                findex: 0,
                regs: [HLTypeKind.u8, HLTypeKind.u8, HLTypeKind.i32],
                args: [HLTypeKind.u8, HLTypeKind.u8],
                ops: [
                    .OJSLt(a: 0, b: 1, offset: 2),

                    // return 3
                    .OInt(dst: 2, ptr: constI_3),
                    .ORet(ret: 2),

                    // return 57005
                    .OInt(dst: 2, ptr: constI_57005),
                    .ORet(ret: 2)
                ]
            ),
            prepareFunction(
                retType: HLTypeKind.i32,
                findex: 1,
                regs: [HLTypeKind.u16, HLTypeKind.u16, HLTypeKind.i32],
                args: [HLTypeKind.u16, HLTypeKind.u16],
                ops: [
                    .OJSLt(a: 0, b: 1, offset: 2),

                    // return 3
                    .OInt(dst: 2, ptr: constI_3),
                    .ORet(ret: 2),

                    // return 57005
                    .OInt(dst: 2, ptr: constI_57005),
                    .ORet(ret: 2)
                ]
            )
        ], ints: [0, 3, 57005])
        
        try compileAndLink(ctx: ctx, 0, 1) {
            mappedMem in
            
            // jump <
            XCTAssertEqual(57005, try mappedMem.calljit_i32(ctx: ctx, fix: 0, u8_0: 0b10000001, u8_1: 0b01000001))
            XCTAssertEqual(57005, try mappedMem.calljit_i32(ctx: ctx, fix: 1, u16_0: 0b1000000000000001, u16_1: 0b0100000000000001))
            // no jump ==
            XCTAssertEqual(3, try mappedMem.calljit_i32(ctx: ctx, fix: 0, u8_0: 0b10000001, u8_1: 0b10000001))
            XCTAssertEqual(3, try mappedMem.calljit_i32(ctx: ctx, fix: 1, u16_0: 0b1000000000000001, u16_1: 0b1000000000000001))
            // no jump >
            XCTAssertEqual(3, try mappedMem.calljit_i32(ctx: ctx, fix: 0, u8_0: 0b01000001, u8_1: 0b10000001))
            XCTAssertEqual(3, try mappedMem.calljit_i32(ctx: ctx, fix: 1, u16_0: 0b0100000000000001, u16_1: 0b1000000000000001))
        }
    }

    func testCompile_OSShr_OUshr() throws {
        let ctx = try prepareContext(compilables: [
            prepareFunction(
                retType: HLTypeKind.u8,
                findex: 0,
                regs: [HLTypeKind.u8, HLTypeKind.u8, HLTypeKind.u8],
                args: [HLTypeKind.u8, HLTypeKind.u8],
                ops: [
                    .OSShr(dst: 2, a: 0, b: 1),
                    .ORet(ret: 2)
                ]),
            prepareFunction(
                retType: HLTypeKind.u8,
                findex: 1,
                regs: [HLTypeKind.u8, HLTypeKind.u8, HLTypeKind.u8],
                args: [HLTypeKind.u8, HLTypeKind.u8],
                ops: [
                    .OUShr(dst: 2, a: 0, b: 1),
                    .ORet(ret: 2)
                ])
        ])

        try compileAndLink(ctx: ctx, 0, 1) {
            mappedMem in
            
            // signed shift right
            XCTAssertEqual(0b11110000, try mappedMem.calljit_u8(ctx: ctx, fix: 0, u8_0: 0b10000001, u8_1: 3))
            // unsigned shift right
            XCTAssertEqual(0b00010000, try mappedMem.calljit_u8(ctx: ctx, fix: 1, u8_0: 0b10000001, u8_1: 3))
        }
    }

    func testCompile_OJSLte() throws {
        // constants
        let constI_3 = 1 // constant value 3
        let constI_57005 = 2 // constant value 57005

        let ctx = try prepareContext(compilables: [
            prepareFunction(
                retType: HLTypeKind.i32,
                findex: 0,
                regs: [HLTypeKind.u8, HLTypeKind.u8, HLTypeKind.i32],
                args: [HLTypeKind.u8, HLTypeKind.u8],
                ops: [
                    .OJSLte(a: 0, b: 1, offset: 2),

                    // return 3
                    .OInt(dst: 2, ptr: constI_3),
                    .ORet(ret: 2),

                    // return 57005
                    .OInt(dst: 2, ptr: constI_57005),
                    .ORet(ret: 2)
                ]),
            prepareFunction(
                retType: HLTypeKind.i32,
                findex: 1,
                regs: [HLTypeKind.u16, HLTypeKind.u16, HLTypeKind.i32],
                args: [HLTypeKind.u16, HLTypeKind.u16],
                ops: [
                    .OJSLte(a: 0, b: 1, offset: 2),

                    // return 3
                    .OInt(dst: 2, ptr: constI_3),
                    .ORet(ret: 2),

                    // return 57005
                    .OInt(dst: 2, ptr: constI_57005),
                    .ORet(ret: 2)
                ])
        ], ints: [0, 3, 57005])

        try compileAndLink(ctx: ctx, 0, 1) {
            mappedMem in
            
            // jump <
            XCTAssertEqual(57005, try mappedMem.calljit_i32(ctx: ctx, fix: 1, u16_0: 0b1000000000000001, u16_1: 0b0100000000000001))
            XCTAssertEqual(57005, try mappedMem.calljit_i32(ctx: ctx, fix: 0, u8_0: 0b10000001, u8_1: 0b01000001))
            // jump ==
            XCTAssertEqual(57005, try mappedMem.calljit_i32(ctx: ctx, fix: 1, u16_0: 0b1000000000000001, u16_1: 0b1000000000000001))
            XCTAssertEqual(57005, try mappedMem.calljit_i32(ctx: ctx, fix: 0, u8_0: 0b10000001, u8_1: 0b10000001))
            // no jump >
            XCTAssertEqual(3, try mappedMem.calljit_i32(ctx: ctx, fix: 1, u16_0: 0b0100000000000001, u16_1: 0b1000000000000001))
            XCTAssertEqual(3, try mappedMem.calljit_i32(ctx: ctx, fix: 0, u8_0: 0b01000001, u8_1: 0b10000001))
        }
    }

    func testCompile_OJSGte() throws {
        // constants
        let constI_3 = 1 // constant value 3
        let constI_57005 = 2 // constant value 57005

        let ctx = try prepareContext(compilables: [
            prepareFunction(
                retType: HLTypeKind.i32,
                findex: 0,
                regs: [HLTypeKind.u8, HLTypeKind.u8, HLTypeKind.i32],
                args: [HLTypeKind.u8, HLTypeKind.u8],
                ops: [
                    .OJSGte(a: 0, b: 1, offset: 2),

                    // return 3
                    .OInt(dst: 2, ptr: constI_3),
                    .ORet(ret: 2),

                    // return 57005
                    .OInt(dst: 2, ptr: constI_57005),
                    .ORet(ret: 2)
                ]),
            prepareFunction(
                retType: HLTypeKind.i32,
                findex: 1,
                regs: [HLTypeKind.u16, HLTypeKind.u16, HLTypeKind.i32],
                args: [HLTypeKind.u16, HLTypeKind.u16],
                ops: [
                    .OJSGte(a: 0, b: 1, offset: 2),

                    // return 3
                    .OInt(dst: 2, ptr: constI_3),
                    .ORet(ret: 2),

                    // return 57005
                    .OInt(dst: 2, ptr: constI_57005),
                    .ORet(ret: 2)
                ])
        ], ints: [0, 3, 57005])

        try compileAndLink(ctx: ctx, 0, 1) {
            mappedMem in
            
            // jump >
            XCTAssertEqual(57005, try mappedMem.calljit_i32(ctx: ctx, fix: 1, u16_0: 0b0100000000000001, u16_1: 0b1000000000000001))
            XCTAssertEqual(57005, try mappedMem.calljit_i32(ctx: ctx, fix: 0, u8_0: 0b01000001, u8_1: 0b10000001))
            // jump ==
            XCTAssertEqual(57005, try mappedMem.calljit_i32(ctx: ctx, fix: 1, u16_0: 0b1000000000000001, u16_1: 0b1000000000000001))
            XCTAssertEqual(57005, try mappedMem.calljit_i32(ctx: ctx, fix: 0, u8_0: 0b10000001, u8_1: 0b10000001))
            // no jump <
            XCTAssertEqual(3, try mappedMem.calljit_i32(ctx: ctx, fix: 1, u16_0: 0b1000000000000001, u16_1: 0b0100000000000001))
            XCTAssertEqual(3, try mappedMem.calljit_i32(ctx: ctx, fix: 0, u8_0: 0b10000001, u8_1: 0b01000001))
        }
    }

    func testCompile_OJEq() throws {
        // constants
        let constI_3 = 1 // constant value 3
        let constI_57005 = 2 // constant value 57005

        let ctx = try prepareContext(compilables: [
            prepareFunction(
                retType: HLTypeKind.i32,
                findex: 0,
                regs: [HLTypeKind.i32, HLTypeKind.i32],
                args: [HLTypeKind.i32, HLTypeKind.i32],
                ops: [
                    .OJEq(a: 0, b: 1, offset: 2),

                    // return 3
                    .OInt(dst: 0, ptr: constI_3),
                    .ORet(ret: 0),

                    // return 57005
                    .OInt(dst: 0, ptr: constI_57005),
                    .ORet(ret: 0)
                ])
        ], ints: [0, 3, 57005])

        try compileAndLink(ctx: ctx, 0) {
            mappedMem in
            
            // jump
            XCTAssertEqual(57005, try mappedMem.calljit_i32(ctx: ctx, fix: 0, i32_0: 3, i32_1: 3))
            // no jump
            XCTAssertEqual(3, try mappedMem.calljit_i32(ctx: ctx, fix: 0, i32_0: 3, i32_1: 2))
            XCTAssertEqual(3, try mappedMem.calljit_i32(ctx: ctx, fix: 0, i32_0: 451, i32_1: 0))
        }
    }
    
    func testCompile_OJNotEq() throws {
        // constants
        let constI_3 = 1 // constant value 3
        let constI_57005 = 2 // constant value 57005

        let ctx = try prepareContext(compilables: [
            prepareFunction(
                retType: HLTypeKind.i32,
                findex: 0,
                regs: [HLTypeKind.i32, HLTypeKind.i32],
                args: [HLTypeKind.i32, HLTypeKind.i32],
                ops: [
                    .OJNotEq(a: 0, b: 1, offset: 2),

                    // return 3
                    .OInt(dst: 0, ptr: constI_3),
                    .ORet(ret: 0),

                    // return 57005
                    .OInt(dst: 0, ptr: constI_57005),
                    .ORet(ret: 0)
                ])
        ], ints: [0, 3, 57005])

        try compileAndLink(ctx: ctx, 0) {
            mappedMem in
            
            // jump
            XCTAssertEqual(57005, try mappedMem.calljit_i32(ctx: ctx, fix: 0, i32_0: 2, i32_1: 3))
            XCTAssertEqual(57005, try mappedMem.calljit_i32(ctx: ctx, fix: 0, i32_0: 3, i32_1: 2))
            // no jump
            XCTAssertEqual(3, try mappedMem.calljit_i32(ctx: ctx, fix: 0, i32_0: 3, i32_1: 3))
            XCTAssertEqual(3, try mappedMem.calljit_i32(ctx: ctx, fix: 0, i32_0: 0, i32_1: 0))
        }
    }
    
    func _robj__bytes_i32(obj: any HLTypeProvider, ops: [HLOpCode], _ callback: @escaping ((UnsafeRawPointer?, Int32)->UnsafeRawPointer?)->()) throws {
        let ctx = try prepareContext(compilables: [
            prepareFunction(
                retType: obj,
                findex: 0,
                regs: [HLTypeKind.bytes, HLTypeKind.i32, obj],
                args: [HLTypeKind.bytes, HLTypeKind.i32],
                ops: ops)
        ])

        try compileAndLink(ctx: ctx, 0) {
            mappedMem in
            
            try mappedMem.jit(ctx: ctx, fix: 0) { (ep: (@convention(c) (UnsafeRawPointer?, Int32) -> UnsafeRawPointer?)) in
                callback(ep)
            }
        }
    }
    
    func _ri32__i32_i32(ops: [HLOpCode], regs: [HLTypeKind] = [.i32, .i32], _ callback: @escaping ((Int32, Int32)->Int32)->()) throws {
        let ctx = try prepareContext(compilables: [
            prepareFunction(
                retType: HLTypeKind.i32,
                findex: 0,
                regs: regs,
                args: [HLTypeKind.i32, HLTypeKind.i32],
                ops: ops)
        ])

        try compileAndLink(ctx: ctx, 0) {
            mappedMem in
            
            try mappedMem.jit(ctx: ctx, fix: 0) { (ep: (@convention(c) (Int32, Int32) -> Int32)) in
                callback(ep)
            }
        }
    }
    
    func _rdyn__i32(ops: [HLOpCode], regs: [HLTypeKind] = [.i32, .dyn], _ callback: @escaping ((Int32)->UnsafePointer<vdynamic>)->()) throws {
        let ctx = try prepareContext(compilables: [
            prepareFunction(
                retType: HLTypeKind.dyn,
                findex: 0,
                regs: regs,
                args: [HLTypeKind.i32],
                ops: ops)
        ])

        try compileAndLink(ctx: ctx, 0) {
            mappedMem in
            
            try mappedMem.jit(ctx: ctx, fix: 0) { (ep: (@convention(c) (Int32) -> UnsafeRawPointer)) in
                let res = { (arg: Int32)->UnsafePointer<vdynamic> in
                    let p = ep(arg)
                    return .init(OpaquePointer(p))
                }
                callback(res)
            }
        }
    }
    
    func _ri64__i32(ops: [HLOpCode], regs: [HLTypeKind] = [.i32], _ callback: @escaping ((Int32)->Int64)->()) throws {
        let ctx = try prepareContext(compilables: [
            prepareFunction(
                retType: HLTypeKind.i64,
                findex: 0,
                regs: regs,
                args: [HLTypeKind.i32],
                ops: ops)
        ])

        try compileAndLink(ctx: ctx, 0) {
            mappedMem in
            
            try mappedMem.jit(ctx: ctx, fix: 0) { (ep: (@convention(c) (Int32) -> Int64)) in
                callback(ep)
            }
        }
    }
    
    func _ri32__i64(ops: [HLOpCode], regs: [HLTypeKind] = [.i32], _ callback: @escaping ((Int64)->Int32)->()) throws {
        let ctx = try prepareContext(compilables: [
            prepareFunction(
                retType: HLTypeKind.i32,
                findex: 0,
                regs: regs,
                args: [HLTypeKind.i64],
                ops: ops)
        ])

        try compileAndLink(ctx: ctx, 0) {
            mappedMem in
            
            try mappedMem.jit(ctx: ctx, fix: 0) { (ep: (@convention(c) (Int64) -> Int32)) in
                callback(ep)
            }
        }
    }
    
    func _ru8__u8(ops: [HLOpCode], regs: [HLTypeKind] = [.u8], _ callback: @escaping ((UInt8)->UInt8)->()) throws {
        let ctx = try prepareContext(compilables: [
            prepareFunction(
                retType: HLTypeKind.u8,
                findex: 0,
                regs: regs,
                args: [HLTypeKind.u8],
                ops: ops)
        ])

        try compileAndLink(ctx: ctx, 0) {
            mappedMem in
            
            try mappedMem.jit(ctx: ctx, fix: 0) { (ep: (@convention(c) (UInt8) -> UInt8)) in
                callback(ep)
            }
        }
    }
    
    func _ru8__u8_u8(ops: [HLOpCode], regs: [HLTypeKind] = [.u8, .u8], _ callback: @escaping ((UInt8, UInt8)->UInt8)->()) throws {
        let ctx = try prepareContext(compilables: [
            prepareFunction(
                retType: HLTypeKind.u8,
                findex: 0,
                regs: regs,
                args: [HLTypeKind.u8],
                ops: ops)
        ])

        try compileAndLink(ctx: ctx, 0) {
            mappedMem in
            
            try mappedMem.jit(ctx: ctx, fix: 0) { (ep: (@convention(c) (UInt8, UInt8) -> UInt8)) in
                callback(ep)
            }
        }
    }
    
    func _ri8__i8_i8(ops: [HLOpCode], regs: [HLTypeKind] = [.u8, .u8], _ callback: @escaping ((Int8, Int8)->Int8)->()) throws {
        let ctx = try prepareContext(compilables: [
            prepareFunction(
                retType: HLTypeKind.u8,
                findex: 0,
                regs: regs,
                args: [HLTypeKind.u8],
                ops: ops)
        ])

        try compileAndLink(ctx: ctx, 0) {
            mappedMem in
            
            try mappedMem.jit(ctx: ctx, fix: 0) { (ep: (@convention(c) (Int8, Int8) -> Int8)) in
                callback(ep)
            }
        }
    }
    
    func _dyn__dyn(ops: [HLOpCode], _ callback: @escaping ((UnsafeRawPointer?)->UnsafeRawPointer?)->()) throws {
        let ctx = try prepareContext(compilables: [
            prepareFunction(
                retType: HLTypeKind.dyn,
                findex: 0,
                regs: [HLTypeKind.dyn],
                args: [HLTypeKind.dyn],
                ops: ops)
        ])

        try compileAndLink(ctx: ctx, 0) {
            mappedMem in
            
            try mappedMem.jit(ctx: ctx, fix: 0) { (ep: (@convention(c) (UnsafeRawPointer?) -> UnsafeRawPointer?)) in
                callback(ep)
            }
        }
    }

    

    func testCompile_OAnd() throws {
        try _ri32__i32_i32(ops: [
            .OAnd(dst: 0, a: 0, b: 1),
            .ORet(ret: 0)
        ]) { entrypoint in
            XCTAssertEqual(0b00000, entrypoint(0b00000, 0b11111))
            XCTAssertEqual(0b00100, entrypoint(0b00111, 0b11100))
            XCTAssertEqual(0b11111, entrypoint(0b11111, 0b11111))
        }
    }
//
    func testCompile_OIncr() throws {
        try _ru8__u8(ops: [
            .OIncr(dst: 0),
            .ORet(ret: 0),
        ]) {
            entrypoint in
            
            XCTAssertEqual(125, entrypoint(124))
            XCTAssertEqual(126, entrypoint(125))
        }
    }

    func testCompile_OSub() throws {
        try _ri32__i32_i32(ops: [
            .OSub(dst: 0, a: 0, b: 1),
            .ORet(ret: 0)
        ]) {
            entrypoint in
            
            XCTAssertEqual(11, entrypoint(12, 1))
            XCTAssertEqual(1000, entrypoint(1252, 252))
        }
    }

    func testCompile_ONull() throws {
        try _dyn__dyn(ops: [
            .ONull(dst: 0),
            .ORet(ret: 0)
        ]) {
            entrypoint in
            
            XCTAssertEqual(nil, entrypoint(UnsafeRawPointer(bitPattern: 0x7b)!))
        }
    }

    func testCompile_ONew_OSetField() throws {
        
        let stringType = Test_HLTypeObj(fieldsProvider: [
            Test_HLObjField(nameProvider: "bytes", typeProvider: HLTypeKind.bytes),
            Test_HLObjField(nameProvider: "length", typeProvider: HLTypeKind.i32)
        ])
        
        try _robj__bytes_i32(obj: stringType, ops: [
            .ONew(dst: 2),
            .OSetField(obj: 2, field: 0, src: 0),
            .OSetField(obj: 2, field: 1, src: 1),
            .ORet(ret: 2)
        ]) {
            entrypoint in
            
            let strIn = "Hello World"
            let data = strIn.data(using: .utf16LittleEndian)!
            
            data.withUnsafeBytes { cstr in
                let result = entrypoint(cstr.baseAddress!, Int32(strIn.count))
                
                XCTAssertNotNil(result)

                let vPtr = result!.bindMemory(to: vdynamic.self, capacity: 1)
                let typePtr = vPtr.pointee.t

                // check type
                XCTAssertEqual(typePtr.pointee.kind, .obj)

                // bytes/str
                let bytes = result!.advanced(by: 8).bindMemory(to: UnsafePointer<CChar16>.self, capacity: 1)
                XCTAssertEqual(bytes.pointee, cstr.baseAddress!)

                let str = String.wrapUtf16(from: bytes.pointee)
                XCTAssertEqual(str, "Hello World")

                // len
                let len = Int(result!.advanced(by: 16).bindMemory(to: Int32.self, capacity: 1).pointee)
                XCTAssertEqual(len, 11)
            }
        }
    }

    func testCompile__OCall3() throws {
        // Prepare function we'll call from JIT
        typealias _JitFunc = (@convention(c) (UInt8, UInt16, Int32) -> Int32)
        let swiftFunc: _JitFunc = { (_ a: UInt8, _ b: UInt16, _ c: Int32) in
            let b1 = Int32(bitPattern: UInt32(b))
            let a1 = Int32(bitPattern: UInt32(a))
            let res = a1 | b1 | c
            print("a: \(a)")
            print("b: \(b)")
            print("c: \(c)")
            print("Returning \(res)")
            return res
        }
        let swiftFuncPtr = unsafeBitCast(swiftFunc, to: UnsafeMutableRawPointer.self)
        
        let ctx = try prepareContext(
            compilables: [
                prepareFunction(
                    retType: HLTypeKind.i32,
                    findex: 0,
                    regs: [HLTypeKind.u8, HLTypeKind.u16, HLTypeKind.i32],
                    args: [HLTypeKind.u8, HLTypeKind.u16, HLTypeKind.i32],
                    ops: [
                        .OCall3(dst: 2, fun: 1, arg0: 0, arg1: 1, arg2: 2),
                        .ORet(ret: 2),
                    ])
            ],
            natives: [
                TestNative2(
                    findex: 1,
                    libProvider: "?",
                    nameProvider: "swiftFunc",
                    typeProvider: Test_HLTypeFun(
                        argsProvider: [HLTypeKind.u8, HLTypeKind.u16, HLTypeKind.i32],
                        retProvider: HLTypeKind.i32),
                    address: swiftFuncPtr
                )
            ]
        )

        try compileAndLink(ctx: ctx, 0) {
            mappedMem in
            
            try mappedMem.jit(ctx: ctx, fix: 0) { (ep: (@convention(c) (UInt8, UInt16, Int32) -> Int32)) in
                let res: Int32 = ep(1, 2, 6)

                // HL called Swift func. Swift func returned 145. HL returned the result it received.
                XCTAssertEqual(0b111, res)
            }
        }
    }

    func testCompile__OCall4() throws {
        // Prepare function we'll call from JIT
        typealias _JitFunc = (@convention(c) (UInt8, UInt8, UInt8, UInt8) -> UInt8)
        let swiftFunc: _JitFunc = { (_ a: UInt8, _ b: UInt8, _ c: UInt8, _ d: UInt8) in
            return ((a & 1) << 3) | ((b & 1) << 2) | ((c & 1) << 1) | (d & 1)
        }
        let swiftFuncPtr = unsafeBitCast(swiftFunc, to: UnsafeMutableRawPointer.self)
        
        let ctx = try prepareContext(
            compilables: [
                prepareFunction(
                    retType: HLTypeKind.u8,
                    findex: 0,
                    regs: [HLTypeKind.u8, HLTypeKind.u8, HLTypeKind.u8, HLTypeKind.u8],
                    args: [HLTypeKind.u8, HLTypeKind.u8, HLTypeKind.u8, HLTypeKind.u8],
                    ops: [
                        // NOTE: sending registers to diff numbered args
                        .OCall4(dst: 0, fun: 1, arg0: 3, arg1: 2, arg2: 1, arg3: 0),
                        .ORet(ret: 0),
                    ])
            ],
            natives: [
                TestNative2(
                    findex: 1,
                    libProvider: "?",
                    nameProvider: "swiftFunc",
                    typeProvider: Test_HLTypeFun(
                        argsProvider: [HLTypeKind.u8, HLTypeKind.u8, HLTypeKind.u8, HLTypeKind.u8],
                        retProvider: HLTypeKind.u8),
                    address: swiftFuncPtr
                )
            ]
        )

        try compileAndLink(ctx: ctx, 0) {
            mappedMem in
            
            try mappedMem.jit(ctx: ctx, fix: 0) { (entrypoint: (@convention(c) (UInt8, UInt8, UInt8, UInt8) -> UInt8)) in
                
                XCTAssertEqual(0, entrypoint(0, 0, 0, 0))
                XCTAssertEqual(0b0001, entrypoint(1, 0, 0, 0))
                XCTAssertEqual(0b0010, entrypoint(0, 1, 0, 0))
                XCTAssertEqual(0b0100, entrypoint(0, 0, 1, 0))
                XCTAssertEqual(0b1000, entrypoint(0, 0, 0, 1))
                XCTAssertEqual(0b1111, entrypoint(1, 1, 1, 1))
                XCTAssertEqual(0b0000, entrypoint(0, 0, 0, 0))
                XCTAssertEqual(0b0110, entrypoint(0, 1, 1, 0))
            }
        }
    }

    func testCompile__OCallN__needStackArgs() throws {
        typealias _JitFunc = (@convention(c) (UInt8, UInt8, UInt32, UInt8, UInt8, UInt8, UInt32, UInt8, UInt8) -> UInt32)
        let swiftFunc: _JitFunc = { (_ a: UInt8, _ b: UInt8, _ c: UInt32, _ d: UInt8, _ e: UInt8, _ f: UInt8, _ g: UInt32, _ h: UInt8, _ i: UInt8) in

            print("Got a: \(a)")
            print("Got b: \(b)")
            print("Got c: \(c)")
            print("Got d: \(d)")
            print("Got e: \(e)")
            print("Got f: \(f)")
            print("Got g: \(g)")
            print("Got h: \(h)")
            print("Got i: \(i)")

            var hash: UInt32 = 17
            hash = hash &* 37 &+ UInt32(a);
            hash = hash &* 37 &+ UInt32(b);
            hash = hash &* 37 &+ UInt32(c);
            hash = hash &* 37 &+ UInt32(d);
            hash = hash &* 37 &+ UInt32(e);
            hash = hash &* 37 &+ UInt32(f);
            hash = hash &* 37 &+ UInt32(g);
            hash = hash &* 37 &+ UInt32(h);
            hash = hash &* 37 &+ UInt32(i);

            let c = String(hash, radix: 2).leftPadding(toLength: 16, withPad: "0")
            print("0b\(c.chunked(into: 4))")
            return hash
        }
        let swiftFuncPtr = unsafeBitCast(swiftFunc, to: UnsafeMutableRawPointer.self)

        let ctx = try prepareContext(
            compilables: [
                prepareFunction(
                    retType: HLTypeKind.i32,
                    findex: 0,
                    regs: [HLTypeKind.u8, HLTypeKind.u8, HLTypeKind.i32, HLTypeKind.u8, HLTypeKind.u8, HLTypeKind.u8, HLTypeKind.i32, HLTypeKind.u8, HLTypeKind.u8],
                    args: [HLTypeKind.u8, HLTypeKind.u8, HLTypeKind.i32, HLTypeKind.u8, HLTypeKind.u8, HLTypeKind.u8, HLTypeKind.i32, HLTypeKind.u8, HLTypeKind.u8],
                    ops: [
                        // NOTE: sending registers to diff numbered args
                        .OCallN(dst: 2, fun: 1, args: [8, 1, 2, 3, 4, 5, 6, 7, 0]),
                        .ORet(ret: 2),
                    ])
            ],
            natives: [
                TestNative2(
                    findex: 1,
                    libProvider: "?",
                    nameProvider: "swiftFunc",
                    typeProvider: Test_HLTypeFun(
                        argsProvider: [HLTypeKind.u8, HLTypeKind.u8, HLTypeKind.i32, HLTypeKind.u8, HLTypeKind.u8, HLTypeKind.u8, HLTypeKind.i32, HLTypeKind.u8, HLTypeKind.u8],
                        retProvider: HLTypeKind.i32),
                    address: swiftFuncPtr
                )
            ]
        )
        
        try compileAndLink(ctx: ctx, 0) {
            mappedMem in
            
            try mappedMem.jit(ctx: ctx, fix: 0) { (entrypoint: (@convention(c) (UInt8, UInt8, UInt32, UInt8, UInt8, UInt8, UInt32, UInt8, UInt8) -> UInt32)) in
                
                XCTAssertEqual(
                    848006100,
                    entrypoint(3, 0, 1, 0, 1, 0, 1, 0, 9))
            }
        }
    }
    
    func testCompile__OCallN__noStackArgs() throws {
        typealias _JitFunc = (@convention(c) (UInt8, UInt8) -> UInt32)
        let swiftFunc: _JitFunc = { (_ a: UInt8, _ b: UInt8) in

            print("Got a: \(a)")
            print("Got b: \(b)")

            var hash: UInt32 = 17
            hash = hash &* 37 &+ UInt32(a);
            hash = hash &* 37 &+ UInt32(b);

            let c = String(hash, radix: 2).leftPadding(toLength: 16, withPad: "0")
            print("0b\(c.chunked(into: 4))")
            return hash
        }
        let swiftFuncPtr = unsafeBitCast(swiftFunc, to: UnsafeMutableRawPointer.self)

        let ctx = try prepareContext(
            compilables: [
                prepareFunction(
                    retType: HLTypeKind.i32,
                    findex: 0,
                    regs: [HLTypeKind.u8, HLTypeKind.u8, HLTypeKind.i32],
                    args: [HLTypeKind.u8, HLTypeKind.u8],
                    ops: [
                        // NOTE: sending registers to diff numbered args
                        .OCallN(dst: 2, fun: 1, args: [1, 0]),
                        .ORet(ret: 2),
                    ])
            ],
            natives: [
                TestNative2(
                    findex: 1,
                    libProvider: "?",
                    nameProvider: "swiftFunc",
                    typeProvider: Test_HLTypeFun(
                        argsProvider: [HLTypeKind.u8, HLTypeKind.u8],
                        retProvider: HLTypeKind.i32),
                    address: swiftFuncPtr
                )
            ]
        )
        
        try compileAndLink(ctx: ctx, 0) {
            mappedMem in
            
            try mappedMem.jit(ctx: ctx, fix: 0) { (entrypoint: (@convention(c) (UInt8, UInt8) -> UInt32)) in
                
                XCTAssertEqual(
                    0b0101_1011_0011_1000,
                    entrypoint(5, 2))
            }
        }
    }

    func testCompile__OAdd() throws {
        
        try _ru8__u8_u8(ops: [
            .OAdd(dst: 1, a: 0, b: 1),
            .ORet(ret: 1),
        ]) { entrypoint in
            XCTAssertEqual(6, entrypoint(2, 4))
        }
    }

    func testCompile__OMov() throws {
        try _ru8__u8(ops: [
            .OMov(dst: 1, src: 0),
            .ORet(ret: 1),
        ], regs: [.u8, .u8]) {
            entrypoint in
            
            XCTAssertEqual(6, entrypoint(6))
        }
    }

    func testCompile__OCall__regression1() throws {
        let f1: (@convention(c) (UInt8) -> UInt8) = { $0 }
        let f1p = unsafeBitCast(f1, to: UnsafeMutableRawPointer.self)
        let f2: (@convention(c) (UInt8, UInt8) -> UInt8) = { a, _ in a }
        let f2p = unsafeBitCast(f2, to: UnsafeMutableRawPointer.self)
        let f3: (@convention(c) (UInt8, UInt8, UInt8) -> UInt8) = { a, _, _ in a }
        let f3p = unsafeBitCast(f3, to: UnsafeMutableRawPointer.self)
        let f4: (@convention(c) (UInt8, UInt8, UInt8, UInt8) -> UInt8) = { a, _, _, _ in a }
        let f4p = unsafeBitCast(f4, to: UnsafeMutableRawPointer.self)

        for sutOp in [
            HLOpCode.OCall1(dst: 0, fun: 1, arg0: 4),
            HLOpCode.OCall2(dst: 0, fun: 2, arg0: 4, arg1: 4),
            HLOpCode.OCall3(dst: 0, fun: 3, arg0: 4, arg1: 4, arg2: 4),
            HLOpCode.OCall4(dst: 0, fun: 4, arg0: 4, arg1: 4, arg2: 4, arg3: 4),
            HLOpCode.OCallN(dst: 0, fun: 4, args: [4, 4, 4, 4]),
        ]
        {
            let ctx = try prepareContext(
                compilables: [
                    prepareFunction(
                        retType: HLTypeKind.u8,
                        findex: 0,
                        regs: [HLTypeKind.u8, HLTypeKind.u8, HLTypeKind.u8, HLTypeKind.u8, HLTypeKind.u8],
                        args: [HLTypeKind.u8],
                        ops: [
                            .OMov(dst: 1, src: 0),
                            .OMov(dst: 2, src: 0),
                            .OMov(dst: 3, src: 0),
                            .OMov(dst: 4, src: 0),
                            sutOp,
                            .ORet(ret: 0),
                        ])
                ],
                natives: [
                    TestNative2(
                        findex: 1,
                        libProvider: "?",
                        nameProvider: "f1",
                        typeProvider: Test_HLTypeFun(
                            argsProvider: [HLTypeKind.u8],
                            retProvider: HLTypeKind.u8),
                        address: f1p
                    ),
                    TestNative2(
                        findex: 2,
                        libProvider: "?",
                        nameProvider: "f2",
                        typeProvider: Test_HLTypeFun(
                            argsProvider: [HLTypeKind.u8, HLTypeKind.u8],
                            retProvider: HLTypeKind.u8),
                        address: f2p
                    ),
                    TestNative2(
                        findex: 3,
                        libProvider: "?",
                        nameProvider: "f3",
                        typeProvider: Test_HLTypeFun(
                            argsProvider: [HLTypeKind.u8, HLTypeKind.u8, HLTypeKind.u8],
                            retProvider: HLTypeKind.u8),
                        address: f3p
                    ),
                    TestNative2(
                        findex: 4,
                        libProvider: "?",
                        nameProvider: "f4",
                        typeProvider: Test_HLTypeFun(
                            argsProvider: [HLTypeKind.u8, HLTypeKind.u8, HLTypeKind.u8, HLTypeKind.u8],
                            retProvider: HLTypeKind.u8),
                        address: f4p
                    )
                ]
            )
            
            try compileAndLink(ctx: ctx, 0) {
                mappedMem in
                
                XCTAssertEqual(
                    6,
                    try mappedMem.calljit_u8(ctx: ctx, fix: 0, u8_0: 6),
                    "failed for \(sutOp.id)")
            }
        }
    }

    /// Test handling void return
    func testCompile__OCall__regression2() throws {
        let f1: (@convention(c) (UInt8) -> ()) = { _ in }
        let f1p = unsafeBitCast(f1, to: UnsafeMutableRawPointer.self)
        let f2: (@convention(c) (UInt8, UInt8) -> ()) = { a, _ in }
        let f2p = unsafeBitCast(f2, to: UnsafeMutableRawPointer.self)
        let f3: (@convention(c) (UInt8, UInt8, UInt8) -> ()) = { a, _, _ in }
        let f3p = unsafeBitCast(f3, to: UnsafeMutableRawPointer.self)
        let f4: (@convention(c) (UInt8, UInt8, UInt8, UInt8) -> ()) = { a, _, _, _ in }
        let f4p = unsafeBitCast(f4, to: UnsafeMutableRawPointer.self)

        for sutOp in [
            HLOpCode.OCall1(dst: 0, fun: 1, arg0: 4),
            HLOpCode.OCall2(dst: 0, fun: 2, arg0: 4, arg1: 4),
            HLOpCode.OCall3(dst: 0, fun: 3, arg0: 4, arg1: 4, arg2: 4),
            HLOpCode.OCall4(dst: 0, fun: 4, arg0: 4, arg1: 4, arg2: 4, arg3: 4),
            HLOpCode.OCallN(dst: 0, fun: 4, args: [4, 4, 4, 4]),
        ]
        {
            let ctx = try prepareContext(
                compilables: [
                    prepareFunction(
                        retType: HLTypeKind.void,
                        findex: 0,
                        regs: [HLTypeKind.void, HLTypeKind.void, HLTypeKind.void, HLTypeKind.void, HLTypeKind.void],
                        args: [HLTypeKind.void],
                        ops: [
                            sutOp,
                            .ORet(ret: 0),
                        ])
                ],
                natives: [
                    TestNative2(
                        findex: 1,
                        libProvider: "?",
                        nameProvider: "f1",
                        typeProvider: Test_HLTypeFun(
                            argsProvider: [HLTypeKind.void],
                            retProvider: HLTypeKind.void),
                        address: f1p
                    ),
                    TestNative2(
                        findex: 2,
                        libProvider: "?",
                        nameProvider: "f2",
                        typeProvider: Test_HLTypeFun(
                            argsProvider: [HLTypeKind.void, HLTypeKind.void],
                            retProvider: HLTypeKind.void),
                        address: f2p
                    ),
                    TestNative2(
                        findex: 3,
                        libProvider: "?",
                        nameProvider: "f3",
                        typeProvider: Test_HLTypeFun(
                            argsProvider: [HLTypeKind.void, HLTypeKind.void, HLTypeKind.void],
                            retProvider: HLTypeKind.void),
                        address: f3p
                    ),
                    
                    TestNative2(
                        findex: 4,
                        libProvider: "?",
                        nameProvider: "f4",
                        typeProvider: Test_HLTypeFun(
                            argsProvider: [HLTypeKind.void, HLTypeKind.void, HLTypeKind.void, HLTypeKind.void],
                            retProvider: HLTypeKind.void),
                        address: f4p
                    )
                ]
            )
            
            try compileAndLink(ctx: ctx, 0) {
                mappedMem in
                
                try mappedMem.jitVoid(ctx: ctx, fix: 0) { (entrypoint: (@convention(c) () -> ())) in
                    entrypoint()
                    print("Got entrypoint \(entrypoint)")
                }
            }
        }
    }

    func testGetRegStackOffset() throws {
        let ctx = try prepareContext(compilables: [])
        let c = try sut(ctx: ctx)
        let regs: [HLTypeKind] = [
            .u8, .u8, .i32, .u8, .u8, .u8, .i32, .u8, .u8]

        XCTAssertEqual(c.getRegStackOffset(regs, 0), 0)
        XCTAssertEqual(c.getRegStackOffset(regs, 1), 1)
        XCTAssertEqual(c.getRegStackOffset(regs, 2), 2)
        XCTAssertEqual(c.getRegStackOffset(regs, 3), 6)
        XCTAssertEqual(c.getRegStackOffset(regs, 7), 13)
        XCTAssertEqual(c.getRegStackOffset(regs, 8), 14)
    }

    func testCompile__OCall1() throws {
        // Prepare function we'll call from JIT
        typealias _JitFunc16 = (@convention(c) (UInt16) -> Int32)
        let swiftFunc16: _JitFunc16 = { (_ a: UInt16) in
            return Int32(a) * 2
        }
        let swiftFuncPtr16 = unsafeBitCast(swiftFunc16, to: UnsafeMutableRawPointer.self)

        typealias _JitFunc8 = (@convention(c) (UInt8) -> Int32)
        let swiftFunc8: _JitFunc8 = { (_ a: UInt8) in
            return Int32(a) * 4
        }
        let swiftFuncPtr8 = unsafeBitCast(swiftFunc8, to: UnsafeMutableRawPointer.self)
        
        let ctx = try prepareContext(
            compilables: [
                prepareFunction(
                    retType: HLTypeKind.i32,
                    findex: 0,
                    regs: [HLTypeKind.u16, HLTypeKind.i32],
                    args: [HLTypeKind.u16],
                    ops: [
                        .OCall1(dst: 1, fun: 2, arg0: 0),
                        .ORet(ret: 1),
                    ]),
                prepareFunction(
                    retType: HLTypeKind.i32,
                    findex: 1,
                    regs: [HLTypeKind.u8, HLTypeKind.i32],
                    args: [HLTypeKind.u8],
                    ops: [
                        .OCall1(dst: 1, fun: 3, arg0: 0),
                        .ORet(ret: 1),
                    ])
            ],
            natives: [
                TestNative2(
                    findex: 2,
                    libProvider: "?",
                    nameProvider: "swiftFunc16",
                    typeProvider: Test_HLTypeFun(
                        argsProvider: [HLTypeKind.u16],
                        retProvider: HLTypeKind.i32),
                    address: swiftFuncPtr16
                ),
                TestNative2(
                    findex: 3,
                    libProvider: "?",
                    nameProvider: "swiftFunc8",
                    typeProvider: Test_HLTypeFun(
                        argsProvider: [HLTypeKind.u8],
                        retProvider: HLTypeKind.i32),
                    address: swiftFuncPtr8
                )
            ]
        )
        
        try compileAndLink(ctx: ctx, 0, 1) {
            mappedMem in
            
            try mappedMem.jit(ctx: ctx, fix: 0) { (entrypoint: (@convention(c) (UInt16) -> (Int32))) in
                
                XCTAssertEqual(246, entrypoint(123))
            }
            
            try mappedMem.jit(ctx: ctx, fix: 1) { (entrypoint: (@convention(c) (UInt8) -> (Int32))) in
                
                XCTAssertEqual(492, entrypoint(123))
            }
        }
    }

    func testCompile__OGetI8() throws {
        let ctx = try prepareContext(
            compilables: [
                prepareFunction(
                    retType: HLTypeKind.u8,
                    findex: 0,
                    regs: [HLTypeKind.bytes, HLTypeKind.i32, HLTypeKind.u8],
                    args: [HLTypeKind.bytes, HLTypeKind.i32],
                    ops: [
                        .OGetI8(dst: 2, bytes: 0, index: 1),
                        .ORet(ret: 2),
                    ]
                )
            ]
        )
        
        try compileAndLink(ctx: ctx, 0) {
            mappedMem in
            
            try mappedMem.jit(ctx: ctx, fix: 0) { (entrypoint: (@convention(c) (UnsafeRawPointer, Int32) -> (UInt8))) in
                
                var x: [UInt8] = [11, 22, 33, 44, 55, 66, 77, 88, 99]

                XCTAssertEqual(11, entrypoint(&x, 0))
                XCTAssertEqual(66, entrypoint(&x, 5))
            }
        }
    }
    
    func testCompile__OGetI16() throws {
        let ctx = try prepareContext(
            compilables: [
                prepareFunction(
                    retType: HLTypeKind.u16,
                    findex: 0,
                    regs: [HLTypeKind.bytes, HLTypeKind.i32, HLTypeKind.u16],
                    args: [HLTypeKind.bytes, HLTypeKind.i32],
                    ops: [
                        .OGetI16(dst: 2, bytes: 0, index: 1),
                        .ORet(ret: 2),
                    ]
                )
            ]
        )
        
        try compileAndLink(ctx: ctx, 0) {
            mappedMem in
            
            try mappedMem.jit(ctx: ctx, fix: 0) { (entrypoint: (@convention(c) (UnsafeRawPointer, Int32) -> (UInt8))) in
                var x: [UInt16] = [11, 22, 33, 44, 55, 66, 77, 88, 99]
        
                XCTAssertEqual(11, entrypoint(&x, 0))
                XCTAssertEqual(22, entrypoint(&x, 1))
                XCTAssertEqual(66, entrypoint(&x, 5))
            }
        }
    }

    func testCompile__OShl() throws {
        
        try _ri32__i32_i32(ops: [
            .OShl(dst: 2, a: 0, b: 1),
            .ORet(ret: 2),
        ], regs: [.i32, .i32, .i32]) {
            entrypoint in
            
            XCTAssertEqual(4, entrypoint(1, 2))
            XCTAssertEqual(20480, entrypoint(5, 12))
            XCTAssertEqual(Int32(bitPattern: UInt32(2147483648)), entrypoint(1, 31))
            // overflow returns 0
            XCTAssertEqual(0, entrypoint(1, 32))
        }
    }

    func testCompile__OCall2() throws {
        // Prepare function we'll call from JIT
        typealias _JitFunc = (@convention(c) (UInt16, Int32) -> Int32)
        let swiftFunc: _JitFunc = { (_ a: UInt16, _ b: Int32) in
            return Int32(Int16(bitPattern: a)) + b
        }
        let swiftFuncPtr = unsafeBitCast(swiftFunc, to: UnsafeMutableRawPointer.self)

        let ctx = try prepareContext(
            compilables: [
                prepareFunction(
                    retType: HLTypeKind.i32,
                    findex: 0,
                    regs: [HLTypeKind.u16, HLTypeKind.i32],
                    args: [HLTypeKind.u16],
                    ops: [
                        .OCall2(dst: 1, fun: 1, arg0: 0, arg1: 1),
                        .ORet(ret: 1),
                    ])
            ],
            natives: [
                TestNative2(
                    findex: 1,
                    libProvider: "?",
                    nameProvider: "swiftFunc",
                    typeProvider: Test_HLTypeFun(
                        argsProvider: [HLTypeKind.u16, HLTypeKind.i32],
                        retProvider: HLTypeKind.i32),
                    address: swiftFuncPtr
                )
            ]
        )
        
        try compileAndLink(ctx: ctx, 0) {
            mappedMem in
            
            try mappedMem.jit(ctx: ctx, fix: 0) { (entrypoint: _JitFunc) in
                   let res: Int32 = entrypoint(100, 156)

                   // HL called Swift func. Swift func returned 145. HL returned the result it received.
                   XCTAssertEqual(256, res)
            }
        }
    }

    func testCompile_OBool() throws {
        let ctx = try prepareContext(
            compilables: [
                prepareFunction(
                    retType: HLTypeKind.i32,
                    findex: 0,
                    regs: [HLTypeKind.i32, HLTypeKind.bool, HLTypeKind.i32],
                    args: [],
                    ops: [
                        .OInt(dst: 0, ptr: 0),
                        .OInt(dst: 2, ptr: 0),
                        .OBool(dst: 1, value: 0),
                        // check the bool didn't overwrite the ints
                        .ORet(ret: 0)
                    ]),
                prepareFunction(
                    retType: HLTypeKind.i32,
                    findex: 1,
                    regs: [HLTypeKind.i32, HLTypeKind.bool, HLTypeKind.i32],
                    args: [],
                    ops: [
                        .OInt(dst: 0, ptr: 0),
                        .OInt(dst: 2, ptr: 0),
                        .OBool(dst: 1, value: 0),
                        // check the bool didn't overwrite the ints
                        .ORet(ret: 2)
                    ]),
                prepareFunction(
                    retType: HLTypeKind.bool,
                    findex: 2,
                    regs: [HLTypeKind.i32, HLTypeKind.bool, HLTypeKind.i32],
                    args: [],
                    ops: [
                        .OBool(dst: 1, value: 0),
                        .OInt(dst: 0, ptr: 0),
                        .OInt(dst: 2, ptr: 0),
                        // check the bool didn't overwrite the ints
                        .ORet(ret: 1)
                    ]
                )
            ], ints: [
                Int32(bitPattern: UInt32.max)
            ]
        )
        
        try compileAndLink(ctx: ctx, 0, 1, 2) {
            mappedMem in
            
            try mappedMem.jit(ctx: ctx, fix: 0) { (entrypoint: (@convention(c) () -> (Int32))) in
                XCTAssertEqual(-1, entrypoint())
            }
            
            try mappedMem.jit(ctx: ctx, fix: 1) { (entrypoint: (@convention(c) () -> (Int32))) in
                XCTAssertEqual(-1, entrypoint())
            }
            
            try mappedMem.jit(ctx: ctx, fix: 2) { (entrypoint: (@convention(c) () -> (UInt8))) in
                XCTAssertEqual(0, entrypoint())
            }
        }
    }

    func testCalcStackArgReq() throws {
        let ctx = try prepareContext(compilables: [])
        let sut = sut(ctx: ctx)

        // test different size combinations (ensure aligned to 16 bytes)
        var (size, _) = sut.calcStackArgReq(regs: [HLTypeKind.array, HLTypeKind.array], args: [])
        XCTAssertEqual(16, size)

        (size, _) = sut.calcStackArgReq(regs: [HLTypeKind.array, HLTypeKind.array, HLTypeKind.i32], args: [])
        XCTAssertEqual(32, size)

        (size, _) = sut.calcStackArgReq(
            regs: [HLTypeKind.array, HLTypeKind.array, HLTypeKind.i32, HLTypeKind.dyn, HLTypeKind.dynobj],
            args: []
        )
        XCTAssertEqual(48, size)

        (size, _) = sut.calcStackArgReq(
            regs: [HLTypeKind.array, HLTypeKind.array, HLTypeKind.i32, HLTypeKind.dyn, HLTypeKind.dynobj],
            args: [HLTypeKind.array, HLTypeKind.array, HLTypeKind.i32, HLTypeKind.dyn, HLTypeKind.dynobj]
        )
        XCTAssertEqual(48, size)

        // args exceeding first 8 should not allocate extra space (as it
        // should already be allocated due to calling convention)
        (size, _) = sut.calcStackArgReq(
            regs: Array(repeating: HLTypeKind.dyn, count: 16),
            args: Array(repeating: HLTypeKind.dyn, count: 16)
        )
        XCTAssertEqual(64, size)

        // non args should take space
        (size, _) = sut.calcStackArgReq(regs: [HLTypeKind.i32], args: [])
        XCTAssertEqual(16, size)

        // 4 regs (all except 1st) and 1 arg should contribute to size here
        (size, _) = sut.calcStackArgReq(
            regs: [HLTypeKind.array] + Array(repeating: HLTypeKind.i32, count: 4),
            args: [HLTypeKind.array]
        )
        XCTAssertEqual(32, size)

        // first 8 args should take space
        (size, _) = sut.calcStackArgReq(
            regs: Array(repeating: HLTypeKind.i32, count: 8),
            args: Array(repeating: HLTypeKind.i32, count: 8)
        )
        XCTAssertEqual(32, size)

        // void should be ignored
        (size, _) = sut.calcStackArgReq(
            regs: Array(repeating: HLTypeKind.void, count: 8) + Array(repeating: HLTypeKind.i32, count: 8),
            args: Array(repeating: HLTypeKind.void, count: 8) + Array(repeating: HLTypeKind.i32, count: 8)
        )
        XCTAssertEqual(32, size)
    }

    func testAppendPrologue() throws {
        let buff = CpuOpBuffer()
        let ctx = try prepareContext(compilables: [])
        let sut = sut(ctx: ctx)
        _ = sut.appendPrologue(builder: buff)
        
        let mapper = BufferMapper(ctx: ctx, buffer: buff)
        let mem = try mapper.emitMachineCode()
        
        XCTAssertEqual(
            mem,
            [0xfd, 0x7b, 0xbf, 0xa9, 0xfd, 0x03, 0x00, 0x91]
        )
    }

    func testAppendEpilogue() throws {
        let buff = CpuOpBuffer()
        let ctx = try prepareContext(compilables: [])
        let sut = sut(ctx: ctx)
        sut.appendEpilogue(builder: buff)
        
        let mapper = BufferMapper(ctx: ctx, buffer: buff)
        let mem = try mapper.emitMachineCode()
        
        XCTAssertEqual(
            mem,
            [0xfd, 0x7b, 0xc1, 0xa8]
        )
    }

    func testAppendStackInit_skipVoid() throws {
        let buff = CpuOpBuffer()
        let ctx = try prepareContext(compilables: [])
        let sut = sut(ctx: ctx)
        try sut.appendStackInit([HLTypeKind.void], args: [HLTypeKind.void], builder: buff, prologueSize: 0)
        
        let mapper = BufferMapper(ctx: ctx, buffer: buff)
        let mem = try mapper.emitMachineCode()
        
        XCTAssertEqual(
            mem,
            []
        )
    }

    func testAppendStackInit_min16() throws {
        let _1_need16 = Array(repeating: HLTypeKind.i32, count: 1)
        let _4_need16 = Array(repeating: HLTypeKind.i32, count: 4)
        let _5_need32 = Array(repeating: HLTypeKind.i32, count: 5)
        let ctx = try prepareContext(compilables: [])
        let sut = sut(ctx: ctx)
        
        // 4 byte requirement should still be aligned to 16 byte boundary
        let mem1 = CpuOpBuffer()
        try sut.appendStackInit(_1_need16, args: _1_need16, builder: mem1, prologueSize: 0)
        //mem1.hexPrint()
        XCTAssertEqual(
            [
                0xff, 0x43, 0x00, 0xd1,  // sub sp, sp, #16
                0xe0, 0x03, 0x00, 0xb8,  // str w0, [sp, #0]
            ],
            try BufferMapper(ctx: ctx, buffer: mem1).emitMachineCode()
        )

        // 16 byte requirement should not round to 32
        let mem2 = CpuOpBuffer()
        try sut.appendStackInit(_4_need16, args: _4_need16, builder: mem2, prologueSize: 0)
        //mem2.hexPrint()
        XCTAssertEqual(
            [
                0xff, 0x43, 0x00, 0xd1,  // sub sp, sp, #16
                0xe0, 0x03, 0x00, 0xb8,  // str w0, [sp, #0]
                0xe1, 0x43, 0x00, 0xb8,  // str w1, [sp, #4]
                0xe2, 0x83, 0x00, 0xb8,  // str w2, [sp, #8]
                0xe3, 0xc3, 0x00, 0xb8,  // str w3, [sp, #12]
            ],
            try BufferMapper(ctx: ctx, buffer: mem2).emitMachineCode()
        )
        // 20 byte requirement should round to 32
        let mem3 = CpuOpBuffer()
        try sut.appendStackInit(_5_need32, args: _5_need32, builder: mem3, prologueSize: 0)
        
        XCTAssertEqual(
            [
                0xff, 0x83, 0x00, 0xd1,  // sub sp, sp, #32
                0xe0, 0x03, 0x00, 0xb8,  // str w0, [sp, #0]
                0xe1, 0x43, 0x00, 0xb8,  // str w1, [sp, #4]
                0xe2, 0x83, 0x00, 0xb8,  // str w2, [sp, #8]
                0xe3, 0xc3, 0x00, 0xb8,  // str w3, [sp, #12]
                0xe4, 0x03, 0x01, 0xb8,  // str w4, [sp, #16]
            ],
            try BufferMapper(ctx: ctx, buffer: mem3).emitMachineCode()
        )
    }

    func testAppendStackInit_multiple() throws {
        let ctx = try prepareContext(compilables: [])
        let mem = CpuOpBuffer()
        let sut = sut(ctx: ctx)
        try sut.appendStackInit(
            [HLTypeKind.void, HLTypeKind.i32, HLTypeKind.i64],
            args: [HLTypeKind.void, HLTypeKind.i32, HLTypeKind.i64],
            builder: mem,
            prologueSize: 0
        )
        
        XCTAssertEqual(
            [
                0xff, 0x43, 0x00, 0xd1,  // sub sp, sp, #16
                0xe0, 0x03, 0x00, 0xb8,  // str w0, [sp, #0]
                0xe1, 0x43, 0x00, 0xf8,  // str x1, [sp, #4]
            ],
            try BufferMapper(ctx: ctx, buffer: mem).emitMachineCode()
        )
    }

    func testAppendStackInit_moreThan8Args() throws {
        let ctx = try prepareContext(compilables: [])
        let sut = sut(ctx: ctx)
        let mem = CpuOpBuffer()
        try sut.appendStackInit(
            Array(repeating: HLTypeKind.i32, count: 12),
            args: Array(repeating: HLTypeKind.i32, count: 12),
            builder: mem,
            prologueSize: 0
        )
        //        mem.hexPrint()
        XCTAssertEqual(
            [
                // Reserving 48 bytes for entire stack
                0xff, 0xc3, 0x00, 0xd1, // sub sp, sp, #48
                0xe0, 0x03, 0x00, 0xb8, // str w0, [sp, #0]
                0xe1, 0x43, 0x00, 0xb8, // str w1, [sp, #4]
                0xe2, 0x83, 0x00, 0xb8, // str w2, [sp, #8]
                0xe3, 0xc3, 0x00, 0xb8, // str w3, [sp, #12]
                0xe4, 0x03, 0x01, 0xb8, // str w4, [sp, #16]
                0xe5, 0x43, 0x01, 0xb8, // str w5, [sp, #20]
                0xe6, 0x83, 0x01, 0xb8, // str w6, [sp, #24]
                0xe7, 0xc3, 0x01, 0xb8, // str w7, [sp, #28]
                0xe1, 0x33, 0x40, 0xb9, // ldr w1, [sp, #48]
                0xe1, 0x03, 0x02, 0xb8, // str w1, [sp, #32]
                0xe1, 0x43, 0x43, 0xb8, // ldr w1, [sp, #52]
                0xe1, 0x43, 0x02, 0xb8, // str w1, [sp, #36]
                0xe1, 0x3b, 0x40, 0xb9, // ldr w1, [sp, #56]
                0xe1, 0x83, 0x02, 0xb8, // str w1, [sp, #40]
                0xe1, 0xc3, 0x43, 0xb8, // ldr w1, [sp, #60]
                0xe1, 0xc3, 0x02, 0xb8, // str w1, [sp, #44]
            ],
            try BufferMapper(ctx: ctx, buffer: mem).emitMachineCode()
        )
    }

    func testAppendStackInit_mismatchedRegs() throws {
        let ctx = try prepareContext(compilables: [])
        let sut = sut(ctx: ctx)
        let mem = CpuOpBuffer()
        
        XCTAssertThrowsError(
            try sut.appendStackInit([HLTypeKind.i32], args: [HLTypeKind.void], builder: mem, prologueSize: 0)
        )
    }

    func testAppendDebugPrintAligned4() throws {
        let ctx = try prepareContext(compilables: [])
        let sutWith = sut(ctx: ctx, strip: false)
        let sutWithout = sut(ctx: ctx, strip: true)
        let memWith = CpuOpBuffer()
        let memWithout = CpuOpBuffer()
        
        sutWith.appendDebugPrintAligned4(
            "Hello World",
            builder: memWith
        )
        sutWithout.appendDebugPrintAligned4(
            "Hello World",
            builder: memWithout
        )

        XCTAssertEqual(
            [],
            try BufferMapper(ctx: ctx, buffer: memWithout).emitMachineCode()
        )

        XCTAssertEqual(
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
            ],
            try BufferMapper(ctx: ctx, buffer: memWith).emitMachineCode()
        )
    }

    func testCompile__OXor() throws {
        try _ru8__u8_u8(ops: [
            .OXor(dst: 1, a: 0, b: 1),
            .ORet(ret: 1),
        ]) {
            entrypoint in
            
            XCTAssertEqual(0b10011111, entrypoint(0b11010110, 0b01001001))
        }
    }

    func testCompile__OToInt__i32_to_i64() throws {
        try _ri64__i32(ops: [
            .OToInt(dst: 1, src: 0),
            .ORet(ret: 1),
        ], regs: [.i32, .i64]) {
            entrypoint_32t64 in
            
            XCTAssertEqual(
                0b10000000_10000000_10000000_10000000,
                entrypoint_32t64(Int32(bitPattern: 0b10000000_10000000_10000000_10000000))
            )
        }
        
        try _ri32__i64(ops: [
            .OToInt(dst: 1, src: 0),
            .ORet(ret: 1),
        ], regs: [.i64, .i32]) {
            entrypoint_64t32 in
            
            XCTAssertEqual(
                Int32(bitPattern: 0b10000000_10000000_10000000_10000000),
                entrypoint_64t32(0b10000000_10000000_10000000_10000000_10000000)
            )
        }
    }

    func testCompile__OMul() throws {
        try _ri8__i8_i8(ops: [
            .OMul(dst: 1, a: 0, b: 1),
            .ORet(ret: 1),
        ]) { entrypoint in
            
            XCTAssertEqual(4, entrypoint(1, 4))
            XCTAssertEqual(4, entrypoint(2, 2))
            XCTAssertEqual(36, entrypoint(4, 9))
            XCTAssertEqual(0, entrypoint(1, 0))
            XCTAssertEqual(-10, entrypoint(-5, 2))
        }
    }
    
    func testCompile__OToDyn__i32_to_nullI32() throws {
        try _rdyn__i32(ops: [
            .OToDyn(dst: 1, src: 0),
            .ORet(ret: 1)
        ]) { entrypoint in
            
            let dyn = entrypoint(1243)
            XCTAssertEqual(dyn.pointee.i, 1243)
        }
    }
    
    func testCompile__OOr() throws {
        try _ri32__i32_i32(ops: [
            .OOr(dst: 0, a: 0, b: 1),
            .ORet(ret: 0)
        ]) {
            entrypoint in
            
            XCTAssertEqual(0b111, entrypoint(0b000, 0b111))
            XCTAssertEqual(0b111, entrypoint(0b010, 0b101))
            XCTAssertEqual(0b101, entrypoint(0b100, 0b001))
            
            let res = entrypoint(Int32(bitPattern: 0b01010101010101010101010101010101), Int32(bitPattern: 0b10101010101010101010101010101010))
            XCTAssertEqual(res, Int32(bitPattern: 0b11111111111111111111111111111111))
        }
    }
}
