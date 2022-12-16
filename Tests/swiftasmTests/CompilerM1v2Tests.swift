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

fileprivate func prepareContext(compilables: [any Compilable2], natives: [any NativeCallable2] = [], ints: [Int32] = [], strings: [String] = [], bytes: [[UInt8]] = [], globals: [any HLTypeProvider] = [], floats: [Float64] = [], v: Int? = nil) throws -> CCompatJitContext {
    let tm = TestJitModule(compilables, natives: natives, ints: ints, strings: strings, bytes: bytes, globals: globals, floats: floats, v: v)
    assert(tm.ntypes > 0)
    return try CCompatJitContext(ctx: tm)
}

fileprivate func compileAndLink(ctx: CCompatJitContext, _ fix: Int..., strip: Bool = true, callback: (UnsafeMutableRawPointer)throws->()) throws {
    let mem = CpuOpBuffer()
    let sut = M1Compiler2(ctx: ctx, stripDebugMessages: strip)
    
    try fix.forEach { try sut.compile(findex: $0, into: mem) }
    
    let mapper = BufferMapper(ctx: ctx, buffer: mem)
    let mappedMem = try mapper.getMemory()
    
    try callback(mappedMem)
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
        let sut = M1Compiler2(ctx: ctx, stripDebugMessages: true)
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
        let sut = M1Compiler2(ctx: ctx, stripDebugMessages: true)
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
    
    func testCompile_OJSLt_u32() throws {
        // constants
        let constI_3 = 1 // constant value 3
        let constI_57005 = 2 // constant value 57005
        
        try _ri32__i32_i32(ops: [
            .OJSLt(a: 0, b: 1, offset: 2),
            
            // return 3
            .OInt(dst: 1, ptr: constI_3),
            .ORet(ret: 1),
            
            // return 57005
            .OInt(dst: 1, ptr: constI_57005),
            .ORet(ret: 1)
        ], ints: [0, 3, 57005]) { entrypoint in
            XCTAssertEqual(57005, entrypoint(Int32(bitPattern: 0xffffffff), 0))
        }
    }
    
    func testCompile_OJSGt() throws {
        let constI_3 = 1 // constant value 3
        let constI_57 = 2 // constant value 57
        
        let ops: [HLOpCode] = [
            .OJSGt(a: 0, b: 1, offset: 2),
            
            // return 3
            .OInt(dst: 1, ptr: constI_3),
            .ORet(ret: 1),
            
            // return 57
            .OInt(dst: 1, ptr: constI_57),
            .ORet(ret: 1)
        ]
        
        try _ri32__i32_i32(ops: ops, ints: [0, 3, 57]) {
            entrypoint in
            
            // gt -> jump
            XCTAssertEqual(57, entrypoint(
                0b00000000_00000000_00000000_00000001,
                Int32(bitPattern: 0b10000000_00000000_00000000_00000000)))
            
            // don't jump
            XCTAssertEqual(3, entrypoint(
                Int32(bitPattern: 0b10000000_00000000_00000000_00000000),
                0b00000000_00000000_00000000_00000001))
            XCTAssertEqual(3, entrypoint(81, 81))
        }
        
        try _ru16__u16_u16(ops: ops, ints: [0, 3, 57]) {
            entrypoint in
            
            // gt -> jump
            XCTAssertEqual(57, entrypoint(0b00000000_00000001, 0b10000000_00000000))
            
            // don't jump
            XCTAssertEqual(3, entrypoint(0b10000000_00000000, 0b00000000_00000001))
            XCTAssertEqual(3, entrypoint(81, 81))
        }
        
        try _ru8__u8_u8(ops: ops, ints: [0, 3, 57]) {
            entrypoint in
            
            // gt -> jump
            XCTAssertEqual(57, entrypoint(0b00000001, 0b10000000))
            
            // don't jump
            XCTAssertEqual(3, entrypoint(0b10000000, 0b00000001))
            XCTAssertEqual(3, entrypoint(81, 81))
        }
    }
    
    func testCompile_OJUGte() throws {
        let constI_3 = 1 // constant value 3
        let constI_57 = 2 // constant value 57005
        
        let ops: [HLOpCode] = [
            .OJUGte(a: 0, b: 1, offset: 2),
            
            // return 3
            .OInt(dst: 1, ptr: constI_3),
            .ORet(ret: 1),
            
            // return 57005
            .OInt(dst: 1, ptr: constI_57),
            .ORet(ret: 1)
        ]
        
        try _ri32__i32_i32(ops: ops, ints: [0, 3, 57]) {
            entrypoint in
            
            // gte -> jump
            XCTAssertEqual(57, entrypoint(
                Int32(bitPattern: 0b10000000_00000000_00000000_00000000),
                0b00000000_00000000_00000000_00000001))
            XCTAssertEqual(57, entrypoint(81, 81))
            
            // don't jump
            XCTAssertEqual(3, entrypoint(
                0b00000000_00000000_00000000_00000001,
                Int32(bitPattern: 0b10000000_00000000_00000000_00000000)))
        }
        
        try _ru16__u16_u16(ops: ops, ints: [0, 3, 57]) {
            entrypoint in
            
            // gte -> jump
            XCTAssertEqual(57, entrypoint(0b10000000_00000000, 0b00000000_00000001))
            XCTAssertEqual(57, entrypoint(81, 81))
            
            // don't jump
            XCTAssertEqual(3, entrypoint(0b00000000_00000001, 0b10000000_00000000))
        }
        
        try _ru8__u8_u8(ops: ops, ints: [0, 3, 57]) {
            entrypoint in
            
            // gte -> jump
            XCTAssertEqual(57, entrypoint(0b10000000, 0b00000001))
            XCTAssertEqual(57, entrypoint(81, 81))
            
            // don't jump
            XCTAssertEqual(3, entrypoint(0b00000001, 0b10000000))
        }
    }
    
    func testCompile_OJNotLt() throws {
        let constI_3 = 1 // constant value 3
        let constI_57 = 2 // constant value 57005
        
        let ops: [HLOpCode] = [
            .OJNotLt(a: 0, b: 1, offset: 2),
            
            // return 3
            .OInt(dst: 1, ptr: constI_3),
            .ORet(ret: 1),
            
            // return 57005
            .OInt(dst: 1, ptr: constI_57),
            .ORet(ret: 1)
        ]
        
        try _ri32__i32_i32(ops: ops, ints: [0, 3, 57]) {
            entrypoint in
            
            // lt -> dont jump
            XCTAssertEqual(3, entrypoint(5, 100))
            
            // jump
            XCTAssertEqual(57, entrypoint(81, 81))
            XCTAssertEqual(57, entrypoint(100, 5))
        }
        
        try _ru16__u16_u16(ops: ops, ints: [0, 3, 57]) {
            entrypoint in
            
            // lt -> dont jump
            XCTAssertEqual(3, entrypoint(5, 100))
            
            // jump
            XCTAssertEqual(57, entrypoint(81, 81))
            XCTAssertEqual(57, entrypoint(100, 5))
        }
        
        try _ru8__u8_u8(ops: ops, ints: [0, 3, 57]) {
            entrypoint in
            
            // lt -> dont jump
            XCTAssertEqual(3, entrypoint(5, 100))
            
            // jump
            XCTAssertEqual(57, entrypoint(81, 81))
            XCTAssertEqual(57, entrypoint(100, 5))
        }
    }
    
    func testCompile_OJNotGte() throws {
        let constI_3 = 1 // constant value 3
        let constI_57 = 2 // constant value 57005
        
        let ops: [HLOpCode] = [
            .OJNotGte(a: 0, b: 1, offset: 2),
            
            // return 3
            .OInt(dst: 1, ptr: constI_3),
            .ORet(ret: 1),
            
            // return 57005
            .OInt(dst: 1, ptr: constI_57),
            .ORet(ret: 1)
        ]
        
        try _ri32__i32_i32(ops: ops, ints: [0, 3, 57]) {
            entrypoint in
            
            // gte -> dont jump
            XCTAssertEqual(3, entrypoint(81, 81))
            XCTAssertEqual(3, entrypoint(100, 5))
            
            // jump
            XCTAssertEqual(57, entrypoint(5, 100))
        }
        
        try _ru16__u16_u16(ops: ops, ints: [0, 3, 57]) {
            entrypoint in
            
            // gte -> dont jump
            XCTAssertEqual(3, entrypoint(81, 81))
            XCTAssertEqual(3, entrypoint(100, 5))
            
            // jump
            XCTAssertEqual(57, entrypoint(5, 100))
        }
        
        try _ru8__u8_u8(ops: ops, ints: [0, 3, 57]) {
            entrypoint in
            
            // gte -> dont jump
            XCTAssertEqual(3, entrypoint(81, 81))
            XCTAssertEqual(3, entrypoint(100, 5))
            
            // jump
            XCTAssertEqual(57, entrypoint(5, 100))
        }
    }
    
    func testCompile_ONot() throws {
        try _rbool__bool(ops: [
            .ONot(dst: 0, src: 0),
            .ORet(ret: 0)
        ], ints: [0, 3, 57]) {
            entrypoint in
            
            // jump
            XCTAssertEqual(false, entrypoint(true))
            XCTAssertEqual(true, entrypoint(false))
        }
    }
    
    func testCompile_OGetGlobal_OSetGlobal() throws {
        
        let globalString: UnsafeMutablePointer<_String> = .allocate(capacity: 1)
        let globalStringBytes: UnsafeMutableBufferPointer<UInt8> = .init(start: .init(OpaquePointer(globalString)), count: MemoryLayout<_String>.stride)
        globalStringBytes.initialize(repeating: 0)
        
        let strA = "Hello World\0"
        let strB = "Updated String\0"
        let bytesA = strA.data(using: .utf16LittleEndian)!
        let bytesB = strB.data(using: .utf16LittleEndian)!
        
        let bytesAPtr: UnsafeMutableBufferPointer<UInt8> = .allocate(capacity: bytesA.count)
        let bytesBPtr: UnsafeMutableBufferPointer<UInt8> = .allocate(capacity: bytesB.count)
        _ = bytesAPtr.initialize(from: bytesA)
        _ = bytesBPtr.initialize(from: bytesB)
        
        defer {
            globalString.deinitialize(count: 1)
            globalString.deallocate()
            bytesAPtr.deallocate()
            bytesBPtr.deallocate()
        }
        
        let globals: [any HLTypeProvider] = [_StringType, _StringType, _StringType]
        let ctx = try prepareContext(
            compilables: [
                prepareFunction(
                    retType: _StringType,
                    findex: 0,
                    regs: [_StringType, HLTypeKind.bool],
                    args: [_StringType, HLTypeKind.bool],
                    ops: [
                        .OJFalse(cond: 1, offset: 1),
                        .OSetGlobal(global: 1, src: 0),
                        .OGetGlobal(dst: 0, global: 1),
                        .ORet(ret: 0)
                    ])
            ],
            // set multiple globals (so we can test fetching the right index
            globals: globals
        )
        
        try compileAndLink(ctx: ctx, 0) {
            mappedMem in
            
            var strType: UnsafePointer<HLType_CCompat>? = nil
            for typeIx in (0..<ctx.ntypes) {
                let t = try ctx.getType(Int(typeIx)) as any HLTypeProvider
                if t.isEquivalent(_StringType) {
                    strType = .init(OpaquePointer(t.ccompatAddress))
                    break
                }
            }
            guard let strType = strType else {
                fatalError("Could not find initialized string type")
            }
            
            var strObjA = _String(t: strType, bytes: .init(OpaquePointer(bytesAPtr.baseAddress!)), length: Int32(strA.count))
            var strObjB = _String(t: strType, bytes: .init(OpaquePointer(bytesBPtr.baseAddress!)), length: Int32(strB.count))
            
            // for manipulating globals data outside of hashlink context
            let globalDataPtr: UnsafeMutablePointer<UnsafePointer<_String>?> = .init(mutating: .init(OpaquePointer(ctx.mainContext.pointee.m!.pointee.globals_data!)))
            let globalDataBufPtr: UnsafeMutableBufferPointer = .init(start: globalDataPtr, count: globals.count)
            
            try mappedMem.jit(ctx: ctx, fix: 0) { (entrypoint: (@convention(c) (OpaquePointer, Bool) -> OpaquePointer?)) in
                let typedEntrypoint: (UnsafePointer<_String>, Bool)->UnsafePointer<_String>? = {
                    .init(entrypoint(OpaquePointer($0), $1))
                }
                
                withUnsafeMutablePointer(to: &strObjA) {
                    strObjAPtr in
                    
                    withUnsafeMutablePointer(to: &strObjB) {
                        strObjBPtr in
                        
                        // OGetGlobal -- global not set (we should be looking for the data in the middle)
                        _ = globalDataBufPtr.initialize(from: [UnsafePointer(strObjAPtr), nil, UnsafePointer(strObjAPtr)])
                        XCTAssertEqual(typedEntrypoint(strObjBPtr, false), nil)
                        
                        // OGetGlobal -- global returned correctly (we should be looking for the data in the middle)
                        _ = globalDataBufPtr.initialize(from: [nil, UnsafePointer(strObjAPtr), nil])
                        XCTAssertEqual(typedEntrypoint(strObjBPtr, false), strObjAPtr)
                        
                        // OSetGlobal first
                        XCTAssertEqual(typedEntrypoint(strObjBPtr, true), strObjBPtr)
                        XCTAssertEqual(Array(globalDataBufPtr), [nil, UnsafePointer(strObjBPtr), nil])
                    }
                }
            }
        }
    }
    
    func testCompile_ONeg() throws {
        try _ri32__i32(ops: [
            .ONeg(dst: 0, src: 0),
            .ORet(ret: 0)
        ]) {
            entrypoint in
            
            XCTAssertEqual(1245, entrypoint(-1245))
            XCTAssertEqual(-1245, entrypoint(1245))
        }
        
        try _ri64__i64(ops: [
            .ONeg(dst: 0, src: 0),
            .ORet(ret: 0)
        ]) {
            entrypoint in
            
            XCTAssertEqual(1245, entrypoint(-1245))
            XCTAssertEqual(-1245, entrypoint(1245))
        }
        
        try _ru16__u16(ops: [
            .ONeg(dst: 0, src: 0),
            .ORet(ret: 0)
        ]) {
            entrypoint in
            
            XCTAssertEqual(1245, entrypoint(UInt16(bitPattern: -1245)))
            XCTAssertEqual(UInt16(bitPattern: -1245), entrypoint(1245))
        }
        
        try _ru8__u8(ops: [
            .ONeg(dst: 0, src: 0),
            .ORet(ret: 0)
        ]) {
            entrypoint in
            
            XCTAssertEqual(12, entrypoint(UInt8(bitPattern: -12)))
            XCTAssertEqual(UInt8(bitPattern: -12), entrypoint(12))
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
        /*
         [jitdebug] f5: #37: OJSLt: if s(reg11) < s(reg13) jump to 4
         [jitdebug] Register x0: 0xffffffff (4294967295)
         [jitdebug] Register x1: 0x0 (4294967295)
         [jitdebug] NOT JUMPING
         
         --
         
         [jitdebug] f0: #0: OJSLt: if s(reg0) < s(reg1) jump to 2
         [jitdebug] Register x0: 0xffffffff (18446744073709551615)
         [jitdebug] Register x1: 0x0 (18446744073709551615)
         [jitdebug] f0: #3: OInt: reg2 = i 2
         */
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
    
    func _robj(obj: any HLTypeProvider, ops: [HLOpCode], args: [any HLTypeProvider] = [], strings: [String] = [], globals: [OpaquePointer], _ callback: @escaping (()->UnsafeRawPointer?)->()) throws {
        let ctx = try prepareContext(compilables: [
            prepareFunction(
                retType: obj,
                findex: 0,
                regs: args + [obj],
                args: args,
                ops: ops)
        ], strings: strings)
        
        try compileAndLink(ctx: ctx, 0) {
            mappedMem in
            
            try mappedMem.jit(ctx: ctx, fix: 0) { (ep: (@convention(c) () -> UnsafeRawPointer?)) in
                callback(ep)
            }
        }
    }
    
    func _rbytes(ops: [HLOpCode], regs: [any HLTypeProvider] = [HLTypeKind.bytes], strings: [String] = [], bytes: [[UInt8]] = [], v: Int? = nil, _ callback: @escaping (()->UnsafeRawPointer?)->()) throws {
        
        let ctx = try prepareContext(compilables: [
            prepareFunction(
                retType: HLTypeKind.bytes,
                findex: 0,
                regs: regs,
                args: regs,
                ops: ops)
        ], strings: strings, bytes: bytes, v: v)
        
        try compileAndLink(ctx: ctx, 0) {
            mappedMem in
            
            try mappedMem.jit(ctx: ctx, fix: 0) { (ep: (@convention(c) () -> UnsafeRawPointer?)) in
                callback(ep)
            }
        }
    }
    
    func _rf64__i32(ops: [HLOpCode], regs: [HLTypeKind] = [.i32, .f64], _ callback: @escaping ((Int32)->Float64)->()) throws {
        let ctx = try prepareContext(compilables: [
            prepareFunction(
                retType: HLTypeKind.f64,
                findex: 0,
                regs: regs,
                args: [HLTypeKind.i32],
                ops: ops)
        ])
        
        try compileAndLink(ctx: ctx, 0) {
            mappedMem in
            
            try mappedMem.jit(ctx: ctx, fix: 0) { (ep: (@convention(c) (Int32) -> Float64)) in
                callback(ep)
            }
        }
    }
    
    func _rf64(ops: [HLOpCode], regs: [HLTypeKind] = [.f64], floats: [Float64], _ callback: @escaping (()->Float64)->()) throws {
        let ctx = try prepareContext(compilables: [
            prepareFunction(
                retType: HLTypeKind.f64,
                findex: 0,
                regs: regs,
                args: [],
                ops: ops)
        ], floats: floats)
        
        try compileAndLink(ctx: ctx, 0) {
            mappedMem in
            
            try mappedMem.jit(ctx: ctx, fix: 0) { (ep: (@convention(c) () -> Float64)) in
                callback(ep)
            }
        }
    }
    
    func _ri32__f64(ops: [HLOpCode], regs: [HLTypeKind] = [.f64, .i32], _ callback: @escaping ((Float64)->Int32)->()) throws {
        let ctx = try prepareContext(compilables: [
            prepareFunction(
                retType: HLTypeKind.i32,
                findex: 0,
                regs: regs,
                args: [HLTypeKind.f64, HLTypeKind.i32],
                ops: ops)
        ])
        
        try compileAndLink(ctx: ctx, 0) {
            mappedMem in
            
            try mappedMem.jit(ctx: ctx, fix: 0) { (ep: (@convention(c) (Float64) -> Int32)) in
                callback(ep)
            }
        }
    }
    
    func _ri32__i32_i32(ops: [HLOpCode], regs: [HLTypeKind] = [.i32, .i32], ints: [Int32] = [], _ callback: @escaping ((Int32, Int32)->Int32)->()) throws {
        let ctx = try prepareContext(compilables: [
            prepareFunction(
                retType: HLTypeKind.i32,
                findex: 0,
                regs: regs,
                args: [HLTypeKind.i32, HLTypeKind.i32],
                ops: ops)
        ], ints: ints)
        
        try compileAndLink(ctx: ctx, 0) {
            mappedMem in
            
            try mappedMem.jit(ctx: ctx, fix: 0) { (ep: (@convention(c) (Int32, Int32) -> Int32)) in
                callback(ep)
            }
        }
    }
    
    func _ru16__u16_u16(ops: [HLOpCode], regs: [HLTypeKind] = [.u16, .u16], ints: [Int32] = [], _ callback: @escaping ((UInt16, UInt16)->UInt16)->()) throws {
        let ctx = try prepareContext(compilables: [
            prepareFunction(
                retType: HLTypeKind.u16,
                findex: 0,
                regs: regs,
                args: [HLTypeKind.u16, HLTypeKind.u16],
                ops: ops)
        ], ints: ints)
        
        try compileAndLink(ctx: ctx, 0) {
            mappedMem in
            
            try mappedMem.jit(ctx: ctx, fix: 0) { (ep: (@convention(c) (UInt16, UInt16) -> UInt16)) in
                callback(ep)
            }
        }
    }
    
    func _ru8__u8_u8(ops: [HLOpCode], regs: [HLTypeKind] = [.u8, .u8], ints: [Int32] = [], _ callback: @escaping ((UInt8, UInt8)->UInt8)->()) throws {
        let ctx = try prepareContext(compilables: [
            prepareFunction(
                retType: HLTypeKind.u8,
                findex: 0,
                regs: regs,
                args: [HLTypeKind.u8, HLTypeKind.u8],
                ops: ops)
        ], ints: ints)
        
        try compileAndLink(ctx: ctx, 0) {
            mappedMem in
            
            try mappedMem.jit(ctx: ctx, fix: 0) { (ep: (@convention(c) (UInt8, UInt8) -> UInt8)) in
                callback(ep)
            }
        }
    }
    
    func _rbool__bool(ops: [HLOpCode], regs: [HLTypeKind] = [.bool], ints: [Int32] = [], _ callback: @escaping ((Bool)->Bool)->()) throws {
        let ctx = try prepareContext(compilables: [
            prepareFunction(
                retType: HLTypeKind.bool,
                findex: 0,
                regs: regs,
                args: [HLTypeKind.bool],
                ops: ops)
        ], ints: ints)
        
        try compileAndLink(ctx: ctx, 0) {
            mappedMem in
            
            try mappedMem.jit(ctx: ctx, fix: 0) { (ep: (@convention(c) (Bool) -> Bool)) in
                callback(ep)
            }
        }
    }
    
    func _ri32__i32(ops: [HLOpCode], regs: [HLTypeKind] = [.i32], ints: [Int32] = [], _ callback: @escaping ((Int32)->Int32)->()) throws {
        let ctx = try prepareContext(compilables: [
            prepareFunction(
                retType: HLTypeKind.i32,
                findex: 0,
                regs: regs,
                args: [HLTypeKind.i32],
                ops: ops)
        ], ints: ints)
        
        try compileAndLink(ctx: ctx, 0) {
            mappedMem in
            
            try mappedMem.jit(ctx: ctx, fix: 0) { (ep: (@convention(c) (Int32) -> Int32)) in
                callback(ep)
            }
        }
    }
    
    func _ru16__u16(ops: [HLOpCode], regs: [HLTypeKind] = [.u16], ints: [Int32] = [], _ callback: @escaping ((UInt16)->UInt16)->()) throws {
        let ctx = try prepareContext(compilables: [
            prepareFunction(
                retType: HLTypeKind.u16,
                findex: 0,
                regs: regs,
                args: [HLTypeKind.u16],
                ops: ops)
        ], ints: ints)
        
        try compileAndLink(ctx: ctx, 0) {
            mappedMem in
            
            try mappedMem.jit(ctx: ctx, fix: 0) { (ep: (@convention(c) (UInt16) -> UInt16)) in
                callback(ep)
            }
        }
    }
    
    func _ru8__u8(ops: [HLOpCode], regs: [HLTypeKind] = [.u8], ints: [Int32] = [], _ callback: @escaping ((UInt8)->UInt8)->()) throws {
        typealias _Arg = UInt8
        let reg = HLTypeKind.u8
        let ctx = try prepareContext(compilables: [
            prepareFunction(
                retType: reg,
                findex: 0,
                regs: regs,
                args: [reg],
                ops: ops)
        ], ints: ints)
        
        try compileAndLink(ctx: ctx, 0) {
            mappedMem in
            
            try mappedMem.jit(ctx: ctx, fix: 0) { (ep: (@convention(c) (_Arg) -> _Arg)) in
                callback(ep)
            }
        }
    }
    
    func _ri64__i64(ops: [HLOpCode], regs: [HLTypeKind] = [.i64], ints: [Int32] = [], _ callback: @escaping ((Int64)->Int64)->()) throws {
        typealias _Arg = Int64
        let reg = HLTypeKind.i64
        let ctx = try prepareContext(compilables: [
            prepareFunction(
                retType: reg,
                findex: 0,
                regs: regs,
                args: [reg],
                ops: ops)
        ], ints: ints)
        
        try compileAndLink(ctx: ctx, 0) {
            mappedMem in
            
            try mappedMem.jit(ctx: ctx, fix: 0) { (ep: (@convention(c) (_Arg) -> _Arg)) in
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
    
    func _ri64__obj(ops: [HLOpCode], regs: [HLTypeKind] = [.obj, .i64], _ callback: @escaping ((UnsafeRawPointer)->Int64)->()) throws {
        let ctx = try prepareContext(compilables: [
            prepareFunction(
                retType: HLTypeKind.i64,
                findex: 0,
                regs: regs,
                args: [HLTypeKind.obj],
                ops: ops)
        ])
        
        try compileAndLink(ctx: ctx, 0) {
            mappedMem in
            
            try mappedMem.jit(ctx: ctx, fix: 0) { (ep: (@convention(c) (UnsafeRawPointer) -> Int64)) in
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
    
    func _rf32__i32_f32(ops: [HLOpCode], regs: [HLTypeKind] = [.i32, .f32], strip: Bool = true, _ callback: @escaping ((Int32, Float32)->Float32)->()) throws {
        let ctx = try prepareContext(compilables: [
            prepareFunction(
                retType: HLTypeKind.f32,
                findex: 0,
                regs: regs,
                args: [HLTypeKind.i32, HLTypeKind.f32],
                ops: ops)
        ])
        
        try compileAndLink(ctx: ctx, 0, strip: strip) {
            mappedMem in
            
            try mappedMem.jit(ctx: ctx, fix: 0) { (ep: (@convention(c) (Int32, Float32) -> Float32)) in
                callback(ep)
            }
        }
    }
    
    func _ri32__i32_f32(ops: [HLOpCode], regs: [HLTypeKind] = [.i32, .f32], strip: Bool = true, _ callback: @escaping ((Int32, Float32)->Int32)->()) throws {
        let ctx = try prepareContext(compilables: [
            prepareFunction(
                retType: HLTypeKind.f32,
                findex: 0,
                regs: regs,
                args: [HLTypeKind.i32, HLTypeKind.f32],
                ops: ops)
        ])
        
        try compileAndLink(ctx: ctx, 0, strip: strip) {
            mappedMem in
            
            try mappedMem.jit(ctx: ctx, fix: 0) { (ep: (@convention(c) (Int32, Float32) -> Int32)) in
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
                args: [HLTypeKind.u8, HLTypeKind.u8],
                ops: ops)
        ])
        
        try compileAndLink(ctx: ctx, 0) {
            mappedMem in
            
            try mappedMem.jit(ctx: ctx, fix: 0) { (ep: (@convention(c) (Int8, Int8) -> Int8)) in
                callback(ep)
            }
        }
    }
    
    func _rf32__u8_u8(ops: [HLOpCode], regs: [HLTypeKind] = [.u8, .u8, .f32], strip: Bool = true, _ callback: @escaping ((Int8, Int8)->Float32)->()) throws {
        let ctx = try prepareContext(compilables: [
            prepareFunction(
                retType: HLTypeKind.f32,
                findex: 0,
                regs: regs,
                args: [HLTypeKind.u8, HLTypeKind.u8],
                ops: ops)
        ])
        
        try compileAndLink(ctx: ctx, 0, strip: strip) {
            mappedMem in
            
            try mappedMem.jit(ctx: ctx, fix: 0) { (ep: (@convention(c) (Int8, Int8) -> Float32)) in
                callback(ep)
            }
        }
    }
    
    func _rf32__f32_f32(ops: [HLOpCode], regs: [HLTypeKind] = [.f32, .f32, .f32], strip: Bool = true, _ callback: @escaping ((Float32, Float32)->Float32)->()) throws {
        let ctx = try prepareContext(compilables: [
            prepareFunction(
                retType: HLTypeKind.f32,
                findex: 0,
                regs: regs,
                args: [HLTypeKind.f32, HLTypeKind.f32],
                ops: ops)
        ])
        
        try compileAndLink(ctx: ctx, 0, strip: strip) {
            mappedMem in
            
            try mappedMem.jit(ctx: ctx, fix: 0) { (ep: (@convention(c) (Float32, Float32) -> Float32)) in
                callback(ep)
            }
        }
    }
    
    func _rf32__f32(ops: [HLOpCode], regs: [HLTypeKind] = [.f32], strip: Bool = true, _ callback: @escaping ((Float32)->Float32)->()) throws {
        let ctx = try prepareContext(compilables: [
            prepareFunction(
                retType: HLTypeKind.f32,
                findex: 0,
                regs: regs,
                args: [HLTypeKind.f32],
                ops: ops)
        ])
        
        try compileAndLink(ctx: ctx, 0, strip: strip) {
            mappedMem in
            
            try mappedMem.jit(ctx: ctx, fix: 0) { (ep: (@convention(c) (Float32) -> Float32)) in
                callback(ep)
            }
        }
    }
    
    func _rf32__f64(ops: [HLOpCode], regs: [HLTypeKind] = [.f64, .f32], strip: Bool = true, _ callback: @escaping ((Float64)->Float32)->()) throws {
        let ctx = try prepareContext(compilables: [
            prepareFunction(
                retType: HLTypeKind.f32,
                findex: 0,
                regs: regs,
                args: [HLTypeKind.f64, HLTypeKind.f32],
                ops: ops)
        ])
        
        try compileAndLink(ctx: ctx, 0, strip: strip) {
            mappedMem in
            
            try mappedMem.jit(ctx: ctx, fix: 0) { (ep: (@convention(c) (Float64) -> Float32)) in
                callback(ep)
            }
        }
    }
    
    func _rf64__f32(ops: [HLOpCode], regs: [HLTypeKind] = [.f32, .f64], strip: Bool = true, _ callback: @escaping ((Float32)->Float64)->()) throws {
        let ctx = try prepareContext(compilables: [
            prepareFunction(
                retType: HLTypeKind.f64,
                findex: 0,
                regs: regs,
                args: [HLTypeKind.f32, HLTypeKind.f64],
                ops: ops)
        ])
        
        try compileAndLink(ctx: ctx, 0, strip: strip) {
            mappedMem in
            
            try mappedMem.jit(ctx: ctx, fix: 0) { (ep: (@convention(c) (Float32) -> Float64)) in
                callback(ep)
            }
        }
    }
    
    func _rf64__f64_f64(ops: [HLOpCode], regs: [HLTypeKind] = [.f64, .f64, .f64], strip: Bool = true, _ callback: @escaping ((Float64, Float64)->Float64)->()) throws {
        let ctx = try prepareContext(compilables: [
            prepareFunction(
                retType: HLTypeKind.f64,
                findex: 0,
                regs: regs,
                args: [HLTypeKind.f64, HLTypeKind.f64],
                ops: ops)
        ])
        
        try compileAndLink(ctx: ctx, 0, strip: strip) {
            mappedMem in
            
            try mappedMem.jit(ctx: ctx, fix: 0) { (ep: (@convention(c) (Float64, Float64) -> Float64)) in
                callback(ep)
            }
        }
    }
    
    func _rf32__f32_i32(ops: [HLOpCode], regs: [HLTypeKind] = [.f32, .i32, .f32], strip: Bool = true, _ callback: @escaping ((Float32, Int32)->Float32)->()) throws {
        let ctx = try prepareContext(compilables: [
            prepareFunction(
                retType: HLTypeKind.f32,
                findex: 0,
                regs: regs,
                args: [HLTypeKind.f32, HLTypeKind.i32],
                ops: ops)
        ])
        
        try compileAndLink(ctx: ctx, 0, strip: strip) {
            mappedMem in
            
            try mappedMem.jit(ctx: ctx, fix: 0) { (ep: (@convention(c) (Float32, Int32) -> Float32)) in
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
    
    func testCompile_OString() throws {
        
        try _rbytes(ops: [
            .OString(dst: 0, ptr: 1),
            .ORet(ret: 0)
        ], strings: ["First", "Second", "Third"]) {
            entrypoint in
            
            guard let opaqueResult = entrypoint() else { return XCTFail("No result returned") }
            let result: UnsafePointer<CChar16> = .init(OpaquePointer(opaqueResult))
            
            XCTAssertEqual(result.stringValue, "Second")
        }
    }
    
    /// Test OBytes below v5 (where it points to the utf8 string encodings)
    func testCompile_OBytes_v4() throws {
        
        try _rbytes(ops: [
            .OBytes(dst: 0, ptr: 1),
            .ORet(ret: 0)
        ], strings: ["First", "Second", "Third"], v: 4) {
            entrypoint in
            
            guard let opaqueResult = entrypoint() else { return XCTFail("No result returned") }
            let result: UnsafePointer<CChar> = .init(OpaquePointer(opaqueResult))
            
            XCTAssertEqual(String.wrapUtf8(from: result), "Second")
        }
    }
    
    /// Test OBytes from v5 onward (where it points to a dedicated bytes resource)
    func testCompile_OBytes_v5() throws {
        
        let bytes = [Array("First\0".utf8), Array("Second\0".utf8), Array("Third\0".utf8)]
        
        try _rbytes(ops: [
            .OBytes(dst: 0, ptr: 1),
            .ORet(ret: 0)
        ], bytes: bytes, v: 5) {
            entrypoint in
            
            guard let opaqueResult = entrypoint() else { return XCTFail("No result returned") }
            let result: UnsafePointer<CChar> = .init(OpaquePointer(opaqueResult))
            
            XCTAssertEqual(String.wrapUtf8(from: result), "Second")
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
            
            let callable = try ctx.getCallable(findex: 0)
            let entrypoint = unsafeBitCast(callable!.address.value, to: _JitFunc.self)
            
            let res: Int32 = entrypoint(1, 2, 6)
            
            XCTAssertEqual(0b111, res)
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
            
            let callable = try ctx.getCallable(findex: 0)
            let entrypoint = unsafeBitCast(callable!.address.value, to: _JitFunc.self)
            
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
    
    func testCompile__ODecr() throws {
        try _ru8__u8(ops: [
            .ODecr(dst: 0),
            .ORet(ret: 0),
        ]) { entrypoint in
            XCTAssertEqual(3, entrypoint(4))
        }
        try _ri32__i32(ops: [
            .ODecr(dst: 0),
            .ORet(ret: 0),
        ]) { entrypoint in
            XCTAssertEqual(3, entrypoint(4))
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
        
        try _rf32__f32(ops: [
            .OMov(dst: 1, src: 0),
            .ORet(ret: 1),
        ], regs: [.f32, .f32]) {
            entrypoint in
            
            XCTAssertEqual(123.456, entrypoint(123.456))
        }
        
        try _rf32__f64(ops: [
            .OMov(dst: 1, src: 0),
            .ORet(ret: 1),
        ], regs: [.f64, .f32]) {
            entrypoint in
            
            XCTAssertEqual(456.789, entrypoint(456.789))
        }
        
        try _rf64__f32(ops: [
            .OMov(dst: 1, src: 0),
            .ORet(ret: 1),
        ], regs: [.f32, .f64]) {
            entrypoint in
            
            XCTAssertEqual(456.789, entrypoint(456.789))
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
    
    func testCompile__OCall__returnFP32() throws {
        let f1: (@convention(c) (Float32) -> Float32) = { $0 }
        let f1p = unsafeBitCast(f1, to: UnsafeMutableRawPointer.self)
        let f2: (@convention(c) (Float32, Float32) -> Float32) = { a, _ in a }
        let f2p = unsafeBitCast(f2, to: UnsafeMutableRawPointer.self)
        let f3: (@convention(c) (Float32, Float32, Float32) -> Float32) = { a, _, _ in a }
        let f3p = unsafeBitCast(f3, to: UnsafeMutableRawPointer.self)
        let f4: (@convention(c) (Float32, Float32, Float32, Float32) -> Float32) = { a, _, _, _ in a }
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
                        retType: HLTypeKind.f32,
                        findex: 0,
                        regs: [HLTypeKind.f32, HLTypeKind.f32, HLTypeKind.f32, HLTypeKind.f32, HLTypeKind.u8],
                        args: [HLTypeKind.f32],
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
                            argsProvider: [HLTypeKind.f32],
                            retProvider: HLTypeKind.f32),
                        address: f1p
                    ),
                    TestNative2(
                        findex: 2,
                        libProvider: "?",
                        nameProvider: "f2",
                        typeProvider: Test_HLTypeFun(
                            argsProvider: [HLTypeKind.f32, HLTypeKind.f32],
                            retProvider: HLTypeKind.f32),
                        address: f2p
                    ),
                    TestNative2(
                        findex: 3,
                        libProvider: "?",
                        nameProvider: "f3",
                        typeProvider: Test_HLTypeFun(
                            argsProvider: [HLTypeKind.f32, HLTypeKind.f32, HLTypeKind.f32],
                            retProvider: HLTypeKind.f32),
                        address: f3p
                    ),
                    TestNative2(
                        findex: 4,
                        libProvider: "?",
                        nameProvider: "f4",
                        typeProvider: Test_HLTypeFun(
                            argsProvider: [HLTypeKind.f32, HLTypeKind.f32, HLTypeKind.f32, HLTypeKind.f32],
                            retProvider: HLTypeKind.f32),
                        address: f4p
                    )
                ]
            )
            
            try compileAndLink(ctx: ctx, 0) {
                mappedMem in
                
                XCTAssertEqual(
                    try mappedMem.calljit_f32(ctx: ctx, fix: 0, f32_0: 123.456),
                    123.456,
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
                XCTAssertEqual(22, entrypoint(&x, 2))
                XCTAssertEqual(66, entrypoint(&x, 10))
            }
        }
    }
    
    func testCompile__OSetI8() throws {
        let ctx = try prepareContext(
            compilables: [
                prepareFunction(
                    retType: HLTypeKind.void,
                    findex: 0,
                    regs: [HLTypeKind.bytes, HLTypeKind.i32, HLTypeKind.i32, HLTypeKind.void],
                    args: [HLTypeKind.bytes, HLTypeKind.i32, HLTypeKind.i32],
                    ops: [
                        .OSetI8(bytes: 0, index: 1, src: 2),
                        .ORet(ret: 3)
                    ]
                )
            ]
        )
        
        try compileAndLink(ctx: ctx, 0) {
            mappedMem in
            
            try mappedMem.jit(ctx: ctx, fix: 0) { (entrypoint: (@convention(c) (UnsafeRawPointer, Int32, Int32) -> ())) in
                var x: [UInt8] = [11, 22, 33, 44, 55, 66, 77, 88, 99]
                
                entrypoint(&x, 1, 79)
                entrypoint(&x, 3, 97)
                XCTAssertEqual(x, [11, 79, 33, 97, 55, 66, 77, 88, 99])
            }
        }
    }
    
    func testCompile__OSetI16() throws {
        let ctx = try prepareContext(
            compilables: [
                prepareFunction(
                    retType: HLTypeKind.void,
                    findex: 0,
                    regs: [HLTypeKind.bytes, HLTypeKind.i32, HLTypeKind.i32, HLTypeKind.void],
                    args: [HLTypeKind.bytes, HLTypeKind.i32, HLTypeKind.i32],
                    ops: [
                        .OSetI16(bytes: 0, index: 1, src: 2),
                        .ORet(ret: 3)
                    ]
                )
            ]
        )
        
        try compileAndLink(ctx: ctx, 0) {
            mappedMem in
            
            try mappedMem.jit(ctx: ctx, fix: 0) { (entrypoint: (@convention(c) (UnsafeRawPointer, Int32, Int32) -> ())) in
                var x: [UInt16] = [11, 22, 33, 44, 55, 66, 77, 88, 99]
                
                entrypoint(&x, 2, 79)
                entrypoint(&x, 6, 97)
                XCTAssertEqual(x, [11, 79, 33, 97, 55, 66, 77, 88, 99])
            }
        }
    }
    
    func testCompile__OGetTID() throws {
        
        let ctx = try prepareContext(compilables: [
            prepareFunction(
                retType: HLTypeKind.i64,
                findex: 0,
                regs: [HLTypeKind.type, HLTypeKind.i64],
                args: [HLTypeKind.type],
                ops: [
                    .OGetTID(dst: 1, src: 0),
                    .ORet(ret: 1)
                ])
        ])
        
        try compileAndLink(ctx: ctx, 0) {
            mappedMem in
            
            for typeIx in 0..<ctx.ntypes {
                let typeP = try ctx.getType(Int(typeIx))
                
                try mappedMem.jit(ctx: ctx, fix: 0) { (entrypoint: (@convention(c) (UnsafeRawPointer) -> (Int64))) in
                    let res = entrypoint(typeP.ccompatAddress)
                    print("Expected type \(typeP.kind) => GetTID returns \(res)")
                    
                    XCTAssertEqual(
                        Int64(typeP.kind.rawValue),
                        res,
                        "Incorrect kind returned for type \(typeP.kind))"
                    )
                }
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
            
            let callable = try ctx.getCallable(findex: 0)
            let entrypoint = unsafeBitCast(callable!.address.value, to: _JitFunc.self)
            
            let res: Int32 = entrypoint(100, 156)
            
            XCTAssertEqual(256, res)
        }
    }
    
    func testCompile_OFloat() throws {
        try _rf64(ops: [
            .OFloat(dst: 0, ptr: 1),
            .ORet(ret: 0)
        ], floats: [0.1, 0.2, 0.3]) { function in
            XCTAssertEqual(function(), 0.2)
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
            [
                0xff, 0xc3, 0x04, 0xd1, // sub sp, sp, #304
                0xef, 0x43, 0x00, 0xa9, // stp x15, x16, [sp, #0]
                0xf1, 0x4b, 0x01, 0xa9, // stp x17, x18, [sp, #16]
                0xf3, 0x53, 0x02, 0xa9, // stp x19, x20, [sp, #32]
                0xf5, 0x5b, 0x03, 0xa9, // stp x21, x22, [sp, #48]
                0xf7, 0x63, 0x04, 0xa9, // stp x23, x24, [sp, #64]
                0xf9, 0x6b, 0x05, 0xa9, // stp x25, x26, [sp, #80]
                0xfb, 0x73, 0x06, 0xa9, // stp x27, x28, [sp, #96]
                0xfd, 0x7b, 0x07, 0xa9, // stp x29, x30, [sp, #112]
                0xe9, 0x2b, 0x08, 0x6d, // stp d9, d10, [sp, #128]
                0xeb, 0x33, 0x09, 0x6d, // stp d11, d12, [sp, #144]
                0xed, 0x3b, 0x0a, 0x6d, // stp d13, d14, [sp, #160]
                0xef, 0x43, 0x0b, 0x6d, // stp d15, d16, [sp, #176]
                0xf1, 0x4b, 0x0c, 0x6d, // stp d17, d18, [sp, #192]
                0xf3, 0x53, 0x0d, 0x6d, // stp d19, d20, [sp, #208]
                0xf5, 0x5b, 0x0e, 0x6d, // stp d21, d22, [sp, #224]
                0xf7, 0x63, 0x0f, 0x6d, // stp d23, d24, [sp, #240]
                0xf9, 0x6b, 0x10, 0x6d, // stp d25, d26, [sp, #256]
                0xfb, 0x73, 0x11, 0x6d, // stp d27, d28, [sp, #272]
                0xfd, 0x7b, 0x12, 0x6d, // stp d29, d30, [sp, #288]
                0xfd, 0x03, 0x00, 0x91, // movr x29, sp
            ]
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
            [
                0xef, 0x43, 0x40, 0xa9, // ldp x15, x16, [sp, #0]
                0xf1, 0x4b, 0x41, 0xa9, // ldp x17, x18, [sp, #16]
                0xf3, 0x53, 0x42, 0xa9, // ldp x19, x20, [sp, #32]
                0xf5, 0x5b, 0x43, 0xa9, // ldp x21, x22, [sp, #48]
                0xf7, 0x63, 0x44, 0xa9, // ldp x23, x24, [sp, #64]
                0xf9, 0x6b, 0x45, 0xa9, // ldp x25, x26, [sp, #80]
                0xfb, 0x73, 0x46, 0xa9, // ldp x27, x28, [sp, #96]
                0xfd, 0x7b, 0x47, 0xa9, // ldp x29, x30, [sp, #112]
                0xe9, 0x2b, 0x48, 0x6d, // ldp d9, d10, [sp, #128]
                0xeb, 0x33, 0x49, 0x6d, // ldp d11, d12, [sp, #144]
                0xed, 0x3b, 0x4a, 0x6d, // ldp d13, d14, [sp, #160]
                0xef, 0x43, 0x4b, 0x6d, // ldp d15, d16, [sp, #176]
                0xf1, 0x4b, 0x4c, 0x6d, // ldp d17, d18, [sp, #192]
                0xf3, 0x53, 0x4d, 0x6d, // ldp d19, d20, [sp, #208]
                0xf5, 0x5b, 0x4e, 0x6d, // ldp d21, d22, [sp, #224]
                0xf7, 0x63, 0x4f, 0x6d, // ldp d23, d24, [sp, #240]
                0xf9, 0x6b, 0x50, 0x6d, // ldp d25, d26, [sp, #256]
                0xfb, 0x73, 0x51, 0x6d, // ldp d27, d28, [sp, #272]
                0xfd, 0x7b, 0x52, 0x6d, // ldp d29, d30, [sp, #288]
                0xff, 0xc3, 0x04, 0x91, // add sp, sp, #304
            ]
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
//        mem1.hexPrint()
        XCTAssertEqual(
            [
                0xff, 0x43, 0x00, 0xd1, // sub sp, sp, #16
                0x0f, 0x00, 0x80, 0xd2, // .mov x15, #0\nstr w0, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xa0, 0xf2, // ... .mov x15, #0\nstr w0, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xc0, 0xf2, // ... .mov x15, #0\nstr w0, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xe0, 0xf2, // ... .mov x15, #0\nstr w0, [sp, x15, sxtx #0]
                0xe0, 0xeb, 0x2f, 0xb8, // ... .mov x15, #0\nstr w0, [sp, x15, sxtx #0]
            ],
            try BufferMapper(ctx: ctx, buffer: mem1).emitMachineCode()
        )
        
        // 16 byte requirement should not round to 32
        let mem2 = CpuOpBuffer()
        try sut.appendStackInit(_4_need16, args: _4_need16, builder: mem2, prologueSize: 0)
//        mem2.hexPrint()
        XCTAssertEqual(
            [
                // Reserving 16 bytes for entire stack
                0xff, 0x43, 0x00, 0xd1, // sub sp, sp, #16
                0x0f, 0x00, 0x80, 0xd2, // .mov x15, #0\nstr w0, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xa0, 0xf2, // ... .mov x15, #0\nstr w0, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xc0, 0xf2, // ... .mov x15, #0\nstr w0, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xe0, 0xf2, // ... .mov x15, #0\nstr w0, [sp, x15, sxtx #0]
                0xe0, 0xeb, 0x2f, 0xb8, // ... .mov x15, #0\nstr w0, [sp, x15, sxtx #0]
                0x8f, 0x00, 0x80, 0xd2, // .mov x15, #4\nstr w1, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xa0, 0xf2, // ... .mov x15, #4\nstr w1, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xc0, 0xf2, // ... .mov x15, #4\nstr w1, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xe0, 0xf2, // ... .mov x15, #4\nstr w1, [sp, x15, sxtx #0]
                0xe1, 0xeb, 0x2f, 0xb8, // ... .mov x15, #4\nstr w1, [sp, x15, sxtx #0]
                0x0f, 0x01, 0x80, 0xd2, // .mov x15, #8\nstr w2, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xa0, 0xf2, // ... .mov x15, #8\nstr w2, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xc0, 0xf2, // ... .mov x15, #8\nstr w2, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xe0, 0xf2, // ... .mov x15, #8\nstr w2, [sp, x15, sxtx #0]
                0xe2, 0xeb, 0x2f, 0xb8, // ... .mov x15, #8\nstr w2, [sp, x15, sxtx #0]
                0x8f, 0x01, 0x80, 0xd2, // .mov x15, #12\nstr w3, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xa0, 0xf2, // ... .mov x15, #12\nstr w3, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xc0, 0xf2, // ... .mov x15, #12\nstr w3, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xe0, 0xf2, // ... .mov x15, #12\nstr w3, [sp, x15, sxtx #0]
                0xe3, 0xeb, 0x2f, 0xb8, // ... .mov x15, #12\nstr w3, [sp, x15, sxtx #0]
            ],
            try BufferMapper(ctx: ctx, buffer: mem2).emitMachineCode()
        )
        // 20 byte requirement should round to 32
        let mem3 = CpuOpBuffer()
        try sut.appendStackInit(_5_need32, args: _5_need32, builder: mem3, prologueSize: 0)
//        mem3.hexPrint()
        XCTAssertEqual(
            [
                // Reserving 32 bytes for entire stack
                0xff, 0x83, 0x00, 0xd1, // sub sp, sp, #32
                0x0f, 0x00, 0x80, 0xd2, // .mov x15, #0\nstr w0, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xa0, 0xf2, // ... .mov x15, #0\nstr w0, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xc0, 0xf2, // ... .mov x15, #0\nstr w0, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xe0, 0xf2, // ... .mov x15, #0\nstr w0, [sp, x15, sxtx #0]
                0xe0, 0xeb, 0x2f, 0xb8, // ... .mov x15, #0\nstr w0, [sp, x15, sxtx #0]
                0x8f, 0x00, 0x80, 0xd2, // .mov x15, #4\nstr w1, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xa0, 0xf2, // ... .mov x15, #4\nstr w1, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xc0, 0xf2, // ... .mov x15, #4\nstr w1, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xe0, 0xf2, // ... .mov x15, #4\nstr w1, [sp, x15, sxtx #0]
                0xe1, 0xeb, 0x2f, 0xb8, // ... .mov x15, #4\nstr w1, [sp, x15, sxtx #0]
                0x0f, 0x01, 0x80, 0xd2, // .mov x15, #8\nstr w2, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xa0, 0xf2, // ... .mov x15, #8\nstr w2, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xc0, 0xf2, // ... .mov x15, #8\nstr w2, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xe0, 0xf2, // ... .mov x15, #8\nstr w2, [sp, x15, sxtx #0]
                0xe2, 0xeb, 0x2f, 0xb8, // ... .mov x15, #8\nstr w2, [sp, x15, sxtx #0]
                0x8f, 0x01, 0x80, 0xd2, // .mov x15, #12\nstr w3, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xa0, 0xf2, // ... .mov x15, #12\nstr w3, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xc0, 0xf2, // ... .mov x15, #12\nstr w3, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xe0, 0xf2, // ... .mov x15, #12\nstr w3, [sp, x15, sxtx #0]
                0xe3, 0xeb, 0x2f, 0xb8, // ... .mov x15, #12\nstr w3, [sp, x15, sxtx #0]
                0x0f, 0x02, 0x80, 0xd2, // .mov x15, #16\nstr w4, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xa0, 0xf2, // ... .mov x15, #16\nstr w4, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xc0, 0xf2, // ... .mov x15, #16\nstr w4, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xe0, 0xf2, // ... .mov x15, #16\nstr w4, [sp, x15, sxtx #0]
                0xe4, 0xeb, 0x2f, 0xb8, // ... .mov x15, #16\nstr w4, [sp, x15, sxtx #0]
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
                // Reserving 16 bytes for entire stack
                0xff, 0x43, 0x00, 0xd1, // sub sp, sp, #16
                0x0f, 0x00, 0x80, 0xd2, // .mov x15, #0\nstr w0, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xa0, 0xf2, // ... .mov x15, #0\nstr w0, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xc0, 0xf2, // ... .mov x15, #0\nstr w0, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xe0, 0xf2, // ... .mov x15, #0\nstr w0, [sp, x15, sxtx #0]
                0xe0, 0xeb, 0x2f, 0xb8, // ... .mov x15, #0\nstr w0, [sp, x15, sxtx #0]
                0x8f, 0x00, 0x80, 0xd2, // .mov x15, #4\nstr x1, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xa0, 0xf2, // ... .mov x15, #4\nstr x1, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xc0, 0xf2, // ... .mov x15, #4\nstr x1, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xe0, 0xf2, // ... .mov x15, #4\nstr x1, [sp, x15, sxtx #0]
                0xe1, 0xeb, 0x2f, 0xf8, // ... .mov x15, #4\nstr x1, [sp, x15, sxtx #0]
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
                0x0f, 0x00, 0x80, 0xd2, // .mov x15, #0\nstr w0, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xa0, 0xf2, // ... .mov x15, #0\nstr w0, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xc0, 0xf2, // ... .mov x15, #0\nstr w0, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xe0, 0xf2, // ... .mov x15, #0\nstr w0, [sp, x15, sxtx #0]
                0xe0, 0xeb, 0x2f, 0xb8, // ... .mov x15, #0\nstr w0, [sp, x15, sxtx #0]
                0x8f, 0x00, 0x80, 0xd2, // .mov x15, #4\nstr w1, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xa0, 0xf2, // ... .mov x15, #4\nstr w1, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xc0, 0xf2, // ... .mov x15, #4\nstr w1, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xe0, 0xf2, // ... .mov x15, #4\nstr w1, [sp, x15, sxtx #0]
                0xe1, 0xeb, 0x2f, 0xb8, // ... .mov x15, #4\nstr w1, [sp, x15, sxtx #0]
                0x0f, 0x01, 0x80, 0xd2, // .mov x15, #8\nstr w2, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xa0, 0xf2, // ... .mov x15, #8\nstr w2, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xc0, 0xf2, // ... .mov x15, #8\nstr w2, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xe0, 0xf2, // ... .mov x15, #8\nstr w2, [sp, x15, sxtx #0]
                0xe2, 0xeb, 0x2f, 0xb8, // ... .mov x15, #8\nstr w2, [sp, x15, sxtx #0]
                0x8f, 0x01, 0x80, 0xd2, // .mov x15, #12\nstr w3, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xa0, 0xf2, // ... .mov x15, #12\nstr w3, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xc0, 0xf2, // ... .mov x15, #12\nstr w3, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xe0, 0xf2, // ... .mov x15, #12\nstr w3, [sp, x15, sxtx #0]
                0xe3, 0xeb, 0x2f, 0xb8, // ... .mov x15, #12\nstr w3, [sp, x15, sxtx #0]
                0x0f, 0x02, 0x80, 0xd2, // .mov x15, #16\nstr w4, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xa0, 0xf2, // ... .mov x15, #16\nstr w4, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xc0, 0xf2, // ... .mov x15, #16\nstr w4, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xe0, 0xf2, // ... .mov x15, #16\nstr w4, [sp, x15, sxtx #0]
                0xe4, 0xeb, 0x2f, 0xb8, // ... .mov x15, #16\nstr w4, [sp, x15, sxtx #0]
                0x8f, 0x02, 0x80, 0xd2, // .mov x15, #20\nstr w5, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xa0, 0xf2, // ... .mov x15, #20\nstr w5, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xc0, 0xf2, // ... .mov x15, #20\nstr w5, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xe0, 0xf2, // ... .mov x15, #20\nstr w5, [sp, x15, sxtx #0]
                0xe5, 0xeb, 0x2f, 0xb8, // ... .mov x15, #20\nstr w5, [sp, x15, sxtx #0]
                0x0f, 0x03, 0x80, 0xd2, // .mov x15, #24\nstr w6, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xa0, 0xf2, // ... .mov x15, #24\nstr w6, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xc0, 0xf2, // ... .mov x15, #24\nstr w6, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xe0, 0xf2, // ... .mov x15, #24\nstr w6, [sp, x15, sxtx #0]
                0xe6, 0xeb, 0x2f, 0xb8, // ... .mov x15, #24\nstr w6, [sp, x15, sxtx #0]
                0x8f, 0x03, 0x80, 0xd2, // .mov x15, #28\nstr w7, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xa0, 0xf2, // ... .mov x15, #28\nstr w7, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xc0, 0xf2, // ... .mov x15, #28\nstr w7, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xe0, 0xf2, // ... .mov x15, #28\nstr w7, [sp, x15, sxtx #0]
                0xe7, 0xeb, 0x2f, 0xb8, // ... .mov x15, #28\nstr w7, [sp, x15, sxtx #0]
                0x01, 0x06, 0x80, 0xd2, // .mov x1, #48\nldr w1, [sp, x1, sxtx #0]
                0x01, 0x00, 0xa0, 0xf2, // ... .mov x1, #48\nldr w1, [sp, x1, sxtx #0]
                0x01, 0x00, 0xc0, 0xf2, // ... .mov x1, #48\nldr w1, [sp, x1, sxtx #0]
                0x01, 0x00, 0xe0, 0xf2, // ... .mov x1, #48\nldr w1, [sp, x1, sxtx #0]
                0xe1, 0xeb, 0x61, 0xb8, // ... .mov x1, #48\nldr w1, [sp, x1, sxtx #0]
                0x0f, 0x04, 0x80, 0xd2, // .mov x15, #32\nstr w1, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xa0, 0xf2, // ... .mov x15, #32\nstr w1, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xc0, 0xf2, // ... .mov x15, #32\nstr w1, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xe0, 0xf2, // ... .mov x15, #32\nstr w1, [sp, x15, sxtx #0]
                0xe1, 0xeb, 0x2f, 0xb8, // ... .mov x15, #32\nstr w1, [sp, x15, sxtx #0]
                0x81, 0x06, 0x80, 0xd2, // .mov x1, #52\nldr w1, [sp, x1, sxtx #0]
                0x01, 0x00, 0xa0, 0xf2, // ... .mov x1, #52\nldr w1, [sp, x1, sxtx #0]
                0x01, 0x00, 0xc0, 0xf2, // ... .mov x1, #52\nldr w1, [sp, x1, sxtx #0]
                0x01, 0x00, 0xe0, 0xf2, // ... .mov x1, #52\nldr w1, [sp, x1, sxtx #0]
                0xe1, 0xeb, 0x61, 0xb8, // ... .mov x1, #52\nldr w1, [sp, x1, sxtx #0]
                0x8f, 0x04, 0x80, 0xd2, // .mov x15, #36\nstr w1, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xa0, 0xf2, // ... .mov x15, #36\nstr w1, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xc0, 0xf2, // ... .mov x15, #36\nstr w1, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xe0, 0xf2, // ... .mov x15, #36\nstr w1, [sp, x15, sxtx #0]
                0xe1, 0xeb, 0x2f, 0xb8, // ... .mov x15, #36\nstr w1, [sp, x15, sxtx #0]
                0x01, 0x07, 0x80, 0xd2, // .mov x1, #56\nldr w1, [sp, x1, sxtx #0]
                0x01, 0x00, 0xa0, 0xf2, // ... .mov x1, #56\nldr w1, [sp, x1, sxtx #0]
                0x01, 0x00, 0xc0, 0xf2, // ... .mov x1, #56\nldr w1, [sp, x1, sxtx #0]
                0x01, 0x00, 0xe0, 0xf2, // ... .mov x1, #56\nldr w1, [sp, x1, sxtx #0]
                0xe1, 0xeb, 0x61, 0xb8, // ... .mov x1, #56\nldr w1, [sp, x1, sxtx #0]
                0x0f, 0x05, 0x80, 0xd2, // .mov x15, #40\nstr w1, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xa0, 0xf2, // ... .mov x15, #40\nstr w1, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xc0, 0xf2, // ... .mov x15, #40\nstr w1, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xe0, 0xf2, // ... .mov x15, #40\nstr w1, [sp, x15, sxtx #0]
                0xe1, 0xeb, 0x2f, 0xb8, // ... .mov x15, #40\nstr w1, [sp, x15, sxtx #0]
                0x81, 0x07, 0x80, 0xd2, // .mov x1, #60\nldr w1, [sp, x1, sxtx #0]
                0x01, 0x00, 0xa0, 0xf2, // ... .mov x1, #60\nldr w1, [sp, x1, sxtx #0]
                0x01, 0x00, 0xc0, 0xf2, // ... .mov x1, #60\nldr w1, [sp, x1, sxtx #0]
                0x01, 0x00, 0xe0, 0xf2, // ... .mov x1, #60\nldr w1, [sp, x1, sxtx #0]
                0xe1, 0xeb, 0x61, 0xb8, // ... .mov x1, #60\nldr w1, [sp, x1, sxtx #0]
                0x8f, 0x05, 0x80, 0xd2, // .mov x15, #44\nstr w1, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xa0, 0xf2, // ... .mov x15, #44\nstr w1, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xc0, 0xf2, // ... .mov x15, #44\nstr w1, [sp, x15, sxtx #0]
                0x0f, 0x00, 0xe0, 0xf2, // ... .mov x15, #44\nstr w1, [sp, x15, sxtx #0]
                0xe1, 0xeb, 0x2f, 0xb8, // ... .mov x15, #44\nstr w1, [sp, x15, sxtx #0]
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
                0xff, 0xc3, 0x03, 0xd1, // sub sp, sp, #240
                0xe0, 0x83, 0x00, 0xf8, // str x0, [sp, #8]
                0xf3, 0x03, 0x00, 0xf8, // str x19, [sp, #0]
                0xe1, 0x0b, 0x01, 0xa9, // stp x1, x2, [sp, #16]
                0xe3, 0x13, 0x02, 0xa9, // stp x3, x4, [sp, #32]
                0xe5, 0x1b, 0x03, 0xa9, // stp x5, x6, [sp, #48]
                0xe7, 0x23, 0x04, 0xa9, // stp x7, x8, [sp, #64]
                0xe9, 0x2b, 0x05, 0xa9, // stp x9, x10, [sp, #80]
                0xeb, 0x33, 0x06, 0xa9, // stp x11, x12, [sp, #96]
                0xed, 0x3b, 0x07, 0xa9, // stp x13, x14, [sp, #112]
                0xef, 0x43, 0x08, 0xa9, // stp x15, x16, [sp, #128]
                0xf1, 0x4b, 0x09, 0xa9, // stp x17, x18, [sp, #144]
                0xe0, 0x07, 0x0a, 0x6d, // stp d0, d1, [sp, #160]
                0xe2, 0x0f, 0x0b, 0x6d, // stp d2, d3, [sp, #176]
                0xe4, 0x17, 0x0c, 0x6d, // stp d4, d5, [sp, #192]
                0xe6, 0x1f, 0x0d, 0x6d, // stp d6, d7, [sp, #208]
                0xe8, 0x27, 0x0e, 0x6d, // stp d8, d9, [sp, #224]
                0x20, 0x00, 0x80, 0xd2, // movz x0, #1
                0xc1, 0x02, 0x00, 0x10, // adr x1, #88
                0xe2, 0x02, 0x80, 0xd2, // movz x2, #23
                0x90, 0x00, 0x80, 0xd2, // movz x16, #4
                0x01, 0x10, 0x00, 0xd4, // svc 0x0080
                0xe0, 0x07, 0x40, 0xf9, // ldr x0, [sp, #8]
                0xf3, 0x03, 0x40, 0xf9, // ldr x19, [sp, #0]
                0xe1, 0x0b, 0x41, 0xa9, // ldp x1, x2, [sp, #16]
                0xe3, 0x13, 0x42, 0xa9, // ldp x3, x4, [sp, #32]
                0xe5, 0x1b, 0x43, 0xa9, // ldp x5, x6, [sp, #48]
                0xe7, 0x23, 0x44, 0xa9, // ldp x7, x8, [sp, #64]
                0xe9, 0x2b, 0x45, 0xa9, // ldp x9, x10, [sp, #80]
                0xeb, 0x33, 0x46, 0xa9, // ldp x11, x12, [sp, #96]
                0xed, 0x3b, 0x47, 0xa9, // ldp x13, x14, [sp, #112]
                0xef, 0x43, 0x48, 0xa9, // ldp x15, x16, [sp, #128]
                0xf1, 0x4b, 0x49, 0xa9, // ldp x17, x18, [sp, #144]
                0xe0, 0x07, 0x4a, 0x6d, // ldp d0, d1, [sp, #160]
                0xe2, 0x0f, 0x4b, 0x6d, // ldp d2, d3, [sp, #176]
                0xe4, 0x17, 0x4c, 0x6d, // ldp d4, d5, [sp, #192]
                0xe6, 0x1f, 0x4d, 0x6d, // ldp d6, d7, [sp, #208]
                0xe8, 0x27, 0x4e, 0x6d, // ldp d8, d9, [sp, #224]
                0xff, 0xc3, 0x03, 0x91, // add sp, sp, #240
                0x07, 0x00, 0x00, 0x14, // b #28
                0x5b, 0x6a, 0x69, 0x74, // [jit
                0x64, 0x65, 0x62, 0x75, // debu
                0x67, 0x5d, 0x20, 0x48, // g].H
                0x65, 0x6c, 0x6c, 0x6f, // ello
                0x20, 0x57, 0x6f, 0x72, // .Wor
                0x6c, 0x64, 0x0a, // ld\n
                0x00, // .zero
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
    
    func testCompile__OToInt__f64_to_i32() throws {
        try _ri32__f64(ops: [
            .OToInt(dst: 1, src: 0),
            .ORet(ret: 1),
        ]) {
            entrypoint in
            
            XCTAssertEqual(
                2147483647,
                UInt32(bitPattern: entrypoint(21474839999))
            )
            XCTAssertEqual(
                2147483647,
                entrypoint(2147483648)
            )
        }
    }
    
    func testCompile__OMul() throws {
        // multiply integers
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

        // multiply integer with float, return as float
        try _rf32__i32_f32(ops: [
            .OMul(dst: 1, a: 0, b: 1),
            .ORet(ret: 1),
        ], strip: false) { entrypoint in

            XCTAssertEqual(4, entrypoint(1, 4))
            XCTAssertEqual(4, entrypoint(2, 2))
            XCTAssertEqual(36, entrypoint(4, 9))
            XCTAssertEqual(0, entrypoint(1, 0))
            XCTAssertEqual(-10, entrypoint(-5, 2))
        }
        
        // multiply integer with float, return as integer
        try _ri32__i32_f32(ops: [
            .OMul(dst: 0, a: 0, b: 1),
            .ORet(ret: 0),
        ], strip: false) { entrypoint in

            XCTAssertEqual(4, entrypoint(1, 4))
            XCTAssertEqual(4, entrypoint(2, 2))
            XCTAssertEqual(36, entrypoint(4, 9))
            XCTAssertEqual(0, entrypoint(1, 0))
            XCTAssertEqual(-10, entrypoint(-5, 2))
        }
    }
    
    /// Test OUDiv. Remember: Haxe division result is always a float.
    func testCompile__OUDiv() throws {
        try _rf32__u8_u8(ops: [
            .OUDiv(dst: 2, a: 0, b: 1),
            .ORet(ret: 2),
        ], strip: false) { entrypoint in
            
            XCTAssertEqual(entrypoint(4, 2), 2.0)
            XCTAssertEqual(entrypoint(-4, 2), 126.0)
        }
    }
    
    /// Test OSDiv. Remember: Haxe division result is always a float.
    func testCompile__OSDiv() throws {
        try _rf32__u8_u8(ops: [
            .OSDiv(dst: 2, a: 0, b: 1),
            .ORet(ret: 2),
        ], strip: false) { entrypoint in

            XCTAssertEqual(entrypoint(4, 2), 2.0)
            XCTAssertEqual(entrypoint(-4, 2), -2.0)
        }

        try _rf64__f64_f64(ops: [
            .OSDiv(dst: 2, a: 0, b: 1),
            .ORet(ret: 2),
        ], strip: false) { entrypoint in

            XCTAssertEqual(entrypoint(4.0, 2.0), 2.0)
            XCTAssertEqual(entrypoint(-4.0, 2.0), -2.0)
        }
        
        try _rf32__f32_f32(ops: [
            .OSDiv(dst: 2, a: 0, b: 1),
            .ORet(ret: 2),
        ], strip: false) { entrypoint in

            XCTAssertEqual(entrypoint(4.0, 2.0), 2.0)
            XCTAssertEqual(entrypoint(-4.0, 2.0), -2.0)
        }
        
        try _rf32__f32_i32(ops: [
            .OSDiv(dst: 2, a: 0, b: 1),
            .ORet(ret: 2),
        ], strip: false) { entrypoint in

            XCTAssertEqual(entrypoint(4.0, 2), 2.0)
            XCTAssertEqual(entrypoint(-4.0, 2), -2.0)
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
    
    /** Test a switch statement with a default case.
     
     static public function testSwitch(v: Int): Int {
     var v2 = v + 1;
     switch(v2) {
     case 0: return v*5;
     case 2: return v*0;
     case 4: return v*1;
     case 6: return v*2;
     case 7: return v*3;
     default: return v*4;
     };
     }
     */
    func testCompile__OSwitch__withDefault() throws {
        try _ri32__i32(ops: [
            .OInt(dst: 2, ptr: 0),          // 0: Int         reg2 = 1
            .OAdd(dst: 1, a: 0, b: 2),      // 1: Add         reg1 = reg0 + reg2
            .OSwitch(reg: 1,                // 2: Switch { reg: Reg(1), offsets: [3, 0, 6, 0, 9, 0, 10, 13], end: 16 }
                     offsets: [
                        3, 0, 6, 0, 9,
                        0, 10, 13], end: 16),
            .OInt(dst: 3, ptr: 1),          // 3: Int         reg3 = 4
            .OMul(dst: 2, a: 0, b: 3),      // 4: Mul         reg2 = reg0 * reg3
            .ORet(ret: 2),                  // 5: Ret         reg2
            .OInt(dst: 3, ptr: 2),          // 6: Int         reg3 = 5
            .OMul(dst: 2, a: 0, b: 3),      // 7: Mul         reg2 = reg0 * reg3
            .ORet(ret: 2),                  // 8: Ret         reg2
            .OInt(dst: 3, ptr: 3),          // 9: Int         reg3 = 0
            .OMul(dst: 2, a: 0, b: 3),      // 10: Mul         reg2 = reg0 * reg3
            .ORet(ret: 2),                  // 11: Ret         reg2
            .ORet(ret: 0),                  // 12: Ret         reg0
            .OInt(dst: 3, ptr: 4),          // 13: Int         reg3 = 2
            .OMul(dst: 2, a: 0, b: 3),      // 14: Mul         reg2 = reg0 * reg3
            .ORet(ret: 2),                  // 15: Ret         reg2
            .OInt(dst: 3, ptr: 5),          // 16: Int         reg3 = 3
            .OMul(dst: 2, a: 0, b: 3),      // 17: Mul         reg2 = reg0 * reg3
            .ORet(ret: 2)                   // 18: Ret         reg2
        ], regs: [.i32, .i32, .i32, .i32], ints: [1, 4, 5, 0, 2, 3]) {
            entrypoint in
            
            XCTAssertEqual(-5,      entrypoint(-1))     // case 0
            XCTAssertEqual(0,       entrypoint(1))      // case 2
            XCTAssertEqual(3,       entrypoint(3))      // case 4
            XCTAssertEqual(10,      entrypoint(5))      // case 6
            XCTAssertEqual(18,      entrypoint(6))      // case 7
            XCTAssertEqual(2048,    entrypoint(512))    // default
        }
    }
    
    /** Test a switch statement without a default case.
     
     static public function testSwitch(v: Int): Int {
     switch(v) {
     case 0: return v*0;
     case 2: return v*1;
     case 4: return v*2;
     case 6: return v*3;
     case 7: return v*4;
     };
     return -1;
     }
     */
    func testCompile__OSwitch__withoutDefault() throws {
        try _ri32__i32(ops: [
            .OSwitch(                       // 0: Switch { reg: Reg(0), offsets: [1, 0, 4, 0, 5, 0, 8, 11], end: 14 }
                reg: 0,
                offsets: [1, 0, 4, 0, 5, 0, 8, 11],
                end: 14),
            .OJAlways(offset: 13),          // 1: JAlways     jump to 15 (== offset 13)
            .OInt(dst: 2, ptr: 0),          // 2: Int         reg2 = 0
            .OMul(dst: 1, a: 0, b: 2),      // 3: Mul         reg1 = reg0 * reg2
            .ORet(ret: 1),                  // 4: Ret         reg1
            .ORet(ret: 0),                  // 5: Ret         reg0
            .OInt(dst: 2, ptr: 1),          // 6: Int         reg2 = 2
            .OMul(dst: 1, a: 0, b: 2),      // 7: Mul         reg1 = reg0 * reg2
            .ORet(ret: 1),                  // 8: Ret         reg1
            .OInt(dst: 2, ptr: 2),          // 9: Int         reg2 = 3
            .OMul(dst: 1, a: 0, b: 2),      // 10: Mul         reg1 = reg0 * reg2
            .ORet(ret: 1),                  // 11: Ret         reg1
            .OInt(dst: 2, ptr: 3),          // 12: Int         reg2 = 4
            .OMul(dst: 1, a: 0, b: 2),      // 13: Mul         reg1 = reg0 * reg2
            .ORet(ret: 1),                  // 14: Ret         reg1
            .OInt(dst: 1, ptr: 4),          // 15: Int         reg1 = -1
            .ORet(ret: 1)                   // 16: Ret         reg1
        ], regs: [.i32, .i32, .i32], ints: [0, 2, 3, 4, -1]) {
            entrypoint in
            
            XCTAssertEqual(0,   entrypoint(0))      // case 0
            XCTAssertEqual(2,   entrypoint(2))      // case 2
            XCTAssertEqual(8,   entrypoint(4))      // case 4
            XCTAssertEqual(18,  entrypoint(6))      // case 6
            XCTAssertEqual(28,  entrypoint(7))      // case 7
            XCTAssertEqual(-1,  entrypoint(123))    // no case
        }
    }
    
    /** Test OGetType
     
     static public function testGetType(something: Dynamic): Int {
     var varType = hl.Type.getDynamic(something);
     if (varType == hl.Type.get(5))
     return 1;
     return 0;
     }
     
     static public function testGetType_bool(): Int {
     return testGetType(true);
     }
     
     static public function testGetType_i32(): Int {
     return testGetType(123);
     }
     
     static public function testGetType_String(): Int {
     return testGetType("asd");
     }
     */
    func testCompile__OGetType() throws {
        let ctx = try prepareContext(
            compilables: [
                /*  fn testGetType (dynamic) -> (i32)
                 
                 reg0  dynamic
                 reg1  type
                 reg2  type
                 reg3  i32
                 
                 0: GetType { dst: Reg(1), src: Reg(0) }
                 1: Type        reg2 = i32
                 2: JNotEq      if reg1 != reg2 jump to 5
                 3: Int         reg3 = 1
                 4: Ret         reg3
                 5: Int         reg3 = 0
                 6: Ret         reg3
                 */
                prepareFunction(
                    retType: HLTypeKind.i32,
                    findex: 0,
                    regs: [HLTypeKind.dyn, HLTypeKind.type, HLTypeKind.type, HLTypeKind.i32],
                    args: [HLTypeKind.dyn],
                    ops: [
                        .OGetType(dst: 1, src: 0),
                        .OType(dst: 2, ty: 1),  // NOTE: TestJitModule guarantees i32 is type 1
                        .OJNotEq(a: 1, b: 2, offset: 2),
                        .OInt(dst: 3, ptr: 0),
                        .ORet(ret: 3),
                        .OInt(dst: 3, ptr: 1),
                        .ORet(ret: 3)
                    ]),
                /*  Same as above but it will compare against BOOL Type */
                prepareFunction(
                    retType: HLTypeKind.i32,
                    findex: 1,
                    regs: [HLTypeKind.dyn, HLTypeKind.type, HLTypeKind.type, HLTypeKind.i32],
                    args: [HLTypeKind.dyn],
                    ops: [
                        .OGetType(dst: 1, src: 0),
                        .OType(dst: 2, ty: 2),  // NOTE: TestJitModule guarantees bool is type 2
                        .OJNotEq(a: 1, b: 2, offset: 2),
                        .OInt(dst: 3, ptr: 0),
                        .ORet(ret: 3),
                        .OInt(dst: 3, ptr: 1),
                        .ORet(ret: 3)
                    ]),
                /*  fn testGetType_i32 () -> (i32)
                 
                 Pass an i32 to testGetType that expects i32
                 
                 reg0  i32
                 reg1  dynamic
                 
                 0: Int         reg0 = 123
                 1: ToDyn       reg1 = cast reg0
                 2: Call1       reg0 = testGetType(reg1)
                 3: Ret         reg0
                 */
                prepareFunction(
                    retType: HLTypeKind.i32,
                    findex: 2,
                    regs: [HLTypeKind.i32, HLTypeKind.dyn],
                    args: [],
                    ops: [
                        .OInt(dst: 0, ptr: 2),
                        .OToDyn(dst: 1, src: 0),
                        .OCall1(dst: 0, fun: 0, arg0: 1),
                        .ORet(ret: 0)
                    ]),
                /*  Same as above but pass an i32 to testGetType that expects bool
                 */
                prepareFunction(
                    retType: HLTypeKind.i32,
                    findex: 3,
                    regs: [HLTypeKind.i32, HLTypeKind.dyn],
                    args: [],
                    ops: [
                        .OInt(dst: 0, ptr: 2),
                        .OToDyn(dst: 1, src: 0),
                        .OCall1(dst: 0, fun: 1, arg0: 1),
                        .ORet(ret: 0)
                    ]),
                /*  fn testGetType_bool () -> (i32) (3 regs, 4 ops)
                 
                 Pass a bool to testGetType that expects i32
                 
                 reg0  i32
                 reg1  bool
                 reg2  dynamic
                 
                 0: Bool        reg1 = true
                 1: ToDyn       reg2 = cast reg1
                 2: Call1       reg0 = testGetType(reg2)
                 3: Ret         reg0
                 */
                prepareFunction(
                    retType: HLTypeKind.i32,
                    findex: 4,
                    regs: [HLTypeKind.i32, HLTypeKind.bool, HLTypeKind.dyn],
                    args: [],
                    ops: [
                        .OBool(dst: 1, value: 1),
                        .OToDyn(dst: 2, src: 1),
                        .OCall1(dst: 0, fun: 0, arg0: 2),
                        .ORet(ret: 0)
                    ]),
                /*  Same as above but pass a bool to testGetType that expects bool
                 */
                prepareFunction(
                    retType: HLTypeKind.i32,
                    findex: 5,
                    regs: [HLTypeKind.i32, HLTypeKind.bool, HLTypeKind.dyn],
                    args: [],
                    ops: [
                        .OBool(dst: 1, value: 1),
                        .OToDyn(dst: 2, src: 1),
                        .OCall1(dst: 0, fun: 1, arg0: 2),
                        .ORet(ret: 0)
                    ]),
            ], ints: [
                1, 0, 123
            ]
        )
        
        try compileAndLink(ctx: ctx,
                           /*expect i32*/0,
                           /*expect bool*/1,
                           /*i32->exp: i32*/2,
                           /*i32->exp: bool*/3,
                           /*bool->exp: i32*/4,
                           /*bool->exp: bool*/5) {
            mappedMem in
            
            try mappedMem.jit(ctx: ctx, fix: 2) { (entrypoint: (@convention(c) () -> (Int32))) in
                XCTAssertEqual(1, entrypoint()) // send i32 expect i32
            }
            try mappedMem.jit(ctx: ctx, fix: 3) { (entrypoint: (@convention(c) () -> (Int32))) in
                XCTAssertEqual(0, entrypoint()) // send i32 expect bool
            }
            try mappedMem.jit(ctx: ctx, fix: 4) { (entrypoint: (@convention(c) () -> (Int32))) in
                XCTAssertEqual(0, entrypoint()) // send bool expect i32
            }
            try mappedMem.jit(ctx: ctx, fix: 5) { (entrypoint: (@convention(c) () -> (Int32))) in
                XCTAssertEqual(1, entrypoint()) // send bool expect bool
            }
        }
    }
    
    func testCompile__OToSFloat() throws {
        try _rf64__i32(ops: [
            .OToSFloat(dst: 1, src: 0),
            .ORet(ret: 1)
        ], regs: [HLTypeKind.i32, HLTypeKind.f64]) {
            entrypoint in
            
            XCTAssertEqual(5,  entrypoint(5))
            //            XCTAssertEqual(5.0,  entrypoint(5))
            //            XCTAssertEqual(-5.0,  entrypoint(-5))
        }
    }
    
    func testCompile__OSetThis_OGetThis__inheritedFields() throws {
        let parentType = Test_HLTypeObj(
            fieldsProvider: [
                Test_HLObjField(nameProvider: "parentField1", typeProvider: HLTypeKind.i32),
                Test_HLObjField(nameProvider: "parentField2", typeProvider: HLTypeKind.u16),
            ],
            nameProvider: "parent")
        let childType = Test_HLTypeObj(
            fieldsProvider: [
                Test_HLObjField(nameProvider: "childField1", typeProvider: HLTypeKind.u8),
                Test_HLObjField(nameProvider: "childField2", typeProvider: HLTypeKind.i32),
            ],
            nameProvider: "child",
            superTypeProvider: parentType)
        
        let ctx = try prepareContext(compilables: [
            prepareFunction(
                retType: HLTypeKind.u8,
                findex: 0,
                regs: [childType, HLTypeKind.i32, HLTypeKind.u8],
                args: [childType],
                ops: [
                    // set first child field (u8) to 123
                    .OInt(dst: 1, ptr: 0),
                    .OSetThis(field: 2, src: 1),
                    .OGetThis(dst: 2, field: 2),
                    .ORet(ret: 2)
                ])
        ], ints: [123])
        
        // find type
        var childTypeCCompat: (any HLTypeProvider)? = nil
        for typeIx in (0..<ctx.ntypes) {
            let candidate = try ctx.getType(Int(typeIx))
            if candidate.isEquivalent(childType) {
                childTypeCCompat = candidate
                break
            }
        }
        XCTAssertNotNil(childTypeCCompat)
        
        // Field indexes can be padded and padding is detected
        // at compile time (e.g. see `hl_pad_struct` in hashlink).
        // Some hardcoded assumptions here.
        let unsafePtr: UnsafePointer<HLType_CCompat> = .init(OpaquePointer(childTypeCCompat!.ccompatAddress))
        let rt = unsafePtr.pointee.obj.pointee.getRt(unsafePtr)
        XCTAssertEqual(8, rt.pointee.fields_indexes[0])
        XCTAssertEqual(12, rt.pointee.fields_indexes[1])
        XCTAssertEqual(14, rt.pointee.fields_indexes[2])
        XCTAssertEqual(16, rt.pointee.fields_indexes[3])
        
        //
        let dyn = LibHl.hl_alloc_dynamic(unsafePtr)
        
        try compileAndLink(ctx: ctx, 0) {
            mappedMem in
            
            try mappedMem.jit(ctx: ctx, fix: 0) { (entrypoint: (@convention(c) (UnsafeRawPointer) -> (UInt8))) in
                let res = entrypoint(dyn)
                XCTAssertEqual(123, res)
            }
        }
    }
    
    /// Test creating a reference, and passing to a different function (which sets the reference)
    func testCompile__ORef() throws {
        let ref = Test_HLTypeRef(tparamProvider: HLTypeKind.i32)
        
        let ctx = try prepareContext(compilables: [
            /*
             fn testRef () -> (i32)
             
             reg0  u8       // to force non-0 offset for test coverage
             reg1  i32
             reg2  void
             reg3  ref<i32>
             
             0: Int         reg1 = 10
             1: Ref         reg3 = &reg0
             2: Call1       reg2 = testRefSet(reg2)
             3: Ret         reg1
             */
            prepareFunction(
                retType: HLTypeKind.u8,
                findex: 0,
                regs: [HLTypeKind.u8,
                       HLTypeKind.i32, HLTypeKind.void, ref],
                args: [HLTypeKind.u8],
                ops: [
                    .OInt(dst: 1, ptr: 0),
                    .ORef(dst: 3, src: 1),
                    .OCall1(dst: 2, fun: 1, arg0: 3),
                    .ORet(ret: 1)
                ]),
            /*
             fn testRefSet (ref<i32>) -> (void)
             reg0  ref<i32>
             reg1  i32
             reg2  void
             
             0: Int         reg1 = 2
             1: Setref { dst: Reg(0), value: Reg(1) }
             2: Ret         reg2
             */
            prepareFunction(
                retType: HLTypeKind.void,
                findex: 1,
                regs: [ref, HLTypeKind.i32, HLTypeKind.void],
                args: [ref],
                ops: [
                    .OInt(dst: 1, ptr: 1),
                    .OSetref(dst: 0, value: 1),
                    .ORet(ret: 2)
                ]),
        ], ints: [10, 2])
        
        typealias _JitFunc = (@convention(c) (UInt8) -> (Int32))
        
        try compileAndLink(ctx: ctx, 0, 1) {
            mappedMem in
            
            let callable = try ctx.getCallable(findex: 0)
            let entrypoint = unsafeBitCast(callable!.address.value, to: _JitFunc.self)
            
            let res = entrypoint(0 /*doesn't matter. Only here for padding*/ )
            XCTAssertEqual(2, res)
        }
    }
    
    /// Same as ORef test, except we don't return the original value, but we Unref the created reference (1 line difference in first function's return)
    func testCompile__OUnref() throws {
        let ref = Test_HLTypeRef(tparamProvider: HLTypeKind.i32)
        
        let ctx = try prepareContext(compilables: [
            /*
             fn testRef () -> (i32)
             
             reg0  u8       // to force non-0 offset for test coverage
             reg1  i32
             reg2  void
             reg3  ref<i32>
             
             0: Int         reg1 = 10
             1: Ref         reg3 = &reg0
             2: Call1       reg2 = testRefSet(reg2)
             3: Unref       reg1 = *reg3
             4: Ret         reg1
             */
            prepareFunction(
                retType: HLTypeKind.u8,
                findex: 0,
                regs: [HLTypeKind.u8,
                       HLTypeKind.i32, HLTypeKind.void, ref],
                args: [HLTypeKind.u8],
                ops: [
                    .OInt(dst: 1, ptr: 0),
                    .ORef(dst: 3, src: 1),
                    .OCall1(dst: 2, fun: 1, arg0: 3),
                    .OUnref(dst: 1, src: 3),
                    .ORet(ret: 1)
                ]),
            /*
             fn testRefSet (ref<i32>) -> (void)
             reg0  ref<i32>
             reg1  i32
             reg2  void
             
             0: Int         reg1 = 2
             1: Setref { dst: Reg(0), value: Reg(1) }
             2: Ret         reg2
             */
            prepareFunction(
                retType: HLTypeKind.void,
                findex: 1,
                regs: [ref, HLTypeKind.i32, HLTypeKind.void],
                args: [ref],
                ops: [
                    .OInt(dst: 1, ptr: 1),
                    .OSetref(dst: 0, value: 1),
                    .ORet(ret: 2)
                ]),
        ], ints: [10, 2])
        
        typealias _JitFunc = (@convention(c) (UInt8) -> (Int32))
        
        try compileAndLink(ctx: ctx,  0, 1, strip: false) {
            mappedMem in
            
            let callable = try ctx.getCallable(findex: 0)
            let entrypoint = unsafeBitCast(callable!.address.value, to: _JitFunc.self)
            
            let res = entrypoint(0 /*doesn't matter. Only here for padding*/ )
            XCTAssertEqual(2, res)
        }
    }
}
