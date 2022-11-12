import XCTest

@testable import swiftasm

extension Sequence where Iterator.Element: Hashable {
    func unique() -> [Iterator.Element] {
        var seen: [Iterator.Element: Bool] = [:]
        return self.filter { seen.updateValue(true, forKey: $0) == nil }
    }
}

func sut(strip: Bool = true) -> M1Compiler { M1Compiler(stripDebugMessages: strip) }

func builder() -> OpBuilder {
    printerr("Builder1")
    let storage = ModuleStorage()
    printerr("Builder2")
    let ctx = JitContext(storage: storage)
    printerr("Builder3")
    return OpBuilder(ctx: ctx)
}

func prepareFunction(
    funcType: Resolvable<HLType>,
    retType: Resolvable<HLType>,
    findex: Int32,
    regs: [Resolvable<HLType>],
    ops: [HLOpCode]
) -> HLFunction {
    return HLFunction(
        type: funcType,
        findex: findex,
        regs: regs,
        ops: ops,
        assigns: []
    )
}

/// NOTE: Deprecated. Stop using this (compiler needs Resolvables with memory initialized)
func prepareFunction(
    retType: HLType,
    findex: Int32,
    regs: [HLType],
    args: [HLType],
    ops: [HLOpCode]
) -> HLFunction {
    let rRegs = Resolvable.array(regs)
    let rArgs = Resolvable.array(args)
    let rRetType = Resolvable(retType)
    let rFuncType = Resolvable(HLType.fun(
        HLTypeFun_Depr(args: rArgs, ret: rRetType)
    ))
    return prepareFunction(funcType: rFuncType, retType: rRetType, findex: findex, regs: rRegs, ops: ops)
}

/*
 typedef struct {
 hl_code *code;
 hl_module *m;
 vdynamic *ret;
 pchar *file;
 int file_time;
 } main_context;
 */
struct MainContext {
    var code: UnsafePointer<HLCode_CCompat>
    var module: UnsafeRawPointer?
    var ret: UnsafeRawPointer?
    var file: UnsafePointer<UInt8>?
    var file_time: Int32
}

final class CompilerM1Tests: XCTestCase {
    static var context: MainContext? = nil
    
    static var code: UnsafePointer<HLCode_CCompat>?
    var code: UnsafePointer<HLCode_CCompat> { Self.code! }
    
    class override func setUp() {
        LibHl.hl_global_init()
        
        //
        let mod1 = Bundle.module.url(forResource: "mod1", withExtension: "hl")!.path
        let code = UnsafePointer(LibHl.load_code(mod1))
        self.context = MainContext(code: code, module: nil, ret: nil, file: nil, file_time: 0)
        self.code = code
        
        self.context!.module = UnsafeRawPointer(LibHl.hl_module_alloc(self.context!.code))
        guard let m = self.context?.module else {
            fatalError("nil module")
        }
        
        let res = LibHl.hl_module_init(m, false)
        guard res == 1 else {
            fatalError("Failed to init module (got \(res))")
        }
    }
    class override func tearDown() {
        LibHl.hl_global_free()
    }
    func testCompile_OGetThis() throws {
        let sut = sut()
        struct _TestMemory {
            var hl_type_addr: Int64 = 0xDEAD
            var field: Int32 = 0xBEEF
        }
        
        XCTAssertEqual(MemoryLayout<_TestMemory>.size, 12)
        
        let structType = HLType.obj(
            HLTypeObj(
                name: Resolvable("_TestMemory"),
                superType: nil,
                global: 0,
                fields: Resolvable.array([
                    HLObjField(
                        name: Resolvable("field"),
                        type: Resolvable(.i32))]),
                proto: [],
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
    
    func testCompile_OJNotNull() throws {
        let strType = code.pointee.getType(13)   // String
        let rstrType: Resolvable<HLType> = .type(fromUnsafe: strType)
        
        let i32Type = code.pointee.getType(3)   // i32
        let ri32Type: Resolvable<HLType> = .type(fromUnsafe: i32Type)
        
        let funcType = code.pointee.getType(10) // (String) -> (String)
        let rFuncType: Resolvable<HLType> = .type(fromUnsafe: funcType)
        
        // constants
        let constI_3 = 1 // constant value 3
        let constI_57005 = 2 // constant value 57005
        
        let storage = ModuleStorage(
            functions: [
                prepareFunction(
                    funcType: rFuncType,
                    retType: rstrType,
                    findex: 0,
                    regs: [rstrType, ri32Type],
                    ops: [
                        // if first arg < second arg, skip 2 following ops
                        .OJNotNull(reg: 0, offset: 2),
                        
                        // return 3
                        .OInt(dst: 1, ptr: constI_3),
                        .ORet(ret: 1),
                        
                        // return 57005
                        .OInt(dst: 1, ptr: constI_57005),
                        .ORet(ret: 1)
                    ]
                )
            ], ints: [0, 3, 57005])
        
        let ctx = JitContext(storage: storage)
        let mem = OpBuilder(ctx: ctx)
        let sut = M1Compiler(stripDebugMessages: true)
        try sut.compile(findex: 0, into: mem)
        
        //         mem.hexPrint()
        
        // run the entrypoint and ensure it works
        typealias _JitFunc = (@convention(c) (UnsafeRawPointer?) -> Int32)
        let entrypoint: _JitFunc = try mem.buildEntrypoint(0)
        
        // jump
        XCTAssertEqual(57005, entrypoint(UnsafeRawPointer(bitPattern: 123)))
        
        // no jump
        XCTAssertEqual(3, entrypoint(nil))
    }
    
    func testCompile_OJFalse() throws {
        // constants
        let constI_3 = 1 // constant value 3
        let constI_57005 = 2 // constant value 57005
        
        let storage = ModuleStorage(
            functions: [
                prepareFunction(
                    retType: .i32,
                    findex: 0,
                    regs: [.bool, .i32],
                    args: [.bool],
                    ops: [
                        .OJFalse(cond: 0, offset: 2),
                        
                        // return 3
                        .OInt(dst: 1, ptr: constI_3),
                        .ORet(ret: 1),
                        
                        // return 57005
                        .OInt(dst: 1, ptr: constI_57005),
                        .ORet(ret: 1)
                    ]
                )
            ], ints: [0, 3, 57005])
        
        let ctx = JitContext(storage: storage)
        let mem = OpBuilder(ctx: ctx)
        let sut = sut(strip: false)
        try sut.compile(findex: 0, into: mem)
        
        typealias _JitFunc = (@convention(c) (UInt8) -> Int32)
        let entrypoint8: _JitFunc = try mem.buildEntrypoint(0)
        
        XCTAssertEqual(3, entrypoint8(1))
        XCTAssertEqual(57005, entrypoint8(0))
    }
    
    func testCompile_OJAlways() throws {
        let i32Type = code.pointee.getType(3)   // i32
        let ri32Type: Resolvable<HLType> = .type(fromUnsafe: i32Type)
        
        let funcType = code.pointee.getType(103) // (i32, i32) -> (i32)
        let rFuncType: Resolvable<HLType> = .type(fromUnsafe: funcType)
        
        let storage = ModuleStorage(
            functions: [
                prepareFunction(
                    funcType: rFuncType,
                    retType: ri32Type,
                    findex: 0,
                    regs: [ri32Type, ri32Type],
                    ops: [
                        .OJAlways(offset: 1),
                        .ORet(ret: 0),
                        .ORet(ret: 1)
                    ]
                )
            ])
        
        let ctx = JitContext(storage: storage)
        let mem = OpBuilder(ctx: ctx)
        let sut = M1Compiler(stripDebugMessages: true)
        try sut.compile(findex: 0, into: mem)
        
        //         mem.hexPrint()
        
        // run the entrypoint and ensure it works
        typealias _JitFunc = (@convention(c) (Int32, Int32) -> Int32)
        let entrypoint: _JitFunc = try mem.buildEntrypoint(0)
        
        // always jump
        XCTAssertEqual(456, entrypoint(123, 456))
    }
    
    func testCompile_negativeJumps_regression() throws {
        let f = prepareFunction(
            retType: .u8,
            findex: 0,
            regs: [.u8],
            args: [.u8],
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
            ]
        )
        
        let storage = ModuleStorage(functions: [f])
        let ctx = JitContext(storage: storage)
        let mem = OpBuilder(ctx: ctx)
        let sut = sut(strip: false)
        try sut.compile(findex: 0, into: mem)
        
        let entrypoint: (@convention(c) (UInt8) -> UInt8) = try mem.buildEntrypoint(0)
        
        XCTAssertEqual(127, entrypoint(125))
//
//        let storage = ModuleStorage(
//            functions: [
//                prepareFunction(
//                    retType: .u8,
//                    findex: 0,
//                    regs: [.u8],
//                    args: [.u8],
//                    ops: [

//                    ])
//            ])
//
//        let ctx = JitContext(storage: storage)
//        let mem = OpBuilder(ctx: ctx)
//        let sut = M1Compiler(stripDebugMessages: true)
//        try sut.compile(findex: 0, into: mem)
//
//        //         mem.hexPrint()
//
//        // run the entrypoint and ensure it works
//        typealias _JitFunc = (@convention(c) (UInt8) -> UInt8)
//        let entrypoint: _JitFunc = try mem.buildEntrypoint(0)
//
//        // always jump
//        XCTAssertEqual(15, entrypoint(15))
    }
    
    func testCompile_OJNull() throws {
        let strType = code.pointee.getType(13)   // String
        let rstrType: Resolvable<HLType> = .type(fromUnsafe: strType)
        
        let i32Type = code.pointee.getType(3)   // i32
        let ri32Type: Resolvable<HLType> = .type(fromUnsafe: i32Type)
        
        let funcType = code.pointee.getType(10) // (String) -> (String)
        let rFuncType: Resolvable<HLType> = .type(fromUnsafe: funcType)
        
        // constants
        let constI_3 = 1 // constant value 3
        let constI_57005 = 2 // constant value 57005
        
        let storage = ModuleStorage(
            functions: [
                prepareFunction(
                    funcType: rFuncType,
                    retType: rstrType,
                    findex: 0,
                    regs: [rstrType, ri32Type],
                    ops: [
                        // if first arg < second arg, skip 2 following ops
                        .OJNull(reg: 0, offset: 2),
                        
                        // return 3
                        .OInt(dst: 1, ptr: constI_3),
                        .ORet(ret: 1),
                        
                        // return 57005
                        .OInt(dst: 1, ptr: constI_57005),
                        .ORet(ret: 1)
                    ]
                )
            ], ints: [0, 3, 57005])
        
        let ctx = JitContext(storage: storage)
        let mem = OpBuilder(ctx: ctx)
        let sut = M1Compiler(stripDebugMessages: true)
        try sut.compile(findex: 0, into: mem)
        
        //         mem.hexPrint()
        
        // run the entrypoint and ensure it works
        typealias _JitFunc = (@convention(c) (UnsafeRawPointer?) -> Int32)
        let entrypoint: _JitFunc = try mem.buildEntrypoint(0)
        
        // jump
        XCTAssertEqual(57005, entrypoint(nil))
        
        // no jump
        XCTAssertEqual(3, entrypoint(UnsafeRawPointer(bitPattern: 123)))
    }
    
    func testCompile_OJULt() throws {
        let i32Type = code.pointee.getType(3)   // i32
        let ri32Type: Resolvable<HLType> = .type(fromUnsafe: i32Type)
        
        let funcType = code.pointee.getType(104) // (i32, i32) -> (i32)
        let rFuncType: Resolvable<HLType> = .type(fromUnsafe: funcType)
        
        // constants
        let constI_3 = 1 // constant value 3
        let constI_57005 = 2 // constant value 57005
        
        let storage = ModuleStorage(
            functions: [
                prepareFunction(
                    funcType: rFuncType,
                    retType: ri32Type,
                    findex: 0,
                    regs: [ri32Type, ri32Type],
                    ops: [
                        // if first arg < second arg, skip 2 following ops
                        .OJULt(a: 0, b: 1, offset: 2),
                        
                        // return 3
                        .OInt(dst: 0, ptr: constI_3),
                        .ORet(ret: 0),
                        
                        // return 57005
                        .OInt(dst: 0, ptr: constI_57005),
                        .ORet(ret: 0)
                    ]
                )
            ], ints: [0, 3, 57005])
        
        let ctx = JitContext(storage: storage)
        let mem = OpBuilder(ctx: ctx)
        let sut = M1Compiler(stripDebugMessages: true)
        try sut.compile(findex: 0, into: mem)
        
        // TODO: need to test w i64
        mem.hexPrint()
        
        // run the entrypoint and ensure it works
        typealias _JitFunc = (@convention(c) (Int32, Int32) -> Int32)
        let entrypoint: _JitFunc = try mem.buildEntrypoint(0)
        
        // jump
        XCTAssertEqual(57005, entrypoint(2, 3))
        
        // no jump
        XCTAssertEqual(3, entrypoint(3, 3))
        XCTAssertEqual(3, entrypoint(4, 3))
    }
    
    func testCompile_OJSLt() throws {
        // constants
        let constI_3 = 1 // constant value 3
        let constI_57005 = 2 // constant value 57005
        
        let storage = ModuleStorage(
            functions: [
                prepareFunction(
                    retType: .i32,
                    findex: 0,
                    regs: [.u8, .u8, .i32],
                    args: [.u8, .u8],
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
                    retType: .i32,
                    findex: 1,
                    regs: [.u16, .u16, .i32],
                    args: [.u16, .u16],
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
        
        let ctx = JitContext(storage: storage)
        let mem = OpBuilder(ctx: ctx)
        let sut = sut(strip: false)
        try sut.compile(findex: 0, into: mem)
        try sut.compile(findex: 1, into: mem)
        
        // run the entrypoint and ensure it works
        typealias _JitFunc = (@convention(c) (UInt16, UInt16) -> Int32)
        let entrypoint8: _JitFunc = try mem.buildEntrypoint(0)
        let entrypoint16: _JitFunc = try mem.buildEntrypoint(1)
        
        // jump <
        XCTAssertEqual(57005, entrypoint16(0b1000000000000001, 0b0100000000000001))
        XCTAssertEqual(57005, entrypoint8(0b10000001, 0b01000001))
        // no jump ==
        XCTAssertEqual(3, entrypoint16(0b1000000000000001, 0b1000000000000001))
        XCTAssertEqual(3, entrypoint8(0b10000001, 0b10000001))
        // no jump >
        XCTAssertEqual(3, entrypoint16(0b0100000000000001, 0b1000000000000001))
        XCTAssertEqual(3, entrypoint8(0b01000001, 0b10000001))
    }
    
    func testCompile_OSShr_OUshr() throws {
        let storage = ModuleStorage(
            functions: [
                prepareFunction(
                    retType: .u8,
                    findex: 0,
                    regs: [.u8, .u8, .u8],
                    args: [.u8, .u8],
                    ops: [
                        .OSShr(dst: 2, a: 0, b: 1),
                        .ORet(ret: 2)
                    ]
                ),
                prepareFunction(
                    retType: .u8,
                    findex: 1,
                    regs: [.u8, .u8, .u8],
                    args: [.u8, .u8],
                    ops: [
                        .OUShr(dst: 2, a: 0, b: 1),
                        .ORet(ret: 2)
                    ]
                )
            ])
        
        let ctx = JitContext(storage: storage)
        let mem = OpBuilder(ctx: ctx)
        let sut = sut(strip: false)
        try sut.compile(findex: 0, into: mem)
        try sut.compile(findex: 1, into: mem)
        
        // run the entrypoint and ensure it works
        typealias _JitFunc = (@convention(c) (UInt8, UInt8) -> UInt8)
        let entrypointS: _JitFunc = try mem.buildEntrypoint(0)
        let entrypointU: _JitFunc = try mem.buildEntrypoint(1)
        
        // signed shift right
        XCTAssertEqual(0b11110000, entrypointS(0b10000001, 3))
        // unsigned shift right
        XCTAssertEqual(0b00010000, entrypointU(0b10000001, 3))
    }
    
    func testCompile_OJSLte() throws {
        // constants
        let constI_3 = 1 // constant value 3
        let constI_57005 = 2 // constant value 57005
        
        let storage = ModuleStorage(
            functions: [
                prepareFunction(
                    retType: .i32,
                    findex: 0,
                    regs: [.u8, .u8, .i32],
                    args: [.u8, .u8],
                    ops: [
                        .OJSLte(a: 0, b: 1, offset: 2),
                        
                        // return 3
                        .OInt(dst: 2, ptr: constI_3),
                        .ORet(ret: 2),
                        
                        // return 57005
                        .OInt(dst: 2, ptr: constI_57005),
                        .ORet(ret: 2)
                    ]
                ),
                prepareFunction(
                    retType: .i32,
                    findex: 1,
                    regs: [.u16, .u16, .i32],
                    args: [.u16, .u16],
                    ops: [
                        .OJSLte(a: 0, b: 1, offset: 2),
                        
                        // return 3
                        .OInt(dst: 2, ptr: constI_3),
                        .ORet(ret: 2),
                        
                        // return 57005
                        .OInt(dst: 2, ptr: constI_57005),
                        .ORet(ret: 2)
                    ]
                )
            ], ints: [0, 3, 57005])
        
        let ctx = JitContext(storage: storage)
        let mem = OpBuilder(ctx: ctx)
        let sut = sut(strip: false)
        try sut.compile(findex: 0, into: mem)
        try sut.compile(findex: 1, into: mem)
        
        // run the entrypoint and ensure it works
        typealias _JitFunc = (@convention(c) (UInt16, UInt16) -> Int32)
        let entrypoint8: _JitFunc = try mem.buildEntrypoint(0)
        let entrypoint16: _JitFunc = try mem.buildEntrypoint(1)
        
        // jump <
        XCTAssertEqual(57005, entrypoint16(0b1000000000000001, 0b0100000000000001))
        XCTAssertEqual(57005, entrypoint8(0b10000001, 0b01000001))
        // jump ==
        XCTAssertEqual(57005, entrypoint16(0b1000000000000001, 0b1000000000000001))
        XCTAssertEqual(57005, entrypoint8(0b10000001, 0b10000001))
        
        // no jump >
        XCTAssertEqual(3, entrypoint16(0b0100000000000001, 0b1000000000000001))
        XCTAssertEqual(3, entrypoint8(0b01000001, 0b10000001))
    }
    
    func testCompile_OJSGte() throws {
        // constants
        let constI_3 = 1 // constant value 3
        let constI_57005 = 2 // constant value 57005
        
        let storage = ModuleStorage(
            functions: [
                prepareFunction(
                    retType: .i32,
                    findex: 0,
                    regs: [.u8, .u8, .i32],
                    args: [.u8, .u8],
                    ops: [
                        .OJSGte(a: 0, b: 1, offset: 2),
                        
                        // return 3
                        .OInt(dst: 2, ptr: constI_3),
                        .ORet(ret: 2),
                        
                        // return 57005
                        .OInt(dst: 2, ptr: constI_57005),
                        .ORet(ret: 2)
                    ]
                ),
                prepareFunction(
                    retType: .i32,
                    findex: 1,
                    regs: [.u16, .u16, .i32],
                    args: [.u16, .u16],
                    ops: [
                        .OJSGte(a: 0, b: 1, offset: 2),
                        
                        // return 3
                        .OInt(dst: 2, ptr: constI_3),
                        .ORet(ret: 2),
                        
                        // return 57005
                        .OInt(dst: 2, ptr: constI_57005),
                        .ORet(ret: 2)
                    ]
                )
            ], ints: [0, 3, 57005])
        
        let ctx = JitContext(storage: storage)
        let mem = OpBuilder(ctx: ctx)
        let sut = sut(strip: false)
        try sut.compile(findex: 0, into: mem)
        try sut.compile(findex: 1, into: mem)
        
        // run the entrypoint and ensure it works
        typealias _JitFunc = (@convention(c) (UInt16, UInt16) -> Int32)
        let entrypoint8: _JitFunc = try mem.buildEntrypoint(0)
        let entrypoint16: _JitFunc = try mem.buildEntrypoint(1)
        
        // jump >
        XCTAssertEqual(57005, entrypoint16(0b0100000000000001, 0b1000000000000001))
        XCTAssertEqual(57005, entrypoint8(0b01000001, 0b10000001))
        // jump ==
        XCTAssertEqual(57005, entrypoint16(0b1000000000000001, 0b1000000000000001))
        XCTAssertEqual(57005, entrypoint8(0b10000001, 0b10000001))
        
        // no jump <
        XCTAssertEqual(3, entrypoint16(0b1000000000000001, 0b0100000000000001))
        XCTAssertEqual(3, entrypoint8(0b10000001, 0b01000001))
    }
    
    func testCompile_OJEq() throws {
        let i32Type = code.pointee.getType(3)   // i32
        let ri32Type: Resolvable<HLType> = .type(fromUnsafe: i32Type)
        
        let funcType = code.pointee.getType(104) // (i32, i32) -> (i32)
        let rFuncType: Resolvable<HLType> = .type(fromUnsafe: funcType)
        
        // constants
        let constI_3 = 1 // constant value 3
        let constI_57005 = 2 // constant value 57005
        
        let storage = ModuleStorage(
            functions: [
                prepareFunction(
                    funcType: rFuncType,
                    retType: ri32Type,
                    findex: 0,
                    regs: [ri32Type, ri32Type],
                    ops: [
                        .OJEq(a: 0, b: 1, offset: 2),
                        
                        // return 3
                        .OInt(dst: 0, ptr: constI_3),
                        .ORet(ret: 0),
                        
                        // return 57005
                        .OInt(dst: 0, ptr: constI_57005),
                        .ORet(ret: 0)
                    ]
                )
            ], ints: [0, 3, 57005])
        
        let ctx = JitContext(storage: storage)
        let mem = OpBuilder(ctx: ctx)
        let sut = M1Compiler(stripDebugMessages: true)
        try sut.compile(findex: 0, into: mem)
        
        // TODO: need to test w i64
        mem.hexPrint()
        
        // run the entrypoint and ensure it works
        typealias _JitFunc = (@convention(c) (Int32, Int32) -> Int32)
        let entrypoint: _JitFunc = try mem.buildEntrypoint(0)
        
        // jump
        XCTAssertEqual(57005, entrypoint(3, 3))
        
        // no jump
        XCTAssertEqual(3, entrypoint(3, 2))
        XCTAssertEqual(3, entrypoint(451, 0))
    }
    
    func testCompile_OJNotEq() throws {
        let i32Type = code.pointee.getType(3)   // i32
        let ri32Type: Resolvable<HLType> = .type(fromUnsafe: i32Type)
        
        let funcType = code.pointee.getType(104) // (i32, i32) -> (i32)
        let rFuncType: Resolvable<HLType> = .type(fromUnsafe: funcType)
        
        // constants
        let constI_3 = 1 // constant value 3
        let constI_57005 = 2 // constant value 57005
        
        let storage = ModuleStorage(
            functions: [
                prepareFunction(
                    funcType: rFuncType,
                    retType: ri32Type,
                    findex: 0,
                    regs: [ri32Type, ri32Type],
                    ops: [
                        .OJNotEq(a: 0, b: 1, offset: 2),
                        
                        // return 3
                        .OInt(dst: 0, ptr: constI_3),
                        .ORet(ret: 0),
                        
                        // return 57005
                        .OInt(dst: 0, ptr: constI_57005),
                        .ORet(ret: 0)
                    ]
                )
            ], ints: [0, 3, 57005])
        
        let ctx = JitContext(storage: storage)
        let mem = OpBuilder(ctx: ctx)
        let sut = M1Compiler(stripDebugMessages: true)
        try sut.compile(findex: 0, into: mem)
        
        // TODO: need to test w i64
        mem.hexPrint()
        
        // run the entrypoint and ensure it works
        typealias _JitFunc = (@convention(c) (Int32, Int32) -> Int32)
        let entrypoint: _JitFunc = try mem.buildEntrypoint(0)
        
        // jump
        XCTAssertEqual(57005, entrypoint(2, 3))
        XCTAssertEqual(57005, entrypoint(3, 2))
        
        // no jump
        XCTAssertEqual(3, entrypoint(3, 3))
        XCTAssertEqual(3, entrypoint(0, 0))
    }
    
    func testCompile_OAnd() throws {
        let i32Type = code.pointee.getType(3)   // i32
        let ri32Type: Resolvable<HLType> = .type(fromUnsafe: i32Type)
        
        let funcType = code.pointee.getType(104) // (i32, i32) -> (i32)
        let rFuncType: Resolvable<HLType> = .type(fromUnsafe: funcType)
        
        let storage = ModuleStorage(
            functions: [
                prepareFunction(
                    funcType: rFuncType,
                    retType: ri32Type,
                    findex: 0,
                    regs: [ri32Type, ri32Type],
                    ops: [
                        .OAnd(dst: 0, a: 0, b: 1),
                        .ORet(ret: 0)
                    ]
                )
            ])
        
        let ctx = JitContext(storage: storage)
        let mem = OpBuilder(ctx: ctx)
        let sut = M1Compiler(stripDebugMessages: true)
        try sut.compile(findex: 0, into: mem)
        
        mem.hexPrint()
        
        // run the entrypoint and ensure it works
        typealias _JitFunc = (@convention(c) (Int32, Int32) -> Int32)
        let entrypoint: _JitFunc = try mem.buildEntrypoint(0)
        
        XCTAssertEqual(0b00000, entrypoint(0b00000, 0b11111))
        XCTAssertEqual(0b00100, entrypoint(0b00111, 0b11100))
        XCTAssertEqual(0b11111, entrypoint(0b11111, 0b11111))
    }
    
    func testCompile_OIncr() throws {
        let f = prepareFunction(
            retType: .u8,
            findex: 0,
            regs: [.u8],
            args: [.u8],
            ops: [
                .OIncr(dst: 0),
                .ORet(ret: 0),
            ]
        )
        
        let storage = ModuleStorage(functions: [f])
        let ctx = JitContext(storage: storage)
        let mem = OpBuilder(ctx: ctx)
        let sut = sut(strip: false)
        try sut.compile(findex: 0, into: mem)
        
        let entrypoint: (@convention(c) (UInt8) -> UInt8) = try mem.buildEntrypoint(0)
        
        XCTAssertEqual(125, entrypoint(124))
        XCTAssertEqual(126, entrypoint(125))
    }
    
    func testCompile_OSub() throws {
        let i32Type = code.pointee.getType(3)   // i32
        let ri32Type: Resolvable<HLType> = .type(fromUnsafe: i32Type)
        
        let funcType = code.pointee.getType(104) // (i32, i32) -> (i32)
        let rFuncType: Resolvable<HLType> = .type(fromUnsafe: funcType)
        
        let storage = ModuleStorage(
            functions: [
                prepareFunction(
                    funcType: rFuncType,
                    retType: ri32Type,
                    findex: 0,
                    regs: [ri32Type, ri32Type],
                    ops: [
                        .OSub(dst: 0, a: 0, b: 1),
                        .ORet(ret: 0)
                    ]
                )
            ])
        
        let ctx = JitContext(storage: storage)
        let mem = OpBuilder(ctx: ctx)
        let sut = M1Compiler(stripDebugMessages: true)
        try sut.compile(findex: 0, into: mem)
        
        mem.hexPrint()
        
        // run the entrypoint and ensure it works
        typealias _JitFunc = (@convention(c) (Int32, Int32) -> Int32)
        let entrypoint: _JitFunc = try mem.buildEntrypoint(0)
        
        XCTAssertEqual(11, entrypoint(12, 1))
        XCTAssertEqual(1000, entrypoint(1252, 252))
    }
    
    func testCompile_ONull() throws {
        let dynType = code.pointee.getType(9)   // dyn
        let rdynType: Resolvable<HLType> = .type(fromUnsafe: dynType)
        
        let funcType = code.pointee.getType(50) // (dyn) -> (dyn)
        let rFuncType: Resolvable<HLType> = .type(fromUnsafe: funcType)
        
        let storage = ModuleStorage(
            functions: [
                prepareFunction(
                    funcType: rFuncType,
                    retType: rdynType,
                    findex: 0,
                    regs: [rdynType],
                    ops: [
                        .ONull(dst: 0),
                        .ORet(ret: 0)
                    ]
                )
            ])
        
        let ctx = JitContext(storage: storage)
        let mem = OpBuilder(ctx: ctx)
        let sut = M1Compiler(stripDebugMessages: true)
        try sut.compile(findex: 0, into: mem)
        
        mem.hexPrint()
        
        // run the entrypoint and ensure it works
        typealias _JitFunc = (@convention(c) (UnsafeRawPointer) -> UnsafeRawPointer?)
        let entrypoint: _JitFunc = try mem.buildEntrypoint(0)
        
        XCTAssertEqual(nil, entrypoint(UnsafeRawPointer(bitPattern: 0x7b)!))
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
        
        mem.hexPrint()
        
        XCTAssertEqual(
            mem.lockAddressesAndBuild(),
            [
                0xfd, 0x7b, 0xbf, 0xa9, // stp x29, x30, [sp, #-16]!
                0xfd, 0x03, 0x00, 0x91, // movr x29, sp
                0x01, 0x00, 0x00, 0x14, // b #4
                0xfd, 0x7b, 0xc1, 0xa8, // ldp x29, x30, [sp], #16
                0xc0, 0x03, 0x5f, 0xd6, // ret
            ]
        )
        
        // run the entrypoint and ensure it works
        let entrypoint: JitVoid = try mem.buildEntrypoint(0)
        entrypoint()
    }
    
    /**
     > fn 16
     16 : fn __alloc__@16 (bytes, i32) -> (String)@18 (3 regs, 4 ops)
     reg0  bytes@14
     reg1  i32@3
     reg2  String@13
     /usr/local/lib/haxe/std/hl/_std/String.hx:220   0: New         reg2 = new String@13
     /usr/local/lib/haxe/std/hl/_std/String.hx:221   1: SetField    reg2.bytes = reg0
     /usr/local/lib/haxe/std/hl/_std/String.hx:222   2: SetField    reg2.length = reg1
     /usr/local/lib/haxe/std/hl/_std/String.hx:223   3: Ret         reg2
     */
    func testCompile_ONew_OSetField() throws {
        let funcType: UnsafePointer<HLType_CCompat> = code.pointee.getType(18)     // (bytes, i32) -> (String)
        let stringType = code.pointee.getType(13)   // String
        let byteType = code.pointee.getType(14)   // bytes
        let i32Type = code.pointee.getType(3)   // i32
        
        let rFuncType: Resolvable<HLType> = .type(fromUnsafe: funcType)
        let rStringType: Resolvable<HLType> = .type(fromUnsafe: stringType)
        let rByteType: Resolvable<HLType> = .type(fromUnsafe: byteType)
        let ri32Type: Resolvable<HLType> = .type(fromUnsafe: i32Type)
        
        let storage = ModuleStorage(
            functions: [
                prepareFunction(
                    funcType: rFuncType,
                    retType: rStringType,
                    findex: 0,
                    regs: [
                        rByteType, ri32Type, rStringType
                    ],
                    ops: [
                        .ONew(dst: 2),
                        .OSetField(obj: 2, field: 0, src: 0),
                        .OSetField(obj: 2, field: 1, src: 1),
                        .ORet(ret: 2)]
                )
            ])
        
        let ctx = JitContext(storage: storage)
        let mem = OpBuilder(ctx: ctx)
        let sut = M1Compiler(stripDebugMessages: true)
        try sut.compile(findex: 0, into: mem)
        
        // mem.hexPrint()
        
        // run the entrypoint and ensure it works
        typealias _JitFunc = (@convention(c) (UnsafeRawPointer, Int32) -> UnsafeRawPointer)
        let entrypoint: _JitFunc = try mem.buildEntrypoint(0)
        
        Array("Hello World\0".utf16).withUnsafeBytes { cstr in
            let result = entrypoint(cstr.baseAddress!, 12)
            
            let vPtr = result.bindMemory(to: vdynamic.self, capacity: 1)
            let typePtr = vPtr.pointee.t
            
            // check type
            XCTAssertEqual(typePtr.pointee.kind, .obj)
            
            // bytes/str
            let bytes = result.advanced(by: 8).bindMemory(to: UnsafePointer<CChar16>.self, capacity: 1)
            XCTAssertEqual(bytes.pointee, cstr.baseAddress!)
            
            let str = String.wrapUtf16(from: bytes.pointee)
            XCTAssertEqual(str, "Hello World")
            
            // len
            let len = Int(result.advanced(by: 16).bindMemory(to: Int32.self, capacity: 1).pointee)
            XCTAssertEqual(len, 12)
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
                            HLTypeFun_Depr(
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
        let sut = sut(strip: false)
        try sut.compile(findex: 0, into: mem)
        // Print debug output of the generated Aarch64 bytecode
        mem.hexPrint()
        // Now place the compiled bytecode in executable memory and get a pointer to it in
        // form of function (JitInt64 means (void)->Int64)
        let entrypoint: _JitFunc = try mem.buildEntrypoint(0)
        let res: Int32 = entrypoint(1, 2, 6)
        
        // HL called Swift func. Swift func returned 145. HL returned the result it received.
        XCTAssertEqual(0b111, res)
    }
    
    func testCompile__OCall4() throws {
        // Prepare function we'll call from JIT
        typealias _JitFunc = (@convention(c) (UInt8, UInt8, UInt8, UInt8) -> UInt8)
        let swiftFunc: _JitFunc = { (_ a: UInt8, _ b: UInt8, _ c: UInt8, _ d: UInt8) in
            return ((a & 1) << 3) | ((b & 1) << 2) | ((c & 1) << 1) | (d & 1)
        }
        let swiftFuncPtr = unsafeBitCast(swiftFunc, to: UnsafeMutableRawPointer.self)
        
        let f = prepareFunction(
            retType: .u8,
            findex: 0,
            regs: [.u8, .u8, .u8, .u8],
            args: [.u8, .u8, .u8, .u8],
            ops: [
                // NOTE: sending registers to diff numbered args
                .OCall4(dst: 0, fun: 1, arg0: 3, arg1: 2, arg2: 1, arg3: 0),
                .ORet(ret: 0),
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
                            HLTypeFun_Depr(
                                args: Resolvable.array([.u8, .u8, .u8, .u8]),
                                ret: Resolvable(.u8)
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
        
        XCTAssertEqual(0, entrypoint(0, 0, 0, 0))
        XCTAssertEqual(0b0001, entrypoint(1, 0, 0, 0))
        XCTAssertEqual(0b0010, entrypoint(0, 1, 0, 0))
        XCTAssertEqual(0b0100, entrypoint(0, 0, 1, 0))
        XCTAssertEqual(0b1000, entrypoint(0, 0, 0, 1))
        XCTAssertEqual(0b1111, entrypoint(1, 1, 1, 1))
        XCTAssertEqual(0b0000, entrypoint(0, 0, 0, 0))
        XCTAssertEqual(0b0110, entrypoint(0, 1, 1, 0))
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
        
        let storage = ModuleStorage(
            functions: [
                prepareFunction(
                    retType: .i32,
                    findex: 0,
                    regs: [.u8, .u8, .i32, .u8, .u8, .u8, .i32, .u8, .u8],
                    args: [.u8, .u8, .i32, .u8, .u8, .u8, .i32, .u8, .u8],
                    ops: [
                        // NOTE: sending registers to diff numbered args
                        .OCallN(dst: 2, fun: 1, args: [8, 1, 2, 3, 4, 5, 6, 7, 0]),
                        .ORet(ret: 2),
                    ]
                )
            ],
            natives: [
                HLNative(
                    lib: Resolvable("builtin"),
                    name: Resolvable("swiftFunc"),
                    type: Resolvable(
                        .fun(
                            HLTypeFun_Depr(
                                args: Resolvable.array([.u8, .u8, .i32, .u8, .u8, .u8, .i32, .u8, .u8]),
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
        let sut = sut(strip: false)
        try sut.compile(findex: 0, into: mem)
        let entrypoint: (@convention(c) (UInt8, UInt8, UInt32, UInt8, UInt8, UInt8, UInt32, UInt8, UInt8) -> UInt32) = try mem.buildEntrypoint(0)
        
        XCTAssertEqual(
            848006100,
            entrypoint(3, 0, 1, 0, 1, 0, 1, 0, 9))
    }
    
    func testCompile__OAdd() throws {
        let storage = ModuleStorage(
            functions: [
                prepareFunction(
                    retType: .u8,
                    findex: 0,
                    regs: [
                        // args
                        .u8, .u8
                    ],
                    args: [
                        // args
                        .u8
                    ],
                    ops: [
                        .OAdd(dst: 1, a: 0, b: 1),
                        .ORet(ret: 1),
                    ]
                ),
            ]
        )
        let ctx = JitContext(storage: storage)
        let mem = OpBuilder(ctx: ctx)
        let sut = sut(strip: false)
        try sut.compile(findex: 0, into: mem)
        let entrypoint: (@convention(c) (UInt8, UInt8) -> UInt8) = try mem.buildEntrypoint(0)
        XCTAssertEqual(6, entrypoint(2, 4))
    }
    
    func testCompile__OMov() throws {
        let storage = ModuleStorage(
            functions: [
                prepareFunction(
                    retType: .u8,
                    findex: 0,
                    regs: [
                        // args
                        .u8, .u8
                    ],
                    args: [
                        // args
                        .u8
                    ],
                    ops: [
                        .OMov(dst: 1, src: 0),
                        .ORet(ret: 1),
                    ]
                ),
            ]
        )
        let ctx = JitContext(storage: storage)
        let mem = OpBuilder(ctx: ctx)
        let sut = sut(strip: false)
        try sut.compile(findex: 0, into: mem)
        let entrypoint: (@convention(c) (UInt8) -> UInt8) = try mem.buildEntrypoint(0)
        XCTAssertEqual(6, entrypoint(6))
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
            let storage = ModuleStorage(
                functions: [
                    prepareFunction(
                        retType: .u8,
                        findex: 0,
                        regs: [
                            // args
                            .u8, .u8, .u8, .u8, .u8,
                        ],
                        args: [
                            // args
                            .u8
                        ],
                        ops: [
                            .OMov(dst: 1, src: 0),
                            .OMov(dst: 2, src: 0),
                            .OMov(dst: 3, src: 0),
                            .OMov(dst: 4, src: 0),
                            sutOp,
                            .ORet(ret: 0),
                        ]
                    ),
                ],
                natives: [
                    HLNative(
                        lib: Resolvable("builtin"),
                        name: Resolvable("f1"),
                        type: Resolvable(
                            .fun(
                                HLTypeFun_Depr(
                                    args: Resolvable.array([.u8]),
                                    ret: Resolvable(.u8)
                                )
                            )
                        ),
                        findex: 1,
                        memory: f1p
                    ),
                    HLNative(
                        lib: Resolvable("builtin"),
                        name: Resolvable("f2"),
                        type: Resolvable(
                            .fun(
                                HLTypeFun_Depr(
                                    args: Resolvable.array([.u8, .u8]),
                                    ret: Resolvable(.u8)
                                )
                            )
                        ),
                        findex: 2,
                        memory: f2p
                    ),
                    HLNative(
                        lib: Resolvable("builtin"),
                        name: Resolvable("f3"),
                        type: Resolvable(
                            .fun(
                                HLTypeFun_Depr(
                                    args: Resolvable.array([.u8, .u8, .u8]),
                                    ret: Resolvable(.u8)
                                )
                            )
                        ),
                        findex: 3,
                        memory: f3p
                    ),
                    HLNative(
                        lib: Resolvable("builtin"),
                        name: Resolvable("f4"),
                        type: Resolvable(
                            .fun(
                                HLTypeFun_Depr(
                                    args: Resolvable.array([.u8, .u8, .u8, .u8]),
                                    ret: Resolvable(.u8)
                                )
                            )
                        ),
                        findex: 4,
                        memory: f4p
                    )
                ]
            )
            let ctx = JitContext(storage: storage)
            let mem = OpBuilder(ctx: ctx)
            let sut = sut(strip: false)
            try sut.compile(findex: 0, into: mem)
            let entrypoint: (@convention(c) (UInt8) -> UInt8) = try mem.buildEntrypoint(0)
            XCTAssertEqual(6, entrypoint(6), "failed for \(sutOp.id)")
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
            let storage = ModuleStorage(
                functions: [
                    prepareFunction(
                        retType: .void,
                        findex: 0,
                        regs: [
                            // args
                            .void, .void, .void, .void, .void
                        ],
                        args: [],
                        ops: [
                            sutOp,
                            .ORet(ret: 0),
                        ]
                    ),
                ],
                natives: [
                    HLNative(
                        lib: Resolvable("builtin"),
                        name: Resolvable("f1"),
                        type: Resolvable(
                            .fun(
                                HLTypeFun_Depr(
                                    args: Resolvable.array([.void]),
                                    ret: Resolvable(.void)
                                )
                            )
                        ),
                        findex: 1,
                        memory: f1p
                    ),
                    HLNative(
                        lib: Resolvable("builtin"),
                        name: Resolvable("f2"),
                        type: Resolvable(
                            .fun(
                                HLTypeFun_Depr(
                                    args: Resolvable.array([.void, .void]),
                                    ret: Resolvable(.void)
                                )
                            )
                        ),
                        findex: 2,
                        memory: f2p
                    ),
                    HLNative(
                        lib: Resolvable("builtin"),
                        name: Resolvable("f3"),
                        type: Resolvable(
                            .fun(
                                HLTypeFun_Depr(
                                    args: Resolvable.array([.void, .void, .void]),
                                    ret: Resolvable(.void)
                                )
                            )
                        ),
                        findex: 3,
                        memory: f3p
                    ),
                    HLNative(
                        lib: Resolvable("builtin"),
                        name: Resolvable("f4"),
                        type: Resolvable(
                            .fun(
                                HLTypeFun_Depr(
                                    args: Resolvable.array([.void, .void, .void, .void]),
                                    ret: Resolvable(.void)
                                )
                            )
                        ),
                        findex: 4,
                        memory: f4p
                    )
                ]
            )
            let ctx = JitContext(storage: storage)
            let mem = OpBuilder(ctx: ctx)
            let sut = sut(strip: false)
            try sut.compile(findex: 0, into: mem)
            let entrypoint: (@convention(c) () -> ()) = try mem.buildEntrypoint(0)
            entrypoint()
        }
    }
    
    func testGetRegStackOffset() throws {
        let c = sut()
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
        
        // Misc. JIT stuff
        let storage = ModuleStorage(
            functions: [
                prepareFunction(
                    retType: .i32,
                    findex: 0,
                    regs: [.u16, .i32],
                    args: [.u16],
                    ops: [
                        .OCall1(dst: 1, fun: 1, arg0: 0),
                        .ORet(ret: 1),
                    ]
                ),
                prepareFunction(
                    retType: .i32,
                    findex: 2,
                    regs: [.u8, .i32],
                    args: [.u8],
                    ops: [
                        .OCall1(dst: 1, fun: 3, arg0: 0),
                        .ORet(ret: 1),
                    ]
                )
            ],
            natives: [
                HLNative(
                    lib: Resolvable("builtin"),
                    name: Resolvable("swiftFunc16"),
                    type: Resolvable(
                        .fun(
                            HLTypeFun_Depr(
                                args: Resolvable.array([.u16]),
                                ret: Resolvable(.i32)
                            )
                        )
                    ),
                    findex: 1,
                    memory: swiftFuncPtr16
                ),
                HLNative(
                    lib: Resolvable("builtin"),
                    name: Resolvable("swiftFunc8"),
                    type: Resolvable(
                        .fun(
                            HLTypeFun_Depr(
                                args: Resolvable.array([.u8]),
                                ret: Resolvable(.i32)
                            )
                        )
                    ),
                    findex: 3,
                    memory: swiftFuncPtr8
                )
            ]
        )
        let ctx = JitContext(storage: storage)
        let mem = OpBuilder(ctx: ctx)
        // Compile HL function with function index 0 (from the whole functions table)
        let sut = sut()
        try sut.compile(findex: 0, into: mem)
        try sut.compile(findex: 2, into: mem)
        // Print debug output of the generated Aarch64 bytecode
        mem.hexPrint()
        // Now place the compiled bytecode in executable memory and get a pointer to it in
        // form of function (JitInt64 means (void)->Int64)
        let entrypoint16: _JitFunc16 = try mem.buildEntrypoint(0)
        let entrypoint8: _JitFunc8 = try mem.buildEntrypoint(2)
        
        XCTAssertEqual(246, entrypoint16(123))
        XCTAssertEqual(492, entrypoint8(123))
    }
    
    func testCompile__OGetI8() throws {
        typealias _JitFunc = (@convention(c) (UnsafeRawPointer, Int32) -> UInt8)
        
        let f = prepareFunction(
            retType: .u8,
            findex: 0,
            regs: [.bytes, .i32, .u8],
            args: [.bytes, .i32],
            ops: [
                .OGetI8(dst: 2, bytes: 0, index: 1),
                .ORet(ret: 2),
            ]
        )
        
        let storage = ModuleStorage(functions: [f])
        let ctx = JitContext(storage: storage)
        let mem = OpBuilder(ctx: ctx)
        let sut = M1Compiler(stripDebugMessages: false)
        try sut.compile(findex: 0, into: mem)
        
        //        mem.hexPrint()
        
        let entrypoint: _JitFunc = try mem.buildEntrypoint(0)
        
        var x: [UInt8] = [11, 22, 33, 44, 55, 66, 77, 88, 99]
        
        XCTAssertEqual(11, entrypoint(&x, 0))
        XCTAssertEqual(66, entrypoint(&x, 5))
    }
    
    func testCompile__OGetI16() throws {
        typealias _JitFunc = (@convention(c) (UnsafeRawPointer, Int32) -> UInt16)
        
        let f = prepareFunction(
            retType: .u16,
            findex: 0,
            regs: [.bytes, .i32, .u16],
            args: [.bytes, .i32],
            ops: [
                .OGetI16(dst: 2, bytes: 0, index: 1),
                .ORet(ret: 2),
            ]
        )
        
        let storage = ModuleStorage(functions: [f])
        let ctx = JitContext(storage: storage)
        let mem = OpBuilder(ctx: ctx)
        let sut = sut()
        try sut.compile(findex: 0, into: mem)
        
        //        mem.hexPrint()
        
        let entrypoint: _JitFunc = try mem.buildEntrypoint(0)
        
        var x: [UInt16] = [11, 22, 33, 44, 55, 66, 77, 88, 99]
        
        XCTAssertEqual(11, entrypoint(&x, 0))
        XCTAssertEqual(22, entrypoint(&x, 1))
        XCTAssertEqual(66, entrypoint(&x, 5))
    }
    
    func testCompile__OShl() throws {
        // Prepare function we'll call from JIT
        let f = prepareFunction(
            retType: .i32,
            findex: 0,
            regs: [.i32, .i32, .i32],
            args: [.i32, .i32],
            ops: [
                .OShl(dst: 2, a: 0, b: 1),
                .ORet(ret: 2),
            ]
        )
        
        // Misc. JIT stuff
        let storage = ModuleStorage(
            functions: [f]
        )
        let ctx = JitContext(storage: storage)
        let mem = OpBuilder(ctx: ctx)
        // Compile HL function with function index 0 (from the whole functions table)
        let sut = sut()
        try sut.compile(findex: 0, into: mem)
        
        mem.hexPrint()
        
        typealias _JitFunc = (@convention(c) (Int32, Int32) -> Int32)
        let entrypoint: _JitFunc = try mem.buildEntrypoint(0)
        
        XCTAssertEqual(4, entrypoint(1, 2))
        XCTAssertEqual(20480, entrypoint(5, 12))
        XCTAssertEqual(Int32(bitPattern: UInt32(2147483648)), entrypoint(1, 31))
        // overflow returns 0
        XCTAssertEqual(0, entrypoint(1, 32))
    }
    
    func testCompile__OCall2() throws {
        // Prepare function we'll call from JIT
        typealias _JitFunc = (@convention(c) (UInt16, Int32) -> Int32)
        let swiftFunc: _JitFunc = { (_ a: UInt16, _ b: Int32) in
            return Int32(Int16(bitPattern: a)) + b
        }
        let swiftFuncPtr = unsafeBitCast(swiftFunc, to: UnsafeMutableRawPointer.self)
        
        let f = prepareFunction(
            retType: .i32,
            findex: 0,
            regs: [.u16, .i32],
            args: [.u16],
            ops: [
                .OCall2(dst: 1, fun: 1, arg0: 0, arg1: 1),
                .ORet(ret: 1),
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
                            HLTypeFun_Depr(
                                args: Resolvable.array([.u16, .i32]),
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
        let res: Int32 = entrypoint(100, 156)
        
        // HL called Swift func. Swift func returned 145. HL returned the result it received.
        XCTAssertEqual(256, res)
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
                            HLTypeFun_Depr(
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
        
        let entrypoint: (@convention(c) () -> Int32) = try mem.buildEntrypoint(0)
        let entrypoint2: (@convention(c) () -> Int32) = try mem.buildEntrypoint(1)
        let res1: Int32 = entrypoint()
        let res2: Int32 = entrypoint2()
        
        XCTAssertEqual(152, res1)
        XCTAssertEqual(res1, res2)
    }
    
    func testCompile_OBool() throws {
        let storage = ModuleStorage(
            functions: [
                prepareFunction(
                    retType: .i32,
                    findex: 0,
                    regs: [.i32, .bool, .i32],
                    args: [],
                    ops: [
                        .OInt(dst: 0, ptr: 0),
                        .OInt(dst: 2, ptr: 0),
                        .OBool(dst: 1, value: 0),
                        // check the bool didn't overwrite the ints
                        .ORet(ret: 0)
                    ]
                ),
                prepareFunction(
                    retType: .i32,
                    findex: 1,
                    regs: [.i32, .bool, .i32],
                    args: [],
                    ops: [
                        .OInt(dst: 0, ptr: 0),
                        .OInt(dst: 2, ptr: 0),
                        .OBool(dst: 1, value: 0),
                        // check the bool didn't overwrite the ints
                        .ORet(ret: 2)
                    ]
                ),
                prepareFunction(
                    retType: .bool,
                    findex: 2,
                    regs: [.i32, .bool, .i32],
                    args: [],
                    ops: [
                        .OBool(dst: 1, value: 0),
                        .OInt(dst: 0, ptr: 0),
                        .OInt(dst: 2, ptr: 0),
                        // check the bool didn't overwrite the ints
                        .ORet(ret: 1)
                    ]
                ),
            ],
            ints: [
                Int32(bitPattern: UInt32.max)
            ]
        )
        let ctx = JitContext(storage: storage)
        let mem = OpBuilder(ctx: ctx)
        let sut = sut()
        try sut.compile(findex: 0, into: mem)
        try sut.compile(findex: 1, into: mem)
        try sut.compile(findex: 2, into: mem)
        
        let entrypoint: (@convention(c) () -> Int32) = try mem.buildEntrypoint(0)
        let entrypoint2: (@convention(c) () -> Int32) = try mem.buildEntrypoint(1)
        let entrypoint3: (@convention(c) () -> UInt8) = try mem.buildEntrypoint(2)
        
        XCTAssertEqual(-1, entrypoint())
        XCTAssertEqual(-1, entrypoint2())
        XCTAssertEqual(0, entrypoint3())
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
        _ = sut().appendPrologue(builder: mem)
        
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
    
    func testAppendStackInit_skipVoid() throws {
        let mem = builder()
        try sut().appendStackInit([.void], args: [.void], builder: mem, prologueSize: 0)
        XCTAssertEqual([], mem.lockAddressesAndBuild())
    }
    
    func testAppendStackInit_min16() throws {
        let _1_need16 = Array(repeating: HLTypeKind.i32, count: 1)
        let _4_need16 = Array(repeating: HLTypeKind.i32, count: 4)
        let _5_need32 = Array(repeating: HLTypeKind.i32, count: 5)
        let sut = sut()
        // 4 byte requirement should still be aligned to 16 byte boundary
        let mem1 = builder()
        try sut.appendStackInit(_1_need16, args: _1_need16, builder: mem1, prologueSize: 0)
        //mem1.hexPrint()
        XCTAssertEqual(
            [
                0xff, 0x43, 0x00, 0xd1,  // sub sp, sp, #16
                0xe0, 0x03, 0x00, 0xb8,  // str w0, [sp, #0]
            ],
            mem1.lockAddressesAndBuild()
        )
        
        // 16 byte requirement should not round to 32
        let mem2 = builder()
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
            mem2.lockAddressesAndBuild()
        )
        // 20 byte requirement should round to 32
        let mem3 = builder()
        try sut.appendStackInit(_5_need32, args: _5_need32, builder: mem3, prologueSize: 0)
        //mem3.hexPrint()
        XCTAssertEqual(
            [
                0xff, 0x83, 0x00, 0xd1,  // sub sp, sp, #32
                0xe0, 0x03, 0x00, 0xb8,  // str w0, [sp, #0]
                0xe1, 0x43, 0x00, 0xb8,  // str w1, [sp, #4]
                0xe2, 0x83, 0x00, 0xb8,  // str w2, [sp, #8]
                0xe3, 0xc3, 0x00, 0xb8,  // str w3, [sp, #12]
                0xe4, 0x03, 0x01, 0xb8,  // str w4, [sp, #16]
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
            builder: mem,
            prologueSize: 0
        )
        //mem.hexPrint()
        XCTAssertEqual(
            [
                0xff, 0x43, 0x00, 0xd1,  // sub sp, sp, #16
                0xe0, 0x03, 0x00, 0xb8,  // str w0, [sp, #0]
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
            mem.lockAddressesAndBuild()
        )
    }
    
    func testAppendStackInit_mismatchedRegs() throws {
        let mem = builder()
        
        XCTAssertThrowsError(
            try sut().appendStackInit([.i32], args: [.void], builder: mem, prologueSize: 0)
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
    
    func testCompile__OMul() throws {
        let storage = ModuleStorage(
            functions: [
                prepareFunction(
                    retType: .u8,
                    findex: 0,
                    regs: [
                        // args
                        .u8, .u8
                    ],
                    args: [
                        // args
                        .u8, .u8
                    ],
                    ops: [
                        .OMul(dst: 1, a: 0, b: 1),
                        .ORet(ret: 1),
                    ]
                ),
            ]
        )
        let ctx = JitContext(storage: storage)
        let mem = OpBuilder(ctx: ctx)
        let sut = sut(strip: false)
        try sut.compile(findex: 0, into: mem)
        let entrypoint: (@convention(c) (Int8, Int8) -> Int8) = try mem.buildEntrypoint(0)
        XCTAssertEqual(4, entrypoint(1, 4))
        XCTAssertEqual(4, entrypoint(2, 2))
        XCTAssertEqual(36, entrypoint(4, 9))
        XCTAssertEqual(0, entrypoint(1, 0))
        XCTAssertEqual(-10, entrypoint(-5, 2))
    }
    
    func testCompile__OXor() throws {
        let storage = ModuleStorage(
            functions: [
                prepareFunction(
                    retType: .u8,
                    findex: 0,
                    regs: [
                        // args
                        .u8, .u8
                    ],
                    args: [
                        // args
                        .u8, .u8
                    ],
                    ops: [
                        .OXor(dst: 1, a: 0, b: 1),
                        .ORet(ret: 1),
                    ]
                ),
            ]
        )
        let ctx = JitContext(storage: storage)
        let mem = OpBuilder(ctx: ctx)
        let sut = sut(strip: false)
        try sut.compile(findex: 0, into: mem)
        let entrypoint: (@convention(c) (UInt8, UInt8) -> UInt8) = try mem.buildEntrypoint(0)
        XCTAssertEqual(0b10011111, entrypoint(0b11010110, 0b01001001))
    }
    
    func testCompile__OToInt__i32_to_i64() throws {
        let storage = ModuleStorage(
            functions: [
                prepareFunction(
                    retType: .i64,
                    findex: 0,
                    regs: [.i32, .i64],
                    args: [.i32],
                    ops: [
                        .OToInt(dst: 1, src: 0),
                        .ORet(ret: 1),
                    ]
                ),
                prepareFunction(
                    retType: .i32,
                    findex: 1,
                    regs: [.i64, .i32],
                    args: [.i64],
                    ops: [
                        .OToInt(dst: 1, src: 0),
                        .ORet(ret: 1),
                    ]
                )
            ]
        )
        let ctx = JitContext(storage: storage)
        let mem = OpBuilder(ctx: ctx)
        let sut = sut(strip: false)
        let count = 2
        try (0..<count).forEach { try sut.compile(findex: Int32($0), into: mem) }
        
        let entrypoint_32t64: (@convention(c) (Int32) -> Int64) = try mem.buildEntrypoint(0)
        XCTAssertEqual(
            0b10000000_10000000_10000000_10000000,
            entrypoint_32t64(Int32(bitPattern: 0b10000000_10000000_10000000_10000000))
        )
        
        let entrypoint_64t32: (@convention(c) (Int64) -> Int32) = try mem.buildEntrypoint(1)
        XCTAssertEqual(
            Int32(bitPattern: 0b10000000_10000000_10000000_10000000),
            entrypoint_64t32(0b10000000_10000000_10000000_10000000_10000000)
        )
    }
    
    // MARK: testing v2
    
    func prepareFunction2(
        retType: HLTypeKind,
        findex: RefFun,
        regs: HLTypeKinds,
        args: HLTypeKinds,
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
    
    func prepareContext(compilables: [any Compilable2]) throws -> CCompatJitContext {
        let tm = TestJitModule(compilables)
        assert(tm.ntypes > 0)
        return try CCompatJitContext(ctx: tm)
    }
    
    func testCompile__OMul_v2() throws {
        let funcs = [
                prepareFunction2(
                    retType: .u8,
                    findex: 0,
                    regs: [
                        // args
                        .u8, .u8
                    ],
                    args: [
                        // args
                        .u8, .u8
                    ],
                    ops: [
                        .OMul(dst: 1, a: 0, b: 1),
                        .ORet(ret: 1),
                    ]
                ),
            ]
        let ctx = try prepareContext(compilables: funcs)
        
        let mem = CpuOpBuffer()
        let sut = M1Compiler2(ctx: ctx, stripDebugMessages: false)
        try sut.compile(findex: 0, into: mem)
        
        //
        let mapper = BufferMapper(ctx: ctx, buffer: mem)
        let mappedMem = try mapper.getMemory()
        
        try mem.hexPrint()
        
        let callable = try ctx.getCallable(findex: 0)!.address.value
        print("Calling \(callable)")
        
//
        let entrypoint = unsafeBitCast(callable, to: (@convention(c) (Int8, Int8) -> Int8).self)
        XCTAssertEqual(4, entrypoint(1, 4))
        XCTAssertEqual(4, entrypoint(2, 2))
        XCTAssertEqual(36, entrypoint(4, 9))
        XCTAssertEqual(0, entrypoint(1, 0))
        XCTAssertEqual(-10, entrypoint(-5, 2))
    }
}

struct TestDummyLinkableAddress : LinkableAddress {
    func setOffset(_ offset: swiftasm.ByteCount) { fatalError("Don't use the test dummy") }
    var hasOffset: Bool { fatalError("Don't use the test dummy") }
    var value: UnsafeMutableRawPointer { fatalError("Don't use the test dummy") }
    func isEqual(_ to: any swiftasm.MemoryAddress) -> Bool { fatalError("Don't use the test dummy") }
}
