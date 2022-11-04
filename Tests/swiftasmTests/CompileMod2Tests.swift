import XCTest

@testable import swiftasm

final class CompileMod2Tests: XCTestCase {
    static var code: UnsafePointer<HLCode_CCompat>?
    static var ctx: JitContext? = nil
    
    static var context: MainContext? = nil
    
    var code: UnsafePointer<HLCode_CCompat> { Self.code! }
    var ctx: JitContext { Self.ctx! }
    
    static let TEST_ARRAY_LENGTH_IX = 44
    static let TEST_TRAP_IX = 32
    static let TEST_TRAP__CALLS = [ 5, 327, 40 ]
    
    static let TEST_GET_ARRAY_INT32_IX = 46
    static let TEST_GET_ARRAY_INT64_IX = 47
    static let TEST_GET_ARRAY__CALLS = [ 5, 327, 40, 48, 49 ]
    
    override class func setUp() {
        LibHl.hl_global_init()
        //
        let mod2 = Bundle.module.url(forResource: "mod2", withExtension: "hl")!.path
        let code = UnsafePointer(LibHl.load_code(mod2))
        self.context = MainContext(code: code, module: nil, ret: nil, file: nil, file_time: 0)
        self.code = code
        
        self.context!.module = LibHl.hl_module_alloc(self.context!.code);
        guard let m = self.context?.module else {
            fatalError("nil module")
        }
            
        let res = LibHl.hl_module_init(m, false)
        guard res == 1 else {
            fatalError("Failed to init module (got \(res))")
        }

                
        //
        let fakeMod = ModuleStorage(code.pointee)
        self.ctx = JitContext(storage: fakeMod, hlcode: code)
    }

    override class func tearDown() {
        LibHl.hl_global_free()
    }
    
    func testCompile__testGetSetArray() throws {
        let sut = sut(strip: true)
        let mem = OpBuilder(ctx: ctx)
        let fix32 = Self.TEST_GET_ARRAY_INT32_IX
        let fix64 = Self.TEST_GET_ARRAY_INT64_IX

        for fix in Self.TEST_GET_ARRAY__CALLS {
            try sut.compile(findex: Int32(fix), into: mem)
        }
        try sut.compile(findex: Int32(fix32), into: mem)
        try sut.compile(findex: Int32(fix64), into: mem)

        let entrypoint32: (@convention(c) (Int32, Int32, Int32) -> Int32) = try mem.buildEntrypoint(fix32)
        let entrypoint64: (@convention(c) (Int32, Int64, Int32) -> Int64) = try mem.buildEntrypoint(fix64)
        
        XCTAssertEqual(1239, entrypoint32(10, 1234, 5))
        XCTAssertEqual(5681, entrypoint64(10, 5678, 3))
    }
    
    func testCompile__testArrayLength() throws {
        let sut = sut(strip: true)
        let mem = OpBuilder(ctx: ctx)
        let fix = Self.TEST_ARRAY_LENGTH_IX
        
        try sut.compile(findex: Int32(fix), into: mem)

        let entrypoint: (@convention(c) (Int32) -> Int32) = try mem.buildEntrypoint(fix)

        XCTAssertEqual(5, entrypoint(5))
    }
    
    /** Test traps
     
            fn testTrap@32 () -> (i32)@83 (6 regs, 10 ops)
            reg0  dynamic@9
            reg1  void@0
            reg2  haxe.Exception@29
            reg3  String@13
            reg4  haxe.Exception@29
            reg5  i32@3
            Main.hx:27    0: Trap        try reg0 jump to 8
            Main.hx:28    1: New         reg2 = new haxe.Exception@29
            Main.hx:28    2: GetGlobal   reg3 = global@8
            Main.hx:28    3: Null        reg4 = null
            Main.hx:28    4: Null        reg0 = null
            Main.hx:28    5: Call4       reg1 = __constructor__@40(reg2, reg3,reg4, reg0)
            Main.hx:28    6: Throw       throw reg2
            Main.hx:28    7: EndTrap     catch reg1
            Main.hx:30    8: Int         reg5 = 1
            Main.hx:30    9: Ret         reg5
     */
    func testCompile__testTrap() throws {
        let sut = M1Compiler()
        let mem = OpBuilder(ctx: ctx)
        
        for fix in Self.TEST_TRAP__CALLS {
            try sut.compile(findex: Int32(fix), into: mem)
        }
        try sut.compile(findex: Int32(Self.TEST_TRAP_IX), into: mem)

        let entrypoint: (@convention(c) () -> Int32) = try mem.buildEntrypoint(0)
        
        XCTAssertEqual(1, entrypoint())
    }
    
    /** Test parsing a type that refers to itself in a field (See `__previousException`)
     
            > t 29
            29 : haxe.Exception
            global: 8
            fields:
                __exceptionMessage: String@13
                __nativeStack: array@11
                __skipStack: i32@3
                __nativeException: dynamic@9
                __previousException: haxe.Exception@29
            protos:
                unwrap: fn unwrap@33 (haxe.Exception) -> (dynamic)@178 (0)
                toString: fn toString@34 (haxe.Exception) -> (String)@179 (1)
                get_message: fn get_message@35 (haxe.Exception) -> (String)@179 (-1)
                get_native: fn get_native@36 (haxe.Exception) -> (dynamic)@178 (-1)
                __string: fn __string@37 (haxe.Exception) -> (bytes)@180 (-1)
            bindings:
     */
    func testParseRecursiveType() throws {
        guard let excT = ctx.hlcode?.pointee.getType(29) else {
            fatalError("Type 29 missing")
        }
        let hlType = HLType(excT)
        let resolvable = Resolvable.type(fromUnsafe: excT)

        XCTAssertEqual(hlType.objData!.name.value, "haxe.Exception")
        XCTAssertNotNil(resolvable.memory)
    }
    
    /**
        This tests proper GetI16 behaviour in the wild. Example disassembly (indexes might be wrong):
     
            30 : fn testGetUI16@30 (i32) -> (i32)@81 (5 regs, 6 ops)
            reg0  i32@3
            reg1  haxe.io.Bytes@25
            reg2  String@13
            reg3  bytes@14
            reg4  i32@3
            Main.hx:11    0: GetGlobal   reg2 = global@5
            Main.hx:11    1: Call1       reg1 = ofHex@28(reg2)
            Main.hx:12    2: NullCheck   if reg1 == null throw exc
            Main.hx:12    3: Field       reg3 = reg1.b
            Main.hx:13    4: GetI16 { dst: Reg(4), bytes: Reg(3), index: Reg(0) }
            Main.hx:13    5: Ret         reg4
     */
    func testCompileFn233__testGetUI16() throws {
        let sut = M1Compiler()
        let mem = OpBuilder(ctx: ctx)
        
        try sut.compile(findex: 231, into: mem)
        try sut.compile(findex: 233, into: mem)

        let entrypoint: (@convention(c) (Int32) -> Int32) = try mem.buildEntrypoint(0)
        
        XCTAssertEqual(0x1211, entrypoint(0))
        XCTAssertEqual(0x1312, entrypoint(1))
        XCTAssertEqual(0x1413, entrypoint(2))
        XCTAssertEqual(0x0014, entrypoint(3))
    }
    
    /**
        This tests proper GetI8 behaviour in the wild.
     
            27 : fn testGetUI8@27 (i32) -> (i32)@81 (5 regs, 6 ops)
            reg0  i32@3
            reg1  haxe.io.Bytes@25
            reg2  String@13
            reg3  bytes@14
            reg4  i32@3
            Main.hx:5     0: GetGlobal   reg2 = global@5
            Main.hx:5     1: Call1       reg1 = ofHex@28(reg2)
            Main.hx:6     2: NullCheck   if reg1 == null throw exc
            Main.hx:6     3: Field       reg3 = reg1.b
            Main.hx:7     4: GetI8 { dst: Reg(4), bytes: Reg(3), index: Reg(0) }
            Main.hx:7     5: Ret         reg4
     */
    func testCompileFn230__testGetUI8() throws {
        let sut = M1Compiler()
        let mem = OpBuilder(ctx: ctx)
        
        try sut.compile(findex: 231, into: mem)
        try sut.compile(findex: 230, into: mem)

        let entrypoint: (@convention(c) (Int32) -> Int32) = try mem.buildEntrypoint(0)
        
        XCTAssertEqual(0x11, entrypoint(0))
        XCTAssertEqual(0x12, entrypoint(1))
        XCTAssertEqual(0x13, entrypoint(2))
        XCTAssertEqual(0x14, entrypoint(3))
    }
    
    /** Test field access.
     
            280: fn testFieldAccess@280 () -> (i32)@88 (3 regs, 5 ops)
            reg0  Path@172
            reg1  i32@3
            reg2  void@0
            Main.hx:43    0: New         reg0 = new Path@172
            Main.hx:43    1: Int         reg1 = 2
            Main.hx:43    2: Call2       reg2 = __constructor__@229(reg0, reg1)
            Main.hx:44    3: Field       reg1 = reg0.test
            Main.hx:44    4: Ret         reg1
     */
    
    func testCompileFn280__testFieldAccess() throws {
        let sut = M1Compiler(stripDebugMessages: false)
        let mem = OpBuilder(ctx: ctx)
        
        try sut.compile(findex: 229, into: mem)
        try sut.compile(findex: 280, into: mem)

        let entrypoint: (@convention(c) () -> Int32) = try mem.buildEntrypoint(280)
        
        XCTAssertEqual(2, entrypoint())
    }
}

