import XCTest

@testable import swiftasm

final class CompileMod2Tests: XCTestCase {
    static var code: UnsafePointer<HLCode_CCompat>?
    static var ctx: JitContext? = nil
    
    static var context: MainContext? = nil
    
    var code: UnsafePointer<HLCode_CCompat> { Self.code! }
    var ctx: JitContext { Self.ctx! }
    
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

