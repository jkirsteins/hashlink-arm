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
        This tests proper GetI16 behaviour in the wild.
     
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
    func testCompileFn30__testGetUI16() throws {
        let sut = M1Compiler()
        let mem = OpBuilder(ctx: ctx)
        
        try sut.compile(findex: 27, into: mem)

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
    func testCompileFn27__testGetUI8() throws {
        let sut = M1Compiler()
        let mem = OpBuilder(ctx: ctx)
        
        try sut.compile(findex: 27, into: mem)

        let entrypoint: (@convention(c) (Int32) -> Int32) = try mem.buildEntrypoint(0)
        
        XCTAssertEqual(0x11, entrypoint(0))
        XCTAssertEqual(0x12, entrypoint(1))
        XCTAssertEqual(0x13, entrypoint(2))
        XCTAssertEqual(0x14, entrypoint(3))
    }
}

