import XCTest

@testable import swiftasm

final class CompileMod1Tests: XCTestCase {
    static var code: UnsafePointer<HLCode_CCompat>?
    static var ctx: JitContext? = nil
    
    static var context: MainContext? = nil
    
    var code: UnsafePointer<HLCode_CCompat> { Self.code! }
    var ctx: JitContext { Self.ctx! }
    
    override class func setUp() {
        LibHl.hl_global_init()
        //
        let mod1 = Bundle.module.url(forResource: "mod1", withExtension: "hl")!.path
        let code = UnsafePointer(LibHl.load_code(mod1))
        self.context = MainContext(code: code, module: nil, ret: nil, file: nil, file_time: 0)
        self.code = code
        
        self.context!.module = UnsafeRawPointer(LibHl.hl_module_alloc(self.context!.code));
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
    
    func testParseFn236() throws {
        let f: UnsafePointer<HLNative_CCompat>! = code.pointee.findNative(236)
        XCTAssertNotNil(f)
        
        XCTAssertEqual(f.pointee.type.kind, .fun)
        XCTAssertEqual(
            f.pointee.args.map { $0.value.kind },
            [.bytes, .i32, .i32])
        XCTAssertEqual(f.pointee.ret.value.kind, .bytes)
    }
    
    func testParseFn0() throws {
        let f: UnsafePointer<HLFunction_CCompat>! = code.pointee.findFunction(0)
        XCTAssertNotNil(f)
        
        XCTAssertEqual(f.pointee.typePtr!.pointee.kind, .fun)
        XCTAssertEqual(
            f.pointee.args.map { $0.value.kind },
            [.obj])
        XCTAssertEqual(
            f.pointee.regs.map { $0.value.kind },
            [.obj, .bytes, .i32, .i32, .obj])
        XCTAssertEqual(f.pointee.ret.value.kind, .obj)
    }
    
    func testParseFn237() throws {
        let f: UnsafePointer<HLNative_CCompat>! = code.pointee.findNative(237)
        XCTAssertNotNil(f)
        
        XCTAssertEqual(f.pointee.typePtr.pointee.kind, .fun)
        XCTAssertEqual(
            f.pointee.args.map { $0.value.kind },
            [.bytes, .i32, .i32])
        XCTAssertEqual(f.pointee.ret.value.kind, .bytes)
    }
    
    func testCompileFn0() throws {
        let sut = M1Compiler()
        let mem = OpBuilder(ctx: ctx)
        
        try sut.compile(findex: 0, into: mem)
    }
    
    func testCompileFn2() throws {
        let sut = M1Compiler()
        let mem = OpBuilder(ctx: ctx)
        
        try sut.compile(findex: 2, into: mem)
    }

    func testCompileAll() throws {
        
        let sut = M1Compiler()
        let mem = OpBuilder(ctx: ctx)
        
        for fix in 0..<code.pointee.nfunctions {
            print("Compiling \(fix)")
            try sut.compile(findex: Int32(fix), into: mem)
        }
        
    }
}

