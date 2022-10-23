import XCTest

@testable import swiftasm

final class CompileMod1Tests: XCTestCase {
    static var code: UnsafePointer<HLCode_CCompat>?
    static var ctx: JitContext? = nil
    
    var code: UnsafePointer<HLCode_CCompat> { Self.code! }
    var ctx: JitContext { Self.ctx! }
    
    override class func setUp() {
        LibHl.hl_global_init()
        let mod1 = Bundle.module.url(forResource: "mod1", withExtension: "hl")!.path
        
        let code = UnsafePointer(LibHl.load_code(mod1))
        let fakeMod = ModuleStorage(code.pointee)
        
        self.ctx = JitContext(storage: fakeMod, hlcode: code)
        self.code = code
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

    func testCompileAll() throws {
        
        let sut = M1Compiler()
        let mem = OpBuilder(ctx: ctx)
        
        for fix in 0..<code.pointee.nfunctions {
            print("Compiling \(fix)")
//            try sut.compile(findex: Int32(fix), into: mem)
        }
        
    }
}

