import XCTest

@testable import swiftasm

extension String {
    static func testResourcePath(_ file: String) -> String {
        URL(fileURLWithPath: FileManager.default.currentDirectoryPath).appendingPathComponent("TestData", isDirectory: true).appendingPathComponent(file, isDirectory: false).path
    }
}

final class LibHlTests: XCTestCase {
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
    
    func test__hl_to_utf16__appendZero() throws {
        let string = "Hello World"


        let ptr = LibHl.hl_to_utf16(string)
        let arr = Array(UnsafeBufferPointer<UInt16>(start: ptr, count: string.count + 1))
        let reloadedString = String(utf16CodeUnits: arr, count: string.count + 1)
        let len = LibHl.hl_ucs2length(ptr, 0)
        XCTAssertEqual(len, 11)
        XCTAssertEqual(reloadedString.count, 12)
        XCTAssertEqual(reloadedString, "\(string)\0")
        XCTAssertEqual(arr, [
            0x48, 0x65, 0x6c, 0x6c, 0x6f, 0x20, 0x57, 0x6f,
            0x72, 0x6c, 0x64, 0x00
        ])
    }
}

