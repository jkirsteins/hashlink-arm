import XCTest

@testable import swiftasm

final class CompilerM1Tests: XCTestCase {
    func testPrologue() throws {
        let buffer = ByteBuffer(incrementSize: 4)
        let compiler = M1Compiler()
        try compiler.prologue(in: buffer)

        // 0: fd 7b bf a9  	stp	x29, x30, [sp, #-16]!
        // 4: fd 03 00 91  	mov	x29, sp
        XCTAssertEqual(
            buffer.buffer,
            [0xfd, 0x7b, 0xbf, 0xa9,
             0xfd, 0x03, 0x00, 0x91]
        )
    }

    func testEpilogue() throws {
        let buffer = ByteBuffer(incrementSize: 4)
        let compiler = M1Compiler()
        try compiler.epilogue(in: buffer)

        // 0: fd 7b c1 a8   # ldp x29, x30, [sp], #16
        XCTAssertEqual(
            buffer.buffer,
            [0xfd, 0x7b, 0xc1, 0xa8]
        )
    }
}
