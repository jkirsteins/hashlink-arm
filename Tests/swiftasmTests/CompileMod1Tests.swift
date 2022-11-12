import XCTest

@testable import swiftasm

final class CompileMod1Tests: RealHLTestCase {
    static var ctx: CCompatJitContext?
    override class func getCtx() -> CCompatJitContext? {
        ctx
    }
    override class func setCtx(_ ctx: CCompatJitContext?) {
        self.ctx = ctx
    }
    
    override class var HL_FILE: String { "mod1" }
    
    private func compileAndLink(_ fix: [Int]) throws -> UnsafeMutableRawPointer {
        let mem = try compileNoLink(fix)

        let mapper = BufferMapper(ctx: ctx, buffer: mem)
        let mappedMem = try mapper.getMemory()

        return mappedMem
    }
    
    private func compileNoLink(_ fix: [Int], in mem: CpuOpBuffer = CpuOpBuffer()) throws -> CpuOpBuffer {
        let sut = M1Compiler2(ctx: ctx, stripDebugMessages: false)
        
        try fix.forEach { try sut.compile(findex: $0, into: mem) }

        return mem
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
        _ = try compileNoLink([0])
    }
//
//    func testCompileFn2() throws {
//        _ = try compileNoLink([2])
//    }
//
//    func testCompileAll() throws {
//
//        let sut = M1Compiler()
//        let mem = OpBuilder(ctx: ctx)
//
//        for fix in 0..<code.pointee.nfunctions {
//            print("Compiling \(fix)")
//            try sut.compile(findex: Int32(fix), into: mem)
//        }
//
//    }
}

