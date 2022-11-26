import XCTest

@testable import swiftasm

class RealHLTestCase : XCTestCase {
    var HL_FILE: String { fatalError("Override HL_FILE to point to a file in TestResources") }
        
    static var logger = LoggerFactory.create(RealHLTestCase.self)
    
    var ctx: CCompatJitContext! = nil
    
    var code: UnsafePointer<HLCode_CCompat> {
        self.ctx!.mainContext.pointee.code!
    }
    
    func sut(strip: Bool) throws -> M1Compiler2 {
        M1Compiler2(ctx: self.ctx!, stripDebugMessages: strip)
    }
    
    override func setUp() {
        Self.logger.info("Setting up HL file for testing: \(self.HL_FILE)")
        let mod = Bundle.module.url(forResource: HL_FILE, withExtension: "hl")!.path
        self.ctx = try! Bootstrap.start2(mod, args: [])
        
        // guard against infinite loops
        self.executionTimeAllowance = 2
    }

    override func tearDown() {
        Self.logger.info("Tearing down HL file after testing: \(self.HL_FILE)")
        Bootstrap.stop(ctx: ctx!)
        ctx = nil
    }
}

final class CompileMod2Tests: RealHLTestCase {
    
    override var HL_FILE: String { "mod2" }
        
    func _compileDeps(strip: Bool, mem: CpuOpBuffer = CpuOpBuffer(), _ ixs: [RefFun]) throws -> CpuOpBuffer {
        let compiler = try sut(strip: strip)
        for fix in ixs {
            try compiler.compile(findex: fix, into: mem)
        }
        return mem
    }
    
    func _compileAndLink(strip: Bool, mem: CpuOpBuffer = CpuOpBuffer(), _ ixs: [RefFun], _ callback: (UnsafeMutableRawPointer) throws->()) throws {
        let buff = try self._compileDeps(strip: strip, ixs)
        let mapper = BufferMapper(ctx: self.ctx, buffer: buff)
        let mem = try mapper.getMemory()
        
        try callback(mem)
        
        try mapper.freeMemory()
    }
    
    func testCompile__testGetSetField() throws {
        let sutFix = 56
        try _compileAndLink(
            strip: false,
            [
                sutFix,
                27
            ]
        ) {
            mem in
            
            try mem.jit(ctx: ctx, fix: sutFix) {
                (entrypoint: (@convention(c) (Int32) -> Int32)) in
                
                XCTAssertEqual(46, entrypoint(23))
            }
        }
    }

    func testCompile__testGetSetArray__32() throws {
        typealias _JitFunc = (@convention(c) (Int32, Int64, Int32) -> Int64)
        let sutFix = 51
        try _compileAndLink(
            strip: false,
            [
                sutFix
            ]
        ) {
            mem in
            
            try mem.jit(ctx: ctx, fix: sutFix) {
                (entrypoint: _JitFunc) in
                
                XCTAssertEqual(1239, entrypoint(10, 1234, 5))
            }
        }
    }
    
    func testCompile__testGetSetArray__64hl() throws {
        typealias _JitFunc = (@convention(c) (Int32, Int64, Int32) -> Int64)
        let sutFix = 55
        try _compileAndLink(
            strip: false,
            [
                sutFix
            ]
        ) {
            mem in
            
            try mem.jit(ctx: ctx, fix: sutFix) {
                (entrypoint: _JitFunc) in
                
                XCTAssertEqual(5681, entrypoint(10, 5678, 3))
            }
        }
    }
    
    func testCompile__testGetSetArray__64haxe() throws {
        typealias _JitFunc = (@convention(c) (Int32, UnsafeRawPointer, Int32) -> UnsafeRawPointer)
        
        struct _haxeInt64 {
            let tptr: UnsafeRawPointer
            let high: Int32
            let low: Int32
        }
        
        let int64In: Int64 = 5678
        let haxeInt64 = _haxeInt64(
            tptr: code.pointee.getType(87),
            high: Int32(truncatingIfNeeded: (int64In >> 32)),
            low: Int32(truncatingIfNeeded: int64In)
        )
        
        let sutFix = 52
        
        try _compileAndLink(
            strip: false,
            [
                // deps
                54, 27, 53,
                // function under test
                sutFix
            ]
        ) {
            mem in
            
            try mem.jit(ctx: ctx, fix: sutFix) {
                (entrypoint: _JitFunc) in
        
                withUnsafePointer(to: haxeInt64) { haxeInt64In in
                    let haxeInt64_out = entrypoint(10, haxeInt64In, 3).bindMemory(to: _haxeInt64.self, capacity: 1)
                    var int64Out: Int64 = 0
                    int64Out = int64Out | (Int64(haxeInt64_out.pointee.high) &<< 32)
                    int64Out = int64Out | (Int64(haxeInt64_out.pointee.low))
                    XCTAssertEqual(5681, int64Out)
                }
            }
        }
    }
    
    func testCompile__testArrayLength() throws {
        typealias _JitFunc = (@convention(c) (Int32) -> Int32)
        let sutFix = 49
        try _compileAndLink(
            strip: false,
            [
                sutFix
            ]
        ) {
            mem in
            
            try mem.jit(ctx: ctx, fix: sutFix) {
                (entrypoint: _JitFunc) in
         
                XCTAssertEqual(5, entrypoint(5))
            }
        }
    }
    
    func testCompile__indexOf() throws {
        struct _String {
            let t: UnsafePointer<HLType_CCompat>
            let bytes: UnsafePointer<CChar16>
            let length: Int32
        }
        
        typealias _JitFunc = (@convention(c) (OpaquePointer, OpaquePointer, OpaquePointer) -> Int32)
        let sutFix = 5
        try _compileAndLink(
            strip: false,
            [
                sutFix
            ]
        ) {
            mem in
            
            let f = "First String"
            let s = "not_found"
            let fstr = (f + "\0").data(using: .utf16LittleEndian)!
            let sstr = (s + "\0").data(using: .utf16LittleEndian)!
            let fstrPtr: UnsafeMutableBufferPointer<UInt8> = .allocate(capacity: fstr.count)
            let sstrPtr: UnsafeMutableBufferPointer<UInt8> = .allocate(capacity: sstr.count)
            defer { fstrPtr.deallocate() }
            defer { sstrPtr.deallocate() }
            _ = fstrPtr.initialize(from: fstr)
            _ = sstrPtr.initialize(from: sstr)
            
            let indexOfType = try ctx.getType(181)
            let nullType = indexOfType.funProvider!.argsProvider[2] as! UnsafePointer<HLType_CCompat>
            
            
            let t = try ctx.getType(13) // string
            var strA = _String(
                t: .init(OpaquePointer(t.ccompatAddress)),
                bytes: .init(OpaquePointer(fstrPtr.baseAddress!)),
                length: Int32(f.count))
            var strB = _String(
                t: .init(OpaquePointer(t.ccompatAddress)),
                bytes: .init(OpaquePointer(sstrPtr.baseAddress!)),
                length: Int32(s.count))
            var nullD = vdynamic(t: nullType, union: nil)
//
            let c = try ctx.getCallable(findex: sutFix)
            let entrypoint = unsafeBitCast(c!.address.value, to: _JitFunc.self)
            withUnsafeMutablePointer(to: &strA) { strAPtr in
                withUnsafeMutablePointer(to: &strB) { strBPtr in
                    withUnsafeMutablePointer(to: &nullD) { nullDPtr in
                        let res = entrypoint(.init(strAPtr), .init(strBPtr), .init(nullDPtr))
                        XCTAssertEqual(-1, res)
                    }
                }
            }
        }
    }
    
    /// Test traps
    func testCompile__testTrap() throws {
        typealias _JitFunc = (@convention(c) () -> Int32)
        let sutFix = 37
        try _compileAndLink(
            strip: false,
            [
                sutFix,
                
                45, 342, 5
            ]
        ) {
            mem in
            
            let c = try ctx.getCallable(findex: sutFix)
            let entrypoint = unsafeBitCast(c!.address.value, to: _JitFunc.self)
            let res = entrypoint()
            XCTAssertEqual(1, res)
        }
    }
    
    /// Test parsing a type that refers to itself in a field (See `__previousException`)
    func testParseRecursiveType() throws {
        let excT = try ctx.getType(30)
        
        XCTAssertEqual(excT.objProvider?.nameProvider.stringValue, "haxe.Exception")
        XCTAssertNotNil(excT.ccompatAddress)
    }
    
    ///
    func testCompile_testGetUI16() throws {
        typealias _JitFunc = (@convention(c) (Int32) -> Int32)
        let sutFix = 36
        try _compileAndLink(
            strip: false,
            [
                33, 44, 34, 3, 41, 340, 47, 14, 296, 45, 342, 5,
                sutFix
            ]
        ) {
            mem in
            
            let callable = try ctx.getCallable(findex: sutFix)
            let entrypoint = unsafeBitCast(callable!.address.value, to: _JitFunc.self)
            
            var res = entrypoint(0)
            XCTAssertEqual(res, 0x1211)
            
            res = entrypoint(1)
            XCTAssertEqual(res, 0x1312)
            
            res = entrypoint(2)
            XCTAssertEqual(res, 0x1413)
            
            res = entrypoint(3)
            XCTAssertEqual(res, 0x0014)
        }
    }
    
    /// This tests proper GetI8 behaviour in the wild.
    func testCompile__testGetUI8() throws {
        typealias _JitFunc =  (@convention(c) (Int32) -> Int32)
        let sutFix = 31
        try _compileAndLink(
            strip: false,
            [
                33, 34, 44, 3, 41, 47, 340, 296, 14, 45, 342, 5,
                sutFix
            ]
        ) {
            mem in
            
            let callable = try ctx.getCallable(findex: sutFix)
            let entrypoint = unsafeBitCast(callable!.address.value, to: _JitFunc.self)
            
            
//            var res = entrypoint(0)
//            XCTAssertEqual(res, 0x11)
//
//            res = entrypoint(1)
//            XCTAssertEqual(res, 0x12)
//
//            res = entrypoint(2)
//            XCTAssertEqual(res, 0x13)
            
            var res = entrypoint(3)
            XCTAssertEqual(res, 0x14)
        }
    }
    
    func testCompile__charCodeAt() throws {
        // fn charCodeAt@3 (String, i32) -> (null<i32>)@178 (6 regs, 10 ops)

        struct _String {
            let t: UnsafePointer<HLType_CCompat>
            let bytes: UnsafePointer<CChar16>
            let length: Int32
        }
        
        typealias _JitFunc =  (@convention(c) (OpaquePointer, Int32) -> OpaquePointer)
        let sutFix = 3
        try _compileAndLink(
            strip: false,
            [
                sutFix
            ]
        ) {
            mem in
            
            let strType = try ctx.getType(13)   // string type
            
            let str = "11121314"
            let data = str.data(using: .utf16LittleEndian)!
            let dataPtr: UnsafeMutableBufferPointer<UInt8> = .allocate(capacity: data.count)
            dataPtr.initialize(from: data)
            defer { dataPtr.deallocate() }
            
            print("---")
            print(UnsafePointer<UInt16>(OpaquePointer(dataPtr.baseAddress!.advanced(by: 0))).pointee)
            print(UnsafePointer<UInt16>(OpaquePointer(dataPtr.baseAddress!.advanced(by: 1))).pointee)
            print(UnsafePointer<UInt16>(OpaquePointer(dataPtr.baseAddress!.advanced(by: 2))).pointee)
            print(UnsafePointer<UInt16>(OpaquePointer(dataPtr.baseAddress!.advanced(by: 3))).pointee)
            print(UnsafePointer<UInt16>(OpaquePointer(dataPtr.baseAddress!.advanced(by: 4))).pointee)
            print(UnsafePointer<UInt16>(OpaquePointer(dataPtr.baseAddress!.advanced(by: 5))).pointee)
            print(UnsafePointer<UInt16>(OpaquePointer(dataPtr.baseAddress!.advanced(by: 6))).pointee)
            print(UnsafePointer<UInt16>(OpaquePointer(dataPtr.baseAddress!.advanced(by: 7))).pointee)
            
            XCTAssertEqual(dataPtr.count, 16)
            
            let strObj = _String(
                t: .init(OpaquePointer(strType.ccompatAddress)),
                bytes: .init(OpaquePointer(dataPtr.baseAddress!)),
                length: Int32(str.count)
            )
            
            let callable = try ctx.getCallable(findex: sutFix)
            let entrypoint = unsafeBitCast(callable!.address.value, to: _JitFunc.self)
            
            withUnsafePointer(to: strObj) { strObjPtr in
                
                var res: UnsafePointer<vdynamic> = .init(entrypoint(.init(strObjPtr), 0))
                XCTAssertEqual(Character(UnicodeScalar(Int(res.pointee.i))!), "1")
                
                res = .init(entrypoint(.init(strObjPtr), 1))
                XCTAssertEqual(Character(UnicodeScalar(Int(res.pointee.i))!), "1")
                
                res = .init(entrypoint(.init(strObjPtr), 2))
                XCTAssertEqual(Character(UnicodeScalar(Int(res.pointee.i))!), "1")
                
                res = .init(entrypoint(.init(strObjPtr), 3))
                XCTAssertEqual(Character(UnicodeScalar(Int(res.pointee.i))!), "2")
                
                res = .init(entrypoint(.init(strObjPtr), 4))
                XCTAssertEqual(Character(UnicodeScalar(Int(res.pointee.i))!), "1")
                
                res = .init(entrypoint(.init(strObjPtr), 5))
                XCTAssertEqual(Character(UnicodeScalar(Int(res.pointee.i))!), "3")
                
                res = .init(entrypoint(.init(strObjPtr), 6))
                XCTAssertEqual(Character(UnicodeScalar(Int(res.pointee.i))!), "1")
                
                res = .init(entrypoint(.init(strObjPtr), 7))
                XCTAssertEqual(Character(UnicodeScalar(Int(res.pointee.i))!), "4")
            }
        }
    }
    
    func testCompile__testGetUI8_2() throws {
        typealias _JitFunc =  (@convention(c) () -> Int32)
        let sutFix = 35
        try _compileAndLink(
            strip: false,
            [
                32, 33, 44, 34, 3, 41, 47, 340, 14, 45, 296, 342, 5,
                sutFix
            ]
        ) {
            mem in
            
            try mem.jit(ctx: ctx, fix: sutFix) {
                (entrypoint: _JitFunc) in
                
                XCTAssertEqual(entrypoint(), 336794129)
            }
        }
    }
    
    func testCompile__testTrace() throws {
        typealias _JitFunc =  (@convention(c) () -> ())
        let sutFix = 58
        try _compileAndLink(
            strip: false,
            [
                // deps
                
                // function under test
                sutFix
            ]
        ) {
            mem in
            
            try mem.jit(ctx: ctx, fix: sutFix) {
                (entrypoint: _JitFunc) in
                
                entrypoint()
            }
        }
    }
    
    func testCompile__testFieldClosure() throws {
        typealias _JitFunc =  (@convention(c) (Int32) -> (Int32))
        let sutFix = 255
        try _compileAndLink(
            strip: false,
            [
                // NOTE: function order is important for coverage
                28, // this references OInstanceClosure before the dependency is compiled
                
                sutFix,
                // deps
                30
            ]
        ) {
            mem in
            
            try mem.jit(ctx: ctx, fix: sutFix) {
                (entrypoint: _JitFunc) in
                
                XCTAssertEqual(28, entrypoint(14))
            }
        }
    }
    
    func testCompile__testStaticClosure() throws {
        typealias _JitFunc =  (@convention(c) (Int32, Int32) -> (Int32))
        let sutFix = 252
        try _compileAndLink(
            strip: false,
            [
                // NOTE: function order is important for coverage
                // sutFix goes first, which contains OStaticClosure from
                // a dependency that must not be compiled yet.
                sutFix,
                
                253
            ]
        ) {
            mem in
            
            try mem.jit(ctx: ctx, fix: sutFix) {
                (entrypoint: _JitFunc) in
                
                XCTAssertEqual(33, entrypoint(11, 22))
            }
        }
    }
    
    func testCompile__testInstanceMethod() throws {
        typealias _JitFunc =  (@convention(c) (Int32) -> (Int32))
        let sutFix = 254
        try _compileAndLink(
            strip: false,
            [
                29, 28, 30,
                sutFix
            ]
        ) {
            mem in
            
            try mem.jit(ctx: ctx, fix: sutFix) {
                (entrypoint: _JitFunc) in
                
                XCTAssertEqual(44, entrypoint(22))
            }
        }
    }
    
    /// This tests fetching global values
    func testCompile__testGlobal() throws {
        let fix = 57
        
        typealias _JitFunc =  (@convention(c) () -> UnsafeRawPointer)
        try _compileAndLink(
            strip: false,
            [
                // deps
                // ...
                // entrypoint
                fix
            ]
        ) {
            mem in
            
            struct _String {
                let t: UnsafePointer<HLType_CCompat>
                let b: UnsafeRawPointer
                let length: Int32
            }
            
            try mem.jit(ctx: ctx, fix: fix) {
                (entrypoint: _JitFunc) in
                
                let x = entrypoint()
                let expected = "Hello Globals"
                let inst = x.bindMemory(to: _String.self, capacity: 1)
                
                XCTAssertEqual(Int(inst.pointee.length), expected.lengthOfBytes(using: .utf8))
                let s = String._wrapUtf16(from: .init(OpaquePointer(inst.pointee.b)))
                XCTAssertEqual(expected, s)
            }
        }
    }
    
    /// Test field access.
    func testCompile__testFieldAccess() throws {
        typealias _JitFunc =  (@convention(c) () -> Int32)
        let sutFix = 48
        try _compileAndLink(
            strip: false,
            [
                27,
                sutFix
            ]
        ) {
            mem in
            
            try mem.jit(ctx: ctx, fix: sutFix) {
                (entrypoint: _JitFunc) in
                
                XCTAssertEqual(2, entrypoint())
            }
        }
    }
    
    func testCompile__testEnum() throws {
        typealias _JitFunc =  (@convention(c) () -> Int32)
        let sutFix = 256
        try _compileAndLink(
            strip: false,
            [
                292,
                sutFix
            ]
        ) {
            mem in
            
            try mem.jit(ctx: ctx, fix: sutFix) {
                (entrypoint: _JitFunc) in
                
                XCTAssertEqual(42, entrypoint())
            }
        }
    }
}

