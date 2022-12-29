import XCTest

@testable import swiftasm

fileprivate var patched_sys_print: [String] = []

final class CompileMod2Tests: RealHLTestCase {
    
    override var HL_FILE: String { "mod2" }
    
    func testCompile__testGetSetField() throws {
        try _compileAndLinkWithDeps(
            strip: true,
            name: "Main.testGetSetField"
        ) {
            sutFix, mem in
            
            try mem.jit(ctx: ctx, fix: sutFix) {
                (entrypoint: (@convention(c) (Int32) -> Int32)) in
                
                XCTAssertEqual(46, entrypoint(23))
            }
        }
    }

    func testCompile__testGetArrayInt32() throws {
        typealias _JitFunc = (@convention(c) (Int32, Int64, Int32) -> Int64)
        try _compileAndLinkWithDeps(
            strip: true,
            name: "Main.testGetArrayInt32"
        ) {
            sutFix, mem in
            
            try mem.jit(ctx: ctx, fix: sutFix) {
                (entrypoint: _JitFunc) in
                
                XCTAssertEqual(1239, entrypoint(10, 1234, 5))
            }
        }
    }
    
    func testCompile__testGetSetArray__64hl() throws {
        typealias _JitFunc = (@convention(c) (Int32, Int64, Int32) -> Int64)
        try _compileAndLinkWithDeps(
            strip: true,
            name: "Main.testGetArrayInt64__hl"
        ) {
            sutFix, mem in
            
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
                
        try _compileAndLinkWithDeps(
            strip: true,
            name: "Main.testGetArrayInt64__haxe"
        ) {
            sutFix, mem in
            
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
        try _compileAndLinkWithDeps(
            strip: true,
            name: "Main.testArrayLength"
        ) {
            sutFix, mem in
            
            try mem.jit(ctx: ctx, fix: sutFix) {
                (entrypoint: _JitFunc) in
         
                XCTAssertEqual(5, entrypoint(5))
            }
        }
    }
    
    func testCompile__indexOf() throws {
        typealias _JitFunc = (@convention(c) (OpaquePointer, OpaquePointer, OpaquePointer) -> Int32)
        try _compileAndLinkWithDeps(
            strip: true,
            name: "String#indexOf"
        ) {
            sutFix, mem in
            
            let f = "First String"
            let sNotFound = "not_found"
            let sFound = "t S"
            let fstr = (f + "\0").data(using: .utf16LittleEndian)!
            let sNotFoundStr = (sNotFound + "\0").data(using: .utf16LittleEndian)!
            let sFoundStr = (sFound + "\0").data(using: .utf16LittleEndian)!
            let fstrPtr: UnsafeMutableBufferPointer<UInt8> = .allocate(capacity: fstr.count)
            let sNotFoundStrPtr: UnsafeMutableBufferPointer<UInt8> = .allocate(capacity: sNotFoundStr.count)
            let sFoundStrPtr: UnsafeMutableBufferPointer<UInt8> = .allocate(capacity: sFoundStr.count)
            defer {
                fstrPtr.deallocate()
                sFoundStrPtr.deallocate()
                sNotFoundStrPtr.deallocate()
            }
            _ = fstrPtr.initialize(from: fstr)
            _ = sFoundStrPtr.initialize(from: sFoundStr)
            _ = sNotFoundStrPtr.initialize(from: sNotFoundStr)
            
            guard let funIndex = ctx.mainContext.pointee.m?.pointee.functions_indexes.advanced(by: sutFix).pointee else {
                fatalError("No real funIndex for \(sutFix)")
            }
            guard let indexOfType = ctx.mainContext.pointee.code?.pointee.functions.advanced(by: Int(funIndex)).pointee.typePtr else {
                fatalError("Can't lookup function type")
            }
            
            let nullType = indexOfType.funProvider!.argsProvider[2] as! UnsafePointer<HLType_CCompat>
            
            
            let t = try ctx.getType(13) // string
            var strA = _String(
                t: .init(OpaquePointer(t.ccompatAddress)),
                bytes: .init(OpaquePointer(fstrPtr.baseAddress!)),
                length: Int32(f.count))
            var strB_found = _String(
                t: .init(OpaquePointer(t.ccompatAddress)),
                bytes: .init(OpaquePointer(sFoundStrPtr.baseAddress!)),
                length: Int32(sFound.count))
            var strB_notFound = _String(
                t: .init(OpaquePointer(t.ccompatAddress)),
                bytes: .init(OpaquePointer(sFoundStrPtr.baseAddress!)),
                length: Int32(sNotFound.count))
            var nullD = vdynamic(t: nullType, union: nil)

            let c = try ctx.getCallable(findex: sutFix)
            let entrypoint = unsafeBitCast(c!.address.value, to: _JitFunc.self)
            withUnsafeMutablePointer(to: &strA) { strAPtr in
                withUnsafeMutablePointer(to: &strB_notFound) { strBPtr in
                    withUnsafeMutablePointer(to: &nullD) { nullDPtr in
                        let res = entrypoint(.init(strAPtr), .init(strBPtr), .init(nullDPtr))
                        XCTAssertEqual(-1, res)
                    }
                }
                
                withUnsafeMutablePointer(to: &strB_found) { strBPtr in
                    withUnsafeMutablePointer(to: &nullD) { nullDPtr in
                        let res = entrypoint(.init(strAPtr), .init(strBPtr), .init(nullDPtr))
                        XCTAssertEqual(4, res)
                    }
                }
            }
        }
    }
    
    // MARK: Test traps
    func testCompile__testTrap() throws {
        typealias _JitFunc = (@convention(c) () -> Int32)
        try _compileAndLinkWithDeps(
            strip: true,
            name: "Main.testTrap"
        ) {
            sutFix, mem in
            
            let c = try ctx.getCallable(findex: sutFix)
            let entrypoint = unsafeBitCast(c!.address.value, to: _JitFunc.self)
            let res = entrypoint()
            XCTAssertEqual(1, res)
            
            let cptr = LibHl.hl_get_thread()
            XCTAssertNil(cptr.pointee.trap_current)
        }
    }
    
    func testCompile__testTrapConditional() throws {
        typealias _JitFunc = (@convention(c) (Bool) -> Int32)
        try _compileAndLinkWithDeps(
            strip: true,
            name: "Main.testTrapConditional"
        ) {
            sutFix, mem in
            
            let c = try ctx.getCallable(findex: sutFix)
            let entrypoint = unsafeBitCast(c!.address.value, to: _JitFunc.self)
            
            var res = entrypoint(false)
            XCTAssertEqual(0, res)
            
            res = entrypoint(true)
            XCTAssertEqual(1, res)
            
            res = entrypoint(false)
            XCTAssertEqual(0, res)
            
            res = entrypoint(true)
            XCTAssertEqual(1, res)
            
            let cptr = LibHl.hl_get_thread()
            XCTAssertNil(cptr.pointee.trap_current)
        }
    }
    
    func testCompile__testTrapContextEnding() throws {
        typealias _JitFunc = (@convention(c) (Bool) -> Int32)
        try _compileAndLinkWithDeps(
            strip: true,
            name: "Main.testTrapContextEnding"
        ) {
            sutFix, mem in
            
            let c = try ctx.getCallable(findex: sutFix)
            let entrypoint = unsafeBitCast(c!.address.value, to: _JitFunc.self)
            
            var res = entrypoint(false)
            XCTAssertEqual(1, res)
            
            res = entrypoint(true)
            XCTAssertEqual(2, res)
            
            let cptr = LibHl.hl_get_thread()
            XCTAssertNil(cptr.pointee.trap_current)
        }
    }
    
    ///
    func testCompile_testGetUI16() throws {
        typealias _JitFunc = (@convention(c) (Int32) -> Int32)
        
        try _compileAndLinkWithDeps(
            strip: true,
            name: "Main.testGetUI16"
        ) {
            sutFix, mem in
            
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
        try _compileAndLinkWithDeps(
            strip: true,
            name: "Main.testGetUI8"
        ) {
            sutFix, mem in
            
            let callable = try ctx.getCallable(findex: sutFix)
            let entrypoint = unsafeBitCast(callable!.address.value, to: _JitFunc.self)
            
            
            var res = entrypoint(0)
            XCTAssertEqual(res, 0x11)

            res = entrypoint(1)
            XCTAssertEqual(res, 0x12)

            res = entrypoint(2)
            XCTAssertEqual(res, 0x13)
            
            res = entrypoint(3)
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
        
        try _compileAndLinkWithDeps(
            strip: true,
            name: "String#charCodeAt"
        ) {
            sutFix, mem in
            
            let strType = try ctx.getType(13)   // string type
            
            let str = "11121314"
            let data = str.data(using: .utf16LittleEndian)!
            let dataPtr: UnsafeMutableBufferPointer<UInt8> = .allocate(capacity: data.count)
            _ = dataPtr.initialize(from: data)
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
        
        try _compileAndLinkWithDeps(
            strip: true,
            name: "Main.testGetUI8_2"
        ) {
            sutFix, mem in
            
            try mem.jit(ctx: ctx, fix: sutFix) {
                (entrypoint: _JitFunc) in
                
                XCTAssertEqual(entrypoint(), 0x14131211)
            }
        }
    }
    
    func testCompile__testTrapDifferentTypes() throws {
        typealias _JitFunc =  (@convention(c) (Bool, Bool) -> Int32)
        
        let valueExcDeps = try _extractTypeProtoDependencies("haxe.ValueException", ["unwrap"])
        
        try _withPatchedEntrypoint(
            strip: true,
            name: "Main.testTrapDifferentTypes",
            
            // can't reliably detect the OCallMethod dependencies
            depHints: valueExcDeps
        ) {
            sutFix, mem in
            
            try mem.jit(ctx: ctx, fix: sutFix) {
                (entrypoint: _JitFunc) in
                
                XCTAssertEqual(5, entrypoint(true, false))
                XCTAssertEqual(3, entrypoint(false, true))
                XCTAssertEqual(0, entrypoint(false, false))
            }
        }
    }
    
    // add strings
    func testCompile__test__add__() throws {
        typealias _JitFunc =  (@convention(c) (OpaquePointer, OpaquePointer) -> (OpaquePointer))
        
        try _compileAndLinkWithDeps(
            strip: true,
            name: "String.__add__"
        ) {
            sutFix, mem in
            
            var strType: UnsafePointer<HLType_CCompat>? = nil
            for typeIx in (0..<ctx.ntypes) {
                let t = try ctx.getType(Int(typeIx)) as any HLTypeProvider
                if t.isEquivalent(_StringType) {
                    strType = .init(OpaquePointer(t.ccompatAddress))
                    break
                }
            }
            guard let strType = strType else {
                fatalError("Could not find initialized string type")
            }
            
            let a = "Hello "
            let b = "World"
            
            let aBytes = (a + "\0").data(using: .utf16LittleEndian)!
            let bBytes = (b + "\0").data(using: .utf16LittleEndian)!
            
            let aBytePtr: UnsafeMutableBufferPointer<UInt8> = .allocate(capacity: aBytes.count)
            let bBytePtr: UnsafeMutableBufferPointer<UInt8> = .allocate(capacity: bBytes.count)
            defer {
                aBytePtr.deallocate()
                bBytePtr.deallocate()
            }
            _ = aBytePtr.initialize(from: aBytes)
            _ = bBytePtr.initialize(from: bBytes)
            
            var strA = _String(
                t: strType,
                bytes: .init(OpaquePointer(aBytePtr.baseAddress!)),
                length: Int32(a.count))
            
            var strB = _String(
                t: strType,
                bytes: .init(OpaquePointer(bBytePtr.baseAddress!)),
                length: Int32(b.count))
            
            try mem.jit(ctx: ctx, fix: sutFix) {
                (entrypoint: _JitFunc) in
            
                withUnsafeMutablePointer(to: &strA) {
                    strAPtr in
                    withUnsafeMutablePointer(to: &strB) {
                        strBPtr in
                        
                        let added: UnsafePointer<_String> = .init(entrypoint(.init(strAPtr), .init(strBPtr)))
                        XCTAssertEqual(added.pointee.bytes.stringValue, "Hello World")
                    }
                }
            }
        }
    }
    
    /// Tests the `trace` method prints to `stdout`.
    ///
    /// This test captures output, which isn't very stable (it flushes and then waits for the data to be available).
    func testCompile__testTrace() throws {
        /* NOTE: depHints must contain the function referenced
         in haxe.$Log bindings under `trace`.
         
         (alternatively, `fnn trace` can help find it)
         
         Otherwise you will get a 0 memory dereference issue.
         
         It is tricky to infer the right function index, so it is not done automatically as a dependency.
         
         If the mod2.hl file changes, this findex might need to be changed.
        */
        
        /* NOTE2: the String object must have it's toString method initialized
         (look at the type, under protos look for `__string` and make sure depHints contains
         the corresponding function)
         
         Otherwise the output will be `String` (i.e. object name, not the actual value)*/
        
        let expected = "haxesrc/Main.hx:270: Hello Trace\n"
        patched_sys_print = []
        
        // Patch the print call to intercept
        let _patchedSysPrint: (@convention(c) (OpaquePointer)->()) = {
            bytePtr in
            
            let byteVal: UnsafePointer<CChar16> = .init(bytePtr)
            patched_sys_print.append(byteVal.stringValue)
        }
        guard ctx.patchNative(name: "sys_print", addr: unsafeBitCast(_patchedSysPrint, to: OpaquePointer.self)) else {
            return XCTFail("Failed to patch sys_print")
        }
        
        let stringToString = try _extractTypeProtoDependencies("String", ["__string"])
        let logBindingDeps = try _extractTypeBindingDependencies("haxe.$Log", ["trace", "formatOutput"])
        let arrayDynDeps = try _extractTypeProtoDependencies("hl.types.ArrayDyn", ["get_length"])
        guard let println = _findFindex(name: "println") else {
            return XCTFail("Could not find Std.println")
        }
        let printlnDeps = try extractDeps(fix: println)
        
        try _withPatchedEntrypoint(
            strip: true,
            name: "Main.testTrace",
            depHints: stringToString + logBindingDeps + arrayDynDeps + printlnDeps + [println]
        ) {
            sutFix, mem in
            
            try mem.jit(ctx: ctx, fix: sutFix) {
                (entrypoint: (@convention(c)()->())) in
        
                entrypoint()
            }
        }
        
        print(patched_sys_print.joined(separator: ""))
        XCTAssertEqual(patched_sys_print.joined(separator: ""), expected)
    }
    
    func testCompile__testCallThis() throws {
        typealias _JitFunc =  (@convention(c) () -> (Int32))
        
        let fix = try _findFindex_fieldNameUnset(className: "CallTest", name: "test", isStatic: false)!
        let depFix = try _findFindex_fieldNameUnset(className: "CallTest", name: "test2", isStatic: false)!
        ctx.patch(findex: fix, ops: [
            // NOTE: careful, if proto points to the wrong place, this can cause recursive calls to self
            .OCallThis(dst: 2, field: 1, args: [1]),
//              These are equivalent:
//              .OCallMethod(dst: 2, obj: 0, proto: 1, args: [1]),
//              .OCall2(dst: 2, fun: 30, arg0: 0, arg1: 1),
            .ORet(ret: 2)

            // Original opcodes should be: 
            // Main.hx:37    0: Call2       reg2 = test2@30(reg0, reg1)
            // Main.hx:37    1: Ret         reg2
        ])
        
        try _withPatchedEntrypoint(
            strip: true,
            name: "Main.testCallThis",
            depHints: [depFix]
        ) {
            sutFix, mem in
            
            try mem.jit(ctx: ctx, fix: sutFix) {
                (entrypoint: _JitFunc) in
                
                XCTAssertEqual(entrypoint(), 10)
            }
        }
    }
    
    func testCompile__testFieldClosure() throws {
        typealias _JitFunc =  (@convention(c) (Int32) -> (Int32))
        
        try _compileAndLinkWithDeps(
            strip: true,
            name: "Main.testFieldClosure"
        ) {
            sutFix, mem in
            
            try mem.jit(ctx: ctx, fix: sutFix) {
                (entrypoint: _JitFunc) in
                
                XCTAssertEqual(28, entrypoint(14))
            }
        }
    }
    
    func testCompile__testStaticClosure() throws {
        typealias _JitFunc =  (@convention(c) (Int32, Int32) -> (Int32))
        
        try _compileAndLinkWithDeps(
            strip: true,
            name: "Main.testStaticClosure"
        ) {
            sutFix, mem in
            
            try mem.jit(ctx: ctx, fix: sutFix) {
                (entrypoint: _JitFunc) in
                
                XCTAssertEqual(33, entrypoint(11, 22))
            }
        }
    }
    
    func testCompile_testDynGetSet() throws {
        typealias _JitFunc =  (@convention(c) (Bool, Int32) -> (Int32))
        
        try _withPatchedEntrypoint(
            strip: true,
            name: "Main.testDynGetSet"
        ) {
            sutFix, mem in
            
            try mem.jit(ctx: ctx, fix: sutFix) {
                (entrypoint: _JitFunc) in
                
                XCTAssertEqual(entrypoint(false, 123), 456)
                XCTAssertEqual(entrypoint(true, 123), 123)
            }
        }
    }
    
    func testCompile_testCallClosure_Dynamic_returnFloat64() throws {
        typealias _JitFunc =  (@convention(c) (Int32) -> (Float64))
        
        let fix = RefFun(_findFindex(name: "testCallClosure_Dynamic_returnFloat64")!)
        let sut = try ctx.getCompilable(findex: fix)!
        guard case .OStaticClosure(_, let dependency) = sut.ops[0] else {
            return XCTFail("Invalid assumption")
        }
        
        try _compileAndLinkWithDeps(
            strip: true,
            name: "Main.testCallClosure_Dynamic_returnFloat64",
            depHints: [dependency]
        ) {
            sutFix, mem in
            
            // first test the dependency is working directly
            guard let x = try ctx.getCallable(findex: dependency) else {
                return XCTFail("Couldn't load dependency function \(dependency)")
            }
            
            let _dep = unsafeBitCast(x.address.value, to: _JitFunc.self)
            XCTAssertEqualDouble(_dep(123), 246.0)
            
            // now test via closure
            try mem.jit(ctx: ctx, fix: sutFix) {
                (entrypoint: _JitFunc) in

                XCTAssertEqualDouble(entrypoint(123), 246.0)
            }
        }
    }
    
    func testCompile_testCallClosure_Dynamic_returnFloat32() throws {
        typealias _JitFunc =  (@convention(c) (Int32) -> (Float32))
        
        let fix = RefFun(_findFindex(name: "testCallClosure_Dynamic_returnFloat32")!)
        let sut = try ctx.getCompilable(findex: fix)!
        guard case .OStaticClosure(_, let dependency) = sut.ops[0] else {
            return XCTFail("Invalid assumption")
        }
        
        try _compileAndLinkWithDeps(
            strip: true,
            name: "Main.testCallClosure_Dynamic_returnFloat32",
            depHints: [dependency]
        ) {
            sutFix, mem in
            
            // first test the dependency is working directly
            guard let x = try ctx.getCallable(findex: dependency) else {
                return XCTFail("Couldn't load dependency function \(dependency)")
            }
            
            let _dep = unsafeBitCast(x.address.value, to: _JitFunc.self)
            XCTAssertEqualFloat(_dep(123), 246.0)
            
            // now test via closure
            try mem.jit(ctx: ctx, fix: sutFix) {
                (entrypoint: _JitFunc) in

                XCTAssertEqualFloat(entrypoint(123), 246.0)
            }
        }
    }
    
    func testCompile_testCallClosure_Dynamic_returnUInt8() throws {
        typealias _JitFunc =  (@convention(c) (Int32) -> (UInt8))
        
        let fix = RefFun(_findFindex(name: "testCallClosure_Dynamic_returnUInt8")!)
        let sut = try ctx.getCompilable(findex: fix)!
        guard case .OStaticClosure(_, let dependency) = sut.ops[0] else {
            return XCTFail("Invalid assumption")
        }
        
        try _compileAndLinkWithDeps(
            strip: true,
            name: "Main.testCallClosure_Dynamic_returnUInt8",
            depHints: [dependency]
        ) {
            sutFix, mem in
            
            // first test the dependency is working directly
            guard let x = try ctx.getCallable(findex: dependency) else {
                return XCTFail("Couldn't load dependency function \(dependency)")
            }
            
            let _dep = unsafeBitCast(x.address.value, to: _JitFunc.self)
            XCTAssertEqual(_dep(123), 246)
            
            // now test via closure
            try mem.jit(ctx: ctx, fix: sutFix) {
                (entrypoint: _JitFunc) in

                XCTAssertEqual(entrypoint(123), 246)
            }
        }
    }
    
    func testCompile_testCallClosure_Dynamic_returnUInt16() throws {
        typealias _JitFunc =  (@convention(c) (Int32) -> (UInt16))
        
        let fix = RefFun(_findFindex(name: "testCallClosure_Dynamic_returnUInt16")!)
        let sut = try ctx.getCompilable(findex: fix)!
        guard case .OStaticClosure(_, let dependency) = sut.ops[0] else {
            return XCTFail("Invalid assumption")
        }
        
        try _compileAndLinkWithDeps(
            strip: true,
            name: "Main.testCallClosure_Dynamic_returnUInt16",
            depHints: [dependency]
        ) {
            sutFix, mem in
            
            // first test the dependency is working directly
            guard let x = try ctx.getCallable(findex: dependency) else {
                return XCTFail("Couldn't load dependency function \(dependency)")
            }
            
            let _dep = unsafeBitCast(x.address.value, to: _JitFunc.self)
            XCTAssertEqual(_dep(123), 246)
            
            // now test via closure
            try mem.jit(ctx: ctx, fix: sutFix) {
                (entrypoint: _JitFunc) in

                XCTAssertEqual(entrypoint(123), 246)
            }
        }
    }
    
    func testCompile_testCallClosure_Dynamic_returnInt32() throws {
        typealias _JitFunc =  (@convention(c) (Int32) -> (Int32))
        
        let fix = RefFun(_findFindex(name: "testCallClosure_Dynamic_returnInt32")!)
        let sut = try ctx.getCompilable(findex: fix)!
        guard case .OStaticClosure(_, let dependency) = sut.ops[0] else {
            return XCTFail("Invalid assumption")
        }
        
        try _compileAndLinkWithDeps(
            strip: true,
            name: "Main.testCallClosure_Dynamic_returnInt32",
            depHints: [dependency]
        ) {
            sutFix, mem in
            
            // first test the dependency is working directly
            guard let x = try ctx.getCallable(findex: dependency) else {
                return XCTFail("Couldn't load dependency function \(dependency)")
            }
            
            let _dep = unsafeBitCast(x.address.value, to: _JitFunc.self)
            XCTAssertEqual(_dep(123), 246)
            
            // now test via closure
            try mem.jit(ctx: ctx, fix: sutFix) {
                (entrypoint: _JitFunc) in

                XCTAssertEqual(entrypoint(123), 246)
            }
        }
    }
    
    func testCompile_testCallClosure_Dynamic_returnInt64() throws {
        typealias _JitFunc =  (@convention(c) (Int32) -> (Int64))
        
        let fix = RefFun(_findFindex(name: "testCallClosure_Dynamic_returnInt64")!)
        let sut = try ctx.getCompilable(findex: fix)!
        guard case .OStaticClosure(_, let dependency) = sut.ops[0] else {
            return XCTFail("Invalid assumption")
        }
        
        try _compileAndLinkWithDeps(
            strip: true,
            name: "Main.testCallClosure_Dynamic_returnInt64",
            depHints: [dependency]
        ) {
            sutFix, mem in
            
            // first test the dependency is working directly
            guard let x = try ctx.getCallable(findex: dependency) else {
                return XCTFail("Couldn't load dependency function \(dependency)")
            }
            
            let _dep = unsafeBitCast(x.address.value, to: _JitFunc.self)
            XCTAssertEqual(_dep(123), 246)
            
            // now test via closure
            try mem.jit(ctx: ctx, fix: sutFix) {
                (entrypoint: _JitFunc) in

                XCTAssertEqual(entrypoint(123), 246)
            }
        }
    }
    
    func testCompile_testDynGetSet_f64() throws {
        typealias _JitFunc =  (@convention(c) (Bool, Float64) -> (Float64))
        
        try _withPatchedEntrypoint(
            strip: true,
            name: "Main.testDynGetSet_f64"
        ) {
            sutFix, mem in
            
            try mem.jit(ctx: ctx, fix: sutFix) {
                (entrypoint: _JitFunc) in
                
                XCTAssertEqual(entrypoint(false, 123.0), 789.0)
//                XCTAssertEqual(entrypoint(true, 123.0), 123.0)
            }
        }
    }
    
    func testCompile__testInstanceMethod() throws {
        typealias _JitFunc =  (@convention(c) (Int32) -> (Int32))
        
        try _compileAndLinkWithDeps(
            strip: true,
            name: "Main.testInstanceMethod"
        ) {
            sutFix, mem in
            
            try mem.jit(ctx: ctx, fix: sutFix) {
                (entrypoint: _JitFunc) in
                
                XCTAssertEqual(44, entrypoint(22))
            }
        }
    }
    
    /// Test floating-point references.
    func testCompile__testRef_fp() throws {
        typealias _JitFunc =  (@convention(c) () -> (Float64))
        
        try _compileAndLinkWithDeps(
            strip: true,
            name: "Main.testRef_fp"
        ) {
            sutFix, mem in
            
            try mem.jit(ctx: ctx, fix: sutFix) {
                (entrypoint: _JitFunc) in
                
                XCTAssertEqualDouble(entrypoint(), 303.0)
            }
        }
    }
    
    /// Test int references.
    func testCompile__testRef_i() throws {
        typealias _JitFunc =  (@convention(c) () -> (Int64))
        
        try _compileAndLinkWithDeps(
            strip: true,
            name: "Main.testRef_i"
        ) {
            sutFix, mem in
            
            try mem.jit(ctx: ctx, fix: sutFix) {
                (entrypoint: _JitFunc) in
                
                XCTAssertEqual(entrypoint(), 303)
            }
        }
    }
    
    ///
    func testCompile__testStaticVirtual_globalVirtual_f32() throws {
        typealias _JitFunc =  (@convention(c) () -> (Float32))
        
        try _withPatchedEntrypoint(
            strip: true,
            name: "Main.testStaticVirtual_globalVirtual_f32"
        ) {
            sutFix, mem in
            
            try mem.jit(ctx: ctx, fix: sutFix) {
                (entrypoint: _JitFunc) in
                
                XCTAssertEqualFloat(entrypoint(), 101.0)
            }
        }
    }
    
    ///
    func testCompile__testStaticVirtual_globalVirtual_f64() throws {
        typealias _JitFunc =  (@convention(c) () -> (Float64))
        
        try _withPatchedEntrypoint(
            strip: true,
            name: "Main.testStaticVirtual_globalVirtual_f64"
        ) {
            sutFix, mem in
            
            try mem.jit(ctx: ctx, fix: sutFix) {
                (entrypoint: _JitFunc) in
                
                XCTAssertEqualDouble(entrypoint(), 789.0)
            }
        }
    }
    
    ///
    func testCompile__testStaticVirtual_globalVirtual() throws {
        typealias _JitFunc =  (@convention(c) (Int32/*field ix 0|1|2*/) -> (Int32))
        
        try _withPatchedEntrypoint(
            strip: true,
            name: "Main.testStaticVirtual_globalVirtual"
        ) {
            sutFix, mem in
            
            try mem.jit(ctx: ctx, fix: sutFix) {
                (entrypoint: _JitFunc) in
                
                // fetch already set fields (ignore the new values)
                XCTAssertEqual(123, entrypoint(0))
                XCTAssertEqual(1, entrypoint(1))
                XCTAssertEqual(456, entrypoint(2))
            }
        }
    }
    
    ///
    func testCompile__testStaticVirtual_setField() throws {
        typealias _JitFunc =  (@convention(c) (Int32/*value*/, Bool/*set or not*/, Int32/*field ix 0|1|2*/) -> (Int32))
        
        try _compileAndLinkWithDeps(
            strip: true,
            name: "Main.testStaticVirtual_setField"
        ) {
            sutFix, mem in
            
            try mem.jit(ctx: ctx, fix: sutFix) {
                (entrypoint: _JitFunc) in
                
                XCTAssertEqual(entrypoint(0, true, 5), -1)  // unknown field
                
                XCTAssertEqual(entrypoint(0, true, 1), 0) // set bool to !0 => sets and returns true (== 1)
                XCTAssertEqual(entrypoint(123, true, 1), 1) // set bool to !0 => sets and returns true (== 1)
                XCTAssertEqual(entrypoint(123, true, 0), 123)
                XCTAssertEqual(entrypoint(456, true, 2), 456)
                
                // fetch already set fields (ignore the new values)
                XCTAssertEqual(entrypoint(999, false, 0), 1)
                XCTAssertEqual(entrypoint(999, false, 1), 1)
                XCTAssertEqual(entrypoint(999, false, 2), 2)
            }
        }
    }
    
    /// This tests fetching global values
    func testCompile__testGlobal() throws {
        
        typealias _JitFunc =  (@convention(c) () -> UnsafeRawPointer)
        try _compileAndLinkWithDeps(
            strip: true,
            name: "Main.testGlobal"
        ) {
            sutFix, mem in
            
            struct _String {
                let t: UnsafePointer<HLType_CCompat>
                let b: UnsafeRawPointer
                let length: Int32
            }
            
            try mem.jit(ctx: ctx, fix: sutFix) {
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
        
        try _compileAndLinkWithDeps(
            strip: true,
            name: "Main.testFieldAccess"
        ) {
            sutFix, mem in
            
            try mem.jit(ctx: ctx, fix: sutFix) {
                (entrypoint: _JitFunc) in
                
                XCTAssertEqual(2, entrypoint())
            }
        }
    }
    
    func testCompile__testEnum() throws {
        typealias _JitFunc =  (@convention(c) () -> Int32)
        
        try _compileAndLinkWithDeps(
            strip: true,
            name: "Main.testEnum"
        ) {
            sutFix, mem in
            
            try mem.jit(ctx: ctx, fix: sutFix) {
                (entrypoint: _JitFunc) in
                
                XCTAssertEqual(42, entrypoint())
            }
        }
    }
    
    func testCompile__testArrayBytes_Float() throws {
        typealias _JitFunc =  (@convention(c) (Int32) -> Float64)
        
        try _compileAndLinkWithDeps(
            strip: true,
            name: "Main.testArrayBytes_Float"
        ) {
            sutFix, mem in
            
            try mem.jit(ctx: ctx, fix: sutFix) {
                (entrypoint: _JitFunc) in
                
                XCTAssertEqual(entrypoint(1), 234.567)
            }
        }
    }
    
    func testCompile_testGetType_nonDynamicSrc() throws {
        typealias _JitFunc =  (@convention(c) () -> Int32)
        
        try _withPatchedEntrypoint(
            strip: true,
            name: "Main.testGetType_nonDynamicSrc"
        ) {
            sutFix, mem in
            
            try mem.jit(ctx: ctx, fix: sutFix) {
                (entrypoint: _JitFunc) in
                
                XCTAssertEqual(entrypoint(), 2)
            }
        }
    }
    
    func testCompile_testVirtualCallMethod() throws {
        typealias _JitFunc =  (@convention(c) () -> Int32)
        
        /* NOTE:
         
         if hlc_static_call tries to call 0x0, it's probably a missing dependency from
         one of the array/iterator types.
         */
        
        /* gather dependencies from protos from involved types */
        let totalDeps = 
        (try self._extractTypeProtoDependencies("hl.types.ArrayBytes_Int", ["iterator"])) +
        (try self._extractTypeProtoDependencies("hl.types.BytesIterator_Int", ["hasNext", "next"])) +
        (try self._extractTypeProtoDependencies("haxe.iterators.ArrayIterator", ["hasNext", "next"])) +
        (try self._extractTypeProtoDependencies("hl.types.ArrayDyn", ["get_length"])) +
        (try self._extractTypeBindingDependencies("hl.types.$BytesIterator_Int", ["__constructor__"])) +
        (try self._extractTypeBindingDependencies("haxe.iterators.$ArrayIterator", ["__constructor__"]))
        
        try _compileAndLinkWithDeps(
            strip: true,
            name: "Main.testVirtualCallMethod",
            depHints: totalDeps
        ) {
            sutFix, mem in
            print(sutFix)
            try mem.jit(ctx: ctx, fix: sutFix) {
                (entrypoint: _JitFunc) in
                
                XCTAssertEqual(entrypoint(), 21)
            }
        }
    }
    
    func testCompile_testInterface_1() throws {
        typealias _JitFunc =  (@convention(c) (Int32, Bool, Int32, Float32, Float64) -> Float64)
        
        /* gather dependencies from protos from involved types */
        let totalDeps: [RefFun] =
        (try self._extractTypeProtoDependencies("VirtualTest")) 
        
        try _compileAndLinkWithDeps(
            strip: true,
            name: "Main.testInterface_1",
            depHints: totalDeps
        ) {
            sutFix, mem in
            print(sutFix)
            try mem.jit(ctx: ctx, fix: sutFix) {
                (entrypoint: _JitFunc) in
                
                XCTAssertEqualDouble(entrypoint(1, true, 2, 3.3, 4.4), 11.69)
            }
        }
    }
    
    func testCompile_testInterface_2() throws {
        typealias _JitFunc =  (@convention(c) (Int32, Bool, Int32, Float32, Float64) -> Float64)
        
        let totalDeps: [RefFun] =
        (try self._extractTypeProtoDependencies("VirtualTest"))
                
        try _compileAndLinkWithDeps(
            strip: true,
            name: "Main.testInterface_2",
            depHints: totalDeps
        ) {
            sutFix, mem in
            print(sutFix)
            try mem.jit(ctx: ctx, fix: sutFix) {
                (entrypoint: _JitFunc) in
                
                XCTAssertEqualDouble(entrypoint(1, true, 2, 3.3, 4.4), 11.69)
            }
        }
    }
}

