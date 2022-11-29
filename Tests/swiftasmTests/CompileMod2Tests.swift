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
    
    func extractDeps(fix: RefFun, ignore: Set<RefFun> = Set(), depHints: [RefFun] = []) throws -> Set<RefFun> {
        var result: Set<RefFun> = Set([fix] + depHints)
        let f = try ctx.getCompilable(findex: fix)
        
        guard !ignore.contains(fix) else {
            // already processed
            return []
        }
        
        guard let f = f else {
            // not a compilable
            return Set()
        }
        
        for op in f.ops {
            switch(op) {
            case .OCall1(_, let depFun, _):
                fallthrough
            case .OCall2(_, let depFun, _, _):
                fallthrough
            case .OCall3(_, let depFun, _, _, _):
                fallthrough
            case .OCall4(_, let depFun, _, _, _, _):
                fallthrough
            case .OCallN(_, let depFun, _):
                fallthrough
            case .OCall0(_, let depFun):
                var realIgnore = ignore.union(Set(result))
                print("\(fix) calls \(depFun)")
                result = result.union(try extractDeps(fix: depFun, ignore: realIgnore))
            default:
                break
            }
        }
        return result
    }
    
    /// Finds the function index assuming the (HLFunction)->field.name is set
    func _findFindex(name: String) -> RefFun? {
        for rawIndex in (0..<ctx.nfunctions) {
            guard let f = ctx.mainContext.pointee.code?.pointee.functions.advanced(by: Int(rawIndex)) else {
                continue
            }
            guard (f.pointee.fieldName?.stringValue == name) else {
                continue
            }
            
            guard let fix = ctx.mainContext.pointee.m?.pointee.functions_indexes.advanced(by: Int(rawIndex)).pointee else {
                continue
            }
            
            return RefFun(fix)
        }
        
        return nil
    }
    
    func _findFindex_fieldNameUnset(className: String, name: String, isStatic: Bool) throws -> RefFun? {
        if isStatic {
            return try _findStaticFindex_fieldNameUnset(className: className, name: name)
        } else {
            return try _findInstanceFindex_fieldNameUnset(className: className, name: name)
        }
    }
    
    /// Finds the static function index
    func _findStaticFindex_fieldNameUnset(className: String, name: String) throws -> RefFun? {
        var mainGlobalType: UnsafePointer<HLType_CCompat>? = nil
        
        for typeIx in (0..<ctx.ntypes) {
            let t = try ctx.getType(Int(typeIx))
            guard let classNameCandidate = t.objProvider?.nameProvider.stringValue, classNameCandidate == className else {
                continue
            }
            
            let hlType: UnsafePointer<HLType_CCompat> = .init(OpaquePointer(t.ccompatAddress))
            let gPtr = hlType.pointee.obj.pointee.globalValue
            
            for gix in (0..<ctx.nglobals) {
                guard let gResolvedIndex = ctx.mainContext.pointee.m?.pointee.globals_indexes?.advanced(by: Int(gix)).pointee else {
                    continue
                }
            
                guard let cand = ctx.mainContext.pointee.m?.pointee.globals_data?.advanced(by: Int(gResolvedIndex)) else {
                    continue
                }
                if (Int(bitPattern: cand) == Int(bitPattern: gPtr)) {
                    print("[main got it at \(gResolvedIndex) @ \(gix)")
                    
                    guard let mainGlobalTypeCandidate = ctx.mainContext.pointee.code?.pointee.globals.advanced(by: Int(gix)).pointee else {
                        continue
                    }
                    
                    mainGlobalType = mainGlobalTypeCandidate
                    break
                }
            }
            guard mainGlobalType == nil else {
                break
            }
        }
        
        guard let mainGlobalType = mainGlobalType else {
            fatalError("Could not locate main global type")
        }
        
        for bindingIx in 0..<mainGlobalType.pointee.obj.pointee.nbindings {
            guard let bindingBase = mainGlobalType.pointee.obj.pointee.bindingsPtr?.advanced(by: Int(bindingIx*2)) else {
                continue
            }
            
            let fid = bindingBase.pointee
            let mid = bindingBase.advanced(by: 1).pointee
            
            let objField = LibHl.hl_obj_field_fetch(mainGlobalType, fid)
            print("Field", objField.pointee.nameProvider.stringValue)
            guard objField.pointee.nameProvider.stringValue == name else {
                continue
            }
            
            return RefFun(mid)
        }
        
        return nil
    }
    
    /// Finds the instance function index
    func _findInstanceFindex_fieldNameUnset(className: String, name: String) throws -> RefFun? {
        var mainGlobalType: UnsafePointer<HLType_CCompat>? = nil
        
        for typeIx in (0..<ctx.ntypes) {
            let t = try ctx.getType(Int(typeIx))
            guard let classNameCandidate = t.objProvider?.nameProvider.stringValue, classNameCandidate == className else {
                continue
            }
            
            mainGlobalType = .init(OpaquePointer(t.ccompatAddress))
        }
        
        guard let mainGlobalType = mainGlobalType else {
            fatalError("Could not locate main global type")
        }
        
        for protoIx in 0..<mainGlobalType.pointee.obj.pointee.nproto {
            guard let proto = mainGlobalType.pointee.obj.pointee.protoPtr?.advanced(by: Int(protoIx)) else {
                continue
            }
            guard proto.pointee.namePtr.stringValue == name else {
                continue
            }
            
            return RefFun(proto.pointee.findex)
        }
        
        return nil
    }
    
    func _compileDeps(strip: Bool, mem: CpuOpBuffer = CpuOpBuffer(), fqname: String, depHints: [RefFun] = []) throws -> (RefFun, CpuOpBuffer) {
        
        var components = fqname.components(separatedBy: "#")
        let isStatic: Bool
        if components.count == 2 {
            isStatic = false
        } else {
            components = fqname.components(separatedBy: ".")
            guard components.count == 2 else {
                throw TestError.unexpected("Invalid fully qualified name \(fqname) (must be separated by # or .)")
            }
            isStatic = true
        }
        
        let className = components[0]
        let funcName = components[1]
        
        
        guard let fix = try _findFindex_fieldNameUnset(className: className, name: funcName, isStatic: isStatic) else {
            throw TestError.unexpected("Function \(fqname) not found")
        }
        
        let compiler = try sut(strip: strip)
        let deps = try extractDeps(fix: fix, depHints: depHints)
        
        print("Function \(fqname)@\(fix) has these dependencies: \(deps)")
        for depFix in deps {
            try compiler.compile(findex: depFix, into: mem)
        }
        return (fix, mem)
    }
    
    func _compileDeps(strip: Bool, mem: CpuOpBuffer = CpuOpBuffer(), fix: RefFun, depHints: [RefFun] = []) throws -> (RefFun, CpuOpBuffer) {
        
        let compiler = try sut(strip: strip)
        let deps = try extractDeps(fix: fix, depHints: depHints)
        
        print("Function ?@\(fix) has these dependencies: \(deps)")
        for depFix in deps {
            try compiler.compile(findex: depFix, into: mem)
        }
        return (fix, mem)
    }
    
    func _compileAndLinkWithDeps(strip: Bool, mem: CpuOpBuffer = CpuOpBuffer(), name: String, depHints: [RefFun] = [], _ callback: (RefFun, UnsafeMutableRawPointer) throws->()) throws {
        let (fix, buff) = try self._compileDeps(strip: strip, fqname: name, depHints: depHints)
        let mapper = BufferMapper(ctx: self.ctx, buffer: buff)
        let mem = try mapper.getMemory()
        
        try callback(fix, mem)
        
        try mapper.freeMemory()
    }
    
    func _compileAndLinkWithDeps(strip: Bool, mem: CpuOpBuffer = CpuOpBuffer(), fix: RefFun, depHints: [RefFun] = [], _ callback: (RefFun, UnsafeMutableRawPointer) throws->()) throws {
        let (fix, buff) = try self._compileDeps(strip: strip, fix: fix, depHints: depHints)
        let mapper = BufferMapper(ctx: self.ctx, buffer: buff)
        let mem = try mapper.getMemory()
        
        try callback(fix, mem)
        
        try mapper.freeMemory()
    }
    
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
    
    func testCompile__testEntrypoint() throws {
        throw XCTSkip("testEntrypoint not finished yet")
        
        typealias _JitFunc =  (@convention(c) () -> ())
        
        guard let ep = ctx.mainContext.pointee.code?.pointee.entrypoint else {
            return XCTFail("No entrypoint")
        }
        guard let testTraceFix = try _findStaticFindex_fieldNameUnset(className: "Main", name: "testTrace") else {
            return XCTFail("No Main.testTrace")
        }
        
        let compilableEntrypoint = try ctx.getCompilable(findex: RefFun(ep))
        guard var ops = compilableEntrypoint?.ops, let secondToLast = ops.dropLast(1).last, let last = ops.last else {
            return XCTFail("Can't fetch entrypoint ops")
        }
        
        guard case .OCall0(let dst, _) = secondToLast, case .ORet(_) = last else {
            return XCTFail("Can't patch entrypoint, op assumption not correct")
        }
        
        ops = Array(ops.dropLast(2)) + [.OCall0(dst: dst, fun: testTraceFix), last]
        ctx.patch(findex: RefFun(ep), ops: ops)
        
        try _compileAndLinkWithDeps(
            strip: false,
            fix: RefFun(ep)
        ) {
            sutFix, mem in
            print("Compiling fix \(sutFix) vs \(ep)")
            try mem.jit(ctx: ctx, fix: sutFix) {
                (entrypoint: _JitFunc) in
                
                entrypoint()
            }
        }
    }
    
    func testCompile__testTrace() throws {
        throw XCTSkip("testTrace not implemented")
        
        typealias _JitFunc =  (@convention(c) () -> ())
        
        try _compileAndLinkWithDeps(
            strip: true,
            name: "Main.testTrace"
        ) {
            sutFix, mem in
            
            try mem.jit(ctx: ctx, fix: sutFix) {
                (entrypoint: _JitFunc) in
                
                entrypoint()
            }
        }
    }
    
    func testCompile__testFieldClosure() throws {
        typealias _JitFunc =  (@convention(c) (Int32) -> (Int32))
        
        try _compileAndLinkWithDeps(
            strip: true,
            name: "Main.testFieldClosure",
            depHints: [30]
        ) {
            sutFix, mem in
            
            try mem.jit(ctx: ctx, fix: sutFix) {
                (entrypoint: _JitFunc) in
                
                XCTAssertEqual(28, entrypoint(14))
            }
        }
    }
    
    func testCompile__testStaticClosure() throws {
        throw XCTSkip("This test is flaky.")
        
        typealias _JitFunc =  (@convention(c) (Int32, Int32) -> (Int32))
        
        try _compileAndLinkWithDeps(
            strip: true,
            name: "Main.testStaticClosure",
            depHints: [259]
        ) {
            sutFix, mem in
            
            try mem.jit(ctx: ctx, fix: sutFix) {
                (entrypoint: _JitFunc) in
                
                XCTAssertEqual(33, entrypoint(11, 22))
            }
        }
    }
    
    func testCompile__testInstanceMethod() throws {
        typealias _JitFunc =  (@convention(c) (Int32) -> (Int32))
        
        try _compileAndLinkWithDeps(
            strip: true,
            name: "Main.testInstanceMethod",
            depHints: [30]
        ) {
            sutFix, mem in
            
            try mem.jit(ctx: ctx, fix: sutFix) {
                (entrypoint: _JitFunc) in
                
                XCTAssertEqual(44, entrypoint(22))
            }
        }
    }
    
    ///
    func testCompile__testStaticVirtual_setField() throws {
        typealias _JitFunc =  (@convention(c) (Int32, Bool) -> (Int32))
        
        try _compileAndLinkWithDeps(
            strip: true,
            name: "Main.testStaticVirtual_setField"
        ) {
            sutFix, mem in
            
            try mem.jit(ctx: ctx, fix: sutFix) {
                (entrypoint: _JitFunc) in
                
                XCTAssertEqual(0, entrypoint(123, false))
                XCTAssertEqual(123, entrypoint(123, true))
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
}

