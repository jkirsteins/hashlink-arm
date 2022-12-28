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
    
    func _extractTypeProtoDependencies(_ name: String, _ only: [String]? = nil) throws -> [RefFun] {
        
        var typeCandidate: (any HLTypeProvider)? = nil
        for typeIx in 0..<ctx.ntypes {
            let type = try ctx.getType(Int(typeIx))
            if type.objProvider?.nameProvider.stringValue == name {
                typeCandidate = type
                print("Type \(name) == \(typeIx)")
                break
            }
        }
        guard let protos = typeCandidate?.objProvider?.protoProvider else {
            throw TestError.unexpected("Type's '\(name)' protos not found")
        }
        
        var result: Set<RefFun> = Set()
        for p in protos {
            guard only == nil || only!.contains(p.nameProvider.stringValue) else {
                continue
            }
            
            result.insert(RefFun(p.findex))
            for dep in try self.extractDeps(fix: RefFun(p.findex), ignore: result) {
                result.insert(dep)
            }
        }
        return Array(result)
    }
    
    func _extractTypeBindingDependencies(_ name: String, _ only: [String]? = nil) throws -> [RefFun] {
        
        var typeCandidate: (any HLTypeProvider)? = nil
        for typeIx in 0..<ctx.ntypes {
            let type = try ctx.getType(Int(typeIx))
            if type.objProvider?.nameProvider.stringValue == name {
                typeCandidate = type
                print("Type \(name) == \(typeIx)")
                break
            }
        }
        guard
            let type: UnsafePointer<HLType_CCompat> = .init(OpaquePointer(typeCandidate?.ccompatAddress)),
            let bindings = typeCandidate?.objProvider?.bindingsProvider
        else {
            throw TestError.unexpected("Type's '\(name)' bindings not found")
        }
        
        var result: Set<RefFun> = Set()
        for (fid, findex) in bindings {
            let objField = LibHl.hl_obj_field_fetch(type, fid)
            print("binding", fid, "findex", findex)
            print(objField.nameProvider.stringValue)
            guard only == nil || only!.contains(objField.nameProvider.stringValue) else {
                continue
            }
            
            result.insert(RefFun(findex))
            for dep in try self.extractDeps(fix: RefFun(findex), ignore: result) {
                result.insert(dep)
            }
        }
        return Array(result)
    }
    
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
            case .OStaticClosure(_, let depFun):
                fallthrough
            case .OInstanceClosure(_, let depFun, _):
                fallthrough
            case .OCall0(_, let depFun):
                var realIgnore = ignore.union(Set(result))
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
            
            return RefFun(f.pointee.findex)
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
        
        return try _compileDeps(strip: strip, mem: mem, fix: fix, depHints: depHints)
    }
    
    func _compileDeps(strip: Bool, mem: CpuOpBuffer = CpuOpBuffer(), fix: RefFun, depHints: [RefFun] = []) throws -> (RefFun, CpuOpBuffer) {
        
        let compiler = try sut(strip: strip)
        let deps = Array(try extractDeps(fix: fix, depHints: depHints)).sorted()
        
        Self.logger.debug("Compile order: \(deps)")
        
        for depFix in deps {
            do {
                try compiler.compile(findex: depFix, into: mem)
            } catch GlobalError.functionAlreadyCompiled {
                print("Not compiling @\(depFix) a second time...")
            }
        }
        return (fix, mem)
    }
    
    
    /// Patch the entrypoint to not start the main function, run it, and then run the function-under-test.
    ///
    /// Use this for functions that depend on global initialization.
    ///
    /// This is useful because the entrypoint initializes globals.
    /// - Parameters:
    ///   - strip:
    ///   - mem:
    ///   - name:
    ///   - depHints: function indexes which are dependencies of the function under test (if they cannot be determined from OCall opcodes)
    ///   - callback:
    func _withPatchedEntrypoint(strip: Bool, mem: CpuOpBuffer = CpuOpBuffer(), name fqname: String, depHints: [RefFun] = [], _ callback: (RefFun, UnsafeMutableRawPointer) throws->()) throws {
        
        guard let ep = ctx.mainContext.pointee.code?.pointee.entrypoint else {
            return XCTFail("No entrypoint")
        }
        
        let compilableEntrypoint = try ctx.getCompilable(findex: RefFun(ep))
        guard var ops = compilableEntrypoint?.ops, let secondToLast = ops.dropLast(1).last, let last = ops.last else {
            return XCTFail("Can't fetch entrypoint ops")
        }
        
        guard case .OCall0(_, _) = secondToLast, case .ORet(_) = last else {
            return XCTFail("Can't patch entrypoint, op assumption not correct")
        }
        
        ops = Array(ops.dropLast(2)) + [.ONop, last]
        ctx.patch(findex: RefFun(ep), ops: ops)
        
        let mem = CpuOpBuffer()
        let (sutFix, _) = try _compileDeps(strip: strip, mem: mem, fqname: fqname, depHints: depHints)
        
        try _compileAndLinkWithDeps(
            strip: strip,
            mem: mem,
            fix: RefFun(ep),
            // these deps can't be determined currently automatically
            // (if hashlink bytecode changes, these indexes
            // might need to be updated)
            depHints: depHints
        ) {
            epFix, jitMemory in
            print("Running entrypoint first _@\(epFix)")
            try jitMemory.jit(ctx: ctx, fix: epFix) { (entrypoint: (@convention(c) ()->())) in
                entrypoint()
            }
            print("Returning to test with \(fqname)@\(sutFix)")
            try callback(sutFix, jitMemory)
        }
    }
    
    func _compileAndLinkWithDeps(strip: Bool, mem: CpuOpBuffer = CpuOpBuffer(), name: String, depHints: [RefFun] = [], _ callback: (RefFun, UnsafeMutableRawPointer) throws->()) throws {
        let (fix, buff) = try self._compileDeps(strip: strip, mem: mem, fqname: name, depHints: depHints)
        let mapper = BufferMapper(ctx: self.ctx, buffer: buff)
        let mem = try mapper.getMemory()
        
        try callback(fix, mem)
    }
    
    func _compileAndLinkWithDeps(strip: Bool, mem: CpuOpBuffer = CpuOpBuffer(), fix: RefFun, depHints: [RefFun] = [], _ callback: (RefFun, UnsafeMutableRawPointer) throws->()) throws {
        let (fix, buff) = try self._compileDeps(strip: strip, mem: mem, fix: fix, depHints: depHints)
        let mapper = BufferMapper(ctx: self.ctx, buffer: buff)
        let mem = try mapper.getMemory()
        
        try callback(fix, mem)
        
        try mapper.freeMemory()
    }
}
