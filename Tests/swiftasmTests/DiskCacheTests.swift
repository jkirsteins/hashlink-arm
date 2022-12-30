import XCTest

@testable import swiftasm

final class DiskCacheTests: RealHLTestCase {
    override var HL_FILE: String { "mod2" }
    
    func testInt64Splitting() throws {
        let a: Int64 = 0xDEADBEEF
        let b = a.getBytes()
        let c = Int64.recombine(b)
        
        XCTAssertEqual(b, [0xEF, 0xBE, 0xAD, 0xDE, 0, 0, 0, 0])
        XCTAssertEqual(c, a)
    }
    
    func testHash_Hasher__same() throws {
        let fixA = _findFindex(name: "testCache_funcA")!
        let fixB = _findFindex(name: "testCache_funcB")!
        
        let funcA = try ctx.getCompilable(findex: fixA)!
        let funcB = try ctx.getCompilable(findex: fixB)!
        
        let hashA = hash_hasher(offset: 123, compilable: funcA)
        let hashB = hash_hasher(offset: 123, compilable: funcB)
        let hashB_wrongOffset = hash_hasher(offset: 124, compilable: funcB)
        
        XCTAssertEqual(hashA, hashB)
        XCTAssertNotEqual(hashA, hashB_wrongOffset)
    }
    
    func testHash_Hasher__diff() throws {
        let fixA = _findFindex(name: "testCache_funcA")!
        let fixB = _findFindex(name: "testEnumAssocData")!
        
        let funcA = try ctx.getCompilable(findex: fixA)!
        let funcB = try ctx.getCompilable(findex: fixB)!
        
        let hashA = hash_hasher(offset: 0, compilable: funcA)
        let hashB = hash_hasher(offset: 0, compilable: funcB)
        
        XCTAssertNotEqual(hashA, hashB)
    }
    
    func testHash_Hasher__cacheWithDependency() throws {
        let sut = DiskCache()
        let fixA = _findFindex(name: "testCache_funcA")!
        let depFix = _findFindex(name: "testCache_targetFunc")!
        let compiler = M1Compiler2(ctx: ctx, cache: sut)
        let mem = CpuOpBuffer()
        let memFromCache = CpuOpBuffer()
        let compilableA = try ctx.getCompilable(findex: fixA)!
        let depCompilable = try ctx.getCompilable(findex: depFix)!
        
        // can't cache, because dependency is not compiled
        try compiler.compile(findex: fixA, into: mem)
        XCTAssertFalse(try sut.cacheExists(offset: 0, compilable: compilableA))
        let fixASlice = mem.opSlice(from: 0, to: mem.position)
        XCTAssertThrowsError(try sut.cache(offset: 0, compilable: compilableA, data: fixASlice))
        
        let depFixOffset = mem.byteSize
        try compiler.compile(findex: depFix, into: mem)
        
        defer {
            do {
                try sut.clear(offset: depFixOffset, compilable: depCompilable)
            } catch {
                print("Failed to clear cache: \(error)")
            }
        }
        
        // still can't cache due to a relative jump
        XCTAssertThrowsError(try sut.cache(offset: 0, compilable: compilableA, data: fixASlice))
        XCTAssertFalse(try sut.cacheExists(offset: 0, compilable: compilableA))
        // but the dependency should be cached
        XCTAssertTrue(try sut.cacheExists(offset: depFixOffset, compilable: depCompilable))
        
        // MARK: now reuse the cache successfully
        
        let mod = Bundle.module.url(forResource: HL_FILE, withExtension: "hl")!.path
        let cacheCtx = try CCompatJitContext(mod)
        
        defer {
            guard let mToRemove = cacheCtx.mainContext.pointee.m else {
                fatalError("Can't call stop when no module is allocated")
            }
            
            cacheCtx.mainContext.pointee.m = nil
            LibHl._hl_module_free(mToRemove)
        }
        
        let compilerFromCache = M1Compiler2(ctx: cacheCtx, cache: sut)
        
        try compilerFromCache.compile(findex: fixA, into: memFromCache)
        try compilerFromCache.compile(findex: depFix, into: memFromCache)
        
        // NOTE: we can't compare the ops but only the bytecode size
        // because the ops:
        //   - in the dependency, will have stripped debugMarker pseudo ops
        //   - in the callee, will include the debugMarker pseudo ops (as this is not cached)
        XCTAssertEqual(memFromCache.byteSize, mem.byteSize)

        // function loaded from cache should be executable
        let mapper = BufferMapper(ctx: cacheCtx, buffer: memFromCache)
        let linkedMem = try mapper.getMemory()
        try linkedMem.jit(ctx: cacheCtx, fix: fixA) {
            (entrypoint: (@convention(c) (Float32, UInt8) -> Float64)) in

            XCTAssertEqualDouble(3.0, entrypoint(1.0, 2))
        }
    }
    
    func testHash_Hasher__cacheWithoutDependency() throws {
        let sut = DiskCache()
        let fix = _findFindex(name: "testCache_targetFunc")!
        let compiler = M1Compiler2(ctx: ctx, cache: sut)
        let mem = CpuOpBuffer()
        let memFromCache = CpuOpBuffer()
        let compilable = try ctx.getCompilable(findex: fix)!
        
        try compiler.compile(findex: fix, into: mem)
        let fixASlice = mem.opSlice(from: 0, to: mem.position)
        
        XCTAssertTrue(try sut.cacheExists(offset: 0, compilable: compilable))
        
        defer { try? sut.clear(offset: 0, compilable: compilable) }
        
        // MARK: now reuse the cache successfully
        
        let mod = Bundle.module.url(forResource: HL_FILE, withExtension: "hl")!.path
        let cacheCtx = try CCompatJitContext(mod)
        
        defer {
            guard let mToRemove = cacheCtx.mainContext.pointee.m else {
                fatalError("Can't call stop when no module is allocated")
            }
            
            cacheCtx.mainContext.pointee.m = nil
            LibHl._hl_module_free(mToRemove)
        }
        
        let compilerFromCache = M1Compiler2(ctx: cacheCtx, cache: sut)
        
        try compilerFromCache.compile(findex: fix, into: memFromCache, requireCache: true)
        
        // should be just the same number of ops (excluding fake 0-size debug markers)
        XCTAssertEqual(memFromCache.ops.count, mem.ops.filter({ $0.size > 0 }).count)
        // should be same size of bytecode
        XCTAssertEqual(memFromCache.byteSize, mem.byteSize)

        // function loaded from cache should be executable
        let mapper = BufferMapper(ctx: self.ctx, buffer: memFromCache)
        let linkedMem = try mapper.getMemory()
        try linkedMem.jit(ctx: ctx, fix: fix) {
            (entrypoint: (@convention(c) (Float32, UInt8) -> Float64)) in

            XCTAssertEqualDouble(3.0, entrypoint(1.0, 2))
        }
    }
}
