import XCTest

@testable import swiftasm

final class DiskCacheTests: RealHLTestCase {
    override var HL_FILE: String { "mod2" }
    
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
    
    func testHash_Hasher__simpleCacheInTmp() throws {
        let sut = DiskCache()
        let fixA = _findFindex(name: "testCache_funcA")!
        let compiler = M1Compiler2(ctx: ctx, cache: sut)
        let mem = CpuOpBuffer()
        let memFromCache = CpuOpBuffer()
        let compilableA = try ctx.getCompilable(findex: fixA)!
        
        // should compile and cache
        try compiler.compile(findex: fixA, into: mem)
        defer { try? sut.clear(offset: 0, compilable: compilableA) }
        
        // should reuse cache
        try compiler.compile(findex: fixA, into: memFromCache)
        
        let cached = try sut.cached(offset: 0, compilable: compilableA)
        XCTAssertNotNil(cached)
        
        // should be just the same number of ops, and same byte size
        XCTAssertEqual(memFromCache.ops.count, mem.ops.count)
        XCTAssertEqual(memFromCache.byteSize, mem.byteSize)
        
        // function loaded from cache should be executable
        let mapper = BufferMapper(ctx: self.ctx, buffer: memFromCache)
        let linkedMem = try mapper.getMemory()
        try linkedMem.jit(ctx: ctx, fix: fixA) {
            (entrypoint: (@convention(c) (Float32, UInt8) -> Float64)) in
            
            XCTAssertEqualDouble(3.0, entrypoint(1.0, 2))
        }
    }
}
