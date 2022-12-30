import Foundation

protocol CompilerCache {
    func cache(offset: ByteCount, compilable: any Compilable2, data: ArraySlice<CpuOp>) throws
    func cached(offset: ByteCount, compilable: any Compilable2) throws -> [CpuOp]?
}

class NoopCache : CompilerCache {
    func cache(offset: ByteCount, compilable: any Compilable2, data: ArraySlice<CpuOp>) throws {
        
    }
    func cached(offset: ByteCount, compilable: any Compilable2) throws -> [CpuOp]? {
        return nil
    }
}

typealias HashFunc = (ByteCount, any Compilable2)->UInt64

class DiskCache : CompilerCache {
    let hashfunc: HashFunc
    let dir: URL
    static let logger = LoggerFactory.create(DiskCache.self)
    
    init(
        dir: URL = URL(fileURLWithPath: "/tmp", isDirectory: true),
        hashfunc: @escaping HashFunc = hash_hasher) {
            self.hashfunc = hashfunc
            self.dir = dir
    }
    
    func cache(offset: ByteCount, compilable: any Compilable2, data: ArraySlice<CpuOp>) throws {
        let cacheFile = getPath(offset, compilable)
        
        print("Caching", data)
        let bytes: [UInt8] = try data.reduce([]) {
            res, opCandidate in
            
            let op: any CpuOp
            if let pseudoOpResolved = (opCandidate as? PseudoOp)?.resolve() {
                op = pseudoOpResolved
            } else {
                op = opCandidate
            }
            
            if case PseudoOp.movCallableAddress = op {
                fatalError("Fatal error, .movCallableAddress should have been resolved to .mov or .movRelative")
            }
            
            if case PseudoOp.movRelative = op {
                print("TODO: skipping this")
                return res
            }
            
            
            
            return res + (try op.emit())
        }
        
        let convertedData = Data(bytes)
        Self.logger.debug("Writing cache to \(cacheFile.absoluteString)")
        try convertedData.write(to: cacheFile, options: .atomic)
    }
    
    func cacheExists(offset: ByteCount, compilable: any Compilable2) throws -> Bool {
        let cacheFile = getPath(offset, compilable)
        return FileManager.default.fileExists(atPath: cacheFile.path)
    }
    
    func getPath(_ offset: ByteCount, _ compilable: any Compilable2) -> URL {
        let key = hashfunc(offset, compilable)
        return dir.appendingPathComponent("\(key).hashlink-arm.cache")
    }
    
    func clear(offset: ByteCount, compilable: any Compilable2) throws {
        let cacheFile = getPath(offset, compilable)
        try FileManager.default.removeItem(at: cacheFile)
    }
    
    func cached(offset: ByteCount, compilable: any Compilable2) throws -> [CpuOp]? {
        return nil
//        let cacheFile = getPath(offset, compilable)
//        guard try cacheExists(offset: offset, compilable: compilable) else {
//            Self.logger.debug("Cache does not exist at \(cacheFile.path) for \(compilable.findex) at \(offset)")
//            return nil
//        }
//
//        let loaded = try Data(contentsOf: cacheFile)
//        return Array(loaded)
    }
}

func hash_hasher(offset: ByteCount, compilable: any Compilable2) -> UInt64 {
    var hash: Int64 = 17
    
    hash = hash &* 37 &+ Int64(compilable.retProvider.kind.rawValue);
    hash = hash &* 37 &+ Int64(offset);
    
    hash = compilable.regsProvider.reduce(into: hash) {
        hasher, reg in
        
        hasher = hasher &* 37 &+ reg.ccompatAddress.immediate
    }
    
    hash = hash &* 37 &+ compilable.typeProvider.ccompatAddress.immediate
    
    hash = compilable.argsProvider.reduce(into: hash) {
        hasher, reg in
        
        hasher = hasher &* 37 &+ Int64(reg.kind.rawValue)
    }
    
    hash = compilable.ops.reduce(into: hash) {
        hasher, op in
        
        var extra: UnsafeMutableBufferPointer<Int32>? = nil
        let cc = HLOpCode_CCompat(op, &extra)
        
        hasher = hasher &* 37 &+ Int64(cc.op)
        hasher = hasher &* 37 &+ Int64(cc.p1)
        hasher = hasher &* 37 &+ Int64(cc.p2)
        hasher = hasher &* 37 &+ Int64(cc.p3)
        if let extra = extra {
            for x in extra {
                hasher = hasher &* 37 &+ Int64(x)
            }
            extra.deallocate()
        }
    }
    
    return UInt64(bitPattern: hash)
}
