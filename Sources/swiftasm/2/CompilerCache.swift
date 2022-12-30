import Foundation

protocol CompilerCache {
    func cache(offset: ByteCount, compilable: any Compilable2, data: ArraySlice<CpuOp>) throws
    func cached(offset: ByteCount, compilable: any Compilable2) throws -> [CpuOp]?
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
            
            if case PseudoOp.movRelative(let Rd, _, let imm) = op {
                // [1, N, *] means the N subsequent bytes can be taken as-is (no relative offsets in there)
                let regRawValue = Rd.rawValue
                guard imm.hasUsableValue else {
                    throw GlobalError.invalidOperation("Fatal error. Relative offset not finalized before caching.")
                }
                
                throw GlobalError.invalidOperation("Can't cache a method with a relative offset")
//                return res + [1, regRawValue] + imm.immediate.getBytes()
            }
            
            // should usually be 4 bytes each, but with PseudoOp you can never be sure, so adding `chunked` for safety
            let opEmitted = try op.emit().chunked(into: 200)
            let opFlattened: [UInt8] = opEmitted.flatMap {
                subArr in
                return [0, UInt8(subArr.count)] + subArr
            }
            
            return res + opFlattened
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
        let cacheFile = getPath(offset, compilable)
        guard try cacheExists(offset: offset, compilable: compilable) else {
            Self.logger.debug("Cache does not exist at \(cacheFile.path) for \(compilable.findex) at \(offset)")
            return nil
        }
        
        Self.logger.debug("Reading cache at \(cacheFile.path) for \(compilable.findex) at \(offset)")

        
        var result: [any CpuOp] = []
        
        let reader = ByteReader(try Data(contentsOf: cacheFile))
        
        while !reader.isAtEnd {
            let x = try reader.readNUInt8(2)
            
            switch((x[0], x[1])) {
            case (0, let count):
                let cachedData = try reader.readNUInt8(Int(count))
                result.append(CachedOp(size: ByteCount(count), data: cachedData))
            case (1, _):
                // if we've relative jumps, we can't be sure they're still valid
                // (e.g. given funcs A,B,C -- if A has a relative jump to C, and B expands in the middle,
                // this will break)
//                let offset = Int64.recombine(try reader.readNUInt8(8))
//                let reg64 = Register64(rawValue: regRawValue)!
//                result.append(PseudoOp.movRelative(reg64, ctx.jitBase, offset))
                return nil
            default:
                fatalError("Invalid cache marker (must be 0 or 1, got \(x[0]))")
            }
        }
    
        
        return result
    }
}

func hash_hasher(offset: ByteCount, compilable: any Compilable2) -> UInt64 {
    var hash: Int64 = 17
    
    hash = hash &* 37 &+ Int64(compilable.retProvider.kind.rawValue);
    hash = hash &* 37 &+ Int64(offset);
    
    hash = compilable.regsProvider.reduce(into: hash) {
        hasher, reg in
        
        hasher = hasher &* 37 &+ Int64(reg.kind.rawValue)
    }
    
    hash = hash &* 37 &+ Int64(compilable.typeProvider.kind.rawValue)
    
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
