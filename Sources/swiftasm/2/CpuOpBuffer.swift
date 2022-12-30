import Darwin
import Foundation

struct CachedOp: CpuOp {
    let size: ByteCount
    let data: [UInt8]
    
    init(size: ByteCount, data: [UInt8]) {
        self.size = size
        self.data = data
    }
    
    func emit() throws -> [UInt8] {
        self.data
    }
    
    var asmDescription: String {
        ".cached(\(size))"
    }
}

class CpuOpBuffer {
    var ops: [any CpuOp] = []
    var position: Int { ops.count }
    var byteSize: ByteCount = 0
    
    static let logger = LoggerFactory.create(CpuOpBuffer.self)
    
    @discardableResult func align(_ to: Int64) -> CpuOpBuffer {
        let origSize = self.byteSize
        var targetSize = origSize
        while targetSize % to != 0 { targetSize += 1 }

        while self.byteSize < targetSize {
            if origSize + 4 > targetSize {
                self.append(PseudoOp.zero)
            }
            else {
                self.append(M1Op.nop)
            }
        }

        return self
    }
    
    @discardableResult func appendWithOffset(offset: inout RelativeDeferredOffset, _ ops: any CpuOp...) -> CpuOpBuffer {
        offset.start(at: self.byteSize)
        return self.append(ops)
    }
    
    @discardableResult func append(_ instructions: any CpuOp...) -> CpuOpBuffer {
        try! _internalAppend(instructions)
    }
    
    @discardableResult func append(_ instructions: [any CpuOp]) -> CpuOpBuffer {
        try! _internalAppend(instructions)
    }

    // This should be the only place that modifies size/ops
    @discardableResult func _internalAppend(_ instructions: [any CpuOp]) throws
        -> CpuOpBuffer
    {
        // simple smoke test to help find invalid operations when they are inserted,
        // as opposed to later (when we lose context of where they originate from)
        for op in instructions {
            switch(op) {
//            case M1Op.blr, M1Op.b, M1Op.bl, M1Op.b_ge, M1Op.b_gt, M1Op.b_le, M1Op.b_lt, M1Op.b_v2, M1Op.br, M1Op.b_eq, M1Op.b_ne:
            case M1Op.b:
//                // Don't validate jumps as they might not be emittable until addresses are known
                break
            case PseudoOp.b_ne_deferred, PseudoOp.b_eq_deferred:
                // deferred likely contains a jump, so skip validation for now
                break
            case PseudoOp.mov, PseudoOp.movRelative, PseudoOp.movCallableAddress:
                // can contain a function address (which might be unavailable at this time)
                break
            default:
                do {
                    _ = try op.emit()
                } catch {
                    Self.logger.error("Error emitting \(String(describing: op)): \(String(describing: error))")
                    throw error
                }
            
            }
        }
    
        
        let increase: Int64 = instructions.reduce(0) {
            return $0 + $1.size
        }
        byteSize += increase
        ops.append(contentsOf: instructions)
        return self
    }
    
    func emitMachineCode(from: Int = 0) throws -> [UInt8] {
        try opSlice(from: 0, to: Int(position)).flatMap { try $0.emit() }
    }
    
    func opSlice(from: Int, to: Int) -> ArraySlice<any CpuOp> {
        ops[from..<position]
    }

    // print copy-pastable into a test
    func hexPrint() {
        do {
//        try lockAddresses()
        print("---- START ----")
        var printedAlready: ByteCount = 0
        for op in self.ops {
            let bytes = try op.emit()
            guard bytes.count > 0 else {
                if case PseudoOp.debugMarker(let message) = op {
                    print("// \(message)")
                }
                continue
            }
            for (ix, row) in bytes.chunked(into: 4).enumerated() {
                let strs = row.map {
                    "0x" + String($0, radix: 16).leftPadding(toLength: 2, withPad: "0")
                }
                let debugString: String
                if case PseudoOp.ascii = op {
                    debugString = String(bytes: row, encoding: .ascii)!
                        .replacingOccurrences(of: " ", with: ".")
                }
                else if ix == 0 {
                    debugString = op.asmDescription
                }
                else {
                    debugString = "... \(op.asmDescription)"
                }
                print(
                    strs.joined(separator: ", ")
                        + ", // \(debugString.replacingOccurrences(of: "\n", with: "\\n"))"
                )
            }

            printedAlready += op.size
        }
        print("---- END ----")
        } catch {
            fatalError("\(error): \(Thread.callStackSymbols.joined(separator: "\n"))")
        }
    }
}
