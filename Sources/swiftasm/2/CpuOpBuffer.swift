import Darwin
import Foundation

class CpuOpBuffer {
    var ops: [any CpuOp] = []
    var position: Int { ops.count }
    var byteSize: ByteCount = 0
    
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
        let increase: Int64 = instructions.reduce(0) {
            return $0 + $1.size
        }
        byteSize += increase
        ops.append(contentsOf: instructions)
        return self
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
                    debugString = op.debugDescription
                }
                else {
                    debugString = "... \(op.debugDescription)"
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
