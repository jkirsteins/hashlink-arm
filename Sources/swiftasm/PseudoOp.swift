enum PseudoOp: CpuOp, CustomDebugStringConvertible {
    case zero
    case ascii(String)

    // This will only show up in bytecode debug output (e.g. hexPrint()) and
    // will be ignored when emitting 
    case debugMarker(String)    

    // Generate movz+movk instructions for moving a 64-bit value 
    // into a register over multiple steps
    case mov(Register64, any Immediate)

    var size: ByteCount {
        switch(self) {
            case .debugMarker: return 0
            case .mov: return 16
            case .zero: return 1
            case .ascii(let v): return ByteCount(v.utf8.count)
        }
    }

    var debugDescription: String {
        switch(self) {
            case .debugMarker(let message):
                return message
            case .mov(let Rd, let val):
                return ".mov \(Rd), #\(val)"
            case .zero: return ".zero"
            case .ascii(let val):
                return ".ascii(\(val))"
        }
    }

    func emit() throws -> [UInt8] {
        switch(self) {
            case .debugMarker: return []
            case .mov(let Rd, let val):

                guard val.hasUsableValue else {
                    throw GlobalError.immediateMissingValue
                }
                
                let v1 = UInt16(Int(val.immediate) & 0xFFFF)
                let v2 = UInt16((Int(val.immediate) >> 16) & 0xFFFF)
                let v3 = UInt16((Int(val.immediate) >> 32) & 0xFFFF)
                let v4 = UInt16((Int(val.immediate) >> 48) & 0xFFFF)
                
                return (try M1Op.movz64(Rd, v1, ._0).emit()) +
                    (try M1Op.movk64(Rd, v2, ._16).emit()) +
                    (try M1Op.movk64(Rd, v3, ._32).emit()) +
                    (try M1Op.movk64(Rd, v4, ._48).emit())
                
            case .zero: return [0]
            case .ascii(let val):
                return Array(val.utf8)
        }
    }
}