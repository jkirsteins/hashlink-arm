import Foundation 

// TODO: rename to PseudoM1Op, this is M1 specific
enum PseudoOp: CpuOp, CustomAsmStringConvertible {
    case zero
    case ascii(String)
    
    // load value from SP+offset using the correct size of the reg
    case ldrVreg(Register64, /*offset from sp*/ByteCount, /*vreg size*/ByteCount)
    
    // store value at SP+offset using the correct size of the reg
    case strVreg(Register64, /*offset from sp*/ByteCount, /*vreg size*/ByteCount)
    
    case deferred(ByteCount, () throws->CpuOp)
    
    // This will only show up in bytecode debug output (e.g. hexPrint()) and
    // will be ignored when emitting
    case debugMarker(String)
    
    // Generate movz+movk instructions for moving a 64-bit value
    // into a register over multiple steps
    case mov(Register64, any Immediate)
    
    static func withPrep(_ prep: ()->(), _ op: any CpuOp) -> any CpuOp {
        prep()
        return op
    }
    
    static func withOffset(offset: inout RelativeDeferredOffset, mem: CpuOpBuffer, _ op: @escaping @autoclosure ()->any CpuOp) -> any CpuOp {
        offset.start(at: mem.byteSize)
        return PseudoOp.deferred(4) { op() }
    }
    
    var size: ByteCount {
        switch(self) {
        case .ldrVreg(let reg, let off, let s):
            return Self._ldrVreg(reg, off, s).reduce(0) { $0 + $1.size }
        case .strVreg(let reg, let off, let s):
            return Self._strVreg(reg, off, s).size
        case .deferred(let size, _):
            return size
        case .debugMarker:
            return 0
        case .mov: return 16
        case .zero: return 1
        case .ascii(let v):
            return ByteCount(v.utf8.count)
        }
    }
    
    var asmDescription: String {
        switch(self) {
        case .deferred(_, let c):
            return "deferred:\(try! c().asmDescription)"
        case .debugMarker(let message):
            return message
        case .mov(let Rd, let val):
            return ".mov \(Rd), #\(val)"
        case .zero: return ".zero"
        case .ldrVreg(let reg, let offset, let regSize):
            return Self._ldrVreg(reg, offset, regSize).map { $0.asmDescription }.joined(separator: "\n")
        case .strVreg(let reg, let offset, let regSize):
            return Self._strVreg(reg, offset, regSize).asmDescription
        case .ascii(let val):
            return ".ascii(\(val))"
        }
    }
    
    static func _ldrVreg(_ reg: Register64, _ offset: ByteCount, _ regSize: ByteCount) -> [any CpuOp] {
        var result: [any CpuOp] = [
            PseudoOp.mov(reg, offset)
        ]
        switch(regSize) {
        case 8:
            result.append(M1Op.ldr(reg, .reg(X.sp, .r64ext(reg, .sxtx(0)))))
        case 4:
            result.append(M1Op.ldr(reg.to32, .reg(X.sp, .r64ext(reg, .sxtx(0)))))
        case 2:
            result.append(M1Op.ldrh(reg.to32, .reg(X.sp, .r64ext(reg, .sxtx(0)))))
        case 1:
            result.append(M1Op.ldrb(reg.to32, .reg(X.sp, .r64ext(reg, .sxtx(0)))))
        case 0:
            result.append(M1Op.nop)
        default:
            fatalError("Unsupported vreg size \(regSize)")
        }
        return result
    }
    
    static func _strVreg(_ reg: Register64, _ offset: ByteCount, _ regSize: ByteCount) -> M1Op {
        switch(regSize) {
        case 8: return M1Op.str(reg, .reg64offset(.sp, offset, nil))
        case 4: return M1Op.str(reg.to32, .reg64offset(.sp, offset, nil))
        case 2: return M1Op.strh(reg.to32, .imm64(.sp, offset, nil))
        case 1: return M1Op.strb(reg.to32, .imm64(.sp, offset, nil))
        case 0: return M1Op.nop
        default:
            fatalError("Unsupported vreg size \(regSize)")
        }
    }
    
    func emit() throws -> [UInt8] {
        switch(self) {
        case .strVreg(let reg, let offset, let regSize):
            return try Self._strVreg(reg, offset, regSize).emit()
        case .ldrVreg(let reg, let offset, let regSize):
            let res = try Self._ldrVreg(reg, offset, regSize).reduce([]) { return $0 + (try $1.emit()) }
            return res
        case .deferred(_, let closure):
            return try closure().emit()
        case .debugMarker: return []
        case .mov(let Rd, let val):
            
            guard val.hasUsableValue else {
                print("Failing in \(self) At: \(Thread.callStackSymbols.joined(separator: "\n"))")
                throw GlobalError.immediateMissingValue("Trying to emit PseudoOp.mov and val \(val) does not have a usable value.")
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
