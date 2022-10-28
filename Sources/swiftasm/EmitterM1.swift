import Darwin 

public protocol Register: CustomDebugStringConvertible {
    associatedtype Shift

    var rawValue: UInt8 { get }
    var is32: Bool { get }
}

typealias X = Register64 
typealias W = Register32

extension OpBuilder
{
    @discardableResult
    func append(_ ops: M1Op...) -> OpBuilder
    {
        let x = ops.map { $0 as any CpuOp }
        return self.append(x)
    }
}

// TODO: wrong, fix w Shift64_Real
public enum Shift64: Int {
    case _0 = 0
    case _16 = 16
    case _32 = 32
    case _48 = 48
}

public enum Shift64_Real {
    case lsl(Int) 
    case lsr(Int) 
    case asr(Int) 
    case ror(Int)
}


public enum Shift32: Int {
    case _0 = 0
    case _16 = 16
}

enum Register64: UInt8, Register {
    typealias Shift = Shift64

    var is32: Bool { false }

    case x0 = 0
    case x1 = 1
    case x2 = 2
    case x3 = 3
    case x4 = 4
    case x5 = 5
    case x6 = 6
    case x7 = 7
    case x8 = 8
    case x9 = 9
    case x10 = 10
    case x11 = 11
    case x12 = 12
    case x13 = 13
    case x14 = 14
    case x15 = 15
    case x16 = 16
    case x17 = 17
    case x18 = 18
    case x19 = 19
    case x20 = 20
    case x21 = 21
    case x22 = 22
    case x23 = 23
    case x24 = 24
    case x25 = 25
    case x26 = 26
    case x27 = 27
    case x28 = 28
    case x29_fp = 29  // frame pointer
    case x30_lr = 30  // /link register
    case sp = 31

    var debugDescription: String {
        switch(self) {
            case .sp:
                return "sp"
            case .x29_fp:
                return "x29"
            case .x30_lr:
                return "x30"
            default:
                return "x\(self.rawValue)"
        }
    }
}

enum ExtendOp32 {
    case sxtw(Register32.Shift?)  // ExtendSigned32To64
    case uxtw(Register32.Shift?)  // ExtendUnsigned32To64
}

enum IndexingMode {
    // If specified, the offset we can use must be in the range -256 to 255
    case pre
    case post
}

enum Register32: UInt8, Register {
    typealias Shift = Shift32
    typealias ExtendOp = ExtendOp32
    var is32: Bool { true }

    var debugDescription: String {
        return "w\(self.rawValue)"
    }

    case w0 = 0
    case w1 = 1
    case w2 = 2
    case w3 = 3
    case w4 = 4
    case w5 = 5
    case w6 = 6
    case w7 = 7
    case w8 = 8
    case w9 = 9
    case w10 = 10
    case w11 = 11
    case w12 = 12
    case w13 = 13
    case w14 = 14
    case w15 = 15
    case w16 = 16
    case w17 = 17
    case w18 = 18
    case w19 = 19
    case w20 = 20
    case w21 = 21
    case w22 = 22
    case w23 = 23
    case w24 = 24
    case w25 = 25
    case w26 = 26
    case w27 = 27
    case w28 = 28
    case wZR = 31 // 0b11111
}

enum LdrMode {
    case _32(Register32, Register64, Offset?)  // e.g. w0 <- [x1]
    case _64(Register64, Register64, Offset?)  // e.g. x0 <- [x1]
}

enum Offset {
    case immediate(Int16)
    case reg64offset(Register64, Int64, IndexingMode?)
    case reg32shift(Register32, Register32.Shift?)
    case reg64shift(Register64, Register64.Shift?)
    case reg32(Register32, Register32.ExtendOp, IndexingMode?)
}

public enum EmitterM1Error: Error, Equatable {
    case invalidShift
    case unsupportedOp
    case invalidRegister(_ reason: String)
    case invalidOffset(_ reason: String)
    case invalidValue(_ reason: String)
}

public class EmitterM1 {
    private static func returnAsArray(_ val: Int64) -> [UInt8] {
        let length: Int = 4 * MemoryLayout<UInt8>.size
        let result = withUnsafeBytes(of: val) { bytes in Array(bytes.prefix(length)) }
         print(
             "Returning \(result.map { String($0, radix: 16).leftPadding(toLength: 2, withPad: "0") })"
         )
        return result
    }

    static func encodeReg(_ reg: any Register, shift: Int64) -> Int64 {
        (Int64(0b11111) & Int64(reg.rawValue)) << shift
    }

    static func truncateOffset(_ val: Int64, divisor: Int64, bits: Int64) throws
        -> Int64
    {
        if val % divisor != 0 {
            throw EmitterM1Error.invalidOffset(
                "truncateOffset: offset immediate must be a multiple of \(divisor) but was \(val)"
            )
        }

        let divided = val / divisor
        let mask: Int64 = ((1 << bits) - 1)
        // Check if we fit in required number of bits
        let compare: Int64
        if divided >= 0 {
            compare = divided & mask
        }
        else {
            let rmask: Int64 = (~mask | 0b1000000)
            compare = (divided & mask) | rmask
        }
        guard compare == divided else {
            throw EmitterM1Error.invalidOffset(
                "Offset immediate \(val) must fit in \(bits) bits"
            )
        }

        // apply mask otherwise a negative value will contain leading 1s,
        // which can mess up when shifting left later
        return (mask & divided)
    }

    // 0b0b10101010000000100000001111100000

    func emit(for op: M1Op) throws -> [UInt8] {
        try Self.emit(for: op)
    }

    static func emit(for op: M1Op) throws -> [UInt8] {
        switch op.resolveFinalForm() {  // resolve potential aliases
        case .sub(let Rd, let Rn, let offset):
            guard Rd.is32 == Rn.is32 else {
                throw EmitterM1Error.invalidRegister("Rd and Rn must have same size")
            }
            guard offset.imm.isPositive else {
                return try emit(for: .add(Rd, Rn, Imm12Lsl12(offset.imm.flippedSign, lsl: offset.lsl)))
            }

            //                  S          sh imm12        Rn    Rd
            let mask: Int64 = 0b0_10100010_0__000000000000_00000_00000
            let encodedRd: Int64 = encodeReg(Rd, shift: 0)
            let encodedRn: Int64 = encodeReg(Rn, shift: 5)
            let size: Int64 = (Rd.is32 ? 0 : 1) << 31
            let sh: Int64 = (offset.lsl == ._0 ? 0 : 1) << 22
            let imm: Int64 = offset.imm.shiftedLeft(10)
            let encoded: Int64 = mask | encodedRd | encodedRn | size | sh | imm
            return returnAsArray(encoded)
        case .add(let Rd, let Rn, let offset):
            guard Rd.is32 == Rn.is32 else {
                throw EmitterM1Error.invalidRegister("Rd and Rn must have same size")
            }
            guard offset.imm.isPositive else {
                return try emit(for: .sub(Rd, Rn, Imm12Lsl12(offset.imm.flippedSign, lsl: offset.lsl)))
            }
            
            //                  S          sh imm12        Rn    Rd
            let mask: Int64 = 0b0_00100010_0__000000000000_00000_00000
            let encodedRd: Int64 = encodeReg(Rd, shift: 0)
            let encodedRn: Int64 = encodeReg(Rn, shift: 5)
            let size: Int64 = (Rd.is32 ? 0 : 1) << 31
            let sh: Int64 = (offset.lsl == ._0 ? 0 : 1) << 22
            let imm: Int64 = offset.imm.shiftedLeft(10)
            let encoded: Int64 = mask | encodedRd | encodedRn | size | sh | imm
            return returnAsArray(encoded)
        case .stur(let Rt, let Rn, let offset ):
            //                    S           imm9         Rn    Rt
            let mask: Int64 = 0b1_0_111000000_000000000_00_00000_00000
            let encodedRt = encodeReg(Rt, shift: 0)
            let encodedRn = encodeReg(Rn, shift: 5)
            let offs = (try truncateOffset(Int64(offset), divisor: 1, bits: 9)) << 12
            let size: Int64 = (Rt.is32 ? 0 : 1) << 30
            let encoded = mask | encodedRt | encodedRn | offs | size
            return returnAsArray(encoded)
        case .str(let Rt, let offset ):
            guard case .reg64offset(let Rn, let offsetCount, let ixMode) = offset else {
                throw EmitterM1Error.invalidOffset(
                    "STR can only have .reg64offset offset"
                )
            }
            let mask: Int64
            switch(ixMode) {
                case nil: 
                    return try Self.emit(for: .stur(Rt, Rn, Int16(offsetCount)))
                    // TODO: stur should only be used when not divisible by 9 ^^^
                case .pre: 
                    //         S           imm9         Rn    Rt
                    mask = 0b1_0_111000000_000000000_11_00000_00000
                case .post: 
                    //         S           imm9         Rn    Rt
                    mask = 0b1_0_111000000_000000000_01_00000_00000
            }
            let encodedRt = encodeReg(Rt, shift: 0)
            let encodedRn = encodeReg(Rn, shift: 5)
            let offs = (try truncateOffset(Int64(offsetCount), divisor: 1, bits: 9)) << 12
            let size: Int64 = (Rt.is32 ? 0 : 1) << 30
            let encoded = mask | encodedRt | encodedRn | offs | size
            return returnAsArray(encoded)
        case .svc(let imm16):
            //                              imm16
            let mask: Int64 = 0b11010100000_0000000000000000_00001
            let encoded = mask | Int64(bitPattern: UInt64(imm16)) << 5
            return returnAsArray(encoded)
        case .adr64(let Rd, let offset):
            guard Rd != .sp else {
                throw EmitterM1Error.invalidRegister("Rd can not be SP for adr")
            }
            let immlo: Int64 = (Int64(offset.value) & 0b11) << 29
            let immhi: Int64 = (Int64(offset.value) & 0b111111111111111111100) << 3
            let encodedRd = encodeReg(Rd, shift: 0)
            let mask: Int64 = 1 << 28
            let encoded = mask | encodedRd | immlo | immhi 
            return returnAsArray(encoded)
        case .orr64(let Rd, let WZr, let Rn, let shift) where WZr == .sp && shift == nil:
            fallthrough
        case .movr64(let Rd, let Rn) where Rd == .sp || Rn == .sp: 
            let mask: Int64 = 0b10010001_00000000_00000000_00000000
            let encodedRd = encodeReg(Rd, shift: 0)
            let encodedRn = encodeReg(Rn, shift: 5)
            let imm12: Int64 = 0
            let encoded = mask | encodedRd | encodedRn | imm12
            return returnAsArray(encoded)
        case .movr64(let Rd, let Rm) where Rd != .sp && Rm != .sp:
            let mask: Int64 = 0b10101010_00000000_00000000_00000000
                              
            let encodedRd = encodeReg(Rd, shift: 0)
            let encodedRn = encodeReg(Register64.sp, shift: 5) // 0b11111
            let encodedRm = encodeReg(Rm, shift: 16) 
            let imm6: Int64 = 0
            let encoded = mask | encodedRm | encodedRd | encodedRn | imm6
            return returnAsArray(encoded)
        case .ldp(let pair, let offset):
            guard case .reg64offset(let Rn, let offsetCount, let ixMode) = offset else {
                throw EmitterM1Error.invalidOffset(
                    "LDP can only have .reg64offset offset"
                )
            }

            let divisor: Int64 = 8  // 64-bit ops. 32-bit ops have divisor 4
            let truncated = try truncateOffset(offsetCount, divisor: divisor, bits: 7)

            let (Rt1, Rt2) = pair
            
            let mask: Int64
            switch ixMode {
            case nil: 
                //       o      ixm imm7    Rt2   Rn    Rt1
                mask = 0b0010100101_0000000_00000_00000_00000
            case .pre: 
                //       o      ixm imm7    Rt2   Rn    Rt1
                mask = 0b0010100111_0000000_00000_00000_00000
            case .post: 
                //       o      ixm imm7    Rt2   Rn    Rt1
                mask = 0b0010100011_0000000_00000_00000_00000
            }

            let opc: Int64 = 0b10   // 64-bit hardcoded (otherwise 0b00)
            let opcOffset: Int64 = 30
            let encodedRt1: Int64 = encodeReg(Rt1, shift: 0)
            let encodedRt2: Int64 = encodeReg(Rt2, shift: 10)
            let encodedRn: Int64 = encodeReg(Rn, shift: 5)
            let imm: Int64 = truncated << 15
            let encoded =
                encodedRt1 | encodedRt2 | encodedRn | (opc << opcOffset) | mask | imm
            return returnAsArray(encoded)
        case .stp(let pair, let offset):
            guard case .reg64offset(let Rn, let offsetCount, let ixMode) = offset else {
                throw EmitterM1Error.invalidOffset(
                    "STP can only have .reg64offset offset"
                )
            }

            let divisor: Int64 = 8  // 64-bit ops. 32-bit ops have divisor 4
            let truncated = try truncateOffset(offsetCount, divisor: divisor, bits: 7)
            let (Rt1, Rt2) = pair
            let encodedRt1: Int64 = encodeReg(Rt1, shift: 0)
            let encodedRt2: Int64 = encodeReg(Rt2, shift: 10)
            let encodedRn: Int64 = encodeReg(Rn, shift: 5)
            let opc: Int64 = 0b10
            let opcOffset: Int64 = 30

            let mask: Int64
            switch ixMode {
            case nil: mask = 0b0010_1001_0000_0000_0000_0000_0000_0000
            case .pre: mask = 0b0010_1001_1000_0000_0000_0000_0000_0000
            case .post: mask = 0b0010_1000_1000_0000_0000_0000_0000_0000
            }
            let imm: Int64 = truncated << 15
            let encoded =
                encodedRt1 | encodedRt2 | encodedRn | (opc << opcOffset) | mask | imm
            return returnAsArray(encoded)
        case .b(let imm26):
            let imm = try truncateOffset(Int64(imm26.value), divisor: 4, bits: 26)
            //                         imm26
            let mask: Int64 = 0b000101_00000000000000000000000000
            let encoded = mask | imm
            return returnAsArray(encoded)
        case .bl(let imm26):
            guard (imm26.immediate & 0x3FFFFFF) == imm26.immediate else {
                throw EmitterM1Error.invalidValue(
                    "BL requires the immediate to fit in 26 bits"
                )
            }
            guard imm26.immediate % 4 == 0 else {
                throw EmitterM1Error.invalidValue(
                    "BL requires the immediate to be a multiple of 4"
                )
            }
            let mask: Int64 = 0b1001_0100_0000_0000_0000_0000_0000_0000
            return returnAsArray(mask | Int64(imm26.immediate / 4))
        case .blr(let Rn):
            let mask: Int64 = 0b1101_0110_0011_1111_0000_0000_0000_0000
            let encodedRn = encodeReg(Rn, shift: 5)
            return returnAsArray(mask | encodedRn)
        case .nop: return [0x1f, 0x20, 0x03, 0xd5]
        case .ret: return [0xc0, 0x03, 0x5f, 0xd6]
        case .ldur(let Rt, let Rn, let offset ):
            //                    S           imm9         Rn    Rt
            let mask: Int64 = 0b1_0_111000010_000000000_00_00000_00000
            let encodedRt = encodeReg(Rt, shift: 0)
            let encodedRn = encodeReg(Rn, shift: 5)
            let offs = offset.immediate.shiftedLeft(12)
            let size: Int64 = (Rt.is32 ? 0 : 1) << 30
            let encoded = mask | encodedRt | encodedRn | offs | size
            return returnAsArray(encoded)
        case .ldr(let Rt, let offset):
        
            guard case .reg64offset(let Rn, let offsetCount, let ixMode) = offset else {
                throw EmitterM1Error.invalidOffset(
                    "LDR can only have .reg64offset offset"
                )
            }

            let mask: Int64
            let immBits: Int64
            let immShift: Int64
            let divider: Int64
            switch(ixMode) {
                case nil: 
                    //         S          imm12        Rn    Rt
                    mask = 0b1_0_11100101_000000000000_00000_00000
                    immBits = 12
                    immShift = 10
                    divider = Rt.is32 ? 4 : 8 // pimm
                case .pre: 
                    //         S           imm9
                    mask = 0b1_0_111000010_000000000_11_00000_00000
                    immBits = 9
                    immShift = 12
                    divider = 1 // simm
                case .post: 
                    //         S           imm9         Rn    Rt
                    mask = 0b1_0_111000010_000000000_01_00000_00000
                    immBits = 9
                    immShift = 12
                    divider = 1 // simm
            }
            let encodedRt = encodeReg(Rt, shift: 0)
            let encodedRn = encodeReg(Rn, shift: 5)
            let imm = (try truncateOffset(Int64(offsetCount), divisor: divider, bits: immBits)) << immShift
            let size: Int64 = (Rt.is32 ? 0 : 1) << 30
            let encoded = mask | encodedRt | encodedRn | imm | size
            return returnAsArray(encoded)
        case .movk64(let register, let val, let shift):
            // xx1x 0010 1xxi iiii iiii iiii iiid dddd
            let encodedR = encodeReg(register, shift: 0)
            let encodedVal: Int64 = (Int64(val) << 5) & 0b0001_1111_1111_1111_1110_0000
            let mask: Int64 = 0b1111_0010_1000_0000_0000_0000_0000_0000
            let hwMask: Int64 = 0b0000_0000_0110_0000_0000_0000_0000_0000
            let shiftVal: Int64

            if let shift = shift {
                let shiftValPre = (Int64)((shift.rawValue / 16) << 21)
                shiftVal = shiftValPre & hwMask
            }
            else {
                shiftVal = 0
            }

            let encoded: Int64 = encodedR | encodedVal | shiftVal | mask
            return returnAsArray(encoded)
        case .movz64(let register, let val, let shift):
            // https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/MOVZ--Move-wide-with-zero-?lang=en#MOVZ_32_movewide

            // x10x 0010 1xxi iiii iiii iiii iiid dddd  -  movz Rd HALF
            let encodedR = encodeReg(register, shift: 0)
            let encodedVal: Int64 = (Int64(val) << 5) & 0b0001_1111_1111_1111_1110_0000
            let mask: Int64 = 0b1101_0010_1000_0000_0000_0000_0000_0000
            let hwMask: Int64 = 0b0000_0000_0110_0000_0000_0000_0000_0000
            let shiftVal: Int64

            if let shift = shift {
                let shiftValPre = (Int64)((shift.rawValue / 16) << 21)
                shiftVal = shiftValPre & hwMask
            }
            else {
                shiftVal = 0
            }

            let encoded: Int64 = encodedR | encodedVal | shiftVal | mask
            return returnAsArray(encoded)
        case .subs(let Rd, let Rn, .reg64shift(let Rm, nil)):
            guard Rd.is32 == Rn.is32 && Rd.is32 == Rm.is32 else {
                fatalError("All registers must be the same size")
            }
            let mask: Int64 = 0b01101011000000000000000000000000
            let encodedRd: Int64 = encodeReg(Rd, shift: 0)
            let encodedRn: Int64 = encodeReg(Rn, shift: 5)
            let encodedRm: Int64 = encodeReg(Rm, shift: 16)
            let imm6: Int64 = 0
            let shift: Int64 = 0
            let size: Int64 = (Rd.is32 ? 0 : 1) << 31
            let encoded: Int64 = mask | size | shift | encodedRm | imm6 | encodedRn | encodedRd
            return returnAsArray(encoded)
        case .b_lt(let imm):
            //                           imm19                 cond
            let mask: Int64 = 0b01010100_0000000000000000000_0_1011
            let imm16: Int64 = (imm.shiftedRight(2) /* div by 4 */) << 5
            let encoded = mask | imm16
            return returnAsArray(encoded)
        case .b_eq(let imm):
            //                           imm19                 cond
            let mask: Int64 = 0b01010100_0000000000000000000_0_0000
            let imm16: Int64 = (imm.shiftedRight(2) /* div by 4 */) << 5
            let encoded = mask | imm16
            return returnAsArray(encoded)
        case .b_ne(let imm):
            //                           imm19                 cond
            let mask: Int64 = 0b01010100_0000000000000000000_0_0001
            let imm16: Int64 = (imm.shiftedRight(2) /* div by 4 */) << 5
            let encoded = mask | imm16
            return returnAsArray(encoded)
        case .b_gt(let imm):
            //                           imm19                 cond
            let mask: Int64 = 0b01010100_0000000000000000000_0_1100
            let imm16: Int64 = (imm.shiftedRight(2) /* div by 4 */) << 5
            let encoded = mask | imm16
            return returnAsArray(encoded)
        default: throw EmitterM1Error.unsupportedOp
        }
    }
}

extension String {
    func splitString(_ withSize: Int) -> [String] {
        let a = self[self.startIndex..<self.index(self.startIndex, offsetBy: withSize)]
        let b = self[self.index(self.startIndex, offsetBy: withSize)..<self.endIndex]
        return [String(a), String(b)]
    }

    func leftPadding(toLength: Int, withPad: String = " ") -> String {

        guard toLength > self.count else { return self }

        let padding = String(repeating: withPad, count: toLength - self.count)
        return padding + self
    }

    func rightPadding(toLength: Int, withPad: String = " ") -> String {

        guard toLength > self.count else { return self }

        let padding = String(repeating: withPad, count: toLength - self.count)
        return self + padding
    }
}
