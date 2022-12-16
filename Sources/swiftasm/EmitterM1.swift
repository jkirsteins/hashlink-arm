import Darwin

protocol Register: Equatable, CustomDebugStringConvertible {
    var rawValue: UInt8 { get }
    var i: (any RegisterI)? { get }
    var fp: (any RegisterFP)? { get }
    
    var is64: Bool { get }
}

protocol RegisterI: Register, Equatable, CustomDebugStringConvertible {
    associatedtype Shift

    var is32: Bool { get }
    
    var to64: Register64 { get }
    var to32: Register32 { get }
    
    func sameSize(as: any RegisterI) -> any RegisterI
}

protocol RegisterFP: Register, Equatable, CustomDebugStringConvertible {
    var rawValue: UInt8 { get }
    
    var half: Bool { get }
    var single: Bool { get }
    var double: Bool { get }
    
    var bits: Int { get }
}

extension RegisterFP {
    var to64: RegisterFP64 { RegisterFP64(rawValue: self.rawValue)! }
    var to32: RegisterFP32 { RegisterFP32(rawValue: self.rawValue)! }
    var to16: RegisterFP16 { RegisterFP16(rawValue: self.rawValue)! }
    
    var is32: Bool { bits == 32 }
    var is64: Bool { bits == 64 }
}

extension RegisterI {
    var is64: Bool { !is32 }
}

typealias X = Register64
typealias D = RegisterFP64
typealias S = RegisterFP32
typealias H = RegisterFP16
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
public enum Shift64: Int, CustomAsmStringConvertible {
    case _0 = 0
    case _16 = 16
    case _32 = 32
    case _48 = 48
    
    public var asmDescription: String {
        "\(self.rawValue)"
    }
}

typealias ShiftAmount = UInt8

enum ExtendOp32 : CustomAsmStringConvertible {
    case sxtw(ShiftAmount)  // ExtendSigned32To64
    case uxtw(ShiftAmount)  // ExtendUnsigned32To64
    
    var asmDescription: String {
        switch(self) {
        case .sxtw(let shift):
            return "sxtw #\(shift)"
        case .uxtw(let shift):
            return "uxtw #\(shift)"
        }
    }
}

enum ExtendOp64 : CustomAsmStringConvertible {
    case sxtx(ShiftAmount)  // ExtendSigned64To64
    
    var asmDescription: String {
        switch(self) {
        case .sxtx(let shift):
            return "sxtx #\(shift)"
        }
    }
}

public enum Shift64_Real : CustomAsmStringConvertible {
    case lsl(Int) 
    case lsr(Int) 
    case asr(Int) 
    case ror(Int)
    
    public var asmDescription: String {
        switch(self) {
        case .lsl(let v): return "lsl #\(v)"
        case .lsr(let v): return "lsr #\(v)"
        case .asr(let v): return "asr #\(v)"
        case .ror(let v): return "ror #\(v)"
        }
    }
}


public enum Shift32: Int, CustomAsmStringConvertible {
    case _0 = 0
    case _16 = 16
    
    public var asmDescription: String {
        "\(self.rawValue)"
    }
}

enum Register64: UInt8, RegisterI {
    var i: (any RegisterI)? { self }
    var fp: (any RegisterFP)? { nil }
    
    typealias Shift = Shift64
    typealias ExtendOp = ExtendOp64

    var is32: Bool { false }
    
    var to32: Register32 {
        Register32(rawValue: rawValue)!
    }
    
    var to64: Register64 {
        self
    }

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
    
    static let xZR = Register64.sp

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

enum RegisterFP64: UInt8, RegisterFP {
    
    var half: Bool { false }
    var single: Bool { false }
    var double: Bool { true }
    
    var i: (any RegisterI)? { nil }
    var fp: (any RegisterFP)? { self }
    
    var bits: Int {
        64
    }
    
    case d0 = 0
    case d1 = 1
    case d2 = 2
    case d3 = 3
    case d4 = 4
    case d5 = 5
    case d6 = 6
    case d7 = 7
    case d8 = 8
    case d9 = 9
    case d10 = 10
    case d11 = 11
    case d12 = 12
    case d13 = 13
    case d14 = 14
    case d15 = 15
    case d16 = 16
    case d17 = 17
    case d18 = 18
    case d19 = 19
    case d20 = 20
    case d21 = 21
    case d22 = 22
    case d23 = 23
    case d24 = 24
    case d25 = 25
    case d26 = 26
    case d27 = 27
    case d28 = 28
    case d29 = 29
    case d30 = 30
    
    var debugDescription: String {
        "d\(self.rawValue)"
    }
}

enum RegisterFP32: UInt8, RegisterFP {
    
    var half: Bool { false }
    var single: Bool { true }
    var double: Bool { false }
    
    var i: (any RegisterI)? { nil }
    var fp: (any RegisterFP)? { self }
    
    var bits: Int {
        32
    }
    
    case s0 = 0
    case s1 = 1
    case s2 = 2
    case s3 = 3
    case s4 = 4
    case s5 = 5
    case s6 = 6
    case s7 = 7
    case s8 = 8
    case s9 = 9
    case s10 = 10
    case s11 = 11
    case s12 = 12
    case s13 = 13
    case s14 = 14
    case s15 = 15
    case s16 = 16
    case s17 = 17
    case s18 = 18
    case s19 = 19
    case s20 = 20
    case s21 = 21
    case s22 = 22
    case s23 = 23
    case s24 = 24
    case s25 = 25
    case s26 = 26
    case s27 = 27
    case s28 = 28
    case s29 = 29
    case s30 = 30
    
    var debugDescription: String {
        "s\(self.rawValue)"
    }
}

enum RegisterFP16: UInt8, RegisterFP {
    
    var half: Bool { true }
    var single: Bool { false }
    var double: Bool { false }
    
    var i: (any RegisterI)? { nil }
    var fp: (any RegisterFP)? { self }
    
    var bits: Int {
        16
    }
    
    case h0 = 0
    case h1 = 1
    case h2 = 2
    case h3 = 3
    case h4 = 4
    case h5 = 5
    case h6 = 6
    case h7 = 7
    case h8 = 8
    case h9 = 9
    case h10 = 10
    case h11 = 11
    case h12 = 12
    case h13 = 13
    case h14 = 14
    case h15 = 15
    case h16 = 16
    case h17 = 17
    case h18 = 18
    case h19 = 19
    case h20 = 20
    case h21 = 21
    case h22 = 22
    case h23 = 23
    case h24 = 24
    case h25 = 25
    case h26 = 26
    case h27 = 27
    case h28 = 28
    case h29 = 29
    case h30 = 30
    
    var debugDescription: String {
        "h\(self.rawValue)"
    }
}

enum IndexingMode {
    // If specified, the offset we can use must be in the range -256 to 255
    case pre
    case post
}

extension RegisterI {
    func sameSize(as other: any RegisterI) -> any RegisterI {
        if other.is64 == self.is64 { return self }
        if other.is64 { return self.to64 }
        return self.to32
    }
}

enum Register32: UInt8, RegisterI {
    typealias Shift = Shift32
    typealias ExtendOp = ExtendOp32
    var is32: Bool { true }

    var i: (any RegisterI)? { self }
    var fp: (any RegisterFP)? { nil }
    
    var debugDescription: String {
        return "w\(self.rawValue)"
    }
    
    var to32: Register32 {
        self
    }
    
    var to64: Register64 {
        Register64(rawValue: self.rawValue)!
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

enum RegModifier {
    case r64ext(Register64, Register64.ExtendOp)
    case r32ext(Register32, Register32.ExtendOp)
    // TODO: rename rshift ?
    case r64shift(any RegisterI, Shift64_Real)
    case imm(Int64, IndexingMode?)
}

// TODO: this should be more accurately named M1Address or something like that (to signify
// it represents where source data comes from, which is not necessarily an offset)

fileprivate func getIxModeDebugDesc(_ r: any RegisterI, _ immVal: Int64, _ ix: IndexingMode?) -> String {
    switch(ix) {
    case .post:
        return "[\(r)], #\(immVal)"
    case .pre:
        return "[\(r), #\(immVal)]!"
    case nil:
        if immVal == 0 {
            return "[\(r)]"
        } else {
            return "[\(r), #\(immVal)]"
        }
    }
}

enum Offset : CustomAsmStringConvertible {

    // Register base + immediate offset
    // Not really an immediate, but more like an address
    case imm64(/*Xn|SP*/Register64, Int64, IndexingMode?)
    
    // Register base + register offset
    case reg(any RegisterI, RegModifier?)
    
    case uimmediate6(UImmediate6)
    case immediate6(Immediate6)
    
    // DEPRECATED
    case immediate_depr(Int16)
    case reg64offset(Register64, Int64, IndexingMode?)
    case reg32shift(Register32, Register32.Shift?)
    case reg64shift(Register64, Register64.Shift?)
    case reg32(Register32, Register32.ExtendOp, IndexingMode?)
    
    var asmDescription: String {
        switch(self) {
        
        case .imm64(let Rt, let immVal, let ixMode):
            return getIxModeDebugDesc(Rt, immVal, ixMode)
        case .reg(let Rt, .r64shift(let Rn, .lsl(0))):
            return "[\(Rt), \(Rn)]"
        case .reg(let Rt, .r64shift(let Rn, let shift)):
            return "[\(Rt), \(Rn), \(shift.asmDescription)]"
        case .reg(let Rt, nil):
            return "[\(Rt)]"
        case .reg(let Rt, .r64ext(let Rn as any RegisterI, let ext as CustomAsmStringConvertible)):
            fallthrough
        case .reg(let Rt, .r32ext(let Rn as any RegisterI, let ext as CustomAsmStringConvertible)):
            return "[\(Rt), \(Rn), \(ext.asmDescription)]"
        case .reg(let Rt, .imm(let immVal, let ixMode)):
            return getIxModeDebugDesc(Rt, immVal, ixMode)
        case .immediate6(let imm6):
            return imm6.immediate.debugDescription
        case .uimmediate6(let imm6):
            return imm6.immediate.debugDescription
        
        // DEPRECATED
        case .immediate_depr(_):
            return "imm DEPRECATED"
        case .reg64offset(_, _, _):
            return "reg64offset DEPRECATED"
        case .reg32shift(_, _):
            return "reg32shift DEPRECATED"
        case .reg64shift(_, _):
            return "reg64shift DEPRECATED"
        case .reg32(_, _, _):
            return "reg32 DEPRECATED"
        }
    }
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
//         print(
//             "Returning \(result.map { String($0, radix: 16).leftPadding(toLength: 2, withPad: "0") })"
//         )
        return result
    }

    static func encodeReg(_ reg: any Register, shift: Int64) -> Int64 {
        (Int64(0b11111) & Int64(reg.rawValue)) << shift
    }
    
    static func sizeMask(is64: Bool, offset: Int = 31) -> Int64 {
        (is64 ? 1 : 0) << offset
    }

    static func truncateOffset(_ val: Int64, divisor: Int64, bits: Int64) throws
        -> Int64
    {
        try truncateOffsetGlobal(val, divisor: divisor, bits: bits)
    }

    // 0b0b10101010000000100000001111100000

    func emit(for op: M1Op) throws -> [UInt8] {
        try Self.emit(for: op)
    }
    
    static fileprivate func encodeRegs(
        Rd: (any Register)? = nil,
        Rn: (any Register)? = nil,
        Rm: (any Register)? = nil,
        Ra: (any Register)? = nil,
        rdOff: Int64 = 0,
        rnOff: Int64 = 5,
        rmOff: Int64 = 16,
        raOff: Int64 = 10) -> Int64
    {
        var regs: Int64 = 0
        if let _Rd = Rd {
            regs = regs | encodeReg(_Rd, shift: rdOff)
        }
        if let _Rn = Rn {
            regs = regs | encodeReg(_Rn, shift: rnOff)
        }
        if let _Rm = Rm {
            regs = regs | encodeReg(_Rm, shift: rmOff)
        }
        if let _Ra = Ra {
            regs = regs | encodeReg(_Ra, shift: raOff)
        }
        return regs
    }
    
    static fileprivate func getShImm6(_ shift: Shift64_Real?, shOff: Int = 22, immOff: Int = 10) -> (Int64, Int64) {
        guard let shift = shift else {
            return (0, 0)
        }
        
        let imm6: Int64
        let sh: Int64
        switch(shift) {
        case .lsl(let amt):
            sh = 0b00
            imm6 = Int64(amt)
        case .lsr(let amt):
            sh = 0b01
            imm6 = Int64(amt)
        case .asr(let amt):
            sh = 0b10
            imm6 = Int64(amt)
        case .ror(let amt):
            sh = 0b11
            imm6 = Int64(amt)
        }
        
        return ((sh << shOff), Int64(imm6) << immOff)
    }

    static func emit(for op: M1Op) throws -> [UInt8] {
        switch try op.resolveFinalForm() {  // resolve potential aliases
        case .sub(let Rd, let Rn, .imm(let offset, nil)):
            // TODO: deduplicate with subImm12
            let imm = try Imm12Lsl12(offset)
            guard Rd.is32 == Rn.is32 else {
                throw EmitterM1Error.invalidRegister("Rd and Rn must have same size")
            }
            guard imm.isPositive else {
                return try emit(for: .addImm12(Rd, Rn, imm.flippedSign))
            }

            //                  S          sh imm12        Rn    Rd
            let mask: Int64 = 0b0_10100010_0__000000000000_00000_00000
            let encodedRd: Int64 = encodeReg(Rd, shift: 0)
            let encodedRn: Int64 = encodeReg(Rn, shift: 5)
            let size: Int64 = (Rd.is32 ? 0 : 1) << 31
            let sh: Int64 = (/*offset.lsl == ._0*/true ? 0 : 1) << 22
            let immf: Int64 = imm.shiftedLeft(10)
            let encoded: Int64 = mask | encodedRd | encodedRn | size | sh | immf
            return returnAsArray(encoded)
        case .subImm12(let Rd, let Rn, let offset):
            guard Rd.is32 == Rn.is32 else {
                throw EmitterM1Error.invalidRegister("Rd and Rn must have same size")
            }
            guard offset.imm.isPositive else {
                return try emit(for: .addImm12(Rd, Rn, Imm12Lsl12(offset.imm.flippedSign, lsl: offset.lsl)))
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
        case .add(let Rd, let Rn, .imm(let off, nil)):
            // TODO: deduplicate with addImm12
            let imm = try Imm12Lsl12(off)
            guard Rd.is32 == Rn.is32 else {
                throw EmitterM1Error.invalidRegister("Rd and Rn must have same size")
            }
            guard imm.isPositive else {
                return try emit(for: .subImm12(Rd, Rn, imm.flippedSign))
            }
            
            //                  S          sh imm12        Rn    Rd
            let mask: Int64 = 0b0_00100010_0__000000000000_00000_00000
            let encodedRd: Int64 = encodeReg(Rd, shift: 0)
            let encodedRn: Int64 = encodeReg(Rn, shift: 5)
            let size: Int64 = (Rd.is32 ? 0 : 1) << 31
            let sh: Int64 = (/*offset.lsl == ._0*/true ? 0 : 1) << 22
            let immf: Int64 = imm.shiftedLeft(10)
            let encoded: Int64 = mask | encodedRd | encodedRn | size | sh | immf
            return returnAsArray(encoded)
        case .add(let Rd, let Rn, .r64shift(let Rm, let shift)):
            //                           SH   Rm    imm6   Rn    Rd
            let mask: Int64 = 0b00001011_00_0_00000_000000_00000_00000
            let s = sizeMask(is64: Rd.is64)
            guard Rd.is64 == Rn.is64, Rd.is64 == Rm.is64 else {
                fatalError("Expected all registers to be the same size")
            }
            
            let (imm6, sh) = getShImm6(shift)
            let regs = encodeRegs(Rd: Rd, Rn: Rn, Rm: Rm)
            
            let encoded = mask | s | imm6 | sh | regs
            return returnAsArray(encoded)
        case .addImm12(let Rd, let Rn, let offset):
            guard Rd.is32 == Rn.is32 else {
                throw EmitterM1Error.invalidRegister("Rd and Rn must have same size")
            }
            guard offset.imm.isPositive else {
                return try emit(for: .subImm12(Rd, Rn, Imm12Lsl12(offset.imm.flippedSign, lsl: offset.lsl)))
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
        case .stur(let Rt as any RegisterI, let Rn, let offset) where offset >= -256 && offset < 256:
            //                    S           imm9         Rn    Rt
            let mask: Int64 = 0b1_0_111000000_000000000_00_00000_00000
            let encodedRt = encodeReg(Rt, shift: 0)
            let encodedRn = encodeReg(Rn, shift: 5)
            let offs = try Immediate9(offset)
            let size: Int64 = (Rt.is32 ? 0 : 1) << 30
            let encoded = mask | encodedRt | encodedRn | offs.shiftedLeft(12) | size
            return returnAsArray(encoded)
        case .stur(let Rt as any RegisterFP, let Rn, let offset) where offset >= -256 && offset < 256:
            //                  Si       opc    imm9         Rn    Rt
            let mask: Int64 = 0b00111100_0___00_000000000_00_00000_00000
            let regs = encodeRegs(Rd: Rt, Rn: Rn)
            let offs = try Immediate12(offset)
            let (size, opc) = fp_sizeOpcMask(Rt, _32opc: 0b00, _64opc: 0b00)
            let encoded = mask | offs.shiftedLeft(12) | opc | regs | size
            return returnAsArray(encoded)
        case .str(let Rt as any RegisterFP, .reg(let Rn as Register64, .r64ext(let Rm, let ext))):
            // https://developer.arm.com/documentation/ddi0596/2020-12/SIMD-FP-Instructions/STR--register--SIMD-FP---Store-SIMD-FP-register--register-offset--?lang=en
            //                  Si       opc   Rm    opt S    Rn    Rt
            let mask: Int64 = 0b00111100_00__1_00000_000_0_10_00000_00000
            
            
            let regs = encodeRegs(Rd: Rt, Rn: Rn, Rm: Rm)
            let (size, opc) = fp_sizeOpcMask(Rt, _32opc: 0b00, _64opc: 0b00)
            let optUnshifted: Int64
            let shiftValUnshifted: Int64
            switch(ext) {
            case .sxtx(let shiftAmount) where (shiftAmount == 0 || shiftAmount == 3) && Rt.double:
                optUnshifted = 0b111
                shiftValUnshifted = shiftAmount == 0 ? 0 : 1
            case .sxtx(let shiftAmount) where (shiftAmount == 0 || shiftAmount == 2) && Rt.single:
                optUnshifted = 0b111
                shiftValUnshifted = shiftAmount == 0 ? 0 : 1
            case .sxtx where Rt.double:
                fatalError("sxtx amount should be #0 or #3")
            case .sxtx where Rt.single:
                fatalError("sxtx amount should be #0 or #2")
            default:
                fatalError("not implemented or invalid sxtx amount")
            }
            let encoded = mask | regs | size | opc | (shiftValUnshifted << 12) | (optUnshifted << 13)
            return returnAsArray(encoded)
        case .str(let Rt as any RegisterI, .reg(let Rn as Register64, .r64ext(let Rm, let ext))):
            //                   S          Rm    opt S    Rn    Rt
            let mask: Int64 = 0b10111000001_00000_000_0_10_00000_00000
            let regs = encodeRegs(Rd: Rt, Rn: Rn, Rm: Rm)
            let s = sizeMask(is64: Rt.is64, offset: 30)
            let optUnshifted: Int64
            let shiftValUnshifted: Int64
            switch(ext) {
            case .sxtx(let shiftAmount) where shiftAmount == 0 || shiftAmount == 3:
                optUnshifted = 0b111
                shiftValUnshifted = shiftAmount == 0 ? 0 : 1
            case .sxtx:
                fatalError("sxtx amount should be #0 or #3")
//                010    UXTW
//                011    LSL
//                110    SXTW
//                111    SXTX
            }
            let encoded = mask | regs | s | (shiftValUnshifted << 12) | (optUnshifted << 13)
            return returnAsArray(encoded)
        case .str(let Rt as any RegisterFP, .reg(let Rn as Register64, .imm(let offsetCount, let ixMode))) where Rn.is64:
            fallthrough
        case .str(let Rt as any RegisterFP, .reg64offset(let Rn, let offsetCount, let ixMode)):
            // https://developer.arm.com/documentation/ddi0596/2020-12/SIMD-FP-Instructions/STR--immediate--SIMD-FP---Store-SIMD-FP-register--immediate-offset--?lang=en
            let mask: Int64
            let imm: any Immediate
            let immShift: Int
            
            switch(ixMode) {
                case .post:
                    //       Si       opc    imm9         Rn    Rt
                    mask = 0b00111100_0___00_000000000_01_00000_00000
                    imm = try Immediate9(offsetCount)
                    immShift = 12
                case .pre:
                    //       Si       opc    imm9         Rn    Rt
                    mask = 0b00111100_0___00_000000000_11_00000_00000
                    imm = try Immediate9(offsetCount)
                    immShift = 12
                case nil:
                    //       Si       opc   imm12        Rn    Rt
                    mask = 0b00111101_0___0_000000000000_00000_00000
                    if Rt.double {
                        guard offsetCount % 8 == 0 else {
                            fatalError("Scaled immediate must be divisible by 8")
                        }
                        imm = try Immediate12(offsetCount/8)
                    } else if Rt.single {
                        guard offsetCount % 4 == 0 else {
                            fatalError("Scaled immediate must be divisible by 4")
                        }
                        imm = try Immediate12(offsetCount/4)
                    } else {
                        fatalError("FP register size not implemented")
                    }
                    immShift = 10
            }
            
            let regs = encodeRegs(Rd: Rt, Rn: Rn)
            let (size, opc) = fp_sizeOpcMask(Rt, _32opc: 0b00, _64opc: 0b00)
            let encoded = mask | imm.shiftedLeft(immShift) | opc | regs | size
            return returnAsArray(encoded)
        case .str(let Rt as any RegisterI, .reg(let Rn as Register64, .imm(let offsetCount, let ixMode))) where Rn.is64:
            fallthrough
        case .str(let Rt as any RegisterI, .reg64offset(let Rn, let offsetCount, let ixMode)):
            let mask: Int64
            let immBits: Int64
            let immShift: Int64
            let divider: Int64
            switch(ixMode) {
                case nil:
                    divider = Rt.is32 ? 4 : 8 // pimm
                    guard offsetCount % divider == 0 else {
                        return try Self.emit(for: .stur(Rt, Rn, Int16(offsetCount)))
                    }
                    //         S          imm12        Rn    Rt
                    mask = 0b1_0_11100100_000000000000_00000_00000
                    immBits = 12
                    immShift = 10
                case .pre: 
                    //         S           imm9         Rn    Rt
                    mask = 0b1_0_111000000_000000000_11_00000_00000
                    immBits = 9
                    immShift = 12
                    divider = 1 // simm
                case .post: 
                    //         S           imm9         Rn    Rt
                    mask = 0b1_0_111000000_000000000_01_00000_00000
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
        case .orr(let Rd as Register64, let xZR, let Rn as Register64, nil) where Rd.is64 && Rn.is64 && xZR.to64 == X.xZR:
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
        case .ldp(let pair as (any RegisterI, any RegisterI), let offset):
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
        case .ldp(let pair as (any RegisterFP, any RegisterFP), let offset):
            // https://developer.arm.com/documentation/ddi0596/2020-12/SIMD-FP-Instructions/LDP--SIMD-FP---Load-Pair-of-SIMD-FP-registers-?lang=en
            guard case .reg64offset(let Rn, let offsetCount, let ixMode) = offset else {
                throw EmitterM1Error.invalidOffset(
                    "LDP can only have .reg64offset offset"
                )
            }

            let divisor: Int64 = 8  // 64-bit ops. 32-bit ops have divisor 4
            let truncated = try truncateOffset(offsetCount, divisor: divisor, bits: 7)
            
            let (Rt1, Rt2) = pair
            try assertMatchingSize(Rt1, Rt2)
            
            let mask: Int64
            switch ixMode {
            case nil:
                //       o      ixm imm7    Rt2   Rn    Rt1
                mask = 0b0010110101_0000000_00000_00000_00000
            case .pre:
                //       o      ixm imm7    Rt2   Rn    Rt1
                mask = 0b0010110111_0000000_00000_00000_00000
            case .post:
                //       o      ixm imm7    Rt2   Rn    Rt1
                mask = 0b0010110011_0000000_00000_00000_00000
            }

            let opc: Int64
            if Rt1.double {
                opc = 0b01
            } else if Rt1.single {
                opc = 0b00
            } else {
                fatalError("ldp implemented for register sizes \(Rt1), \(Rt2)")
            }
            let opcOffset: Int64 = 30
            let encodedRt1: Int64 = encodeReg(Rt1, shift: 0)
            let encodedRt2: Int64 = encodeReg(Rt2, shift: 10)
            let encodedRn: Int64 = encodeReg(Rn, shift: 5)
            let imm: Int64 = truncated << 15
            let encoded =
                encodedRt1 | encodedRt2 | encodedRn | (opc << opcOffset) | mask | imm
            return returnAsArray(encoded)
        case .stp(let pair as (any RegisterFP, any RegisterFP), let offset):
            // https://developer.arm.com/documentation/ddi0596/2020-12/SIMD-FP-Instructions/STP--SIMD-FP---Store-Pair-of-SIMD-FP-registers-?lang=en
            guard case .reg64offset(let Rn, let offsetCount, let ixMode) = offset else {
                throw EmitterM1Error.invalidOffset(
                    "STP can only have .reg64offset offset"
                )
            }

            let divisor: Int64 = 8  // 64-bit ops. 32-bit ops have divisor 4
            let truncated = try truncateOffset(offsetCount, divisor: divisor, bits: 7)
            let (Rt1, Rt2) = pair
            try assertMatchingSize(Rt1, Rt2)
            let encodedRt1: Int64 = encodeReg(Rt1, shift: 0)
            let encodedRt2: Int64 = encodeReg(Rt2, shift: 10)
            let encodedRn: Int64 = encodeReg(Rn, shift: 5)
            let opc: Int64
            if Rt1.double {
                opc = 0b01
            } else if Rt1.single {
                opc = 0b00
            } else {
                fatalError("ldp implemented for register sizes \(Rt1), \(Rt2)")
            }
            let opcOffset: Int64 = 30

            let mask: Int64
            switch ixMode {
            case nil: mask =    0b0010_1101_0000_0000_0000_0000_0000_0000
            case .pre: mask =   0b0010_1101_1000_0000_0000_0000_0000_0000
            case .post: mask =  0b0010_1100_1000_0000_0000_0000_0000_0000
            }
            let imm: Int64 = truncated << 15
            let encoded =
                encodedRt1 | encodedRt2 | encodedRn | (opc << opcOffset) | mask | imm
            return returnAsArray(encoded)
        case .stp(let pair as (any RegisterI, any RegisterI), let offset):
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
        case .b_v2(let imm26):
            //                         imm26
            let mask: Int64 = 0b000101_00000000000000000000000000
            let encoded = mask | imm26.signedShiftedRight(2) // divisor 4
            
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
        case .br(let Rn):
            //                                         Rn
            let mask: Int64 = 0b1101011000011111000000_00000_00000
            let encodedRn = encodeReg(Rn, shift: 5)
            return returnAsArray(mask | encodedRn)
        case .blr(let Rn):
            let mask: Int64 = 0b1101_0110_0011_1111_0000_0000_0000_0000
            let encodedRn = encodeReg(Rn, shift: 5)
            return returnAsArray(mask | encodedRn)
        case .nop: return [0x1f, 0x20, 0x03, 0xd5]
        case .ret: return [0xc0, 0x03, 0x5f, 0xd6]
        case .ldur(let Rt as any RegisterI, let Rn, let offset ):
            //                    S           imm9         Rn    Rt
            let mask: Int64 = 0b1_0_111000010_000000000_00_00000_00000
            let encodedRt = encodeReg(Rt, shift: 0)
            let encodedRn = encodeReg(Rn, shift: 5)
            let offs = offset.immediate.shiftedLeft(12)
            let size: Int64 = (Rt.is32 ? 0 : 1) << 30
            let encoded = mask | encodedRt | encodedRn | offs | size
            return returnAsArray(encoded)
        case .ldur(let Rt as any RegisterFP, let Rn, let offset ):
            // https://developer.arm.com/documentation/ddi0596/2020-12/SIMD-FP-Instructions/LDUR--SIMD-FP---Load-SIMD-FP-Register--unscaled-offset--?lang=en
            //                  Si       opc    imm9         Rn    Rt
            let mask: Int64 = 0b00111100_0___10_000000000_00_00000_00000
            let regs = encodeRegs(Rd: Rt, Rn: Rn)
            let (size, opc) = fp_sizeOpcMask(Rt)
            let encoded = mask | offset.shiftedLeft(12) | opc | regs | size
            return returnAsArray(encoded)
        case .ldr(let Rt as any RegisterI, .reg(let Rn as Register64, .r64shift(let Rm, let shift))):
            //                   S          Rm    opt S    Rn    Rt
            let mask: Int64 = 0b10111000011_00000_000_0_10_00000_00000
            
            let regs = encodeRegs(Rd: Rt, Rn: Rn, Rm: Rm)
            let s = sizeMask(is64: Rt.is64, offset: 30)
            let optUnshifted: Int64
            let shiftValUnshifted: Int64
            switch(shift) {
            case .lsl(let shiftAmount) where shiftAmount == 0 || shiftAmount == 2:
                optUnshifted = 0b011
                shiftValUnshifted = shiftAmount == 0 ? 0 : 2
            case .lsl:
                fatalError("lsl amount should be #0 or #2")
//                010    UXTW
//                011    LSL
//                110    SXTW
//                111    SXTX
            default:
                fatalError("Not implemented")
            }
            let encoded = mask | regs | s | (shiftValUnshifted << 12) | (optUnshifted << 13)
            return returnAsArray(encoded)
        case .ldr(let Rt as any RegisterI, .reg(let Rn as Register64, .r64ext(let Rm, let ext))):
            //                   S          Rm    opt S    Rn    Rt
            let mask: Int64 = 0b10111000011_00000_000_0_10_00000_00000
            
            let regs = encodeRegs(Rd: Rt, Rn: Rn, Rm: Rm)
            let s = sizeMask(is64: Rt.is64, offset: 30)
            let optUnshifted: Int64
            let shiftValUnshifted: Int64
            switch(ext) {
            case .sxtx(let shiftAmount) where shiftAmount == 0 || shiftAmount == 3:
                optUnshifted = 0b111
                shiftValUnshifted = shiftAmount == 0 ? 0 : 1
            case .sxtx:
                fatalError("sxtx amount should be #0 or #3")
            
//                010    UXTW
//                011    LSL
//                110    SXTW
//                111    SXTX
            }
            let encoded = mask | regs | s | (shiftValUnshifted << 12) | (optUnshifted << 13)
            return returnAsArray(encoded)
        case .ldr(let Rt as any RegisterFP, .reg(let Rn as Register64, .r64ext(let Rm, let ext))):
            // https://developer.arm.com/documentation/ddi0596/2020-12/SIMD-FP-Instructions/LDR--register--SIMD-FP---Load-SIMD-FP-Register--register-offset--?lang=en
            //                  Si       opc   Rm    opt S    Rn    Rt
            let mask: Int64 = 0b00111100_01__1_00000_000_0_10_00000_00000
            
            let regs = encodeRegs(Rd: Rt, Rn: Rn, Rm: Rm)
            let (size, opc) = fp_sizeOpcMask(Rt)
            let optUnshifted: Int64
            let shiftValUnshifted: Int64
            switch(ext) {
            case .sxtx(let shiftAmount) where (shiftAmount == 0 || shiftAmount == 3) && Rt.double:
                optUnshifted = 0b111
                shiftValUnshifted = shiftAmount == 0 ? 0 : 1
            case .sxtx(let shiftAmount) where (shiftAmount == 0 || shiftAmount == 2) && Rt.single:
                optUnshifted = 0b111
                shiftValUnshifted = shiftAmount == 0 ? 0 : 1
            case .sxtx where Rt.double:
                fatalError("sxtx amount should be #0 or #3")
            case .sxtx where Rt.single:
                fatalError("sxtx amount should be #0 or #2")
            default:
                fatalError("not implemented or invalid sxtx amount")
            }
            let encoded = mask | regs | size | opc | (shiftValUnshifted << 12) | (optUnshifted << 13)
            return returnAsArray(encoded)
        case .ldr(let Rt as any RegisterFP, .reg(let Rn as Register64, .imm(let offsetCount, let ixMode))) where Rn.is64:
            fallthrough
        case .ldr(let Rt as any RegisterFP, .reg64offset(let Rn, let offsetCount, let ixMode)):
            // TODO: reg64offset should be removed
            fallthrough
        case .ldr(let Rt as any RegisterFP, .reg64offset(let Rn, let offsetCount, let ixMode)):
            // https://developer.arm.com/documentation/ddi0596/2020-12/SIMD-FP-Instructions/LDR--immediate--SIMD-FP---Load-SIMD-FP-Register--immediate-offset--?lang=en
            let mask: Int64
            let imm: any Immediate
            let immShift: Int
            
            switch(ixMode) {
                case .post:
                    //       Si       opc    imm9         Rn    Rt
                    mask = 0b00111100_0___10_000000000_01_00000_00000
                    imm = try Immediate9(offsetCount)
                    immShift = 12
                case .pre:
                    //       Si       opc    imm9         Rn    Rt
                    mask = 0b00111100_0___10_000000000_11_00000_00000
                    imm = try Immediate9(offsetCount)
                    immShift = 12
                case nil:
                    //       Si       opc   imm12        Rn    Rt
                    mask = 0b00111101_0___1_000000000000_00000_00000
                    if Rt.double {
                        guard offsetCount % 8 == 0 else {
                            fatalError("Scaled immediate must be divisible by 8")
                        }
                        imm = try Immediate12(offsetCount/8)
                    } else if Rt.single {
                        guard offsetCount % 4 == 0 else {
                            fatalError("Scaled immediate must be divisible by 4")
                        }
                        imm = try Immediate12(offsetCount/4)
                    } else {
                        fatalError("FP register size not implemented")
                    }
                    immShift = 10
            }
            
            let regs = encodeRegs(Rd: Rt, Rn: Rn)
            let (size, opc) = fp_sizeOpcMask(Rt)
            let encoded = mask | imm.shiftedLeft(immShift) | opc | regs | size
            return returnAsArray(encoded)
        case .ldr(let Rt as any RegisterI, .reg(let Rn as Register64, .imm(let offsetCount, let ixMode))) where Rn.is64:
            fallthrough
        case .ldr(let Rt as any RegisterI, .reg64offset(let Rn, let offsetCount, let ixMode)):
            // TODO: reg64offset should be removed
            fallthrough
        case .ldr(let Rt as any RegisterI, .imm64(let Rn, let offsetCount, let ixMode)):
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
            let imm16: Int64 = (imm.signedTruncate(lsr: 2, capBits: 19)) << 5
            let encoded = mask | imm16
            return returnAsArray(encoded)
        case .b_eq(let imm):
            //                           imm19                 cond
            let mask: Int64 = 0b01010100_0000000000000000000_0_0000
            let imm16: Int64 = (imm.signedTruncate(lsr: 2, capBits: 19)) << 5
            let encoded = mask | imm16
            return returnAsArray(encoded)
        case .b_ne(let imm):
            //                           imm19                 cond
            let mask: Int64 = 0b01010100_0000000000000000000_0_0001
            let imm16: Int64 = (imm.signedTruncate(lsr: 2, capBits: 19)) << 5
            let encoded = mask | imm16
            return returnAsArray(encoded)
        case .b_gt(let imm):
            //                           imm19                 cond
            let mask: Int64 = 0b01010100_0000000000000000000_0_1100
            let imm16: Int64 = (imm.signedTruncate(lsr: 2, capBits: 19)) << 5
            let encoded = mask | imm16
            return returnAsArray(encoded)
        case .b_ge(let imm):
            //                           imm19                 cond
            let mask: Int64 = 0b01010100_0000000000000000000_0_1010
            let imm16: Int64 = (imm.signedTruncate(lsr: 2, capBits: 19)) << 5
            let encoded = mask | imm16
            return returnAsArray(encoded)
        case .b_le(let imm):
            //                           imm19                 cond
            let mask: Int64 = 0b01010100_0000000000000000000_0_1101
            let imm16: Int64 = (imm.signedTruncate(lsr: 2, capBits: 19)) << 5
            let encoded = mask | imm16
            return returnAsArray(encoded)
        case .ubfm(let Rd, let Rn, let immr, let imms):
            guard Rd.is32 == Rn.is32 else { fatalError("Registers must have the same size") }
            let N: Int64
            if Rd.is32 {
                guard immr.immediate >= 0 && immr.immediate < 32 else {
                    fatalError("immediate must be an integer in range [0, 31]")
                }
                N = 0 << 22
            }
            else if Rd.is64 {
                guard immr.immediate >= 0 && immr.immediate < 64 else {
                    fatalError("immediate must be an integer in range [0, 63]")
                }
                N = 1 << 22
            } else {
                fatalError("Registers must be either 32 bit or 64 bit")
            }
            //                  S          N immr   imms   Rn    Rd
            let mask: Int64 = 0b0_10100110_0_000000_000000_00000_00000
            let size: Int64 = (Rd.is32 ? 0 : 1) << 31
            let encodedRd = encodeReg(Rd, shift: 0)
            let encodedRn = encodeReg(Rn, shift: 5)
            let encoded = size | mask | N | immr.shiftedLeft(16) | imms.shiftedLeft(10) | encodedRd | encodedRn
            return returnAsArray(encoded)
        case .lslv(let Rd, let Rn, let Rm):
            //                  S            Rm           Rn    Rd
            let mask: Int64 = 0b0_0011010110_00000_001000_00000_00000
            let encodedRd = encodeReg(Rd, shift: 0)
            let encodedRn = encodeReg(Rn, shift: 5)
            let encodedRm = encodeReg(Rm, shift: 16)
            let size = sizeMask(is64: Rd.is64)
            let encoded = mask | encodedRd | encodedRn | encodedRm | size
            return returnAsArray(encoded)
        case .ldrb(let Wt, .reg(let Xn as Register64, .imm(let immRaw, let ixMode))) where Xn.is64:
            fallthrough
        case .ldrb(let Wt, .imm64(let Xn, let immRaw, let ixMode)):
            let mask: Int64
            let imm: any Immediate
            let immShift: Int
            switch(ixMode) {
            case .post:
                //                   imm9         Rn    Rt
                mask = 0b00111000010_000000000_01_00000_00000
                imm = try Immediate9(immRaw)
                immShift = 12
            case .pre:
                //                   imm9         Rn    Rt
                mask = 0b00111000010_000000000_11_00000_00000
                imm = try Immediate9(immRaw)
                immShift = 12
            case nil:
                //                  imm12        Rn    Rt
                mask = 0b0011100101_000000000000_00000_00000_
                imm = try Immediate12(immRaw)
                immShift = 10
            }
            let encodedRt = encodeReg(Wt, shift: 0)
            let encodedRn = encodeReg(Xn, shift: 5)
            let encodedImm = imm.shiftedLeft(immShift)
            let encoded = encodedRt | encodedRn | encodedImm | mask
            return returnAsArray(encoded)
        case .ldrb(let Wt, .reg(let Xn, let mod)):
            //                              Rm    opt      Rn    Rt
            let mask: Int64 = 0b00111000011_00000_000_0_10_00000_00000
            let option: Int64
            let Rm: any RegisterI
            let S: Int64
            switch(mod) {
            case .r64ext(let _Rm, let mod):
                Rm = _Rm
                switch(mod) {
                case .sxtx(0):
                    S = 1
                    option = 0b111
                case .sxtx:
                    fatalError("Expected .sxtx #0")
                }
            case .r32ext(let _Rm, let mod):
                Rm = _Rm
                S = 1
                switch(mod) {
                case .uxtw(0):
                    option = 0b010
                case .sxtw(0):
                    option = 0b110
                case .sxtw:
                    fatalError("Expected .sxtw #0")
                case .uxtw:
                    fatalError("Expected .uxtw #0")
                }
            case .r64shift(let _Rm, .lsl(0)):
                Rm = _Rm
                option = 0b011
                S = 0
            case nil:
                Rm = X.x0
                S = 0
                option = 0b000
            default:
                fatalError("not implemented")
            }
            let shiftedS: Int64 = S << 12
            let shiftedOpt = option << 13
            let encodedRn = encodeReg(Xn, shift: 5)
            let encodedRt = encodeReg(Wt, shift: 0)
            let encodedRm = encodeReg(Rm, shift: 16)
            
            let encoded = mask | encodedRn | encodedRt | encodedRm | shiftedOpt | shiftedS
            return returnAsArray(encoded)
        case .ldrh(let Wt, .imm64(let Xn, let immRaw, let ixMode)):
            let mask: Int64
            let imm: any Immediate
            let immShift: Int
            switch(ixMode) {
            case .post:
                //                   imm9         Rn    Rt
                mask = 0b01111000010_000000000_01_00000_00000
                imm = try Immediate9(immRaw)
                immShift = 12
            case .pre:
                //                   imm9         Rn    Rt
                mask = 0b01111000010_000000000_11_00000_00000
                imm = try Immediate9(immRaw)
                immShift = 12
            case nil:
                //                  imm12        Rn    Rt
                mask = 0b0111100101_000000000000_00000_00000_
                imm = try Immediate12(immRaw)
                immShift = 9 // shift left +10 for the position, but shift -1 back cause pimm==imm*2. So total shift 9
            }
            let encodedRt = encodeReg(Wt, shift: 0)
            let encodedRn = encodeReg(Xn, shift: 5)
            let encodedPimm = imm.shiftedLeft(immShift)
            let encoded = encodedRt | encodedRn | encodedPimm | mask
            return returnAsArray(encoded)
        case .ldrh(let Wt, .reg(let Xn, let mod)):
            //                              Rm    opt      Rn    Rt
            let mask: Int64 = 0b01111000011_00000_000_0_10_00000_00000
            let option: Int64
            let Rm: any RegisterI
            let S: Int64
            switch(mod) {
            case .r64shift(let _Rm, .lsl(let lslAmount)):
                Rm = _Rm
                guard lslAmount == 0 || lslAmount == 1 else {
                    fatalError("Expected .lsl #0 or #1")
                }
                S = Int64(lslAmount)
                option = 0b011
            case .r64ext(let _Rm, let mod):
                Rm = _Rm
                switch(mod) {
                case .sxtx(0):
                    S = 0
                    option = 0b111
                case .sxtx(1):
                    S = 1
                    option = 0b111
                case .sxtx:
                    fatalError("Expected .sxtx #0 or #1")
                }
            case .r32ext(let _Rm, let mod):
                Rm = _Rm
                switch(mod) {
                case .uxtw(0):
                    S = 0
                    option = 0b010
                case .uxtw(1):
                    S = 1
                    option = 0b010
                case .sxtw(0):
                    S = 0
                    option = 0b110
                case .sxtw(1):
                    S = 1
                    option = 0b110
                case .sxtw:
                    fatalError("Expected .sxtw #0 or #1")
                case .uxtw:
                    fatalError("Expected .uxtw #0 or #1")
                }
            case nil:
                Rm = X.x0
                S = 0
                option = 0b000
            default:
                fatalError("not implemented")
            }
            let shiftedS: Int64 = S << 12
            let shiftedOpt = option << 13
            let encodedRn = encodeReg(Xn, shift: 5)
            let encodedRt = encodeReg(Wt, shift: 0)
            let encodedRm = encodeReg(Rm, shift: 16)
            
            let encoded = mask | encodedRn | encodedRt | encodedRm | shiftedOpt | shiftedS
            return returnAsArray(encoded)
        case .and(let Rd, let Rn, .imm(let imm, nil)):
            let sf = sizeMask(is64: Rd.is64)
            //                  S          N immr   imms   Rn    Rd
            let mask: Int64 = 0b0_00100100_0_000000_000000_00000_00000
            
            guard Rd.is32 == Rn.is32 else {
                fatalError("Rd and Rn must be the same size for AND (immediate)")
            }
            
            let bmi = try! BitmaskImmediate(UInt64(bitPattern: imm))
            guard (Rd.is32 && bmi.n == 0) || Rd.is64 else {
                fatalError("n can't be 1 for 32-bit operation")
            }
            
            let encodedRn = encodeReg(Rn, shift: 5)
            let encodedRd = encodeReg(Rd, shift: 0)
            let encoded = sf | mask | (Int64(bmi.n) << 22) | Int64(bmi.imms) << 10 | Int64(bmi.immr) << 16 | encodedRn | encodedRd
            return returnAsArray(encoded)
        case .and(let Rd, let Rn, .r64shift(let Rm, let shift)):
            let sf = sizeMask(is64: Rd.is64)
            //                  S        SH N Rm    imm6   Rn    Rd
            let mask: Int64 = 0b00001010_00_0_00000_000000_00000_00000
                              
            let (sh, imm6) = getShImm6(shift)
            
            guard Rd.is32 == Rn.is32, Rd.is32 == Rm.is32 else {
                fatalError("Rd, Rn, Rm must be the same size for AND (shifted register)")
            }
            
            let encodedRd = encodeReg(Rd, shift: 0)
            let encodedRn = encodeReg(Rn, shift: 5)
            let encodedRm = encodeReg(Rm, shift: 16)
            let encoded = sf | mask | imm6 | encodedRm | encodedRn | encodedRd | sh
            return returnAsArray(encoded)
        case .sbfm(let Rd, let Rn, let immr, let imms):
            //                  S        N immr   imms  Rn    Rd
            let mask: Int64 = 0b0001001100_000000_000000_00000_00000
            guard Rd.is32 == Rn.is32 else {
                fatalError("\(Rd) must be same size as second register. Received: \(Rn)")
            }
            let s = sizeMask(is64: Rd.is64)
            let n: Int64 = Rd.is64 ? 1 : 0
            let encodedRd = encodeReg(Rd, shift: 0)
            let encodedRn = encodeReg(Rn, shift: 5)
            let encoded = mask | s | (n << 22) | immr.shiftedLeft(16) | imms.shiftedLeft(10) | encodedRn | encodedRd
            return returnAsArray(encoded)
        case .sub(let Rd, let Rn, .r64shift(let Rm, let shift)):
            //                  S        SH   Rm    imm6   Rn    Rd
            let mask: Int64 = 0b01001011_00_0_00000_000000_00000_00000
            let s = sizeMask(is64: Rd.is64)
            let imm6: Immediate6
            let sh: Int64
            switch(shift) {
            case .lsl(let shiftAmt):
                sh = 0b00
                imm6 = try! Immediate6(shiftAmt)
            case .lsr(let shiftAmt):
                sh = 0b01
                imm6 = try! Immediate6(shiftAmt)
            case .asr(let shiftAmt):
                sh = 0b11
                imm6 = try! Immediate6(shiftAmt)
            default:
                fatalError("Unsupported shift")
            }
            let encodedRd = encodeReg(Rd, shift: 0)
            let encodedRn = encodeReg(Rn, shift: 5)
            let encodedRm = encodeReg(Rm, shift: 16)
            let encoded = s | mask | (sh << 22) | encodedRd | encodedRm | encodedRn | imm6.shiftedLeft(10)
            return returnAsArray(encoded)
        case .sturh(let Rt, .imm64(let Xn, let off, nil)):
            //                  S           imm9_        Rn    Rt
            let mask: Int64 = 0b01111000000_000000000_00_00000_00000
            let s = sizeMask(is64: false)
            let imm12 = try Immediate12(off)
            let encodedRn = encodeReg(Xn, shift: 5)
            let encodedRt = encodeReg(Rt, shift: 0)
            let encoded = mask | s | imm12.shiftedLeft(12).immediate | encodedRn | encodedRt
            return returnAsArray(encoded)
        case .ldurh(let Rt, .imm64(let Xn, let off, nil)):
            //                  S           imm9_        Rn    Rt
            let mask: Int64 = 0b01111000010_000000000_00_00000_00000
            let s = sizeMask(is64: false)
            let imm12 = try Immediate12(off)
            let encodedRn = encodeReg(Xn, shift: 5)
            let encodedRt = encodeReg(Rt, shift: 0)
            let encoded = mask | s | imm12.shiftedLeft(12).immediate | encodedRn | encodedRt
            return returnAsArray(encoded)
        case .strh(let Rt, .imm64(let Xn, let off, let ixMode)):
            let mask: Int64
            let imm: Int64
            switch(ixMode) {
            case .post:
                //                   imm9         Rn    Rt
                mask = 0b01111000000_000000000_01_00000_00000
                imm = off << 12
            case .pre:
                //                   imm9         Rn    Rt
                mask = 0b01111000000_000000000_11_00000_00000
                imm = off << 12
            case nil:
                //                  imm12        Rn    Rt
                mask = 0b0111100100_000000000000_00000_00000
                guard off % 2 == 0 else {
                    throw EmitterM1Error.invalidOffset("Offset must be divisible by 2")
                }
                imm = (off/2) << 10
            }
            let encodedRn = encodeReg(Xn, shift: 5)
            let encodedRt = encodeReg(Rt, shift: 0)
            let s = sizeMask(is64: Rt.is64)
            let encoded = mask | imm | encodedRn | encodedRt | s
            return returnAsArray(encoded)
        case .strh(let Wt, .reg(let Xn, let mod)):
            //                              Rm    opt S    Rn    Rt
            let mask: Int64 = 0b01111000001_00000_000_0_10_00000_00000
            let option: Int64
            let Rm: any RegisterI
            let S: Int64
            switch(mod) {
            case .r64shift(let _Rm, .lsl(let lslAmount)):
                Rm = _Rm
                guard lslAmount == 0 || lslAmount == 1 else {
                    fatalError("Expected .lsl #0 or #1")
                }
                S = Int64(lslAmount)
                option = 0b011
            case .r64ext(let _Rm, let mod):
                Rm = _Rm
                switch(mod) {
                case .sxtx(0):
                    S = 0
                    option = 0b111
                case .sxtx(1):
                    S = 1
                    option = 0b111
                case .sxtx:
                    fatalError("Expected .sxtx #0 or #1")
                }
            case .r32ext(let _Rm, let mod):
                Rm = _Rm
                switch(mod) {
                case .uxtw(0):
                    S = 0
                    option = 0b010
                case .uxtw(1):
                    S = 1
                    option = 0b010
                case .sxtw(0):
                    S = 0
                    option = 0b110
                case .sxtw(1):
                    S = 1
                    option = 0b110
                case .sxtw:
                    fatalError("Expected .sxtw #0 or #1")
                case .uxtw:
                    fatalError("Expected .uxtw #0 or #1")
                }
            case nil:
                Rm = X.x0
                S = 0
                option = 0b000
            default:
                fatalError("not implemented")
            }
            let shiftedS: Int64 = S << 12
            let shiftedOpt = option << 13
            let encodedRn = encodeReg(Xn, shift: 5)
            let encodedRt = encodeReg(Wt, shift: 0)
            let encodedRm = encodeReg(Rm, shift: 16)
            
            let encoded = mask | encodedRn | encodedRt | encodedRm | shiftedOpt | shiftedS
            return returnAsArray(encoded)
        case .strb(let Rt, .imm64(let Xn, let off, let ixMode)):
            let mask: Int64
            let imm: Int64
            switch(ixMode) {
            case .post:
                //                   imm9         Rn    Rt
                mask = 0b00111000000_000000000_01_00000_00000
                imm = off << 12
            case .pre:
                //                   imm9         Rn    Rt
                mask = 0b00111000000_000000000_11_00000_00000
                imm = off << 12
            case nil:
                //                  imm12        Rn    Rt
                mask = 0b0011100100_000000000000_00000_00000
                imm = off << 10
            }
            let encodedRn = encodeReg(Xn, shift: 5)
            let encodedRt = encodeReg(Rt, shift: 0)
            let s = sizeMask(is64: Rt.is64)
            let encoded = mask | imm | encodedRn | encodedRt | s
            return returnAsArray(encoded)
        case .strb(let Wt, .reg(let Xn, let mod)):
            //                              Rm    opt S    Rn    Rt
            let mask: Int64 = 0b00111000001_00000_000_0_10_00000_00000
            let option: Int64
            let Rm: any RegisterI
            let S: Int64
            switch(mod) {
            case .r64ext(let _Rm, let mod):
                Rm = _Rm
                switch(mod) {
                case .sxtx(0):
                    S = 1
                    option = 0b111
                case .sxtx:
                    fatalError("Expected .sxtx #0")
                }
            case .r32ext(let _Rm, let mod):
                Rm = _Rm
                S = 1
                switch(mod) {
                case .uxtw(0):
                    option = 0b010
                case .sxtw(0):
                    option = 0b110
                case .sxtw:
                    fatalError("Expected .sxtw #0")
                case .uxtw:
                    fatalError("Expected .uxtw #0")
                }
            case .r64shift(let _Rm, .lsl(0)):
                Rm = _Rm
                option = 0b011
                S = 0
            case nil:
                Rm = X.x0
                S = 0
                option = 0b000
            default:
                fatalError("not implemented")
            }
            let shiftedS: Int64 = S << 12
            let shiftedOpt = option << 13
            let encodedRn = encodeReg(Xn, shift: 5)
            let encodedRt = encodeReg(Wt, shift: 0)
            let encodedRm = encodeReg(Rm, shift: 16)
            
            let encoded = mask | encodedRn | encodedRt | encodedRm | shiftedOpt | shiftedS
            return returnAsArray(encoded)
        case .asrv(let Rd, let Rn, let Rm):
            let s = sizeMask(is64: Rd.is64)
            guard Rd.is64 == Rn.is64, Rn.is64 == Rm.is64 else {
                fatalError("All registers must be the same size")
            }
            
            //                              Rm           Rn    Rd
            let mask: Int64 = 0b00011010110_00000_001010_00000_00000
            let regs = encodeRegs(Rd: Rd, Rn: Rn, Rm: Rm)
            let encoded = s | mask | regs
            return returnAsArray(encoded)
        case .lsrv(let Rd, let Rn, let Rm):
            let s = sizeMask(is64: Rd.is64)
            guard Rd.is64 == Rn.is64, Rn.is64 == Rm.is64 else {
                fatalError("All registers must be the same size")
            }
            
            //                              Rm           Rn    Rd
            let mask: Int64 = 0b00011010110_00000_001001_00000_00000
            let regs = encodeRegs(Rd: Rd, Rn: Rn, Rm: Rm)
            let encoded = s | mask | regs
            return returnAsArray(encoded)
        case .eor_r(let Rd, let Rn, let Rm, let shift):
            try assertMatchingSize(Rd, Rn, Rm)
            let mask: Int64 = 0b01001010_00_0_00000_000000_00000_00000
            let s = sizeMask(is64: Rd.is64)
            let (imm6, sh) = getShImm6(shift)
            let regs = encodeRegs(Rd: Rd, Rn: Rn, Rm: Rm)
            let encoded = mask | s | imm6 | sh | regs
            return returnAsArray(encoded)
        case .madd(let Rd, let Rn, let Rm, let Ra):
            try assertMatchingSize(Rd, Rn, Rm, Ra)
            
            //                              Rm      Ra    Rn    Rd
            let mask: Int64 = 0b00011011000_00000_0_00000_00000_00000
            let sf = sizeMask(is64: Rd.is64)
            let regs = encodeRegs(Rd: Rd, Rn: Rn, Rm: Rm, Ra: Ra)
            let encoded = mask | sf | regs
            return returnAsArray(encoded)
        case .fmul(let Rd, let Rn, let Rm):
            try assertMatchingSize(Rd, Rn, Rm)
            
            //                           ftype   Rm           Rn    Rd
            let mask: Int64 = 0b00011110_00____1_00000_000010_00000_00000
            let regs = encodeRegs(Rd: Rd, Rn: Rn, Rm: Rm)
            let ftype = ftypeMask(Rd)
            let encoded = mask | regs | ftype
            return returnAsArray(encoded)
        case .orr(let Rd, let Rn, let Rm, let shift):
            //                           SH   Rm    imm6   Rn    Rd
            let mask: Int64 = 0b00101010_00_0_00000_000000_00000_00000
            let regs = encodeRegs(Rd: Rd, Rn: Rn, Rm: Rm)
            let sf = sizeMask(is64: Rd.is64)
            let (imm6, sh) = getShImm6(shift)
            let encoded = mask | regs | sf | imm6 | sh
            return returnAsArray(encoded)
        case .fcvtzs(let Rt, let Rn) where Rn.single || Rn.double:
            //                  S        FT              Rn    Rd
            let mask: Int64 = 0b00011110_00_111000000000_00000_00000
            let ftype: Int64 = ftypeMask(Rn)
            let s = sizeMask(is64: Rt.is64)
            let regs = encodeRegs(Rd: Rt, Rn: Register64(rawValue: Rn.rawValue))
            let encoded = mask | s | ftype | regs
            return returnAsArray(encoded)
        case .scvtf(let Rt, let Rn) where Rt.single || Rt.double || Rt.half:
            //                  S        FT              Rn    Rd
            let mask: Int64 = 0b00011110_00_100010000000_00000_00000
            let ftype: Int64 = ftypeMask(Rt)
            let s = sizeMask(is64: Rn.is64)
            let regs = encodeRegs(Rd: Register64(rawValue: Rt.rawValue), Rn: Rn)
            let encoded = mask | s | ftype | regs
            return returnAsArray(encoded)
        case .ucvtf(let Rt, let Rn) where Rt.single || Rt.double:
            //                  S        FT              Rn    Rd
            let mask: Int64 = 0b00011110_00_100011000000_00000_00000
            let ftype = ftypeMask(Rt)
            let s = sizeMask(is64: Rn.is64)
            let regs = encodeRegs(Rd: Register64(rawValue: Rt.rawValue), Rn: Rn)
            let encoded = mask | s | ftype | regs
            return returnAsArray(encoded)
        case .udiv(let Rd, let Rn, let Rm):
            //                  S           Rm           Rn    Rd
            let mask: Int64 = 0b00011010110_00000_000010_00000_00000
            try assertMatchingSize(Rd, Rn, Rd)
            let s = sizeMask(is64: Rd.is64)
            let regs = encodeRegs(Rd: Rd, Rn: Rn, Rm: Rm)
            let encoded = mask | s | regs
            return returnAsArray(encoded)
        case .sdiv(let Rd, let Rn, let Rm):
            //                  S           Rm           Rn    Rd
            let mask: Int64 = 0b00011010110_00000_000011_00000_00000
            try assertMatchingSize(Rd, Rn, Rd)
            let s = sizeMask(is64: Rd.is64)
            let regs = encodeRegs(Rd: Rd, Rn: Rn, Rm: Rm)
            let encoded = mask | s | regs
            return returnAsArray(encoded)
        case .fdiv(let Rd, let Rn, let Rm):
            //                           ftype   Rm           Rn    Rd
            let mask: Int64 = 0b00011110_00____1_00000_000110_00000_00000
            try assertMatchingSize(Rd, Rn, Rm)
            let ftype: Int64 = ftypeMask(Rd)
            let regs = encodeRegs(Rd: Rd, Rn: Rn, Rm: Rm)
            let encoded = mask | ftype | regs
            return returnAsArray(encoded)
        case .fcvt(let target, let source):
            // https://developer.arm.com/documentation/ddi0596/2020-12/SIMD-FP-Instructions/FCVT--Floating-point-Convert-precision--scalar--?lang=en
            //                           ftype       opc       Rn    Rd
            let mask: Int64 = 0b00011110_00____10001_00__10000_00000_00000
            let ftype: Int64
            let opc: Int64
            switch(source.bits, target.bits) {
            case (32, 64):
                // single to double
                ftype   = 0b00 << 22
                opc     = 0b01 << 15
            case (64, 32):
                // double to single
                ftype   = 0b01 << 22
                opc     = 0b00 << 15
            case (16, 64):
                // half to double
                ftype   = 0b11 << 22
                opc     = 0b01 << 15
            case (64, 16):
                // double to half
                ftype   = 0b01 << 22
                opc     = 0b11 << 15
            default:
                fatalError("fcvt for \(source) -> \(target) not implemented")
            }
            let regs = encodeRegs(Rd: target, Rn: source)
            let encoded = mask | ftype | opc | regs
            return returnAsArray(encoded)
        default:
            print("Can't compile \(op)")
            throw EmitterM1Error.unsupportedOp
        }
    }
    
    /// Calculate size/opc for floating point ops.
    ///
    ///
    /// - Parameters:
    ///   - reg:ref register for size determination
    ///   - _32opc: what should opc be set to (before shift) for 32-bit regs
    ///   - _32opc: what should opc be set to (before shift) for 64-bit regs
    /// - Returns: <#description#>
    static func fp_sizeOpcMask(_ reg: any RegisterFP, _32opc: Int64 = 0b01, _64opc: Int64 = 0b01) -> (Int64, Int64) {
        if reg.double {
            return ((0b11 << 30), (_64opc << 22))
        } else if reg.single {
            return ((0b10 << 30), (_32opc << 22))
        } else {
            // https://developer.arm.com/documentation/ddi0596/2020-12/SIMD-FP-Instructions/STR--immediate--SIMD-FP---Store-SIMD-FP-register--immediate-offset--?lang=en
            fatalError("FP register size and opc not implemented")
        }
    }
    
    static func ftypeMask(_ reg: any RegisterFP) -> Int64 {
        let res: Int64
        if reg.double {
            res = 0b01
        } else if reg.single {
            res = 0b00
        } else if reg.half {
            res = 0b11
        } else {
            fatalError("Not implemented")
        }
        return res << 22
    }
    
    static func assertMatchingSize(_ regs: any RegisterFP...) throws {
        guard regs.count > 0 else { return }
        let f = regs[0]
        for reg in regs.dropFirst(1) {
            guard reg.double == f.double || reg.single == f.single else {
                throw GlobalError.invalidValue("Mismatched register sizes \(reg) and \(f)")
            }
        }
    }
    
    static func assertMatchingSize(_ regs: any RegisterI...) throws {
        guard regs.count > 0 else { return }
        let f = regs[0]
        for reg in regs.dropFirst(1) {
            guard reg.is64 == f.is64 else {
                throw GlobalError.invalidValue("Mismatched register sizes \(reg) and \(f)")
            }
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
