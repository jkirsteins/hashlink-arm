typealias ByteCount = Int64
protocol CpuOp : CustomDebugStringConvertible {
    func emit() throws -> [UInt8]
    var size: ByteCount { get }
}

extension M1Op {
    func resolveFinalForm() -> M1Op {
        switch(self) {
            
        case .ldrh(let Wt, .reg64(let Rn, nil)):
            return .ldrh(Wt, .imm64(Rn, 0, nil))
        case .ldrb(let Wt, .reg64(let Rn, nil)):
            return .ldrb(Wt, .imm64(Rn, 0, nil))
        case .lsl_r(let Rd, let Rn, let Rm):
            return .lslv(Rd, Rn, Rm)
        case .lsl_i(let Rd, let Rn, let immr) where Rd.is32 && Rn.is32:
            let newImmr = try! Immediate6(immr.flippedSign.immediate % 32)
            let imms: Immediate6 = try! Immediate6(31 - immr.immediate)
            return .ubfm(Rd, Rn, newImmr, imms)
        case .lsl_i(let Rd, let Rn, let immr) where Rd.is64 && Rn.is64:
            let newImmr = try! Immediate6(immr.flippedSign.immediate % 64)
            let imms: Immediate6 = try! Immediate6(63 - immr.immediate)
            return .ubfm(Rd, Rn, newImmr, imms)
        case .cmp(let Rn, let Rm) where Rn.is32 && Rm.is32:
            return .subs(W.wZR, Rn, .reg32shift(Rm as! Register32, nil))
        case .cmp(let Rn, let Rm) where !Rn.is32 && !Rm.is32:
            return .subs(X.sp, Rn, .reg64shift(Rm as! Register64, nil))
        case .ldr(let Rt, .reg64offset(let Rn, let offsetCount, nil)) where (offsetCount % 8) != 0:
            let imm9: Immediate9
            do {
                imm9 = try Immediate9(offsetCount)
            } catch {
                fatalError("offsetCount must fit in 9 bits for .ldur")
            }
            return .ldur(Rt, Rn, imm9)
        default:
            return self
        }
    }
}

enum M1Op : CpuOp {
    var size: ByteCount { 4 }
    
    var debugDescription: String {
        switch(self) {
        case .nop: return "nop"
        case .ret: return "ret"
        case .add(let rt, let rn, let off) where off.imm.isNegative:
            // We're sure this fits
            return M1Op.sub(rt, rn, try! Imm12Lsl12(off.imm.flippedSign, lsl: off.lsl)).debugDescription
        case .sub(let rt, let rn, let off) where off.imm.isPositive:
            return "sub \(rt), \(rn), \(off.debugDescription)"
        case .sub(let rt, let rn, let off):
            // We're sure this fits
            return M1Op.add(rt, rn, try! Imm12Lsl12(off.imm.flippedSign, lsl: off.lsl)).debugDescription
        case .add(let rt, let rn, let off):
            return "add \(rt), \(rn), \(off.debugDescription)"
        case .svc(let x): return "svc 0x\(String(x, radix: 16).leftPadding(toLength: 4, withPad: "0"))"
        case .str(let rt, .reg64offset(let rn, let offsetC, nil)):
            return "str \(rt), [\(rn), #\(offsetC)]"
        case .str(let rt, .reg64offset(let rn, let offsetC, .pre)):
            return "str \(rt), [\(rn), #\(offsetC)]!"
        case .str(let rt, .reg64offset(let rn, let offsetC, .post)):
            return "str \(rt), [\(rn)], #\(offsetC)"
        case .stur(let rt, let rn, let offset):
            return "stur \(rt), \(rn), #\(offset)"
        case .ldur(let rt, let rn, let offset):
            return "ldur \(rt), \(rn), #\(offset)"
        case .adr64(let rt, let offset):
            return "adr \(rt), #\(offset)"
        case .blr(let r):
            return "blr \(r)"
        case .bl(let r):
            return "bl #\(r)"
        case .b(let r):
            return "b #\(r)"
        case .movz32(let rt, let v, nil):
            return "movz \(rt), #\(v)"
        case .movz32(let rt, let v, let shift) where shift != nil:
            return "movz \(rt), #\(v), \(String(describing: shift))"
        case .movz64(let rt, let v, nil):
            return "movz \(rt), #\(v)"
        case .movz64(let rt, let v, let shift) where shift != nil:
            return "movz \(rt), #\(v), \(String(describing: shift)))"
        case .movr64(let rt, let rn):
            return "movr \(rt), \(rn)"
        case .orr64:
            fatalError("orr debugdesc not implemente4d")
        case .movk64(let rt, let val, nil):
            return "movk \(rt), #\(val)"
        case .movk64(let rt, let val, let shift) where shift != nil:
            return "movk \(rt), #\(val), \(shift!)"
        case .stp((let rt1, let rt2), Offset.reg64offset(let rn, let offset, .pre)):
            return "stp \(rt1), \(rt2), [\(rn), #\(offset)]!"
        case .stp((let rt1, let rt2), Offset.reg64offset(let rn, let offset, .post)):
            return "stp \(rt1), \(rt2), [\(rn)], #\(offset)"
        case .stp((let rt1, let rt2), Offset.reg64offset(let rn, let offset, nil)):
            return "stp \(rt1), \(rt2), [\(rn), #\(offset)]"
        case .ldp((let rt1, let rt2), Offset.reg64offset(let rn, let offset, .pre)):
            return "ldp \(rt1), \(rt2), [\(rn), #\(offset)]!"
        case .ldp((let rt1, let rt2), Offset.reg64offset(let rn, let offset, .post)):
            return "ldp \(rt1), \(rt2), [\(rn)], #\(offset)"
        case .ldp((let rt1, let rt2), Offset.reg64offset(let rn, let offset, nil)):
            return "ldp \(rt1), \(rt2), [\(rn), #\(offset)]"
        case .ldr(let rt, Offset.reg64offset(let rn, let offset, nil)):
            return "ldr \(rt), [\(rn), #\(offset)]"
        case .ldr(let rt, Offset.reg64offset(let rn, let offset, .pre)):
            return "ldr \(rt), [\(rn), #\(offset)]!"
        case .ldr(let rt, Offset.reg64offset(let rn, let offset, .post)):
            return "ldr \(rt), [\(rn)], #\(offset)"
        case .str(_, .immediate(_)):
            return "str immediate not implemented"
        case .str(_, .reg64shift(_, _)):
            return "str reg64shift not implemented"
        case .str(_, .reg32(_, _, _)):
            return "str reg32 not implemented"
        case .movz32(_, _, .some(_)):
            return "movz32 .some not implemented"
        case .movz64(_, _, .some(_)):
            return "movz64 .some not implemented"
        case .movk64(_, _, .some(_)):
            return "movk64 .some not implemented"
        case .stp(_, .immediate(_)):
            return "stp .immediate not implemented"
        case .stp(_, .reg64shift(_, _)):
            return "stp .reg64shift not implemented"
        case .stp(_, .reg32(_, _, _)):
            return "stp .reg32 not implemented"
        case .ldp(_, .immediate(_)):
            return "ldp .immediate not implemented"
        case .ldp(_, .reg64shift(_, _)):
            return "ldp .reg64shift not implemented"
        case .ldp(_, .reg32(_, _, _)):
            return "ldp .reg32 not implemented"
        case .ldr(_, .immediate(_)):
            return "ldr .immediate"
        case .ldr(_, .reg64shift(_, _)):
            return "ldr .reg64shift not implemented"
        case .ldr(_, .reg32(_, _, _)):
            return "ldr .reg32 not implemented"
        case .subs(let Rd, let Rn, let offset):
            return "subs \(Rd), \(Rn), \(offset)"
        case .cmp(let Rn, let Rm):
            return "cmp \(Rn), \(Rm)"
        case .str(_, .reg32shift(_, _)):
            return "str reg32shift not implemented"
        case .b_lt(let imm):
            return "b.lt #\(imm.signedImmediate)"
        case .b_eq(let imm):
            return "b.eq #\(imm.signedImmediate)"
        case .b_gt(let imm):
            return "b.gt #\(imm.signedImmediate)"
        case .b_ne(let imm):
            return "b.ne #\(imm.signedImmediate)"
        case .b_le(let imm):
            return "b.le #\(imm.signedImmediate)"
        case .b_ge(let imm):
            return "b.ge #\(imm.signedImmediate)"
        case .stp(_, .reg32shift(_, _)):
            return "stp reg32shift not implemented"
        case .ldp(_, .reg32shift(_, _)):
            return "ldp reg32shift not implemented"
        case .ldr(_, .reg32shift(_, _)):
            return "ldr reg32shift not implemented"
        case .ubfm(let Rd, let Rn, let immr, let imms):
            return "ubfx \(Rd), \(Rn), #\(immr.immediate), #\(imms.immediate)"
        case .lsl_i(let Rd, let Rn, let immr):
            return "lsl \(Rd), \(Rn), #\(immr.immediate)"
        case .lsl_r(let Rd, let Rn, let Rm):
            return "lsl \(Rd), \(Rn), \(Rm)"
        case .lslv(let Rd, let Rn, let Rm):
            return "lslv \(Rd), \(Rn), \(Rm)"
//        case .ldrb(let Rt, .reg64(let Rn, nil)):
//            return "ldrb \(Rt), [\(Rn)]"
//        case .ldrb(let Rt, .reg64(let Rn, .r64ext(let Xm, let extMode))):
//            return "ldrb \(Rt), [\(Rn), \(Xm), \(extMode)]"
//        case .ldrb(let Rt, .reg64(let Rn, .r32ext(let Wm, let extMode))):
//            return "ldrb \(Rt), [\(Rn), \(Wm), \(extMode)]"
//        case .ldrb(let Rt, .imm64(let Rn, let imm, nil)):
//            return "ldrb \(Rt), [\(Rn), #\(imm)]"
//        case .ldrb(let Rt, .imm64(let Rn, let imm, .post)):
//            return "ldrb \(Rt), [\(Rn)], #\(imm)"
//        case .ldrb(let Rt, .imm64(let Rn, let imm, .pre)):
//            return "ldrb \(Rt), [\(Rn), #\(imm)]!"
//        case .ldrb(let Rt, .reg64(let Rn, .r64shift(let Rm, ._0))):
//            return "ldrb \(Rt), [\(Rn), \(Rm)]"
        case .ldrh(let Rt, let val):
            return "ldrh \(Rt), \(val)"
        case .ldrb(let Rt, let val):
            return "ldrb \(Rt), \(val)"
        case .str(_, let mod):
            return "str mod \(String(describing: mod)) NOT IMPLEMENTED"
        case .stp(_, let mod):
            return "stp mod \(String(describing: mod)) NOT IMPLEMENTED"
        case .ldp(_, let mod):
            return "ldp mod \(String(describing: mod)) NOT IMPLEMENTED"
        case .ldr(_, let mod):
            return "ldr mod \(String(describing: mod)) NOT IMPLEMENTED"
        case .and(let Rd, let Rn, .imm(let imm, nil)):
            return "and \(Rd), \(Rn), #\(imm)"
        case .and(let Rd, let Rn, .r64shift(let Rm, .lsl(0))):
            return "and \(Rd), \(Rn), \(Rm)"
        case .and(_, _, .r64ext(_, _)):
            fallthrough
        case .and(_, _, .r32ext(_, _)):
            fallthrough
        case .and(_, _, .r64shift(_, _)):
            fallthrough
        case .and(_, _, .imm(_, .some(_))):
            return "and invalid w provided args"
        }
    }
    
    static func _add(_ r1: any Register, _ r2: any Register, _ offs: ByteCount) throws -> M1Op {
        .add(r1, r2, try Imm12Lsl12(Immediate12(offs), lsl: ._0))
    }
    
    func emit() throws -> [UInt8] {
        try EmitterM1.emit(for: self)
    }
    
    case nop
    case ret
    
    /* Supervisor call
     
     https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/SVC--Supervisor-Call-?lang=en
     */
    case svc(UInt16)
    
    /*
     -
     - STUR when not using pre/post indexing:
     https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/STUR--Store-Register--unscaled--?lang=en
     - When using pre/post indexing:
     https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/STR--immediate---Store-Register--immediate--?lang=en
     */
    case str(any Register, Offset)
    
    // https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/STUR--Store-Register--unscaled--?lang=en
    case stur(any Register, Register64, Int16)
    
    // https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/LDUR--Load-Register--unscaled--?lang=en
    case ldur(any Register, Register64, Immediate9)
    
    /*
     Form PC-relative address adds an immediate value to the PC value to form a PC-relative address, and writes the result to the destination register.
     
     https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/ADR--Form-PC-relative-address-?lang=en
     */
    case adr64(Register64, RelativeOffset)
    
    case sub(any Register, any Register, Imm12Lsl12) // negative -> alias for add
    case add(any Register, any Register, Imm12Lsl12) // negative -> alias for sub
    
    case b(RelativeOffset) // 26 bits max
    case blr(Register64)
    case bl(Immediate26)  // 26 bits max
    
    case cmp(any Register, any Register)
    case subs(any Register, any Register, Offset)
    
    case b_lt(Immediate19)
    case b_gt(Immediate19)
    case b_le(Immediate19)
    case b_ge(Immediate19)
    case b_eq(Immediate19)
    case b_ne(Immediate19)
    
    // https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/UBFM--Unsigned-Bitfield-Move-?lang=en
    case ubfm(  /*Rd*/any Register,
                      /*Rn*/any Register,
                      /*immr*/Immediate6,
                      /*imms*/Immediate6)
    
    /* alias of ubfm
     
     LSL (immediate)    32-bit    imms != '011111' && imms + 1 == immr
     LSL (immediate)    64-bit    imms != '111111' && imms + 1 == immr
     */
    case lsl_i(/*Rd*/any Register, /*Rn*/any Register, /*immr*/Immediate6)
    
    // https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/LSL--register---Logical-Shift-Left--register---an-alias-of-LSLV-
    case lsl_r(/*Rd*/any Register, /*Rn*/any Register, /*Rm*/any Register)
    
    // https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/LSLV--Logical-Shift-Left-Variable-?lang=en#LSLV_32_dp_2src
    case lslv(/*Rd*/any Register, /*Rn*/any Register, /*Rm*/any Register)
    
    // https://developer.arm.com/documentation/dui0802/a/A64-General-Instructions/MOVZ
    case movz32(Register32, UInt16, Register32.Shift?)
    case movz64(Register64, UInt16, Register64.Shift?)
    
    // when SP not included:
    //  - https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/MOV--register---Move--register---an-alias-of-ORR--shifted-register--?lang=en
    //  - MOV <Wd, Wm> is an alias of ORR <Wd>, WZR, <Wm> when SP not present
    // when SP included:
    //  - https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/MOV--to-from-SP---Move-between-register-and-stack-pointer--an-alias-of-ADD--immediate--
    //
    case movr64(Register64, Register64)
    
    // https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/ORR--shifted-register---Bitwise-OR--shifted-register--?lang=en
    case orr64(Register64, Register64, Register64, Register64.Shift?)
    
    // Shifted register
    //     https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/AND--shifted-register---Bitwise-AND--shifted-register--?lang=en
    // Immediate
    //    https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/AND--immediate---Bitwise-AND--immediate--?lang=en
    case and(any Register, any Register, RegModifier)
    
    // https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/MOVK--Move-wide-with-keep-
    case movk64(Register64, UInt16, Register64.Shift?)
    
    case stp((Register64, Register64), Offset)
    
    // https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/LDP--Load-Pair-of-Registers-?lang=en
    case ldp((Register64, Register64), Offset)
    
    /*
     # LDR (immediate)
     
     Loads a word or doubleword from memory and writes it to a register.
     
     The address that is used for the load is calculated from a base register and an immediate offset.
     
     Overview:
     - https://thinkingeek.com/2016/11/13/exploring-aarch64-assembler-chapter-5/
     
     Encoding:
     - https://developer.arm.com/documentation/ddi0596/2021-06/Index-by-Encoding/Loads-and-Stores?lang=en#ldst_pos
     - https://developer.arm.com/documentation/ddi0596/2021-06/Base-Instructions/LDR--immediate---Load-Register--immediate--?lang=en
     */
    case ldr(any Register, Offset)
    
    // https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/LDRB--register---Load-Register-Byte--register--
    case ldrb(/*Wt*/Register32, Offset)
    
    // https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/LDRH--register---Load-Register-Halfword--register--?lang=en
    case ldrh(/*Wt*/Register32, Offset)
}


