typealias ByteCount = Int64
protocol CpuOp : CustomDebugStringConvertible {
    func emit() throws -> [UInt8]
    var size: ByteCount { get }
}

extension M1Op {
    func resolveFinalForm() -> M1Op {
        switch(self) {
        case .asr(let Rd, let Rn, .reg(let Rm, nil)):
            return .asrv(Rd, Rn, Rm)
        case .asr(let Rd, let Rn, .immediate6(let shift)) where Rd.is32 && Rn.is32:
            return .sbfm(Rd, Rn, shift, 31)
        case .asr(let Rd, let Rn, .immediate6(let shift)) where Rd.is64 && Rn.is64:
            return .sbfm(Rd, Rn, shift, 63)
        case .lsr(let Rd, let Rn, .reg(let Rm, nil)):
            return .lsrv(Rd, Rn, Rm)
        case .lsr(let Rd, let Rn, .immediate6(let shift)) where Rd.is32 && Rn.is32:
            return .ubfm(Rd, Rn, shift, 31)
        case .lsr(let Rd, let Rn, .immediate6(let shift)) where Rd.is64 && Rn.is64:
            return .ubfm(Rd, Rn, shift, 63)
        case .strh(let Wt, .imm64(let Rn, let off, nil)) where off % 2 != 0:
            return .sturh(Wt, .imm64(Rn, off, nil))
        case .strh(let Wt, .reg64offset(let Rn as any Register, let off, let ixMode)):
            fallthrough
        case .strh(let Wt, .reg(let Rn, .imm(let off, let ixMode))):
            return .strh(Wt, .imm64(Rn.to64, off, ixMode))
        case .strh(let Wt, .reg(let Rn, nil)):
            return .strh(Wt, .imm64(Rn.to64, 0, nil))
        case .strb(let Wt, .reg64offset(let Rn as any Register, let off, let ixMode)):
            fallthrough
        case .strb(let Wt, .reg(let Rn, .imm(let off, let ixMode))):
            return .strb(Wt, .imm64(Rn.to64, off, ixMode))
        case .strb(let Wt, .reg(let Rn, nil)):
            return .strb(Wt, .imm64(Rn.to64, 0, nil))
        case .ldrh(let Wt, .imm64(let Rn, let off, nil)) where off % 2 != 0:
            return .ldurh(Wt, .imm64(Rn, off, nil))
        case .ldrh(let Wt, .reg(let Rn, nil)):
            return .ldrh(Wt, .imm64(Rn.to64, 0, nil))
        case .ldrb(let Wt, .reg(let Rn, nil)):
            return .ldrb(Wt, .imm64(Rn.to64, 0, nil))
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
        case .sxtw(let Rd, let Rn):
            return .sbfm(Rd, Rn.to64, 0, 31)
        case .sxth(let Rd, let Rn):
            return .sbfm(Rd, Rn.to64, 0, 15)
        case .sxtb(let Rd, let Rn):
            return .sbfm(Rd, Rn.to64, 0, 7)
        case .uxth(let Rd, let Rn):
            return .ubfm(Rd, Rn, 0, 15)
        case .uxtb(let Rd, let Rn):
            return .ubfm(Rd, Rn, 0, 7)
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
        case .addImm12(let rt, let rn, let off) where off.imm.isNegative:
            // We're sure this fits
            return M1Op.subImm12(rt, rn, try! Imm12Lsl12(off.imm.flippedSign, lsl: off.lsl)).debugDescription
        case .subImm12(let rt, let rn, let off) where off.imm.isPositive:
            return "sub \(rt), \(rn), \(off.debugDescription)"
        case .subImm12(let rt, let rn, let off):
            // We're sure this fits
            return M1Op.addImm12(rt, rn, try! Imm12Lsl12(off.imm.flippedSign, lsl: off.lsl)).debugDescription
        case .addImm12(let rt, let rn, let off):
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
        case .str(_, .immediate_depr(_)):
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
        case .stp(_, .immediate_depr(_)):
            return "stp .immediate not implemented"
        case .stp(_, .reg64shift(_, _)):
            return "stp .reg64shift not implemented"
        case .stp(_, .reg32(_, _, _)):
            return "stp .reg32 not implemented"
        case .ldp(_, .immediate_depr(_)):
            return "ldp .immediate not implemented"
        case .ldp(_, .reg64shift(_, _)):
            return "ldp .reg64shift not implemented"
        case .ldp(_, .reg32(_, _, _)):
            return "ldp .reg32 not implemented"
        case .ldr(_, .immediate_depr(_)):
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
        case .ldrh(let Rt, let val):
            if case .imm64(_, let offv, _) = val, offv % 2 != 0 {
                return M1Op.ldurh(Rt, val).debugDescription
            }
            return "ldrh \(Rt), \(val)"
        case .ldrb(let Rt, let val):
            return "ldrb \(Rt), \(val)"
        case .str(_, let mod):
            return "str mod \(String(describing: mod)) NOT IMPLEMENTED"
        case .stp(_, let mod):
            return "stp mod \(String(describing: mod)) NOT IMPLEMENTED"
        case .ldp(_, let mod):
            return "ldp mod \(String(describing: mod)) NOT IMPLEMENTED"
        case .ldr(let Rd, let mod):
            return "ldr \(Rd), \(mod)"
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
        case .sxtw(let Rd, let Rn):
            return "sxtw \(Rd), \(Rn)"
        case .sbfm(let Rd, let Rn, let immr, let imms):
            return "sbfm \(Rd), \(Rn), #\(immr.immediate), #\(imms.immediate)"
        case .sxth(let Rd, let Rn):
            return "sxth \(Rd), \(Rn)"
        case .sxtb(let Xd, let Wn):
            return "sxtb \(Xd), \(Wn)"
        case .uxtw(let Wd, let Wn):
            return "uxtw \(Wd), \(Wn)"
        case .uxth(let Wd, let Wn):
            return "uxth \(Wd), \(Wn)"
        case .uxtb(let Wd, let Wn):
            return "uxtb \(Wd), \(Wn)"
        case .sub(let Rd, let Rn, .r64shift(let Rm, let shift)):
            if case .lsl(0) = shift {
                return "sub \(Rd), \(Rn), \(Rm)"
            } else {
                return "sub \(Rd), \(Rn), \(Rm), \(shift)"
            }
        case .sub(_, _, .none):
            fallthrough
        case .sub(_, _, .some(.r64ext(_, _))):
            fallthrough
        case .sub(_, _, .some(.r32ext(_, _))):
            fallthrough
        case .sub(_, _, .some(.imm(_, _))):
            return "sub NOT IMPLEMENTED"
        case .strb(let Rd, let off):
            return "strb \(Rd), \(off)"
        case .strh(let Rd, let off):
            if case .imm64(_, let offv, _) = off, offv % 2 != 0 {
                return M1Op.sturh(Rd, off).debugDescription
            }
            return "strh \(Rd), \(off)"
        case .sturh(let Rd, let off):
            return "sturh \(Rd), \(off)"
        case .ldurh(let Rd, let off):
            return "ldurh \(Rd), \(off)"
        case .add(let Rd, let Rn, .r64shift(let Rm, let shift)):
            if case .lsl(0) = shift {
                return "add \(Rd), \(Rn), \(Rm)"
            } else {
                return "add \(Rd), \(Rn), \(Rm), \(shift)"
            }
        case .add(let rt, let rn, .imm(let off, nil)):
            return "add \(rt), \(rn), #\(off)"
        case .add(_, _, .some(.imm(_, .some(_)))):
            fallthrough
        case .add(_, _, .none):
            fallthrough
        case .add(_, _, .some(.r64ext(_, _))):
            fallthrough
        case .add(_, _, .some(.r32ext(_, _))):
            return "add <not impl>"
        case .lsr(let Rd, let Rn, .immediate6(let imm)):
            return "lsr \(Rd), \(Rn), #\(imm.immediate)"
        case .lsr(let Rd, let Rn, .reg(let Rm, nil)):
            return "lsr \(Rd), \(Rn), \(Rm)"
        case .lsr(let Rd, let Rn, let off):
            return "lsr \(Rd), \(Rn), \(off)"
        case .asr(_, _, .imm64(_, _, _)):
            fallthrough
        case .asr(_, _, .immediate_depr(_)):
            fallthrough
        case .asr(_, _, .reg64offset(_, _, _)):
            fallthrough
        case .asr(_, _, .reg32shift(_, _)):
            fallthrough
        case .asr(_, _, .reg64shift(_, _)):
            fallthrough
        case .asr(_, _, .reg(_, .some(_))):
            fallthrough
        case .asr(_, _, .reg32(_, _, _)):
            return "asr <not impl>"
        case .asr(let Rd, let Rn, .immediate6(let imm)):
            return "asr \(Rd), \(Rn), #\(imm.immediate)"
        case .asr(let Rd, let Rn, .reg(let Rm, nil)):
            return "asr \(Rd), \(Rn), \(Rm)"
        case .asrv(let Rd, let Rn, let Rm):
            return "asrv \(Rd), \(Rn), \(Rm)"
        case .lsrv(let Rd, let Rn, let Rm):
            return "lsrv \(Rd), \(Rn), \(Rm)"
        }
    }
    
    static func _add(_ r1: any Register, _ r2: any Register, _ offs: ByteCount) throws -> M1Op {
        .addImm12(r1, r2, try Imm12Lsl12(Immediate12(offs), lsl: ._0))
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
    case strb(Register32, Offset)
    case strh(Register32, Offset)
    
    // https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/STURH--Store-Register-Halfword--unscaled--
    case sturh(Register32, Offset)
    
    // https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/STUR--Store-Register--unscaled--?lang=en
    case stur(any Register, Register64, Int16)
    
    // https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/LDUR--Load-Register--unscaled--?lang=en
    case ldur(any Register, Register64, Immediate9)
    
    /*
     Form PC-relative address adds an immediate value to the PC value to form a PC-relative address, and writes the result to the destination register.
     
     https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/ADR--Form-PC-relative-address-?lang=en
     */
    case adr64(Register64, RelativeOffset)
    
    // deprecated
    case subImm12(any Register, any Register, Imm12Lsl12) // negative -> alias for add
    case sub(any Register, any Register, RegModifier?)
    
    // deprecated
    case addImm12(any Register, any Register, Imm12Lsl12) // negative -> alias for sub
    case add(any Register, any Register, RegModifier?)
    
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
    
    // https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/SXTW--Sign-Extend-Word--an-alias-of-SBFM-?lang=en
    case sxtw(Register64, Register32)   // SBFM <Xd>, <Xn>, #0, #31

    // https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/SXTH--Sign-Extend-Halfword--an-alias-of-SBFM-?lang=en
    case sxth(Register64, Register32)   // SBFM <Xd>, <Xn>, #0, #15
    
    case sxtb(Register64, Register32)   // SBFM <Xd>, <Xn>, #0, #7
    
    case uxtw(Register32, Register32)
    case uxth(Register32, Register32)
    case uxtb(Register32, Register32)
    
    // https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/SBFM--Signed-Bitfield-Move-?lang=en#sa_immr
    case sbfm(any Register, any Register, Immediate6, Immediate6)
    
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
    
    /* can be alias of ubfm or lsrv
     */
    case lsr(any Register, any Register, Offset)
    
    // https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/LSRV--Logical-Shift-Right-Variable-?lang=en
    case lsrv(any Register, any Register, any Register)
    
    case asr(any Register, any Register, Offset)
    case asrv(any Register, any Register, any Register)
    
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
    
    // https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/LDURH--Load-Register-Halfword--unscaled--
    case ldurh(Register32, Offset)
}


