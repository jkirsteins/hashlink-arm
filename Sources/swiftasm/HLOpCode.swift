typealias Reg = Int32
typealias JumpOffset = Int32
typealias RefInt = TableIndex
typealias Ref = TableIndex
typealias RefFloat = TableIndex
typealias ValBool = Int32
typealias RefBytes = TableIndex
typealias RefString = TableIndex
typealias RefFun = TableIndex
typealias RefField = TableIndex
typealias RefGlobal = TableIndex
typealias RefType = TableIndex
typealias RefEnumConstruct = TableIndex

// https://github.com/Gui-Yom/hlbc/blob/master/hlbc/src/opcodes.rs

extension HLOpCode {
    static func read_2reg_varReg(from reader: ByteReader) throws -> (Reg, Ref, [Reg]) {
        let reg = try reader.readVarInt()
        let ref = try reader.readIndex()
        let count = try reader.readUInt8()
        let regs: [Reg] = try (0..<count).map { _ in 
            try reader.readVarInt()
        }

        return (reg, ref, regs)
    }

    static func read(from reader: ByteReader) throws -> HLOpCode {
        let code = try reader.readUInt8()
        switch(code) {
            case 0: fatalError(" OMov ")
            case 1: fatalError(" OInt ")
            case 2: fatalError(" OFloat ")
            case 3: fatalError(" OBool ")
            case 4: fatalError(" OBytes ")
            case 5: fatalError(" OString ")
            case 6: fatalError(" ONull ")

            case 7: fatalError(" OAdd ")
            case 8: fatalError(" OSub ")
            case 9: fatalError(" OMul ")
            case 10: fatalError(" OSDiv ")
            case 11: fatalError(" OUDiv ")
            case 12: fatalError(" OSMod ")
            case 13: fatalError(" OUMod ")
            case 14: fatalError(" OShl ")
            case 15: fatalError(" OSShr ")
            case 16: fatalError(" OUShr ")
            case 17: fatalError(" OAnd ")
            case 18: fatalError(" OOr ")
            case 19: fatalError(" OXor ") 

            case 20: fatalError(" ONeg ")
            case 21: fatalError(" ONot ")
            case 22: fatalError(" OIncr ")
            case 23: fatalError(" ODecr ")

            case 24: fatalError(" OCall0 ")
            case 25: fatalError(" OCall1 ")
            case 26: fatalError(" OCall2 ")
            case 27: fatalError(" OCall3 ")
            case 28: fatalError(" OCall4 ")
            case 29: 
                let result = try HLOpCode.read_2reg_varReg(from: reader)
                return .OCallN(dst: result.0, fun: result.1, args: result.2)
            case 30: 
                let result = try HLOpCode.read_2reg_varReg(from: reader)
                return .OCallMethod(dst: result.0, field: result.1, args: result.2)
            case 31: 
                let result = try HLOpCode.read_2reg_varReg(from: reader)
                return .OCallThis(dst: result.0, field: result.1, args: result.2)
            case 32: 
                let result = try HLOpCode.read_2reg_varReg(from: reader)
                return .OCallClosure(dst: result.0, fun: result.1, args: result.2)

            case 33: fatalError(" OStaticClosure ")
            case 34: fatalError(" OInstanceClosure ")
            case 35: fatalError(" OVirtualClosure ")

            case 36: fatalError(" OGetGlobal ")
            case 37: fatalError(" OSetGlobal ")
            case 38: fatalError(" OField ")
            case 39: fatalError(" OSetField ")
            case 40: fatalError(" OGetThis ")
            case 41: 
                return .OSetThis(
                    field: try reader.readIndex(), 
                    src: try reader.readVarInt())
            case 42: fatalError(" ODynGet ")
            case 43: fatalError(" ODynSet ")

            case 44: fatalError(" OJTrue ")
            case 45: fatalError(" OJFalse ")
            case 46: fatalError(" OJNull ")
            case 47: fatalError(" OJNotNull ")
            case 48: fatalError(" OJSLt ")
            case 49: fatalError(" OJSGte ")
            case 50: fatalError(" OJSGt ")
            case 51: fatalError(" OJSLte ")
            case 52: fatalError(" OJULt ")
            case 53: fatalError(" OJUGte ")
            case 54: fatalError(" OJNotLt ")
            case 55: fatalError(" OJNotGte ")
            case 56: fatalError(" OJEq ")
            case 57: fatalError(" OJNotEq ")
            case 58: fatalError(" OJAlways ")

            case 59: fatalError(" OToDyn ")
            case 60: fatalError(" OToSFloat ")
            case 61: fatalError(" OToUFloat ")
            case 62: fatalError(" OToInt ")
            case 63: fatalError(" OSafeCast ")
            case 64: fatalError(" OUnsafeCast ")
            case 65: fatalError(" OToVirtual ") 

            case 66: fatalError(" OLabel ")
            case 67: 
                let reg = try reader.readVarInt()
                return .ORet(ret: reg)
            case 68: fatalError(" OThrow ")
            case 69: fatalError(" ORethrow ")
            case 70: fatalError(" OSwitch ")
            case 71: fatalError(" ONullCheck ")
            case 72: fatalError(" OTrap ")
            case 73: fatalError(" OEndTrap ") 

            case 74: fatalError(" OGetI8 ")
            case 75: fatalError(" OGetI16 ")
            case 76: fatalError(" OGetMem ")
            case 77: fatalError(" OGetArray ")
            case 78: fatalError(" OSetI8 ")
            case 79: fatalError(" OSetI16 ")
            case 80: fatalError(" OSetMem ")
            case 81: fatalError(" OSetArray ")

            case 82: fatalError(" ONew ")
            case 83: fatalError(" OArraySize ")
            case 84: fatalError(" OType ")
            case 85: fatalError(" OGetType ")
            case 86: fatalError(" OGetTID ") 

            case 87: fatalError(" ORef ")
            case 88: fatalError(" OUnref ")
            case 89: fatalError(" OSetref ")

            case 90: 
                let result = try HLOpCode.read_2reg_varReg(from: reader)
                return .OMakeEnum(dst: result.0, construct: result.1, args: result.2)
            case 91: fatalError(" OEnumAlloc ")
            case 92: fatalError(" OEnumIndex ")
            case 93: fatalError(" OEnumField ")
            case 94: fatalError(" OSetEnumField ")

            case 95: fatalError(" OAssert ")
            case 96: fatalError(" ORefData ")
            case 97: fatalError(" ORefOffset ")
            case 98: fatalError(" ONop ")
            default:
            fatalError("Unknown opcode \(code)")
        }
    }
}

enum HLOpCode {
    /// Copy value from *src* into *dst*
    case OMov(dst: Reg, src: Reg)
    case OInt(dst: Reg, ptr: RefInt)
    case OFloat(dst: Reg, ptr: RefFloat)
    case OBool(dst: Reg, value: ValBool)
    case OBytes(dst: Reg, ptr: RefBytes)
    case OString(dst: Reg, ptr: RefString)
    /// Nullify a register
    ///
    /// *dst* = null
    case ONull(dst: Reg)

    /// Add two numbers
    ///
    /// *dst* = *a* + *b*
    case OAdd(dst: Reg, a: Reg, b: Reg)
    case OSub(dst: Reg, a: Reg, b: Reg)
    case OMul(dst: Reg, a: Reg, b: Reg)
    case OSDiv(dst: Reg, a: Reg, b: Reg)
    case OUDiv(dst: Reg, a: Reg, b: Reg)
    case OSMod(dst: Reg, a: Reg, b: Reg)
    case OUMod(dst: Reg, a: Reg, b: Reg)
    case OShl(dst: Reg, a: Reg, b: Reg)
    case OSShr(dst: Reg, a: Reg, b: Reg)
    case OUShr(dst: Reg, a: Reg, b: Reg)
    case OAnd(dst: Reg, a: Reg, b: Reg)
    case OOr(dst: Reg, a: Reg, b: Reg)
    case OXor(dst: Reg, a: Reg, b: Reg)

    case ONeg(dst: Reg, src: Reg)
    case ONot(dst: Reg, src: Reg)
    case OIncr(dst: Reg)
    case ODecr(dst: Reg)

    /// Call a function with no argument
    ///
    /// *dst* = *fun*()
    case OCall0(dst: Reg, fun: RefFun)

    /// Call a function with one argument
    ///
    /// *dst* = *fun*(*arg0*)
    case OCall1(dst: Reg, fun: RefFun, arg0: Reg)

    /// Call a function with two arguments
    ///
    /// *dst* = *fun*(*arg0*, *arg1*)
    case OCall2(dst: Reg, fun: RefFun, arg0: Reg, arg1: Reg)
    case OCall3(dst: Reg, fun: RefFun, arg0: Reg, arg1: Reg, arg2: Reg)
    case OCall4(dst: Reg, fun: RefFun, arg0: Reg, arg1: Reg, arg2: Reg, arg3: Reg)

    /// Call a function with N arguments
    ///
    /// *dst* = *fun*(*arg0*, *arg1*, ...)
    case OCallN(dst: Reg, fun: RefFun, args: [Reg])

    /// Call a function with N arguments, using the first argument as the receiver
    ///
    /// *dst* = *arg0*.*field*(*arg1*, *arg2*, ...)
    case OCallMethod(dst: Reg, field: RefField, args: [Reg])
    /// Call a function with N arguments, the receiver is the first register of the parent function
    ///
    /// *dst* = *reg0*.*field*(*arg0*, *arg1*, ...)
    case OCallThis(dst: Reg, field: RefField, args: [Reg])
    /// Call a closure with N arguments. Here *fun* is a register.
    ///
    /// *dst* = *fun*(*arg0*, *arg1*, ...)
    case OCallClosure(dst: Reg, fun: RefFun, args: [Reg])
    /// Create a closure from a function reference.
    ///
    /// *dst* = *fun*
    case OStaticClosure(dst: Reg, fun: RefFun)
    /// Create a closure from an object method.
    ///
    /// *dst* = *obj*.*fun*
    case OInstanceClosure(dst: Reg, fun: RefFun, obj: Reg)
    /// Create a closure from an object field.
    ///
    /// *dst* = *obj*.*field*
    case OVirtualClosure(dst: Reg, obj: Reg, field: Reg)
    /// Get a global value.
    ///
    /// *dst* = *global*
    case OGetGlobal(dst: Reg, global: RefGlobal)
    /// Set a global value.
    ///
    /// `global = src`
    case OSetGlobal(global: RefGlobal, src: Reg)
    case OField(dst: Reg, obj: Reg, field: RefField)
    case OSetField(obj: Reg, field: RefField, src: Reg)
    case OGetThis(dst: Reg, field: RefField)
    case OSetThis(field: RefField, src: Reg)
    case ODynGet(dst: Reg, obj: Reg, field: RefString)
    case ODynSet(obj: Reg, field: RefString, src: Reg)
    /// Jump by an offset if the condition is true
    case OJTrue(cond: Reg, offset: JumpOffset)
    /// Jump by an offset if the condition is false
    case OJFalse(cond: Reg, offset: JumpOffset)
    case OJNull(reg: Reg, offset: JumpOffset)
    case OJNotNull(reg: Reg, offset: JumpOffset)
    case OJSLt(a: Reg, b: Reg, offset: JumpOffset)
    case OJSGte(a: Reg, b: Reg, offset: JumpOffset)
    case OJSGt(a: Reg, b: Reg, offset: JumpOffset)
    case OJSLte(a: Reg, b: Reg, offset: JumpOffset)
    case OJULt(a: Reg, b: Reg, offset: JumpOffset)
    case OJUGte(a: Reg, b: Reg, offset: JumpOffset)
    case OJNotLt(a: Reg, b: Reg, offset: JumpOffset)
    case OJNotGte(a: Reg, b: Reg, offset: JumpOffset)
    case OJEq(a: Reg, b: Reg, offset: JumpOffset)
    case OJNotEq(a: Reg, b: Reg, offset: JumpOffset)
    case OJAlways(offset: JumpOffset)

    case OToDyn(dst: Reg, src: Reg)
    case OToSFloat(dst: Reg, src: Reg)
    case OToUFloat(dst: Reg, src: Reg)
    case OToInt(dst: Reg, src: Reg)
    case OSafeCast(dst: Reg, src: Reg)
    case OUnsafeCast(dst: Reg, src: Reg)
    case OToVirtual(dst: Reg, src: Reg)

    case OLabel
    case ORet(ret: Reg)
    case OThrow(exc: Reg)
    case ORethrow(exc: Reg)
    /// Select a jump offset based on the integer value
    case OSwitch(reg: Reg, offsets: [JumpOffset], end: JumpOffset)
    case ONullCheck(reg: Reg)
    case OTrap(exc: Reg,
        offset: JumpOffset)
    case OEndTrap(exc: Reg)

    case OGetI8(dst: Reg, bytes: Reg, index: Reg)
    case OGetI16(dst: Reg, bytes: Reg, index: Reg)
    case OGetMem(dst: Reg, bytes: Reg, index: Reg)
    case OGetArray(dst: Reg, array: Reg, index: Reg)
    case OSetI8(bytes: Reg, index: Reg, src: Reg)
    case OSetI16(bytes: Reg, index: Reg, src: Reg)
    case OSetMem(bytes: Reg, index: Reg, src: Reg)
    case OSetArray(bytes: Reg, index: Reg, src: Reg)

    case ONew(dst: Reg)
    case OArraySize(dst: Reg, array: Reg)
    case OType(dst: Reg, ty: RefType)
    case OGetType(dst: Reg, src: Reg)
    case OGetTID(dst: Reg, src: Reg)

    case ORef(dst: Reg, src: Reg)
    case OUnref(dst: Reg, src: Reg)
    case OSetref(dst: Reg, value: Reg)

    /// Allocate and initialize an enum variant
    case OMakeEnum(dst: Reg, construct: RefEnumConstruct, args: [Reg])
    case OEnumAlloc(dst: Reg, construct: RefEnumConstruct)
    case OEnumIndex(dst: Reg, value: Reg)
    case OEnumField(dst: Reg, value: Reg, construct: RefEnumConstruct, field: RefField)
    case OSetEnumField(value: Reg, field: RefField, src: Reg)

    case OAssert
    case ORefData(dst: Reg, src: Reg)
    case ORefOffset(dst: Reg, reg: Reg, offset: Reg)
    case ONop

    //  case OLast
} 
