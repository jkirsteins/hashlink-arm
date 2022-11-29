// https://github.com/Gui-Yom/hlbc/blob/master/hlbc/src/opcodes.rs

// position in function (for calculating jumps)
typealias OpCodePosition = UInt32

enum HLOpCodeId: UInt32, Equatable {
    case OMov
    case OInt
    case OFloat
    case OBool
    case OBytes
    case OString
    case ONull
    case OAdd
    case OSub
    case OMul
    case OSDiv
    case OUDiv
    case OSMod
    case OUMod
    case OShl
    case OSShr
    case OUShr
    case OAnd
    case OOr  // 18
    case OXor
    case ONeg  // 20
    case ONot
    case OIncr
    case ODecr
    case OCall0
    case OCall1
    case OCall2
    case OCall3
    case OCall4
    case OCallN
    case OCallMethod
    case OCallThis
    case OCallClosure
    case OStaticClosure
    case OInstanceClosure
    case OVirtualClosure
    case OGetGlobal
    case OSetGlobal
    case OField
    case OSetField
    case OGetThis
    case OSetThis
    case ODynGet
    case ODynSet
    case OJTrue
    case OJFalse
    case OJNull
    case OJNotNull
    case OJSLt
    case OJSGte
    case OJSGt  // 50
    case OJSLte
    case OJULt
    case OJUGte
    case OJNotLt
    case OJNotGte
    case OJEq
    case OJNotEq
    case OJAlways
    case OToDyn
    case OToSFloat
    case OToUFloat
    case OToInt
    case OSafeCast
    case OUnsafeCast
    case OToVirtual
    case OLabel  // 66
    case ORet
    case OThrow
    case ORethrow
    case OSwitch
    case ONullCheck
    case OTrap
    case OEndTrap
    case OGetI8
    case OGetI16
    case OGetMem
    case OGetArray
    case OSetI8
    case OSetI16
    case OSetMem
    case OSetArray
    case ONew
    case OArraySize
    case OType
    case OGetType
    case OGetTID
    case ORef
    case OUnref
    case OSetref
    case OMakeEnum
    case OEnumAlloc
    case OEnumIndex
    case OEnumField
    case OSetEnumField
    case OAssert  // 95
    case ORefData
    case ORefOffset
    case ONop  // 98
}

extension HLOpCode {
    static func read_2reg_varReg(from reader: ByteReader) throws -> (Reg, Ref, [Reg]) {
        let reg = try reader.readVarInt()
        let ref = try reader.readIndex()
        let count = try reader.readUInt8()
        let regs: [Reg] = try (0..<count).map { _ in try reader.readVarInt() }

        return (reg, ref, regs)
    }

    static func read(for pos: OpCodePosition, from reader: ByteReader) throws
        -> HLOpCode
    {
        let code = try reader.readUInt8()
        switch code {
        case 0: return .OMov(dst: try reader.readReg(), src: try reader.readReg())
        case 1: return .OInt(dst: try reader.readReg(), ptr: try reader.readRef())
        case 2: return .OFloat(dst: try reader.readReg(), ptr: try reader.readRef())
        case 3: return .OBool(dst: try reader.readReg(), value: try reader.readBool())
        case 4: return .OBytes(dst: try reader.readReg(), ptr: try reader.readRef())
        case 5: return .OString(dst: try reader.readReg(), ptr: try reader.readRef())
        case 6: return .ONull(dst: try reader.readReg())

        case 7:
            return .OAdd(
                dst: try reader.readReg(),
                a: try reader.readReg(),
                b: try reader.readReg()
            )
        case 8:
            return .OSub(
                dst: try reader.readReg(),
                a: try reader.readReg(),
                b: try reader.readReg()
            )
        case 9:
            return .OMul(
                dst: try reader.readReg(),
                a: try reader.readReg(),
                b: try reader.readReg()
            )
        case 10:
            return .OSDiv(
                dst: try reader.readReg(),
                a: try reader.readReg(),
                b: try reader.readReg()
            )
        case 11:
            return .OUDiv(
                dst: try reader.readReg(),
                a: try reader.readReg(),
                b: try reader.readReg()
            )
        case 12:
            return .OSMod(
                dst: try reader.readReg(),
                a: try reader.readReg(),
                b: try reader.readReg()
            )
        case 13:
            return .OUMod(
                dst: try reader.readReg(),
                a: try reader.readReg(),
                b: try reader.readReg()
            )
        case 14:
            return .OShl(
                dst: try reader.readReg(),
                a: try reader.readReg(),
                b: try reader.readReg()
            )
        case 15:
            return .OSShr(
                dst: try reader.readReg(),
                a: try reader.readReg(),
                b: try reader.readReg()
            )
        case 16:
            return .OUShr(
                dst: try reader.readReg(),
                a: try reader.readReg(),
                b: try reader.readReg()
            )
        case 17:
            return .OAnd(
                dst: try reader.readReg(),
                a: try reader.readReg(),
                b: try reader.readReg()
            )
        case 18:
            return .OOr(
                dst: try reader.readReg(),
                a: try reader.readReg(),
                b: try reader.readReg()
            )
        case 19:
            return .OXor(
                dst: try reader.readReg(),
                a: try reader.readReg(),
                b: try reader.readReg()
            )

        case 20: return .ONeg(dst: try reader.readReg(), src: try reader.readReg())
        case 21: return .ONot(dst: try reader.readReg(), src: try reader.readReg())
        case 22: return .OIncr(dst: try reader.readReg())
        case 23: return .ODecr(dst: try reader.readReg())

        case 24: return .OCall0(dst: try reader.readReg(), fun: try reader.readRef())
        case 25:
            return .OCall1(
                dst: try reader.readReg(),
                fun: try reader.readRef(),
                arg0: try reader.readReg()
            )
        case 26:
            return .OCall2(
                dst: try reader.readReg(),
                fun: try reader.readRef(),
                arg0: try reader.readReg(),
                arg1: try reader.readReg()
            )
        case 27:
            return .OCall3(
                dst: try reader.readReg(),
                fun: try reader.readRef(),
                arg0: try reader.readReg(),
                arg1: try reader.readReg(),
                arg2: try reader.readReg()
            )
        case 28:
            return .OCall4(
                dst: try reader.readReg(),
                fun: try reader.readRef(),
                arg0: try reader.readReg(),
                arg1: try reader.readReg(),
                arg2: try reader.readReg(),
                arg3: try reader.readReg()
            )
        case 29:
            let result = try HLOpCode.read_2reg_varReg(from: reader)
            return .OCallN(dst: result.0, fun: result.1, args: result.2)
        case 30:
            let p1 = try reader.readReg()
            let p2 = try reader.readReg()
            let p3 = try reader.readUInt8()
            let args = try (0..<p3).map { _ in try reader.readIndex() }
//            let result = try HLOpCode.read_2reg_varReg(from: reader)
            fatalError("Not implemented. Allocate memory for args")
        case 31:
            let result = try HLOpCode.read_2reg_varReg(from: reader)
            return .OCallThis(dst: result.0, field: result.1, args: result.2)
        case 32:
            let result = try HLOpCode.read_2reg_varReg(from: reader)
            fatalError("Need to allocate the mem, and put all args in there")
//            return .OCallClosure(dst: result.0, fun: result.1, args: result.2)

        case 33:
            return .OStaticClosure(dst: try reader.readReg(), fun: try reader.readRef())
        case 34:
            return .OInstanceClosure(
                dst: try reader.readReg(),
                fun: try reader.readRef(),
                obj: try reader.readReg()
            )
        case 35:
            return .OVirtualClosure(
                dst: try reader.readReg(),
                obj: try reader.readReg(),
                field: try reader.readReg()
            )

        case 36:
            return .OGetGlobal(dst: try reader.readReg(), global: try reader.readRef())
        case 37:
            return .OSetGlobal(global: try reader.readRef(), src: try reader.readReg())
        case 38:
            return .OField(
                dst: try reader.readReg(),
                obj: try reader.readReg(),
                field: try reader.readRef()
            )
        case 39:
            return .OSetField(
                obj: try reader.readReg(),
                field: try reader.readRef(),
                src: try reader.readReg()
            )
        case 40:
            return .OGetThis(dst: try reader.readReg(), field: try reader.readRef())
        case 41:
            return .OSetThis(
                field: try reader.readIndex(),
                src: try reader.readVarInt()
            )
        case 42:
            return .ODynGet(
                dst: try reader.readReg(),
                obj: try reader.readReg(),
                field: try reader.readRef()
            )
        case 43:
            return .ODynSet(
                obj: try reader.readReg(),
                field: try reader.readRef(),
                src: try reader.readReg()
            )

        case 44:
            return .OJTrue(
                cond: try reader.readReg(),
                offset: try reader.readJumpOffset() + Int32(pos) + 1
            )
        case 45:
            return .OJFalse(
                cond: try reader.readReg(),
                offset: try reader.readJumpOffset() + Int32(pos) + 1
            )
        case 46:
            return .OJNull(
                reg: try reader.readReg(),
                offset: try reader.readJumpOffset() + Int32(pos) + 1
            )
        case 47:
            return .OJNotNull(
                reg: try reader.readReg(),
                offset: try reader.readJumpOffset() + Int32(pos) + 1
            )
        case 48:
            return .OJSLt(
                a: try reader.readReg(),
                b: try reader.readReg(),
                offset: try reader.readJumpOffset() + Int32(pos) + 1
            )
        case 49:
            return .OJSGte(
                a: try reader.readReg(),
                b: try reader.readReg(),
                offset: try reader.readJumpOffset() + Int32(pos) + 1
            )
        case 50:
            return .OJSGt(
                a: try reader.readReg(),
                b: try reader.readReg(),
                offset: try reader.readJumpOffset() + Int32(pos) + 1
            )
        case 51:
            return .OJSLte(
                a: try reader.readReg(),
                b: try reader.readReg(),
                offset: try reader.readJumpOffset() + Int32(pos) + 1
            )
        case 52:
            return .OJULt(
                a: try reader.readReg(),
                b: try reader.readReg(),
                offset: try reader.readJumpOffset() + Int32(pos) + 1
            )
        case 53:
            return .OJUGte(
                a: try reader.readReg(),
                b: try reader.readReg(),
                offset: try reader.readJumpOffset() + Int32(pos) + 1
            )
        case 54:
            return .OJNotLt(
                a: try reader.readReg(),
                b: try reader.readReg(),
                offset: try reader.readJumpOffset() + Int32(pos) + 1
            )
        case 55:
            return .OJNotGte(
                a: try reader.readReg(),
                b: try reader.readReg(),
                offset: try reader.readJumpOffset() + Int32(pos) + 1
            )
        case 56:
            return .OJEq(
                a: try reader.readReg(),
                b: try reader.readReg(),
                offset: try reader.readJumpOffset() + Int32(pos) + 1
            )
        case 57:
            return .OJNotEq(
                a: try reader.readReg(),
                b: try reader.readReg(),
                offset: try reader.readJumpOffset() + Int32(pos) + 1
            )
        case 58: return .OJAlways(offset: try reader.readJumpOffset() + Int32(pos) + 1)
        case 59: return .OToDyn(dst: try reader.readReg(), src: try reader.readReg())
        case 60: return .OToSFloat(dst: try reader.readReg(), src: try reader.readReg())
        case 61: return .OToUFloat(dst: try reader.readReg(), src: try reader.readReg())
        case 62: return .OToInt(dst: try reader.readReg(), src: try reader.readReg())
        case 63: return .OSafeCast(dst: try reader.readReg(), src: try reader.readReg())
        case 64:
            return .OUnsafeCast(dst: try reader.readReg(), src: try reader.readReg())
        case 65:
            return .OToVirtual(dst: try reader.readReg(), src: try reader.readReg())
        case 66: return .OLabel
        case 67:
            let reg = try reader.readVarInt()
            return .ORet(ret: reg)
        case 68: return .OThrow(exc: try reader.readReg())
        case 69: return .ORethrow(exc: try reader.readReg())
        case 70:
            let reg = try reader.readReg()
            let noffsets = try reader.readVarInt()
            let offsets = try (0..<noffsets).map { _ in try reader.readJumpOffset() }
            let end = try reader.readJumpOffset()
            return .OSwitch(reg: reg, offsets: offsets, end: end)
        case 71: return .ONullCheck(reg: try reader.readReg())
        case 72:
            return .OTrap(
                exc: try reader.readReg(),
                offset: try reader.readJumpOffset() + Int32(pos) + 1
            )
        case 73: return .OEndTrap(exc: try reader.readReg())

        case 74:
            return .OGetI8(
                dst: try reader.readReg(),
                bytes: try reader.readReg(),
                index: try reader.readReg()
            )
        case 75:
            return .OGetI16(
                dst: try reader.readReg(),
                bytes: try reader.readReg(),
                index: try reader.readReg()
            )
        case 76:
            return .OGetMem(
                dst: try reader.readReg(),
                bytes: try reader.readReg(),
                index: try reader.readReg()
            )
        case 77:
            return .OGetArray(
                dst: try reader.readReg(),
                array: try reader.readReg(),
                index: try reader.readReg()
            )
        case 78:
            return .OSetI8(
                bytes: try reader.readReg(),
                index: try reader.readReg(),
                src: try reader.readReg()
            )
        case 79:
            return .OSetI16(
                bytes: try reader.readReg(),
                index: try reader.readReg(),
                src: try reader.readReg()
            )
        case 80:
            return .OSetMem(
                bytes: try reader.readReg(),
                index: try reader.readReg(),
                src: try reader.readReg()
            )
        case 81:
            return .OSetArray(
                array: try reader.readReg(),
                index: try reader.readReg(),
                src: try reader.readReg()
            )
        case 82: return .ONew(dst: try reader.readReg())
        case 83:
            return .OArraySize(dst: try reader.readReg(), array: try reader.readReg())
        case 84: return .OType(dst: try reader.readReg(), ty: try reader.readRef())
        case 85: return .OGetType(dst: try reader.readReg(), src: try reader.readReg())
        case 86: return .OGetTID(dst: try reader.readReg(), src: try reader.readReg())
        case 87: return .ORef(dst: try reader.readReg(), src: try reader.readReg())
        case 88: return .OUnref(dst: try reader.readReg(), src: try reader.readReg())
        case 89: return .OSetref(dst: try reader.readReg(), value: try reader.readReg())
        case 90:
            let result = try HLOpCode.read_2reg_varReg(from: reader)
            return .OMakeEnum(dst: result.0, construct: result.1, args: result.2)
        case 91:
            return .OEnumAlloc(
                dst: try reader.readReg(),
                construct: try reader.readRef()
            )
        case 92:
            return .OEnumIndex(dst: try reader.readReg(), value: try reader.readReg())
        case 93:
            return .OEnumField(
                dst: try reader.readReg(),
                value: try reader.readReg(),
                construct: try reader.readRef(),
                field: try reader.readRef()
            )
        case 94:
            return .OSetEnumField(
                value: try reader.readReg(),
                field: try reader.readRef(),
                src: try reader.readReg()
            )

        case 95: return .OAssert
        case 96: return .ORefData(dst: try reader.readReg(), src: try reader.readReg())
        case 97:
            return .ORefOffset(
                dst: try reader.readReg(),
                reg: try reader.readReg(),
                offset: try reader.readReg()
            )
        case 98: return .ONop
        default: fatalError("Unknown opcode \(code)")
        }
    }
}

enum HLOpCode : Equatable, Hashable {
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
    case OCallMethod(dst: Reg, obj: Reg, proto: RefProto, args: [Reg])
    /// Call a function with N arguments, the receiver is the first register of the parent function
    ///
    /// *dst* = *reg0*.*field*(*arg0*, *arg1*, ...)
    case OCallThis(dst: Reg, field: RefField, args: [Reg])
    /// Call a closure with N arguments. Here *fun* is a register.
    ///
    /// *dst* = *fun*(*arg0*, *arg1*, ...)
    case OCallClosure(dst: Reg, closure: Reg, args: [Reg])
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
    
    //    jult [a], [b], [offset] jump by offset opcodes if register values comparison a < b (integer types only) unsigned mode
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
    
    /// safecast [dst], [src] cast register src into register dst, throw an exception if there is no way to perform such operation
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
    case OTrap(exc: Reg, offset: JumpOffset)
    case OEndTrap(exc: Reg)

    case OGetI8(dst: Reg, bytes: Reg, index: Reg)
    case OGetI16(dst: Reg, bytes: Reg, index: Reg)
    case OGetMem(dst: Reg, bytes: Reg, index: Reg)
    case OGetArray(dst: Reg, array: Reg, index: Reg)
    case OSetI8(bytes: Reg, index: Reg, src: Reg)
    
    /// Stores 16 bits unsigned integer from register `src` into register `bytes`, offset by `index` bytes
    /// - Parameters:
    ///   - bytes: vreg containing `unsigned short*`
    ///   - index: vreg containing the offset (offset is in bytes, and NOT multiples of `sizeof(unsigned short)`)
    ///   - src: vreg containing `unsigned short` value to store
    case OSetI16(bytes: Reg, index: Reg, src: Reg)
    
    case OSetMem(bytes: Reg, index: Reg, src: Reg)
    case OSetArray(array: Reg, index: Reg, src: Reg)

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
