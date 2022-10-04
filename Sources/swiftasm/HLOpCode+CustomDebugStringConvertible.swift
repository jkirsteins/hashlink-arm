extension HLOpCode: CustomDebugStringConvertible {
    var debugDescription: String {
        switch self {
        case .OMov(let dst, let src): return "reg\(dst) = reg\(src)"
        case .OInt(let dst, let ptr): return "reg\(dst) = i \(ptr)"
        case .OFloat(let dst, let ptr): return "reg\(dst) = f \(ptr)"
        case .OBool(let dst, let value): return "reg\(dst) = bool \(value)"
        case .OBytes(let dst, let ptr): return "reg\(dst) = bytes \(ptr)"
        case .OString(let dst, let ptr): return "reg\(dst) = str \(ptr)"
        case .ONull(let dst): return "reg\(dst) = null"
        case .OAdd(let dst, let a, let b): return "reg\(dst) = reg\(a) + reg\(b)"
        case .OSub(let dst, let a, let b): return "reg\(dst) = reg\(a) - reg\(b)"
        case .OMul(let dst, let a, let b): return "reg\(dst) = reg\(a) * reg\(b)"
        case .OSDiv(let dst, let a, let b): return "reg\(dst) = s(reg\(a)) / s(reg\(b))"
        case .OUDiv(let dst, let a, let b): return "reg\(dst) = u(reg\(a)) / u(reg\(b))"
        case .OSMod(let dst, let a, let b): return "reg\(dst) = s(reg\(a)) % s(reg\(b))"
        case .OUMod(let dst, let a, let b): return "reg\(dst) = u(reg\(a)) % u(reg\(b))"
        case .OShl(let dst, let a, let b): return "reg\(dst) = reg\(a) << reg\(b)"
        case .OSShr(let dst, let a, let b):
            return "reg\(dst) = s(reg\(a)) >> s(reg\(b))"
        case .OUShr(let dst, let a, let b):
            return "reg\(dst) = u(reg\(a)) >> u(reg\(b))"
        case .OAnd(let dst, let a, let b): return "reg\(dst) = reg\(a) & reg\(b)"
        case .OOr(let dst, let a, let b): return "reg\(dst) = reg\(a) | reg\(b)"
        case .OXor(let dst, let a, let b): return "reg\(dst) = reg\(a) ^ reg\(b)"
        case .ONeg(let dst, let src): return "reg\(dst) = -reg\(src)"
        case .ONot(let dst, let src): return "reg\(dst) = !reg\(src)"
        case .OIncr(let dst): return "reg\(dst)++"
        case .ODecr(let dst): return "reg\(dst)--"

        case .OCall0(let dst, let fun): return "reg\(dst) = fun@\(fun)()"
        case .OCall1(let dst, let fun, let arg0):
            return "reg\(dst) = fun@\(fun)(reg\(arg0))"

        case .OCall2(let dst, let fun, let arg0, let arg1):
            return "reg\(dst) = fun@\(fun)(reg\(arg0), reg\(arg1))"
        case .OCall3(let dst, let fun, let arg0, let arg1, let arg2):
            return "reg\(dst) = fun@\(fun)(reg\(arg0), reg\(arg1), reg\(arg2))"
        case .OCall4(let dst, let fun, let arg0, let arg1, let arg2, let arg3):
            return
                "reg\(dst) = fun@\(fun)(reg\(arg0), reg\(arg1), reg\(arg2), reg\(arg3))"
        case .OCallN(let dst, let fun, let args):
            return "reg\(dst) = fun@\(fun)(...<\(args.count) args>)"

        case .OCallMethod(let dst, let field, let args): fatalError("OCallMethod")
        case .OCallThis(let dst, let field, let args): fatalError("OCallThis")
        case .OCallClosure(let dst, let fun, let args): 
            return "reg\(dst) = reg\(fun)(<\(args.count)> args)"
        case .OStaticClosure(let dst, let fun): fatalError("OStaticClosure")
        case .OInstanceClosure(let dst, let fun, let obj):
            fatalError("OInstanceClosure")
        case .OVirtualClosure(let dst, let obj, let field):
            fatalError("OVirtualClosure")
        case .OGetGlobal(let dst, let global): fatalError("OGetGlobal")
        case .OSetGlobal(let global, let src): fatalError("OSetGlobal")
        case .OField(let dst, let obj, let field): 
            return "reg\(dst) = reg\(obj).<\(field)>"
        case .OSetField(let obj, let field, let src):
            return "reg\(obj).<\(field)> = reg\(src)"
            fatalError("OSetField")
        case .OGetThis(let dst, let field): return "reg\(dst) = this.<\(field)>"
        case .OSetThis(let field, let src): return "this.<\(field)> = reg\(src)"
        case .ODynGet(let dst, let obj, let field): fatalError("ODynGet")
        case .ODynSet(let obj, let field, let src): fatalError("ODynSet")
        /// Jump by an offset if the condition is true
        case .OJTrue(let cond, let offset): fatalError("OJTrue")
        /// Jump by an offset if the condition is false
        case .OJFalse(let cond, let offset): fatalError("OJFalse")
        case .OJNull(let reg, let offset): fatalError("OJNull")
        case .OJNotNull(let reg, let offset): fatalError("OJNotNull")
        case .OJSLt(let a, let b, let offset): fatalError("OJSLt")
        case .OJSGte(let a, let b, let offset): fatalError("OJSGte")
        case .OJSGt(let a, let b, let offset): fatalError("OJSGt")
        case .OJSLte(let a, let b, let offset): fatalError("OJSLte")
        case .OJULt(let a, let b, let offset): fatalError("OJULt")
        case .OJUGte(let a, let b, let offset): fatalError("OJUGte")
        case .OJNotLt(let a, let b, let offset): fatalError("OJNotLt")
        case .OJNotGte(let a, let b, let offset): fatalError("OJNotGte")
        case .OJEq(let a, let b, let offset): fatalError("OJEq")
        case .OJNotEq(let a, let b, let offset): fatalError("OJNotEq")
        case .OJAlways(let offset): fatalError("OJAlways")

        case .OToDyn(let dst, let src): fatalError("OToDyn")
        case .OToSFloat(let dst, let src): fatalError("OToSFloat")
        case .OToUFloat(let dst, let src): fatalError("OToUFloat")
        case .OToInt(let dst, let src): fatalError("OToInt")
        case .OSafeCast(let dst, let src): fatalError("OSafeCast")
        case .OUnsafeCast(let dst, let src): fatalError("OUnsafeCast")
        case .OToVirtual(let dst, let src): fatalError("OToVirtual")

        case .OLabel: return "label"
        case .ORet(let ret): return "ret reg\(ret)"
        case .OThrow(let exc): fatalError("OThrow")
        case .ORethrow(let exc): fatalError("ORethrow")
        case .OSwitch(let reg, let offsets, let end): fatalError("OSwitch")
        case .ONullCheck(let reg):
            return "if reg\(reg) == null throw exc"
        case .OTrap(let exc, let offset): fatalError("OTrap")
        case .OEndTrap(let exc): fatalError("OEndTrap")

        case .OGetI8(let dst, let bytes, let index): fatalError("OGetI8")
        case .OGetI16(let dst, let bytes, let index): fatalError("OGetI16")
        case .OGetMem(let dst, let bytes, let index): fatalError("OGetMem")
        case .OGetArray(let dst, let array, let index): fatalError("OGetArray")
        case .OSetI8(let bytes, let index, let src): fatalError("OSetI8")
        case .OSetI16(let bytes, let index, let src): fatalError("OSetI16")
        case .OSetMem(let bytes, let index, let src): fatalError("OSetMem")
        case .OSetArray(let bytes, let index, let src): fatalError("OSetArray")

        case .ONew(let dst): return "reg\(dst) = new"
        case .OArraySize(let dst, let array): fatalError("OArraySize")
        case .OType(let dst, let ty): fatalError("OType")
        case .OGetType(let dst, let src): fatalError("OGetType")
        case .OGetTID(let dst, let src): fatalError("OGetTID")

        case .ORef(let dst, let src): return "reg\(dst) = &reg\(src)"
        case .OUnref(let dst, let src): fatalError("OUnref")
        case .OSetref(let dst, let value): fatalError("OSetref")

        /// Allocate and initialize an enum variant
        case .OMakeEnum(let dst, let construct, let args): fatalError("OMakeEnum")
        case .OEnumAlloc(let dst, let construct): fatalError("OEnumAlloc")
        case .OEnumIndex(let dst, let value): fatalError("OEnumIndex")
        case .OEnumField(let dst, let value, let construct, let field):
            fatalError("OEnumField")
        case .OSetEnumField(let value, let field, let src): fatalError("OSetEnumField")

        case .OAssert: return "assert"
        case .ORefData(let dst, let src): fatalError("ORefData")
        case .ORefOffset(let dst, let reg, let offset): fatalError("ORefOffset")
        case .ONop: return "nop"
        }
    }
}
