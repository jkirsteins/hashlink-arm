extension HLOpCode: CustomDebugStringConvertible {
    var debugDescription: String {
        "\(id): \(debugDescriptionInternal)"
    }

    fileprivate var debugDescriptionInternal: String {
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

        case .OCallMethod(let dst, let field, let args):
            return "reg\(dst) = <\(field)>(\(args))"
        case .OCallThis(let dst, let field, let args): 
            return "reg\(dst) = this.<\(field)>(\(args))"
        case .OCallClosure(let dst, let closure, let args):
            return "reg\(dst) = closure@\(closure)(\(args))"
        case .OStaticClosure(let dst, let fun): 
            return "staticclosure(dst \(dst) fun \(fun))"
        case .OInstanceClosure(let dst, let fun, let obj):
            return "instanceclosure(dst \(dst) fun \(fun) obj \(obj))"
        case .OVirtualClosure(let dst, let obj, let field):
            return "instanceclosure(dst \(dst) obj \(obj) field \(field))"
        case .OGetGlobal(let dst, let global): 
            return "reg\(dst) = global\(global)"
        case .OSetGlobal(let global, let src): 
            return "global\(global) = reg\(src)"
        case .OField(let dst, let obj, let field): 
            return "reg\(dst) = reg\(obj).<\(field)>"
        case .OSetField(let obj, let field, let src):
            return "reg\(obj).<\(field)> = reg\(src)"
        case .OGetThis(let dst, let field): return "reg\(dst) = this.<\(field)>"
        case .OSetThis(let field, let src): return "this.<\(field)> = reg\(src)"
        case .ODynGet(let dst, let obj, let field): 
            return "reg\(dst) = obj\(obj).<\(field)>"
        case .ODynSet(let obj, let field, let src): 
            return "obj\(obj).<\(field)> = reg\(src)"
        /// Jump by an offset if the condition is true
        case .OJTrue(let cond, let offset): 
            return "if reg\(cond) == true jump to \(offset)"
        /// Jump by an offset if the condition is false
        case .OJFalse(let cond, let offset): 
            return "if reg\(cond) == false jump to \(offset)"
        case .OJNull(let reg, let offset): 
            return "if reg\(reg) == null jump to \(offset)"
        case .OJNotNull(let reg, let offset): 
            return "if reg\(reg) != null jump to \(offset)"
        case .OJSLt(let a, let b, let offset): 
            return "if s(reg\(a)) < s(reg\(b)) jump to \(offset)"
        case .OJSGte(let a, let b, let offset): 
            return "if s(reg\(a)) >= s(reg\(b)) jump to \(offset)"
        case .OJSGt(let a, let b, let offset): 
            return "if s(reg\(a)) > s(reg\(b)) jump to \(offset)"
        case .OJSLte(let a, let b, let offset): 
            return "if s(reg\(a)) <= s(reg\(b)) jump to \(offset)"
        case .OJULt(let a, let b, let offset): 
            return "if u(reg\(a)) < u(reg\(b)) jump to \(offset)"
        case .OJUGte(let a, let b, let offset): 
            return "if u(reg\(a)) >= u(reg\(b)) jump to \(offset)"
        case .OJNotLt(let a, let b, let offset): 
            return "if reg\(a) >= reg\(b) jump to \(offset)"
        case .OJNotGte(let a, let b, let offset): 
            return "if reg\(a) < reg\(b) jump to \(offset)"
        case .OJEq(let a, let b, let offset): 
            return "if reg\(a) == reg\(b) jump to \(offset)"
        case .OJNotEq(let a, let b, let offset): 
            return "if reg\(a) != reg\(b) jump to \(offset)"
        case .OJAlways(let offset): 
            return "jump to \(offset)"

        case .OToDyn(let dst, let src): 
            return "reg\(dst) = cast<dyn> reg\(src)"
        case .OToSFloat(let dst, let src): 
            return "reg\(dst) = cast<sfloat> reg\(src)"
        case .OToUFloat(let dst, let src): 
            return "reg\(dst) = cast<ufloat> reg\(src)"
        case .OToInt(let dst, let src):
            return "reg\(dst) = cast<int> reg\(src)"
        case .OSafeCast(let dst, let src): 
            return "reg\(dst) = safe cast reg\(src)"
        case .OUnsafeCast(let dst, let src): 
            return "reg\(dst) = unsafe cast reg\(src)"
        case .OToVirtual(let dst, let src): 
            return "reg\(dst) = cast<virt> reg\(src)"

        case .OLabel: return "label"
        case .ORet(let ret): return "ret reg\(ret)"
        case .OThrow(let exc): 
            return "throw \(exc)"
        case .ORethrow(let exc): 
            return "rethrow \(exc)"
        case .OSwitch(let reg, let offsets, let end):
            return "switch(reg\(reg), offsets \(offsets), end \(end))"
        case .ONullCheck(let reg):
            return "if reg\(reg) == null throw exc"
        case .OTrap(let exc, let offset): 
            return "try reg\(exc) jump to \(offset)"
        case .OEndTrap(let exc):
            return "catch reg\(exc)"
        case .OGetI8(let dst, let bytes, let index): 
            return "geti8(reg\(dst), reg\(bytes), index \(index))"
        case .OGetI16(let dst, let bytes, let index): 
            return "geti16(reg\(dst), reg\(bytes), index \(index))"
        case .OGetMem(let dst, let bytes, let index): 
            return "getmem(reg\(dst), reg\(bytes), index \(index))"
        case .OGetArray(let dst, let array, let index): 
            return "getarray(reg\(dst), array\(array), index \(index))"
        case .OSetI8(let bytes, let index, let src): 
            return "seti8(reg\(bytes), index \(index), src\(src))"
        case .OSetI16(let bytes, let index, let src): 
            return "seti16(reg\(bytes), index \(index), src\(src))"
        case .OSetMem(let bytes, let index, let src): 
            return "setmem(reg\(bytes), index \(index), src\(src))"
        case .OSetArray(let bytes, let index, let array): 
            return "setarray(reg\(bytes), index \(index), array\(array))"

        case .ONew(let dst): return "reg\(dst) = new"
        case .OArraySize(let dst, let array): 
            return "arraysize(dst \(dst), array \(array))"
        case .OType(let dst, let ty): 
            return "type(dst \(dst), ty \(ty))"
        case .OGetType(let dst, let src): 
            return "gettype(dst \(dst), src \(src))"
        case .OGetTID(let dst, let src): 
            return "gettid(dst \(dst), src \(src))"

        case .ORef(let dst, let src): return "reg\(dst) = &reg\(src)"
        case .OUnref(let dst, let src): return "reg\(dst) = *reg\(src)"
        case .OSetref(let dst, let value): 
            return "setref(dst \(dst) value \(value))"

        /// Allocate and initialize an enum variant
        case .OMakeEnum(let dst, let construct, let args): 
            return "makeenum(dst \(dst), construct \(construct), args \(args))"
        case .OEnumAlloc(let dst, let construct): 
            return "enumalloc(dst \(dst), construct \(construct))"
        case .OEnumIndex(let dst, let value): 
            return "enumindex(dst \(dst), value \(value))"
        case .OEnumField(let dst, let value, let construct, let field):
            return "enumfield(dst \(dst), value \(value), construct \(construct), field \(field))"
        case .OSetEnumField(let value, let field, let src): 
            return "setenumfield(value \(value), field \(field), src \(src))"
        case .OAssert: return "assert"
        case .ORefData(let dst, let src): 
            return "refdata(dst\(dst), src\(src))"
        case .ORefOffset(let dst, let reg, let offset): 
            return "refoffset(dst\(dst), reg\(reg), offset\(offset))"
        case .ONop: return "nop"
        }
    }
}
