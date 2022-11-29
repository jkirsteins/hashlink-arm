struct HLOpCode_CCompat : Equatable, Hashable {
    let op: Int32
    let p1: Int32
    let p2: Int32
    let p3: Int32
    let extra: UnsafePointer<Int32>?
    
    init(op: Int32, p1: Int32 = 0, p2: Int32 = 0, p3: Int32 = 0, extra: UnsafePointer<Int32>? = nil) {
        self.op = op
        self.p1 = p1
        self.p2 = p2
        self.p3 = p3
        self.extra = extra
    }
    
    init(op: HLOpCodeId, p1: Int32 = 0, p2: Int32 = 0, p3: Int32 = 0, extra: UnsafePointer<Int32>? = nil) {
        self.init(op: Int32(op.rawValue), p1: p1, p2: p2, p3: p3, extra: extra)
    }
    
    /// Useful in tests
    init(_ opc: HLOpCode, _ extra: inout UnsafeMutableBufferPointer<Int32>?) {
        switch(opc) {
        case .OMov(dst: let dst, src: let src):
            self = HLOpCode_CCompat(op: opc.id, p1: dst, p2: src)
        case .OInt(dst: let dst, ptr: let ptr):
            self = HLOpCode_CCompat(op: opc.id, p1: dst, p2: Int32(ptr), p3: 0, extra: nil)
        case .OFloat(dst: let dst, ptr: let ptr):
            self = HLOpCode_CCompat(op: opc.id, p1: dst, p2: Int32(ptr), p3: 0, extra: nil)
        case .OBool(dst: let dst, value: let value):
	    self = HLOpCode_CCompat(op: opc.id, p1: dst, p2: value, p3: 0, extra: nil)
        case .OBytes(dst: let dst, ptr: let ptr):
            self = HLOpCode_CCompat(op: opc.id, p1: dst, p2: Int32(ptr), p3: 0, extra: nil)
        case .OString(dst: let dst, ptr: let ptr):
            self = HLOpCode_CCompat(op: opc.id, p1: dst, p2: Int32(ptr), p3: 0, extra: nil)
        case .ONull(dst: let dst):
            self = HLOpCode_CCompat(op: opc.id, p1: dst)
        case .OAdd(dst: let dst, a: let a, b: let b):
            self = HLOpCode_CCompat(op: opc.id, p1: dst, p2: a, p3: b)
        case .OSub(dst: let dst, a: let a, b: let b):
            self = HLOpCode_CCompat(op: opc.id, p1: dst, p2: a, p3: b)
        case .OMul(dst: let dst, a: let a, b: let b):
            self = HLOpCode_CCompat(op: opc.id, p1: dst, p2: a, p3: b)
        case .OSDiv(dst: let dst, a: let a, b: let b):
            self = HLOpCode_CCompat(op: opc.id, p1: dst, p2: a, p3: b)
        case .OUDiv(dst: let dst, a: let a, b: let b):
            self = HLOpCode_CCompat(op: opc.id, p1: dst, p2: a, p3: b)
        case .OSMod(dst: let dst, a: let a, b: let b):
            self = HLOpCode_CCompat(op: opc.id, p1: dst, p2: a, p3: b)
        case .OUMod(dst: let dst, a: let a, b: let b):
            self = HLOpCode_CCompat(op: opc.id, p1: dst, p2: a, p3: b)
        case .OShl(dst: let dst, a: let a, b: let b):
            self = HLOpCode_CCompat(op: opc.id, p1: dst, p2: a, p3: b)
        case .OSShr(dst: let dst, a: let a, b: let b):
            self = HLOpCode_CCompat(op: opc.id, p1: dst, p2: a, p3: b)
        case .OUShr(dst: let dst, a: let a, b: let b):
            self = HLOpCode_CCompat(op: opc.id, p1: dst, p2: a, p3: b)
        case .OAnd(dst: let dst, a: let a, b: let b):
            self = HLOpCode_CCompat(op: opc.id, p1: dst, p2: a, p3: b)
        case .OOr(dst: let dst, a: let a, b: let b):
            self = HLOpCode_CCompat(op: opc.id, p1: dst, p2: a, p3: b)
        case .OXor(dst: let dst, a: let a, b: let b):
            self = HLOpCode_CCompat(op: opc.id, p1: dst, p2: a, p3: b)
        case .ONeg(dst: let dst, src: let src):
            self = HLOpCode_CCompat(op: opc.id, p1: dst, p2: src)
        case .ONot(dst: let dst, src: let src):
            self = HLOpCode_CCompat(op: opc.id, p1: dst, p2: src)
        case .OIncr(dst: let dst):
            self = HLOpCode_CCompat(op: opc.id, p1: dst)
        case .ODecr(dst: let dst):
            self = HLOpCode_CCompat(op: opc.id, p1: dst)
        case .OCall0(dst: let dst, fun: let fun):
            self = HLOpCode_CCompat(op: opc.id, p1: dst, p2: Int32(fun))
        case .OCall1(dst: let dst, fun: let fun, arg0: let arg0):
            self = HLOpCode_CCompat(op: opc.id, p1: dst, p2: Int32(fun), p3: arg0)
        case .OCall2(dst: let dst, fun: let fun, arg0: let arg0, arg1: let arg1):
            self = HLOpCode_CCompat(op: opc.id, p1: dst, p2: Int32(fun), p3: arg0, extra: .init(bitPattern: Int(arg1)))
        case .OCall3(dst: let dst, fun: let fun, arg0: let arg0, arg1: let arg1, arg2: let arg2):
            let args = [arg1, arg2]
            extra = .allocate(capacity: args.count)
            _ = extra!.initialize(from: args)
            self = HLOpCode_CCompat(op: opc.id, p1: dst, p2: Int32(fun), p3: arg0, extra: extra!.baseAddress!)
        case .OCall4(dst: let dst, fun: let fun, arg0: let arg0, arg1: let arg1, arg2: let arg2, arg3: let arg3):
            let args = [arg1, arg2, arg3]
            extra = .allocate(capacity: args.count)
            _ = extra!.initialize(from: args)
            self = HLOpCode_CCompat(op: opc.id, p1: dst, p2: Int32(fun), p3: arg0, extra: extra!.baseAddress!)
        case .OCallN(let dst, let fun, let args):
            extra = .allocate(capacity: args.count)
            _ = extra!.initialize(from: args)
            self = HLOpCode_CCompat(op: opc.id, p1: dst, p2: Int32(fun), p3: Int32(args.count), extra: extra!.baseAddress!)
        case .OCallMethod:
            fatalError("wip")
        case .OCallThis:
            fatalError("wip")
        case .OCallClosure:
            fatalError("wip")
        case .OStaticClosure:
            fatalError("wip")
        case .OInstanceClosure:
            fatalError("wip")
        case .OVirtualClosure(dst: let dst, obj: let obj, field: let field):
            self = HLOpCode_CCompat(op: dst, p1: obj, p2: field)
        case .OGetGlobal(dst: let dst, global: let global):
            self = HLOpCode_CCompat(op: opc.id, p1: dst, p2: Int32(global))
        case .OSetGlobal(global: let global, src: let src):
            self = HLOpCode_CCompat(op: opc.id, p1: Int32(global), p2: src)
        case .OField(dst: let dst, obj: let obj, field: let field):
            self = HLOpCode_CCompat(op: opc.id, p1: dst, p2: obj, p3: Int32(field))
        case .OSetField(obj: let obj, field: let field, src: let src):
            self = HLOpCode_CCompat(op: opc.id, p1: obj, p2: Int32(field), p3: src)
        case .OGetThis(dst: let dst, field: let field):
            self = HLOpCode_CCompat(op: opc.id, p1: dst, p2: Int32(field))
        case .OSetThis(field: let field, src: let src):
            self = HLOpCode_CCompat(op: opc.id, p1: Int32(field), p2: src)
        case .ODynGet(dst: let dst, obj: let obj, field: let field):
            self = HLOpCode_CCompat(op: opc.id, p1: dst, p2: obj, p3: Int32(field))
        case .ODynSet(obj: let obj, field: let field, src: let src):
            self = HLOpCode_CCompat(op: opc.id, p1: obj, p2: Int32(field), p3: src)
        case .OJTrue(cond: let cond, offset: let offset):
            self = HLOpCode_CCompat(op: opc.id, p1: cond, p2: offset)
        case .OJFalse(cond: let cond, offset: let offset):
            self = HLOpCode_CCompat(op: opc.id, p1: cond, p2: offset)
        case .OJNull(reg: let reg, offset: let offset):
            self = HLOpCode_CCompat(op: opc.id, p1: reg, p2: offset)
        case .OJNotNull(reg: let reg, offset: let offset):
            self = HLOpCode_CCompat(op: opc.id, p1: reg, p2: offset)
        case .OJSLt(a: let a, b: let b, offset: let offset):
            self = HLOpCode_CCompat(op: opc.id, p1: a, p2: b, p3: offset)
        case .OJSGte(a: let a, b: let b, offset: let offset):
            self = HLOpCode_CCompat(op: opc.id, p1: a, p2: b, p3: offset)
        case .OJSGt(a: let a, b: let b, offset: let offset):
            self = HLOpCode_CCompat(op: opc.id, p1: a, p2: b, p3: offset)
        case .OJSLte(a: let a, b: let b, offset: let offset):
            self = HLOpCode_CCompat(op: opc.id, p1: a, p2: b, p3: offset)
        case .OJULt(a: let a, b: let b, offset: let offset):
            self = HLOpCode_CCompat(op: opc.id, p1: a, p2: b, p3: offset)
        case .OJUGte(a: let a, b: let b, offset: let offset):
            self = HLOpCode_CCompat(op: opc.id, p1: a, p2: b, p3: offset)
        case .OJNotLt(a: let a, b: let b, offset: let offset):
            self = HLOpCode_CCompat(op: opc.id, p1: a, p2: b, p3: offset)
        case .OJNotGte(a: let a, b: let b, offset: let offset):
            self = HLOpCode_CCompat(op: opc.id, p1: a, p2: b, p3: offset)
        case .OJEq(a: let a, b: let b, offset: let offset):
            self = HLOpCode_CCompat(op: opc.id, p1: a, p2: b, p3: offset)
        case .OJNotEq(a: let a, b: let b, offset: let offset):
            self = HLOpCode_CCompat(op: opc.id, p1: a, p2: b, p3: offset)
        case .OJAlways(offset: let offset):
            self = HLOpCode_CCompat(op: opc.id, p1: offset)
        case .OToDyn(dst: let dst, src: let src):
            self = HLOpCode_CCompat(op: opc.id, p1: dst, p2: src)
        case .OToSFloat(dst: let dst, src: let src):
            self = HLOpCode_CCompat(op: opc.id, p1: dst, p2: src)
        case .OToUFloat(dst: let dst, src: let src):
            self = HLOpCode_CCompat(op: opc.id, p1: dst, p2: src)
        case .OToInt(dst: let dst, src: let src):
            self = HLOpCode_CCompat(op: opc.id, p1: dst, p2: src)
        case .OSafeCast(dst: let dst, src: let src):
            self = HLOpCode_CCompat(op: opc.id, p1: dst, p2: src)
        case .OUnsafeCast(dst: let dst, src: let src):
            self = HLOpCode_CCompat(op: opc.id, p1: dst, p2: src)
        case .OToVirtual(dst: let dst, src: let src):
            self = HLOpCode_CCompat(op: opc.id, p1: dst, p2: src)
        case .OLabel:
            self = HLOpCode_CCompat(op: opc.id)
        case .ORet(ret: let ret):
            self = HLOpCode_CCompat(op: opc.id, p1: ret)
        case .OThrow(exc: let exc):
            self = HLOpCode_CCompat(op: opc.id, p1: exc)
        case .ORethrow(exc: let exc):
            self = HLOpCode_CCompat(op: opc.id, p1: exc)
        case .OSwitch(let reg, let offsets, let end):
            extra = .allocate(capacity: offsets.count)
            _ = extra!.initialize(from: offsets)
            self = HLOpCode_CCompat(op: opc.id, p1: reg, p2: Int32(offsets.count), p3: end, extra: extra!.baseAddress!)
        case .ONullCheck(reg: let reg):
            self = HLOpCode_CCompat(op: opc.id, p1: reg)
        case .OTrap(exc: let exc, offset: let offset):
            self = HLOpCode_CCompat(op: opc.id, p1: exc, p2: offset)
        case .OEndTrap(exc: let exc):
            self = HLOpCode_CCompat(op: opc.id, p1: exc)
        case .OGetI8(dst: let dst, bytes: let bytes, index: let index):
            self = HLOpCode_CCompat(op: opc.id, p1: dst, p2: bytes, p3: index)
        case .OGetI16(dst: let dst, bytes: let bytes, index: let index):
            self = HLOpCode_CCompat(op: opc.id, p1: dst, p2: bytes, p3: index)
        case .OGetMem(dst: let dst, bytes: let bytes, index: let index):
            self = HLOpCode_CCompat(op: opc.id, p1: dst, p2: bytes, p3: index)
        case .OGetArray(dst: let dst, array: let array, index: let index):
            self = HLOpCode_CCompat(op: opc.id, p1: dst, p2: array, p3: index)
        case .OSetI8(bytes: let bytes, index: let index, src: let src):
            self = HLOpCode_CCompat(op: opc.id, p1: bytes, p2: index, p3: src)
        case .OSetI16(bytes: let bytes, index: let index, src: let src):
            self = HLOpCode_CCompat(op: opc.id, p1: bytes, p2: index, p3: src)
        case .OSetMem(bytes: let bytes, index: let index, src: let src):
            self = HLOpCode_CCompat(op: opc.id, p1: bytes, p2: index, p3: src)
        case .OSetArray(array: let array, index: let index, src: let src):
            self = HLOpCode_CCompat(op: opc.id, p1: array, p2: index, p3: src)
        case .ONew(dst: let dst):
            self = HLOpCode_CCompat(op: opc.id, p1: dst)
        case .OArraySize(dst: let dst, array: let array):
            self = HLOpCode_CCompat(op: opc.id, p1: dst, p2: array)
        case .OType(dst: let dst, ty: let ty):
            self = HLOpCode_CCompat(op: opc.id, p1: dst, p2: Int32(ty))
        case .OGetType(dst: let dst, src: let src):
            self = HLOpCode_CCompat(op: opc.id, p1: dst, p2: src)
        case .OGetTID(dst: let dst, src: let src):
            self = HLOpCode_CCompat(op: opc.id, p1: dst, p2: src)
        case .ORef(dst: let dst, src: let src):
            self = HLOpCode_CCompat(op: opc.id, p1: dst, p2: src)
        case .OUnref(dst: let dst, src: let src):
            self = HLOpCode_CCompat(op: opc.id, p1: dst, p2: src)
        case .OSetref(dst: let dst, value: let value):
            self = HLOpCode_CCompat(op: opc.id, p1: dst, p2: value, p3: 0, extra: nil)
        case .OMakeEnum:
            fatalError("wip")
        case .OEnumAlloc(dst: let dst, construct: let construct):
            self = HLOpCode_CCompat(op: opc.id, p1: dst, p2: Int32(construct))
        case .OEnumIndex(dst: let dst, value: let value):
            self = HLOpCode_CCompat(op: opc.id, p1: dst, p2: value, p3: 0, extra: nil)
        case .OEnumField(dst: let dst, value: let value, construct: let construct, field: let field):
            self = HLOpCode_CCompat(op: opc.id, p1: dst, p2: value, p3: Int32(construct), extra: .init(bitPattern: field))
        case .OSetEnumField(value: let value, field: let field, src: let src):
            self = HLOpCode_CCompat(op: opc.id, p1: value, p2: Int32(field), p3: src)
        case .OAssert:
            self = HLOpCode_CCompat(op: opc.id)
        case .ORefData(dst: let dst, src: let src):
            self = HLOpCode_CCompat(op: opc.id, p1: dst, p2: src)
        case .ORefOffset(dst: let dst, reg: let reg, offset: let offset):
            self = HLOpCode_CCompat(op: opc.id, p1: dst, p2: reg, p3: offset)
        case .ONop:
            self = HLOpCode_CCompat(op: opc.id)
        }
    }
}

extension HLOpCode {
    static func parseCCompat(_ cop: HLOpCode_CCompat) -> HLOpCode {
        guard let opId = HLOpCodeId(rawValue: UInt32(cop.op)) else {
            fatalError("Unknown op id: \(cop.op)")
        }
        
        switch(opId) {
        case .OMov:
            return .OMov(dst: cop.p1, src: cop.p2)
        case .OInt:
            return .OInt(dst: cop.p1, ptr: RefInt(cop.p2))
        case .OGetThis:
            return .OGetThis(dst: cop.p1, field: RefField(cop.p2))
        case .ONew:
            return .ONew(dst: cop.p1)
        case .OSetField:
            return .OSetField(obj: cop.p1, field: RefField(cop.p2), src: cop.p3)
        case .ORet:
            return .ORet(ret: cop.p1)
        case .OJULt:
            return .OJULt(a: cop.p1, b: cop.p2, offset: cop.p3)
        case .OJSLt:
            return .OJSLt(a: cop.p1, b: cop.p2, offset: cop.p3)
        case .OJSLte:
            return .OJSLte(a: cop.p1, b: cop.p2, offset: cop.p3)
        case .OJSGt:
            return .OJSGt(a: cop.p1, b: cop.p2, offset: cop.p3)
        case .OJSGte:
            return .OJSGte(a: cop.p1, b: cop.p2, offset: cop.p3)
        case .OJNull:
            return .OJNull(reg: cop.p1, offset: cop.p2)
        case .OJNotNull:
            return .OJNotNull(reg: cop.p1, offset: cop.p2)
        case .OGetGlobal:
            return .OGetGlobal(dst: cop.p1, global: RefGlobal(cop.p2))
        case .OCall0:
            return .OCall0(dst: cop.p1, fun: RefFun(cop.p2))
        case .OCall1:
            return .OCall1(dst: cop.p1, fun: RefFun(cop.p2), arg0: cop.p3)
        case .OCall2:
            let arg1: Reg
            if let extra = cop.extra {
                arg1 = Reg(Int(bitPattern: extra))
            } else {
                arg1 = 0
            }
            return .OCall2(dst: cop.p1, fun: RefFun(cop.p2), arg0: cop.p3, arg1: arg1)
        case .OCall3:
            guard let extra = cop.extra else { fatalError("OCall3 missing extra") }
            return .OCall3(dst: cop.p1, fun: RefFun(cop.p2), arg0: cop.p3, arg1: extra.pointee, arg2: extra.advanced(by: 1).pointee)
        case .OCall4:
            guard let extra = cop.extra else { fatalError("OCall4 missing extra") }
            return .OCall4(
                dst: cop.p1,
                fun: RefFun(cop.p2),
                arg0: cop.p3,
                arg1: extra.pointee,
                arg2: extra.advanced(by: 1).pointee,
                arg3: extra.advanced(by: 2).pointee)
        case .OShl:
            return .OShl(dst: cop.p1, a: cop.p2, b: cop.p3)
        case .OGetI16:
            return .OGetI16(dst: cop.p1, bytes: cop.p2, index: cop.p3)
        case .OSetI16:
            return .OSetI16(bytes: cop.p1, index: cop.p2, src: cop.p3)
        case .OGetI8:
            return .OGetI8(dst: cop.p1, bytes: cop.p2, index: cop.p3)
        case .OSetI8:
            return .OSetI8(bytes: cop.p1, index: cop.p2, src: cop.p3)
        case .ONullCheck:
            return .ONullCheck(reg: cop.p1)
        case .OField:
            return .OField(dst: cop.p1, obj: cop.p2, field: RefField(cop.p3))
        case .OSetThis:
            return .OSetThis(field: RefField(cop.p1), src: cop.p2)
        case .OAnd:
            return .OAnd(dst: cop.p1, a: cop.p2, b: cop.p3)
        case .OJEq:
            return .OJEq(a: cop.p1, b: cop.p2, offset: cop.p3)
        case .OJNotEq:
            return .OJNotEq(a: cop.p1, b: cop.p2, offset: cop.p3)
        case .OThrow:
            return .OThrow(exc: cop.p1)
        case .OTrap:
            return .OTrap(exc: cop.p1, offset: cop.p2)
        case .OEndTrap:
            return .OEndTrap(exc: cop.p1)
        case .OSShr:
            return .OSShr(dst: cop.p1, a: cop.p2, b: cop.p3)
        case .OLabel:
            return .OLabel
        case .OIncr:
            return .OIncr(dst: cop.p1)
        case .OMul:
            return .OMul(dst: cop.p1, a: cop.p2, b: cop.p3)
        case .OAdd:
            return .OAdd(dst: cop.p1, a: cop.p2, b: cop.p3)
        case .OSafeCast:
            return .OSafeCast(dst: cop.p1, src: cop.p2)
        case .OToDyn:
            return .OToDyn(dst: cop.p1, src: cop.p2)
        case .OOr:
            return .OOr(dst: cop.p1, a: cop.p2, b: cop.p3)
        case .OJAlways:
            return .OJAlways(offset: cop.p1)
        case .ONull:
            return .ONull(dst: cop.p1)
        case .OBool:
            return .OBool(dst: cop.p1, value: cop.p2)
        case .OJFalse:
            return .OJFalse(cond: cop.p1, offset: cop.p2)
        case .OJTrue:
            return .OJTrue(cond: cop.p1, offset: cop.p2)
        case .OSub:
            return .OSub(dst: cop.p1, a: cop.p2, b: cop.p3)
        case .OCallN:
            let c = cop.p3
            guard let extra = cop.extra else {
                fatalError("OCallN missing extra")
            }
            let args = (0..<c).map {
                Reg(extra.advanced(by: Int($0)).pointee)
            }
            return .OCallN(dst: cop.p1, fun: RefFun(cop.p2), args: args)
        case .OArraySize:
            return .OArraySize(dst: cop.p1, array: cop.p2)
        case .OGetArray:
            return .OGetArray(dst: cop.p1, array: cop.p2, index: cop.p3)
        case .OSetArray:
            return .OSetArray(array: cop.p1, index: cop.p2, src: cop.p3)
        case .OType:
            return .OType(dst: cop.p1, ty: RefType(cop.p2))
        case .OXor:
            return .OXor(dst: cop.p1, a: cop.p2, b: cop.p3)
        case .OToInt:
            return .OToInt(dst: cop.p1, src: cop.p2)
        case .OFloat:
            return .OFloat(dst: cop.p1, ptr: RefFloat(cop.p2))
        case .OBytes:
            return .OBytes(dst: cop.p1, ptr: RefBytes(cop.p2))
        case .OString:
            return .OString(dst: cop.p1, ptr: RefString(cop.p2))
        case .OSDiv:
            return .OSDiv(dst: cop.p1, a: cop.p2, b: cop.p3)
        case .OUDiv:
            return .OUDiv(dst: cop.p1, a: cop.p2, b: cop.p3)
        case .OSMod:
            return .OSMod(dst: cop.p1, a: cop.p2, b: cop.p3)
        case .OUMod:
            return .OUMod(dst: cop.p1, a: cop.p2, b: cop.p3)
        case .OUShr:
            return .OUShr(dst: cop.p1, a: cop.p2, b: cop.p3)
        case .ONeg:
            return .ONeg(dst: cop.p1, src: cop.p2)
        case .ONot:
            return .ONot(dst: cop.p1, src: cop.p2)
        case .ODecr:
            return .ODecr(dst: cop.p1)
        case .OCallMethod:
            guard let extra = cop.extra else {
                fatalError("OCallClosure missing extra")
            }
            
            let argc = cop.p3
            let extraValues = UnsafeMutableBufferPointer(start: .init(mutating: extra), count: Int(argc))
            
            let obj: Reg = extra.pointee.advanced(by: 0)
            let proto: RefField = RefField(cop.p2)
            let dst: Reg = cop.p1
            let args = Array(extraValues.dropFirst())
            
            return .OCallMethod(dst: dst, obj: obj, proto: proto, args: args)
        case .OCallThis:
            fatalError("wip")
        case .OCallClosure:
            let c = cop.p3
            guard let extra = cop.extra else {
                fatalError("OCallClosure missing extra")
            }
            let args = (0..<c).map {
                Reg(extra.advanced(by: Int($0)).pointee)
            }
            return .OCallClosure(dst: cop.p1, closure: cop.p2, args: args)
        case .OStaticClosure:
            return .OStaticClosure(dst: cop.p1, fun: RefFun(cop.p2))
        case .OInstanceClosure:
            return .OInstanceClosure(dst: cop.p1, fun: RefFun(cop.p2), obj: cop.p3)
        case .OVirtualClosure:
            return .OVirtualClosure(dst: cop.p1, obj: cop.p2, field: cop.p3)
        case .OSetGlobal:
            return .OSetGlobal(global: RefGlobal(cop.p1), src: cop.p2)
        case .ODynGet:
            return .ODynGet(dst: cop.p1, obj: cop.p2, field: RefString(cop.p3))
        case .ODynSet:
            return .ODynSet(obj: cop.p1, field: RefString(cop.p2), src: cop.p3)
        case .OJUGte:
            return .OJUGte(a: cop.p1, b: cop.p2, offset: cop.p3)
        case .OJNotLt:
            return .OJNotLt(a: cop.p1, b: cop.p2, offset: cop.p3)
        case .OJNotGte:
            return .OJNotGte(a: cop.p1, b: cop.p2, offset: cop.p3)
        case .OToSFloat:
            return .OToSFloat(dst: cop.p1, src: cop.p2)
        case .OToUFloat:
            return .OToUFloat(dst: cop.p1, src: cop.p2)
        case .OUnsafeCast:
            return .OUnsafeCast(dst: cop.p1, src: cop.p2)
        case .OToVirtual:
            return .OToVirtual(dst: cop.p1, src: cop.p2)
        case .ORethrow:
            return .ORethrow(exc: cop.p1)
        case .OSwitch:
            let offsetCount = cop.p2
            guard let extra = cop.extra else {
                fatalError("OSwitch missing extra")
            }
            let offsets = (0..<offsetCount).map {
                Reg(extra.advanced(by: Int($0)).pointee)
            }
            return .OSwitch(reg: cop.p1, offsets: offsets, end: cop.p3)
        case .OGetMem:
            return .OGetMem(dst: cop.p1, bytes: cop.p2, index: cop.p3)
        case .OSetMem:
            return .OSetMem(bytes: cop.p1, index: cop.p2, src: cop.p3)
        case .OGetType:
            return .OGetType(dst: cop.p1, src: cop.p2)
        case .OGetTID:
            return .OGetTID(dst: cop.p1, src: cop.p2)
        case .ORef:
            return .ORef(dst: cop.p1, src: cop.p2)
        case .OUnref:
            return .OUnref(dst: cop.p1, src: cop.p2)
        case .OSetref:
            return .OSetref(dst: cop.p1, value: cop.p2)
        case .OMakeEnum:
            let c = cop.p3
            guard let extra = cop.extra else {
                fatalError("OCallN missing extra")
            }
            let args = (0..<c).map {
                Reg(extra.advanced(by: Int($0)).pointee)
            }
            return .OMakeEnum(dst: cop.p1, construct: RefEnumConstruct(cop.p2), args: args)
        case .OEnumAlloc:
            return .OEnumAlloc(dst: cop.p1, construct: RefEnumConstruct(cop.p2))
        case .OEnumIndex:
            return .OEnumIndex(dst: cop.p1, value: cop.p2)
        case .OEnumField:
            let field: RefField
            if let extra = cop.extra {
                field = RefField(Int(bitPattern: extra))
            } else {
                field = 0
            }
            return .OEnumField(dst: cop.p1, value: cop.p2, construct: RefEnumConstruct(cop.p3), field: field)
        case .OSetEnumField:
            return .OSetEnumField(value: cop.p1, field: RefField(cop.p2), src: cop.p3)
        case .OAssert:
            return .OAssert
        case .ORefData:
            return .ORefData(dst: cop.p1, src: cop.p2)
        case .ORefOffset:
            return .ORefOffset(dst: cop.p1, reg: cop.p2, offset: cop.p3)
        case .ONop:
            return .ONop
        }
    }
}
