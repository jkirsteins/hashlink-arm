//
//  File.swift
//  
//
//  Created by Janis Kirsteins on 22/10/2022.
//

import Foundation

//protocol OpProvider {
//    func getOps() -> [HLOpCode]
//}

protocol NativeCallable : Callable {
    
}

extension HLNative_CCompat : IncompleteCallable {
    var args: [Resolvable<HLType>] {
        (0..<self.typePtr.pointee.fun.pointee.nargs).map { ix in
            let ptrPtr = self.typePtr.pointee.fun.pointee.argsPtr.advanced(by: Int(ix))
            
            return Resolvable(unsafeType: ptrPtr.pointee)
        }
    }
    
    var ret: Resolvable<HLType> {
        Resolvable(HLType(self.type.fun.pointee.ret), memory: self.type.fun.pointee.retPtr)
    }
}

struct HLNative_CCompat__Resolved : NativeCallable {
    let ptr: UnsafePointer<HLNative_CCompat>
    let entrypoint: any MemoryAddress
    
    var args: [Resolvable<HLType>] { self.ptr.pointee.args }
    var ret: Resolvable<HLType> { self.ptr.pointee.ret }
}

extension HLType {
    var _fun_ret: Resolvable<HLType> {
        switch(self) {
        case .fun(let funData): return funData.ret
        default: fatalError("_fun_ret only defined for .fun (got \(self))")
        }
    }
    
    var _fun_args: [Resolvable<HLType>] {
        switch(self) {
        case .fun(let funData): return funData.args
        default: fatalError("_fun_args only defined for .fun (got \(self))")
        }
    }
}

extension HLNative : NativeCallable {
    var entrypoint: any MemoryAddress { self.memory }
    var ret: Resolvable<HLType> { self.type.value._fun_ret }
    var args: [Resolvable<HLType>] { self.type.value._fun_args }
}

protocol IncompleteCallable {
    var ret: Resolvable<HLType> { get }
    var args: [Resolvable<HLType>] { get }
}

protocol Callable: IncompleteCallable {
    var entrypoint: any MemoryAddress { get }
}

protocol Compilable : Callable {
    func getFindex() -> Int
    var ops: [HLOpCode] { get }
    var regs: [Resolvable<HLType>] { get }
}

extension HLCompiledFunction : Compilable {
    var regs: [Resolvable<HLType>] {
        self.function.regs
    }
    
    var ret: Resolvable<HLType> {
        self.function.type.value._fun_ret
    }
    
    var args: [Resolvable<HLType>] {
        self.function.regs
    }
    
    func getFindex() -> Int { Int(self.function.findex) }
    
    func getOps() -> [HLOpCode] {
        self.function.ops
    }
    
    func getRet() -> HLType {
        guard case .fun(let funData) = self.function.type.value else {
            fatalError("invalid type \(self.function.type)")
        }
        return funData.ret.value
    }
    
    var entrypoint: any MemoryAddress { self.memory }
}

extension HLOpCode {
    static func parseCCompat(_ cop: HLOpCode_CCompat) -> HLOpCode {
        let opId = HLOpCodeId(rawValue: UInt32(cop.op))
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
        case .OGetGlobal:
            return .OGetGlobal(dst: cop.p1, global: RefGlobal(cop.p2))
        case .OCall0:
            return .OCall0(dst: cop.p1, fun: RefFun(cop.p2))
        case .OCall1:
            return .OCall1(dst: cop.p1, fun: RefFun(cop.p2), arg0: cop.p3)
        case .OCall2:
            return .OCall2(dst: cop.p1, fun: RefFun(cop.p2), arg0: cop.p3, arg1: Reg(Int(bitPattern: cop.extra!)))
        case .OCall3:
            guard let extra = cop.extra else { fatalError("OCall3 missing extra") }
            return .OCall3(dst: cop.p1, fun: RefFun(cop.p2), arg0: cop.p3, arg1: extra.pointee, arg2: extra.advanced(by: 1).pointee)
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
        // implementation line (below are missing)
        case .OAnd:
            return .OAnd(dst: cop.p1, a: cop.p2, b: cop.p3)
        case .OJEq:
            return .OJEq(a: cop.p1, b: cop.p2, offset: cop.p3)
        case .OThrow:
            return .OThrow(exc: cop.p1)
        case .OSShr:
            return .OSShr(dst: cop.p1, a: cop.p2, b: cop.p3)
        case .OLabel:
            return .OLabel
        case .OJSGte:
            return .OJSGte(a: cop.p1, b: cop.p2, offset: cop.p3)
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
        default:
            fatalError("Unknown op to parse \(String(describing: opId))")
        }
    }
}

extension HLType {
    init(_ c: HLType_CCompat) {
        switch(c.kind) {
        case .void: self = .void
        case .i32: self = .i32
        case .u8: self = .u8
        case .u16: self = .u16
        case .i64: self = .i64
        case .f32: self = .f32
        case .f64: self = .f64
        case .type: self = .type
        case .dynobj: self = .dynobj
        case .dyn: self = .dyn
        case .bytes: self = .bytes
        case .array: self = .array
        case .null: self = .null(HLTypeNullData(type: Resolvable(HLType(c.tparam.pointee), memory: c.tparam)))
        case .obj: self = .obj(HLTypeObj(c.obj.pointee))
        case .struct: self = .struct(HLTypeObj(c.obj.pointee))
        case .bool: self = .bool
        
        case .method: self = .method(HLTypeFun(c.fun.pointee))
        case .fun: self = .fun(HLTypeFun(c.fun.pointee))
            
        default:
            fatalError("not implemented HLType from \(c.kind)")
        }
    }
}

extension HLFunction_CCompat : Compilable {
    func getFindex() -> Int { Int(self.findex) }
    
    var regs: [Resolvable<HLType>] {
        (0..<nregs).map { ix in
            let ptrPtr = self.regsPtr?.advanced(by: Int(ix))
            guard let ptr = ptrPtr?.pointee else {
                fatalError("No pointer available")
            }
            
            return Resolvable(unsafeType: ptr)
        }
    }
    
    var entrypoint: any MemoryAddress {
        fatalError("No memory, need to wrap this in HLFunction_CCompat__WithMemory")
    }
    
    var args: [Resolvable<HLType>] {
        let res = (0..<cType.fun.pointee.nargs).map { ix in
            let ptr: UnsafePointer<HLType_CCompat> = self.cType.fun.pointee.argsPtr.advanced(by: Int(ix)).pointee
            return Resolvable(unsafeType: ptr)
        }
        
        return res
    }
    var ret: Resolvable<HLType> {
        Resolvable(HLType(self.cType.fun.pointee.ret), memory: self.cType.fun.pointee.retPtr)
    }
    
    var ops: [HLOpCode] { cOps.map { HLOpCode.parseCCompat($0) } }
}

struct HLFunction_CCompat__WithMemory : Compilable {
    func getFindex() -> Int {
        self.ptr.pointee.getFindex()
    }
    var ops: [HLOpCode] {
        self.ptr.pointee.ops
    }
    var regs: [Resolvable<HLType>] {
        self.ptr.pointee.regs
    }
    
    
    var args: [Resolvable<HLType>] {
        self.ptr.pointee.args
    }
    var ret: Resolvable<HLType> {
        self.ptr.pointee.ret
    }
    
    let ptr: UnsafePointer<HLFunction_CCompat>
    let entrypoint: any MemoryAddress

}

