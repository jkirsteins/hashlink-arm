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
        self.type.fun.pointee.args.enumerated().map { ix, item in
            let ptr = self.type.fun.pointee.argsPtr.advanced(by: ix).pointee
            return Resolvable(HLType(item), memory: ptr)
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
        case .OCall3:
            guard let extra = cop.extra else { fatalError("OCall3 missing extra") }
            return .OCall3(dst: cop.p1, fun: RefFun(cop.p2), arg0: cop.p3, arg1: extra.pointee, arg2: extra.advanced(by: 1).pointee)
        case .ONew:
            return .ONew(dst: cop.p1)
        case .OSetField:
            return .OSetField(obj: cop.p1, field: RefField(cop.p2), src: cop.p3)
        case .ORet:
            return .ORet(ret: cop.p1)
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
        self.cRegs.enumerated().map { ix, item in
            let ptr = self.regsPtr?.advanced(by: ix)
            return Resolvable(HLType(item), memory: ptr?.pointee)
        }
    }
    
    var entrypoint: any MemoryAddress {
        fatalError("No memory, need to wrap this in HLFunction_CCompat__WithMemory")
    }
    
    var args: [Resolvable<HLType>] {
        cType.fun.pointee.args.enumerated().map { ix, item in
            let ptr = self.cType.fun.pointee.argsPtr.advanced(by: ix)
            return Resolvable(HLType(item), memory: ptr.pointee)
        }
    }
    var ret: Resolvable<HLType> {
        Resolvable(HLType(self.cType.fun.pointee.ret), memory: self.cType.fun.pointee.retPtr)
    }
    
    var ops: [HLOpCode] { cOps.map { HLOpCode.parseCCompat($0) } }
}

struct HLFunction_CCompat__WithMemory : Compilable {
    func getFindex() -> Int { self.ptr.pointee.getFindex() }
    var ops: [HLOpCode] { self.ptr.pointee.ops }
    var regs: [Resolvable<HLType>] { self.ptr.pointee.regs }
    
    
    var args: [Resolvable<HLType>] { self.ptr.pointee.args }
    var ret: Resolvable<HLType> { self.ptr.pointee.ret }
    
    let ptr: UnsafePointer<HLFunction_CCompat>
    let entrypoint: any MemoryAddress

}

