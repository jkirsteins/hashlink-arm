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

struct HLNative_CCompat__Resolved : NativeCallable {
    let ptr: UnsafePointer<HLNative_CCompat>
    let entrypoint: any MemoryAddress
    
    var args: [HLType] {
        self.ptr.pointee.type.fun.args.map { HLType($0) }
    }
    
    var ret: HLType {
        HLType(self.ptr.pointee.type.fun.ret)
    }
}

extension HLType {
    var _fun_ret: HLType {
        switch(self) {
        case .fun(let funData): return funData.ret.value
        default: fatalError("_fun_ret only defined for .fun (got \(self))")
        }
    }
    
    var _fun_args: [HLType] {
        switch(self) {
        case .fun(let funData): return funData.args.map { $0.value }
        default: fatalError("_fun_args only defined for .fun (got \(self))")
        }
    }
}

extension HLNative : NativeCallable {
    var entrypoint: any MemoryAddress { self.memory }
    var ret: HLType { self.type.value._fun_ret }
    var args: [HLType] { self.type.value._fun_args }
}

protocol Callable {
    var entrypoint: any MemoryAddress { get }
    var ret: HLType { get }
    var args: [HLType] { get }
}

protocol Compilable : Callable {
    func getFindex() -> Int
    func getOps() -> [HLOpCode]
    func getRegs() -> [HLType]
//    func getAddress() -> any MemoryAddress
}

extension HLCompiledFunction : Compilable {
    var ret: HLType {
        self.function.type.value._fun_ret
    }
    
    var args: [HLType] {
        self.function.regs.map { $0.value }
    }
    
    func getFindex() -> Int { Int(self.function.findex) }
    
    func getOps() -> [HLOpCode] {
        self.function.ops
    }
    
    func getRegs() -> [HLType] {
        self.function.regs.map { $0.value }
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
    static func parseCCompat(_ cop: HLOpCode__CCompat) -> HLOpCode {
        let opId = HLOpCodeId(rawValue: UInt32(cop.op))
        switch(opId) {
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
        case .i64: self = .i64
        case .f32: self = .f32
        case .f64: self = .f64
        default:
            fatalError("not implemented HLType from \(c.kind)")
        }
    }
}

struct HLFunction_CCompat__WithMemory : Compilable {
    var args: [HLType] {
        self.ptr.pointee.type.fun.args.map { HLType($0) }
    }
    
    var ret: HLType { HLType(self.ptr.pointee.type.fun.ret) }
    
    let ptr: UnsafePointer<HLFunction_CCompat>
    let entrypoint: any MemoryAddress

    func getFindex() -> Int {
        return Int(ptr.pointee.findex)
    }
    
    func getOps() -> [HLOpCode] {
        return ptr.pointee.ops.map { HLOpCode.parseCCompat($0) }
    }
    
    func getRegs() -> [HLType] {
        return ptr.pointee.regs.map {
            HLType($0)
        }
    }
    
    func getArgs() -> [HLType] {
        return ptr.pointee.type.fun.args.map {
            HLType($0)
        }
    }
}

