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
            
            return Resolvable.type(fromUnsafe: ptrPtr.pointee)
        }
    }
    
    var ret: Resolvable<HLType> {
        .type(fromUnsafe: self.type.fun.pointee.retPtr)
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

extension HLType {
    init(_ c: UnsafePointer<HLType_CCompat>) {
        switch(c.pointee.kind) {
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
        case .null: self = .null(HLTypeNullData(type: .type(fromUnsafe: c.pointee.tparam)))
        case .obj: self = .obj(.fromPointer(c.pointee.obj))
        case .struct: self = .struct(.fromPointer(c.pointee.obj))
        case .bool: self = .bool
        
        case .method: self = .method(HLTypeFun_Depr(c.pointee.fun.pointee))
        case .fun: self = .fun(HLTypeFun_Depr(c.pointee.fun.pointee))
            
        default:
            fatalError("not implemented HLType from \(c.pointee.kind)")
        }
    }
}


struct HLFunction_CCompat__WithMemory : Compilable {
    func getFindex() -> Int {
        self.ptr.pointee.getFindex()
    }
    var ops: [HLOpCode] {
        return self.ptr.pointee.ops
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

