//
//  File.swift
//  
//
//  Created by Janis Kirsteins on 15/11/2022.
//

import Foundation

extension M1Compiler2 {
    /// Implementation for `OCallClosure`.
    ///
    /// - Parameters:
    ///   - dst: destination register
    ///   - fun: register that contains the address of the closure (we'll load the value in this register, and jump to the loaded value)
    ///   - regs: call target registers
    ///   - args: call target arguments
    ///   - reservedStackBytes: calling function's stack usage
    ///   - mem: buffer
    func __ocallclosure(
        dst: Reg,
        fun: Reg,
        regs: [any HLTypeProvider],
        args: [Reg],
        reservedStackBytes: ByteCount,
        mem: CpuOpBuffer
    ) throws
    {
        let funType = requireType(reg: fun, regs: regs)
        
        assert(reg: fun, from: regs, is: HLTypeKind.fun)
        
        try __ocall_impl(
            dst: dst,
            appendCall: {
                appendLoad(reg: .x15, from: fun, kinds: regs, mem: $0)
                appendDebugPrintRegisterAligned4(X.x15, builder: mem)
                $0.append(M1Op.blr(X.x15))
            },
            regs: regs,
            preArgs: [],
            args: args,
            reservedStackBytes: reservedStackBytes,
            mem: mem)
    }
    
    
    /// Shared implementation of various call methods (including closures).
    ///
    /// - Parameters:
    ///   - dst: virtual register where to store the function result.
    ///   - funType: type of the function being invoked (e.g. data that can provide argument and return types)
    ///   - appendCall: a closure that appends the actual jump instruction. This is needed because the call target address can come from different sources only known by the call site
    ///   - regs: list of register types for the function being called
    ///   - preArgs: list of arguments to prepend, which are not part of the function signature (this is typically used for prepending a sender object when invoking a closure on an object. The function type will not include the `this` value, so we prepend it via `preArgs`).
    ///   - args: list of argument types for the function being called
    ///   - reservedStackBytes: number of bytes that were reserved in the calling function (not call target) stack. This is needed to properly figure out stack offsets when preparing the arguments.
    ///   - mem: operation buffer
    func __ocall_impl(
        dst: Reg,
        appendCall: (CpuOpBuffer)->(),
        regs: [any HLTypeProvider],
        preArgs: [((CpuOpBuffer, RegisterRawValue, HLTypeKind)->(), HLTypeKind)],
        args: [Reg],
        reservedStackBytes: ByteCount,
        mem: CpuOpBuffer
    ) throws {
        
        appendDebugPrintAligned4("__ocall_impl", builder: mem)
        
        let dstStackOffset = getRegStackOffset(regs, dst)
        let dstKind = requireTypeKind(reg: dst, from: regs)
        
        let totalArgKinds = preArgs.map({ $0.1 }) + args.map({ requireTypeKind(reg: $0, from: regs) })
        
        let additionalSizeUnrounded = totalArgKinds.dropFirst(ARG_REGISTER_COUNT).reduce(0) {
            return $0 + Int($1.hlRegSize)
        }
        let additionalSize = StackInfo.roundUpStackReservation(Int16(additionalSizeUnrounded))
        
        if additionalSize > 0 {
            appendDebugPrintAligned4("Reserving \(additionalSize) bytes for stack (OCallN)", builder: mem)
            mem.append(
                M1Op.subImm12(X.sp, X.sp, try .i(additionalSize))
            )
        }

        let regWkindToPass: [((CpuOpBuffer, RegisterRawValue, HLTypeKind)->(), HLTypeKind)] = preArgs + args.enumerated().map {
            (position, argReg) in
            
            let argTypeKind = requireTypeKind(reg: argReg, from: regs)
            let load: (CpuOpBuffer, RegisterRawValue, HLTypeKind)->()
            
            load = { memIn, regRVIn, regKindIn in
                let offset = self.getRegStackOffset(regs, argReg) + Int64(additionalSize)
                self.appendDebugPrintAligned4("[__ocall_impl] loading arg \(argReg) into \(regRVIn) from \(offset) (\(self.getRegStackOffset(regs, argReg)) + \(Int64(additionalSize)))", builder: mem)
                self.appendLoad(regRVIn, from: argReg, kinds: regs, offset: offset, mem: mem)
            }
            
//            if position >= ARG_REGISTER_COUNT {
//                load = { memIn, regRVIn, regKindIn in
//                    let offset = self.getRegStackOffset(regs, argReg) + Int64(additionalSize)
//                    self.appendLoad(regRVIn, as: argReg, addressRegister: .sp, offset: offset, kinds: regs, mem: memIn)
//                }
//            } else {
//                load = { memIn, regRVIn, regKindIn in
//                    let offset = self.getRegStackOffset(regs, argReg) + Int64(additionalSize)
//
//                    self.appendLoad(regRVIn, from: argReg, kinds: regs, offset: offset, mem: mem)
//                }
//            }
            
            return (
                load, argTypeKind
            )
        }.filter {
            (_, kind) in
            kind != .void
        }
        
        var skippedFP = 0
        var skippedGP = 0
        let stackRegWkindToPass = regWkindToPass.filter {
            _, kind in
            
            if FP_TYPE_KINDS.contains(kind) {
                if skippedFP >= ARG_REGISTER_COUNT {
                    return true
                } else {
                    skippedFP += 1
                    return false
                }
            } else {
                if skippedGP >= ARG_REGISTER_COUNT {
                    return true
                } else {
                    skippedGP += 1
                    return false
                }
            }
        }
        
        // prepare the stack args
        // NOTE: these must be in the expected order (so mixing GP and FP values)
        // NOTE: the values on stack must be aligned
        // TODO: deduplicate alignment code with `appendStackInit`
        ;{
            var stackArgOffset: Int64 = 0
            var prevStackReg: HLTypeKind? = nil
            for (load, regKind) in stackRegWkindToPass {
                defer { prevStackReg = regKind }
                
                if let prevStackReg = prevStackReg {
                    let align: Int
                    switch(regKind) {
                    case .u8: align = MemoryLayout<UInt8>.alignment
                    case .bool: align = MemoryLayout<Bool>.alignment
                    case .u16: align = MemoryLayout<UInt16>.alignment
                    case .i32: align = MemoryLayout<Int32>.alignment
                    case .i64: align = MemoryLayout<Int64>.alignment
                    case .f32: align = MemoryLayout<Float32>.alignment
                    case .f64: align = MemoryLayout<Float64>.alignment
                    default:
                        align = MemoryLayout<OpaquePointer>.alignment
                    }
                    stackArgOffset = Int64(StackInfo.roundUpStackReservation(Int16(stackArgOffset + prevStackReg.hlRegSize), Int16(align)))
                }
                
                load(mem, 0, regKind)
                
                appendStore(0, as: 0, intoAddressFrom: .sp, offsetFromAddress: stackArgOffset, kinds: [regKind], mem: mem)
                appendDebugPrintRegisterAligned4(0, kind: regKind, prepend: "[__ocall_impl] stored stack arg \(regKind) at offset \(stackArgOffset)", builder: mem)
            }
        }()
        
        // load first 8 GP args into the respective registers
        ;{
            var gpIx: UInt8 = 0
            for (vregIx, (load, regKind)) in regWkindToPass.enumerated() {
                guard gpIx < ARG_REGISTER_COUNT else { break }
                guard !FP_TYPE_KINDS.contains(regKind) else { continue }
                defer { gpIx += 1 }
                
                appendDebugPrintAligned4("[__ocall_impl] loading GP \(gpIx) as \(regKind) from vreg \(vregIx)", builder: mem)
                load(mem, gpIx, regKind)
                appendDebugPrintRegisterAligned4(gpIx, kind: regKind, prepend: "[__ocall_impl] loaded GP \(gpIx) as \(regKind) from vreg \(vregIx)", builder: mem)
            }
        }()
        
        // load first 8 FP args into the respective registers
        ;{
            var fpIx: UInt8 = 0
            for (vregIx, (load, regKind)) in regWkindToPass.enumerated() {
                guard fpIx < ARG_REGISTER_COUNT else { break }
                guard FP_TYPE_KINDS.contains(regKind) else { continue }
                defer { fpIx += 1 }
                
                appendDebugPrintAligned4("[__ocall_impl] loading FP \(fpIx) as \(regKind) from vreg \(vregIx)", builder: mem)
                load(mem, fpIx, regKind)
                appendDebugPrintRegisterAligned4(fpIx, kind: regKind, prepend: "[__ocall_impl] loaded FP \(fpIx) as \(regKind) from vreg \(vregIx)", builder: mem)
            }
        }()
        
        // PERFORM BLR
        appendDebugPrintAligned4("[__ocall_impl] Appending call...", builder: mem)
        appendCall(mem)
        appendDebugPrintAligned4("[__ocall_impl] Finished call...", builder: mem)
        
        if dstKind != .void {
            self.appendStore(0, as: dst, intoAddressFrom: .sp, offsetFromAddress: dstStackOffset + Int64(additionalSize), kinds: regs, mem: mem)
            self.appendDebugPrintRegisterAligned4(0, kind: dstKind, prepend: "__ocall_impl result", builder: mem)
        } else {
            appendDebugPrintAligned4("[__ocall_impl] Ignoring .void return", builder: mem)
        }
                         
        if additionalSize > 0 {
            appendDebugPrintAligned4("Free \(additionalSize) bytes (OCallN)", builder: mem)
            mem.append(
                (try M1Op._add(X.sp, X.sp, ByteCount(additionalSize)))
            )
        }
    }
    
    /// OCallMethod implementation (differs from others because
    /// we don't know function info at call time)
    ///
    /// Call address must be in X9 when calling this. This method is guaranteed
    /// to not change that register.
    func __ocallmethod_impl__addrInX9(
        dst: Reg,
        regs: [any HLTypeProvider],
        preArgs: [((CpuOpBuffer, RegisterRawValue, HLTypeKind)->(), HLTypeKind)],
        args: [Reg],
        reservedStackBytes: ByteCount,
        mem: CpuOpBuffer
    ) throws {
        appendDebugPrintRegisterAligned4(X.x9, prepend: "__ocallmethod_impl__addrInX9 (start)", builder: mem)
        try __ocall_impl(dst: dst, appendCall: { buff in
            appendDebugPrintRegisterAligned4(X.x9, prepend: "__ocallmethod_impl__addrInX9 (end)", builder: buff)
            buff.append(M1Op.blr(X.x9))
        }, regs: regs, preArgs: preArgs, args: args, reservedStackBytes: reservedStackBytes, mem: mem)
    }
    
    /// OCallN
    func __ocalln(
        dst: Reg,
        funIndex: RefFun,
        regs: [any HLTypeProvider],
        args: [Reg],
        reservedStackBytes: ByteCount,
        mem: CpuOpBuffer
    ) throws
    {
        let callTarget = try ctx.requireCallable(findex: funIndex)
        ctx.funcTracker.referenced2(callTarget)
        
        appendDebugPrintAligned4("CallN fn@\(funIndex)(\(args)) -> \(dst)", builder: mem)
        
        try __ocall_impl(
            dst: dst,
            appendCall: { buff in
                buff.append(
                    PseudoOp.mov(.x19, callTarget.address),
                    M1Op.blr(.x19)
                )
            },
            regs: regs,
            preArgs: [],
            args: args,
            reservedStackBytes: reservedStackBytes,
            mem: mem)
    }
}
