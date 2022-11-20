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
            funType: funType,
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
        funType: any HLTypeProvider,
        appendCall: (CpuOpBuffer)->(),
        regs: [any HLTypeProvider],
        preArgs: [((CpuOpBuffer, Register64)->(), HLTypeKind)],
        args: [Reg],
        reservedStackBytes: ByteCount,
        mem: CpuOpBuffer
    ) throws {
        
        appendDebugPrintAligned4("__ocall_impl", builder: mem)
        
        let dstStackOffset = getRegStackOffset(regs, dst)
        let dstKind = requireTypeKind(reg: dst, from: regs)
        
        let totalArgKinds = preArgs.map({ $0.1 }) + args.map({ requireTypeKind(reg: $0, from: regs) })
        
        let additionalSizeUnrounded = totalArgKinds.dropFirst(ARG_REGISTER_COUNT).reduce(0) {
            print("Adding size for \($1)")
            return $0 + Int($1.hlRegSize)
        }
        let additionalSize = roundUpStackReservation(Int16(additionalSizeUnrounded))
        
        if additionalSize > 0 {
            mem.append(
                PseudoOp.debugPrint2(self, "Reserving \(additionalSize) bytes for stack (OCallN)"),
                M1Op.subImm12(X.sp, X.sp, try .i(additionalSize))
            )
        }

        let regWkindToPass: [((CpuOpBuffer, Register64)->(), HLTypeKind)] = preArgs + args.enumerated().map {
            (position, argReg) in
            
            assert(reg: argReg, from: regs, matchesCallArg: Reg(position), inFunArgs: funType.funProvider!.argsProvider)
            let argTypeKind = requireTypeKind(reg: argReg, from: regs)
            let load: (CpuOpBuffer, Register64)->()
            
            if position >= ARG_REGISTER_COUNT {
                load = {
                    let offset = self.getRegStackOffset(regs, argReg) + Int64(additionalSize)
                    
                    $0.append(
                        PseudoOp.ldrVreg($1, offset, argTypeKind.hlRegSize)
                    )
                }
            } else {
                load = {
                    let offset = self.getRegStackOffset(regs, argReg) + Int64(additionalSize)
                    
                    self.appendLoad(reg: $1,
                               from: argReg,
                               kinds: regs, // careful, pass all kinds, not just the arg ones
                               offset: offset,
                               mem: $0)
                }
            }
            
            return (
                load, argTypeKind
            )
        }
        
        var argOffset: Int64 = 0
        for (load, regKind) in regWkindToPass.dropFirst(ARG_REGISTER_COUNT) {
            load(mem, X.x0)
            mem.append(
                PseudoOp.strVreg(X.x0, argOffset, regKind.hlRegSize)
            )
            argOffset += regKind.hlRegSize
        }
        
        for (regIx, (load, _)) in regWkindToPass.enumerated() {
            guard regIx < ARG_REGISTER_COUNT else { break }
            let armReg = Register64(rawValue: UInt8(regIx))!
            
            appendDebugPrintAligned4("loading \(armReg)", builder: mem)
            load(mem, armReg)
            appendDebugPrintRegisterAligned4(armReg, builder: mem)
        }
        
        // PERFORM BLR
        mem.append(PseudoOp.debugPrint2(self, "append call"))
        appendCall(mem)
        mem.append(PseudoOp.debugPrint2(self, "appended call"))
        
        mem.append(
            PseudoOp.strVreg(X.x0, dstStackOffset + Int64(additionalSize), dstKind.hlRegSize),
            PseudoOp.debugPrint2(self, "Got back and put result at offset \(dstStackOffset + Int64(additionalSize))")
        )
        
        if additionalSize > 0 {
            mem.append(
                PseudoOp.debugPrint2(self, "Free \(additionalSize) bytes (OCallN)"),
                (try M1Op._add(X.sp, X.sp, ByteCount(reservedStackBytes)))
            )
        }
    }
    
    /// Reusable implem for OGetThis and OField
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
        
        mem.append(PseudoOp.debugPrint2(self, "CallN fn@\(funIndex)(\(args)) -> \(dst)"))
        
        try __ocall_impl(
            dst: dst,
            funType: callTarget.typeProvider,
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
