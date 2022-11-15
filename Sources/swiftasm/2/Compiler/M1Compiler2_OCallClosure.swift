//
//  File.swift
//  
//
//  Created by Janis Kirsteins on 15/11/2022.
//

import Foundation

extension M1Compiler2 {
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
                appendLoad(reg: .x19, from: fun, kinds: regs, mem: $0)
                $0.append(M1Op.blr(X.x19))
            },
            regs: regs,
            args: args,
            reservedStackBytes: reservedStackBytes,
            mem: mem)
    }
    
    func __ocall_impl(
        dst: Reg,
        funType: any HLTypeProvider,
        appendCall: (CpuOpBuffer)->(),
        regs: [any HLTypeProvider],
        args: [Reg],
        reservedStackBytes: ByteCount,
        mem: CpuOpBuffer
    ) throws {
        let dstStackOffset = getRegStackOffset(regs, dst)
        let dstKind = requireTypeKind(reg: dst, from: regs)

        let regWkindToPass = args.enumerated().map {
            (reg, argReg) in
            
            assert(reg: argReg, from: regs, matchesCallArg: Reg(reg), inFunArgs: funType.funProvider!.argsProvider)
            return (reg, requireTypeKind(reg: argReg, from: regs))
        }
        
        let additionalSizeUnrounded = regWkindToPass.dropFirst(ARG_REGISTER_COUNT).reduce(0) {
            print("Adding size for \($1.1)")
            return $0 + Int($1.1.hlRegSize)
        }
        let additionalSize = roundUpStackReservation(Int16(additionalSizeUnrounded))
        
        if additionalSize > 0 {
            mem.append(
                PseudoOp.debugPrint2(self, "Reserving \(additionalSize) bytes for stack (OCallN)"),
                M1Op.subImm12(X.sp, X.sp, try .i(additionalSize))
            )
        }
        
        var argOffset: Int64 = 0
        for (regIx, regKind) in regWkindToPass.dropFirst(ARG_REGISTER_COUNT) {
            guard args.count > regIx else { break }
            let argReg = args[regIx]
            let offset = getRegStackOffset(regs, argReg) + Int64(additionalSize)
            
            mem.append(
                PseudoOp.ldrVreg(X.x0, offset, regKind.hlRegSize),
                PseudoOp.strVreg(X.x0, argOffset, regKind.hlRegSize)
            )
            mem.append(PseudoOp.debugPrint2(self,
                                            "Loaded \(offset) -> \(argOffset) -> \(regKind.hlRegSize)"))
            
            argOffset += regKind.hlRegSize
        }
        
        for regIx in 0..<ARG_REGISTER_COUNT {
            guard args.count > regIx else { break }
            let argReg = args[regIx]
            
            puts("Putting varg \(argReg) in nreg \(regIx)")
            let offset = getRegStackOffset(regs, argReg) + Int64(additionalSize)
            //
            appendLoad(reg: Register64(
                rawValue: UInt8(regIx))!,
                       from: argReg,
                       kinds: regs, // careful, pass all kinds, not just the arg ones
                       offset: offset,
                       mem: mem)
            appendDebugPrintRegisterAligned4(Register64(rawValue: UInt8(regIx))!, builder: mem)
        }
        
        // PERFORM BLR
        appendCall(mem)
        
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
            args: args,
            reservedStackBytes: reservedStackBytes,
            mem: mem)
//
//        assert(reg: dst, from: regs, matches: callTarget.retProvider)
//        let dstStackOffset = getRegStackOffset(regs, dst)
//        let dstKind = requireTypeKind(reg: dst, from: regs)
//
//        let regWkindToPass = args.enumerated().map {
//            (reg, argReg) in
//            assert(reg: argReg, from: regs, matchesCallArg: Reg(reg), inFun: callTarget)
//            return (reg, requireTypeKind(reg: argReg, from: regs))
//        }
//
//        let additionalSizeUnrounded = regWkindToPass.dropFirst(ARG_REGISTER_COUNT).reduce(0) {
//            print("Adding size for \($1.1)")
//            return $0 + Int($1.1.hlRegSize)
//        }
//        let additionalSize = roundUpStackReservation(Int16(additionalSizeUnrounded))
//
//        if additionalSize > 0 {
//            mem.append(
//                PseudoOp.debugPrint2(self, "Reserving \(additionalSize) bytes for stack (OCallN)"),
//                M1Op.subImm12(X.sp, X.sp, try .i(additionalSize))
//            )
//        }
//
//        var argOffset: Int64 = 0
//        for (regIx, regKind) in regWkindToPass.dropFirst(ARG_REGISTER_COUNT) {
//            guard args.count > regIx else { break }
//            let argReg = args[regIx]
//            let offset = getRegStackOffset(regs, argReg) + Int64(additionalSize)
//
//            mem.append(
//                PseudoOp.ldrVreg(X.x0, offset, regKind.hlRegSize),
//                PseudoOp.strVreg(X.x0, argOffset, regKind.hlRegSize)
//            )
//            mem.append(PseudoOp.debugPrint2(self,
//                                            "Loaded \(offset) -> \(argOffset) -> \(regKind.hlRegSize)"))
//
//            argOffset += regKind.hlRegSize
//        }
//
//        mem.append(PseudoOp.debugPrint2(self, "CallN fn@\(funIndex)(\(args)) -> \(dst)"))
//
//        for regIx in 0..<ARG_REGISTER_COUNT {
//            guard args.count > regIx else { break }
//            let argReg = args[regIx]
//
//            puts("Putting varg \(argReg) in nreg \(regIx)")
//            let offset = getRegStackOffset(regs, argReg) + Int64(additionalSize)
//            //
//            appendLoad(reg: Register64(
//                rawValue: UInt8(regIx))!,
//                       from: argReg,
//                       kinds: regs, // careful, pass all kinds, not just the arg ones
//                       offset: offset,
//                       mem: mem)
//            appendDebugPrintRegisterAligned4(Register64(rawValue: UInt8(regIx))!, builder: mem)
//        }
//
//        // TODOFIX
//        let fnAddr = callTarget.address
//        print("Target entrypoint is \(funIndex) \(fnAddr)")
//
//        mem.append(
//            PseudoOp.mov(.x19, fnAddr),
//            M1Op.blr(.x19)
//        )
//
//        mem.append(
//            PseudoOp.strVreg(X.x0, dstStackOffset + Int64(additionalSize), dstKind.hlRegSize),
//            PseudoOp.debugPrint2(self, "Got back and put result at offset \(dstStackOffset + Int64(additionalSize))")
//        )
//
//        if additionalSize > 0 {
//            mem.append(
//                PseudoOp.debugPrint2(self, "Free \(additionalSize) bytes (OCallN)"),
//                (try M1Op._add(X.sp, X.sp, ByteCount(reservedStackBytes)))
//            )
//        }
    }
}
