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
    
    func __ocallmethod__ocallthis(
        dst: Reg,
        obj: Reg,
        funcProto: RefField,
        args: [Reg],
        regs: [any HLTypeProvider],
        reservedStackBytes: ByteCount,
        mem: CpuOpBuffer,
        _name: String = "__ocallmethod__ocallthis",
        _callsite: (findex: RefFun, opnum: Int)? = nil) throws {
            
        var prependHeader: String = _name
        if let cs = _callsite {
            prependHeader = "f\(cs.findex): #\(cs.opnum): \(prependHeader)"
        }
            
        let objType = requireType(reg: obj, regs: regs)
            
        switch(objType.kind) {
        case .obj:
            prependHeader = "\(prependHeader)/obj"
            print("header is", prependHeader)
            
            let _getCallTarget: (@convention(c)(OpaquePointer, Int32)->OpaquePointer) = {
                (objPtr, protoIx) in
                
                let vd: UnsafePointer<vdynamic> = .init(objPtr)
                guard let vobjProto: UnsafePointer<OpaquePointer> = .init(OpaquePointer(vd.pointee.t.pointee.vobjProto)) else {
                    
                    // If this happens in a test, maybe some dependency function was not compiled
                    fatalError("OCallMethod/Obj failed - vobjProto not set")
                }
                let rightProto = vobjProto.advanced(by: Int(protoIx)).pointee
                
                return rightProto
            }
            
            // Fetch proto function index
            appendLoad(reg: X.x0, from: obj, kinds: regs, mem: mem)
            mem.append(PseudoOp.mov(X.x1, funcProto))
            
            // Fetch proto function address
            appendLoad(reg: X.x0, from: obj, kinds: regs, mem: mem)
            mem.append(PseudoOp.mov(X.x1, funcProto))
            mem.append(
                PseudoOp.mov(X.x2, unsafeBitCast(_getCallTarget, to: OpaquePointer.self)),
                M1Op.blr(X.x2),
                
                M1Op.movr64(X.x9, X.x0) // proto function address in x9
            )

            try __ocallmethod_impl__addrInX9(
                dst: dst,
                regs: regs,
                preArgs: [
                    ({ (inmem, regIxForPreArg, regKind) in
                        Swift.assert(regKind.hlRegSize == 8)
                        self.appendLoad(regIxForPreArg, from: obj, kinds: regs, mem: inmem)
                    }, HLTypeKind.obj)
                ],
                args: args,
                reservedStackBytes: reservedStackBytes,
                mem: mem)
        case .virtual:
            prependHeader = "\(prependHeader)/virtual"
            
            // TODO: deduplicate across osetfield
            /* ASM for -->
             if( hl_vfields(o)[f] )
                dst = *hl_vfields(o)[f](o->value,args...);
             else
                dst = hl_dyn_call_obj(o->value,field,args,&ret)
             */
            
            let dstKind = requireTypeKind(reg: dst, from: regs)
            
            // x0 -> points to ((*_vvirtual)(obj))+1
            appendLoad(reg: X.x0, from: obj, kinds: regs, mem: mem)
            appendDebugPrintRegisterAligned4(X.x0, prepend: "\(prependHeader) object", builder: mem)
            
            // x1 point to field base (right after the (vvirtual) content at x0
            mem.append(M1Op.add(X.x1, X.x0, .imm(Int64(MemoryLayout<vvirtual>.stride), nil)))
            appendDebugPrintRegisterAligned4(X.x1, prepend: "\(prependHeader) field base", builder: mem)
            
            // x2 load field index multiplied by size of (void*)
            let fieldOffsetInBytes = funcProto * MemoryLayout<OpaquePointer>.stride
            mem.append(M1Op.movz64(X.x2, UInt16(fieldOffsetInBytes), nil))
            appendDebugPrintAligned4("\(prependHeader) func proto: \(funcProto)", builder: mem)
 
            
            // add field offset to base
            mem.append(M1Op.add(X.x1, X.x1, .r64shift(X.x2, .lsl(0))))
            appendDebugPrintRegisterAligned4(X.x1, prepend: "\(prependHeader) addr of func#\(funcProto)", builder: mem)
            
            // field source is pointer to a pointer, so we need to dereference it once before
            // we check if it is null or not (and if null - don't use)
            mem.append(M1Op.ldr(X.x1, .reg(X.x1, .imm(0, nil))))
            mem.append(M1Op.movr64(X.x28, X.x1))    // store first in x28
            appendDebugPrintRegisterAligned4(X.x1, prepend: "\(prependHeader) dereferenced address", builder: mem)
            
            // compare x1 to 0
            var jmpTarget_hlvfieldNoAddress = RelativeDeferredOffset()
            var jmpTarget_postCheck = RelativeDeferredOffset()
            mem.append(M1Op.movz64(X.x2, 0, nil))
            mem.append(M1Op.cmp(X.x1, X.x2))
            
            mem.appendWithOffset(offset: &jmpTarget_hlvfieldNoAddress, PseudoOp.b_eq_deferred(jmpTarget_hlvfieldNoAddress))
            // MARK: OCallMethod/virtual has address
            appendDebugPrintAligned4("\(prependHeader) HAS ADDRESS", builder: mem)
            
            // load field value into x9
            mem.append(M1Op.movr64(X.x9, X.x28)) // restore x28 to x9
            appendDebugPrintRegisterAligned4(X.x9, prepend: "\(prependHeader) address", builder: mem)

            appendDebugPrintAligned4("\(prependHeader) has \(args.count) args", builder: mem)
            try __ocallmethod_impl__addrInX9(
                dst: dst,
                regs: regs,
                preArgs: [
                    ({ (inmem, regIxForPreArg, regKind) in
                        Swift.assert(regKind.hlRegSize == 8)
                        guard let regI = Register64(rawValue: regIxForPreArg) else {
                            fatalError("Could not create GP register from raw value \(regIxForPreArg)")
                        }
                        guard let offsetToValue = MemoryLayout.offset(of: \vvirtual.value) else {
                            fatalError("Could not generate offset to vvirtual.value")
                        }
                        self.appendLoad(reg: regI, from: obj, kinds: regs, mem: inmem)
                        self.appendDebugPrintRegisterAligned4(regI, prepend: "\(prependHeader) obj", builder: inmem)
                        inmem.append(
//                            M1Op.add(regI, regI, .imm(Int64(offsetToValue), nil))
                            M1Op.ldr(regI, .reg(regI, .imm(Int64(offsetToValue), nil)))
                        )
                        self.appendDebugPrintRegisterAligned4(regI, prepend: "\(prependHeader) obj->value", builder: inmem)
                    }, HLTypeKind.dyn)
                ],
                args: args,
                reservedStackBytes: reservedStackBytes,
                mem: mem)
            
            // finish this branch
            mem.appendWithOffset(offset: &jmpTarget_postCheck, M1Op.b(jmpTarget_postCheck))            
            // marker for other branch
            jmpTarget_hlvfieldNoAddress.stop(at: mem.byteSize)
            
            // MARK: OCallMethod/virtual has no address
            appendDebugPrintAligned4("\(prependHeader) HAS NO ADDRESS", builder: mem)
            
            // create pointer for holding output as x4
            let retBuffer: UnsafeMutablePointer<vdynamic>?
            let retReg = X.x4
            if !dstKind.isPointer {
                retBuffer = .allocate(capacity: 1)
                mem.append(PseudoOp.mov(retReg, OpaquePointer(retBuffer!)))
            } else {
                mem.append(M1Op.movz64(retReg, 0, nil))
                retBuffer = nil
            }
            
            
            // prepare dynamic arguments as x3
            // NOTE: HL uses (cop->p3-1) but args.count already has removed the first item (see `parseCCompat`)
            let nargs = args.count
            let dynArgs: UnsafeMutableBufferPointer<OpaquePointer> = .allocate(capacity: nargs)
            mem.append(PseudoOp.mov(X.x0, OpaquePointer(dynArgs.baseAddress!)))
            appendDebugPrintAligned4("\(prependHeader) HAS NO ADDRESS - with \(args.count) args", builder: mem)
            for (ix, argRegister) in args.enumerated() {
                let argKind = requireTypeKind(reg: argRegister, from: regs)
                let offset: Int64 = Int64(ix * MemoryLayout<UnsafePointer<vdynamic>>.stride)
                
                if argKind.isPointer {
                    // if pointer -> we store the address directly
                    appendLoad(reg: X.x1, from: argRegister, kinds: regs, mem: mem)
                } else {
                    // if not a pointer, we need to get the address of the arg, and store that
                    let offsetToVreg = getRegStackOffset(regs, argRegister)
                    mem.append(
                        M1Op.movr64(X.x1, .sp),
                        M1Op.add(X.x1, X.x1, .imm(offsetToVreg, nil))
                    )
                }
                
                // force storage as address (-> force the kind)
                appendStore(reg: X.x1, as: 0, intoAddressFrom: X.x0, offsetFromAddress: offset, kinds: [HLTypeKind.dyn], mem: mem)
                appendDebugPrintRegisterAligned4(X.x1, prepend: "\(prependHeader)/no address/arg \(ix)/\(argKind)", builder: mem)
            }
            
            // load x2: obj->t->virt->fields[o->p2].hashed_name
            //          where o->p2 is funcProto
            let _getFidFromObj: (@convention(c) (OpaquePointer, Int64)->Int64) = {
                virtPtr, funcProto in
                
                let v: UnsafePointer<vvirtual> = .init(virtPtr)
                let field = v.pointee.t.pointee.virt.pointee.fields.advanced(by: Int(funcProto)).pointee
                let fid = field.hashedName
                
                
                let lookup: UnsafePointer<HLFieldLookup_CCompat>? = v.pointee.t.pointee.virt.pointee.lookup
                
                
                
                print("OCallMethod name: \(field.tPtr._overrideDebugDescription)")
                print("OCallMethod field: \(field.name)")
                print("OCallMethod v->fid (from native): \(fid) (or \(Int32(truncatingIfNeeded: fid)))")
                return .init(fid)
            }
            appendLoad(reg: X.x0, from: obj, kinds: regs, mem: mem)
            mem.append(M1Op.movz64(X.x1, UInt16(funcProto), nil))
            appendFuncCall(unsafeBitCast(_getFidFromObj, to: OpaquePointer.self), via: X.x20, mem: mem)
            mem.append(M1Op.movr64(X.x21, X.x0))
            
            // load x1: obj->t->virt->fields[o->p2].t
            //          where o->p2 is funcProto
            let _getFtFromObj: (@convention(c) (OpaquePointer, Int64)->OpaquePointer) = {
                virtPtr, funcProto in
                
                let v: UnsafePointer<vvirtual> = .init(virtPtr)
                let ft = v.pointee.t.pointee.virt.pointee.fields.advanced(by: Int(funcProto)).pointee.tPtr
                
                print("OCallMethod v (from native): \(virtPtr)")
                print("OCallMethod v->value (from native): \(v.pointee.value)")
                print("OCallMethod v->ft (from native): \(ft)")
                return .init(ft)
            }
            appendLoad(reg: X.x0, from: obj, kinds: regs, mem: mem)
            mem.append(M1Op.movz64(X.x1, UInt16(funcProto), nil))
            appendFuncCall(unsafeBitCast(_getFtFromObj, to: OpaquePointer.self), via: X.x20, mem: mem)
            mem.append(M1Op.movr64(X.x1, X.x0))
            appendDebugPrintRegisterAligned4(X.x1, prepend: "\(prependHeader) ft", builder: mem)
            
            // load x0: obj->value
            let valueOffset: Int64 = Int64(MemoryLayout<vvirtual>.offset(of: \vvirtual.value)!)
            appendLoad(reg: X.x0, from: obj, kinds: regs, mem: mem)
            appendDebugPrintRegisterAligned4(X.x0, prepend: "\(prependHeader) value (pre)", builder: mem)
            mem.append(M1Op.ldr(X.x0, .reg(X.x0, .imm(valueOffset, nil))))
            appendDebugPrintRegisterAligned4(X.x0, prepend: "\(prependHeader) value", builder: mem)
            
            
            // load previously stashed, and print values
            mem.append(
                M1Op.movr64(X.x2, X.x21)
            )
            appendDebugPrintRegisterAligned4(X.x0, prepend: "\(prependHeader) obj->value (final)", builder: mem, format: "%p")
            appendDebugPrintRegisterAligned4(X.x1, prepend: "\(prependHeader) ft (final)", builder: mem, format: "%p")
            appendDebugPrintRegisterAligned4(W.w2, prepend: "\(prependHeader) fid (final)", builder: mem, format: "%d")
            
            mem.append(PseudoOp.mov(X.x3, OpaquePointer(dynArgs.baseAddress!)))
            appendDebugPrintRegisterAligned4(X.x3, prepend: "\(prependHeader) args (final)", builder: mem, format: "%d")
            
            if let rb = retBuffer {
                mem.append(PseudoOp.mov(X.x4, OpaquePointer(rb)))
            } else {
                mem.append(M1Op.movz64(X.x4, 0, nil))
            }
            appendDebugPrintRegisterAligned4(X.x4, prepend: "\(prependHeader) ret (final)", builder: mem, format: "%d")
            
            
            appendFuncCall(
                unsafeBitCast(LibHl._hl_dyn_call_obj, to: OpaquePointer.self),
                via: X.x20,
                mem: mem)
            
            // appendDeallocateBufferPointer(dynArgs, mem: mem)
            
            // assuming dst will be a pointer and hl_call_dyn_obj returns a vvirtual
            // and we need to store result->value
            let _outConvert: (@convention(c) (OpaquePointer, OpaquePointer?, Int32)->(OpaquePointer)) = {
                oPtr, retBufferPtr, dstKindRawVal in
                
                let dstKind: HLTypeKind = .init(rawValue: UInt32(dstKindRawVal))
                let v: UnsafePointer<vvirtual> = .init(oPtr)
                print("[OCallMethod/virtual] v", v)
                print("[OCallMethod/virtual] v.pointee.value", v.pointee.value)
                print("[OCallMethod/virtual] v type", v.pointee.t._overrideDebugDescription)
                // TODO: wat
                print("retBufferPtr", retBufferPtr)
                if !dstKind.isPointer {
//                    let ptrToDyn = UnsafePointer<vdynamic>(rb)
//                    let raw: UnsafeRawPointer = .init(rb)
//                    let offs = MemoryLayout<vdynamic>.offset(of: \vdynamic.union)!
//                    let res = OpaquePointer(raw.advanced(by: offs))
                    if FP_TYPE_KINDS.contains(dstKind) {
                        fatalError("TODO: test coverage for OCallMethod/virtual when no address and returns FP")
                    }
                    return .init(v.pointee.value)
//                    print("Res", res, "from", rb, "for", dstKind)
//                    return res
                }
                if dstKind == .virtual {
                    print(v.pointee.value.pointee.t._overrideDebugDescription)
                } else {
                    print(v.pointee.t._overrideDebugDescription)
                }
                return oPtr
//                return .init(v.pointee.value)
            }
            if let rb = retBuffer {
                mem.append(PseudoOp.mov(X.x1, OpaquePointer(rb)))
            } else {
                mem.append(M1Op.movz64(X.x1, 0, nil))
            }
            mem.append(M1Op.movz64(X.x2, UInt16(dstKind.rawValue), nil))
            appendFuncCall(unsafeBitCast(_outConvert, to: OpaquePointer.self), via: X.x20, mem: mem)
            
            if isFP(vreg: dst, kinds: regs) {
                print(dstKind._overrideDebugDescription)
                if dstKind.hlRegSize == 8 {
                    mem.append(M1Op.fmov(D.d0, X.x0))
                } else if dstKind.hlRegSize == 4 {
                    mem.append(M1Op.fmov(S.s0, W.w0))
                } else {
                    fatal("Unrecognized floating point register size: \(dstKind)", Self.logger)
                }
            }
            if !Self.isVoid(vreg: dst, kinds: regs) {
                appendStore(0, into: dst, kinds: regs, mem: mem)
                appendDebugPrintRegisterAligned4(0, kind: dstKind, prepend: "\(prependHeader) final stored result (\(dstKind))", builder: mem)
            } else {
                appendDebugPrintAligned4("\(prependHeader) void result", builder: mem)
            }
            
            jmpTarget_postCheck.stop(at: mem.byteSize)
            appendDebugPrintAligned4("\(prependHeader) EXITING", builder: mem)
        default:
            print("Target", objType.kind)
            fatalError("Invalid target for OCallMethod or OCallThis")
        }
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
        appendCall: (CpuOpBuffer) throws->(),
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
        try appendCall(mem)
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
            appendDebugPrintRegisterAligned4(X.x9, prepend: "__ocallmethod_impl__addrInX9 (pre-call)", builder: buff)
            buff.append(M1Op.blr(X.x9))
            appendDebugPrintAligned4("__ocallmethod_impl__addrInX9 (post-call)", builder: buff)
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
                    PseudoOp.movCallableAddress(X.x19, ctx.jitBase, callTarget.address),
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
