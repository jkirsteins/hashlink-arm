typealias HLTypeKinds = [HLTypeKind]

protocol CompilerUtilities {
    func appendDebugPrintAligned4(_ val: String, builder: OpBuilder);
}

extension M1Compiler : CompilerUtilities {
    
}

// x0 through x7
private let ARG_REGISTER_COUNT = 8
extension M1Compiler {
    // Get offset into stack for a given register. This should skip void registers
    func getRegStackOffset(_ regs: HLTypeKinds, args: HLTypeKinds, reg: Int) -> ByteCount {
        fatalError("is this used?")
        let base: ByteCount
        let ptr: ArraySlice<HLTypeKind>
        let skip: Int

        /* If we want a non-function-arg register, then start
        counting from stack reservation offset and look at regs.

        Otherwise start counting args immediately. */
        if reg >= min(ARG_REGISTER_COUNT, args.count) {
            let (stackRes, _) = calcStackArgReq(regs: regs, args: args)
            base = ByteCount(stackRes)
            skip = args.count
            ptr = regs.dropFirst(args.count)
        }
        else {
            base = 0
            skip = 0
            ptr = args.dropFirst(0)
        }
        
        var result: ByteCount = base
        // note: careful, arrayslice inherits indexes from base array
        for ix in (skip..<reg) { result += ptr[ix].hlRegSize }
        return result
    }

    /* SP movements must be aligned to 16-bytes */func roundUpStackReservation(
        _ val: Int16
    ) -> Int16 { return (val + (16 &- 1)) & (0 &- 16) }

    /// Stack space should be allocated for first 8 function args (will be filled from registers) and
    /// any register, which is not part of args.
    /// - Parameters:
    ///   - regs:
    ///   - args:
    /// - Returns:
    func calcStackArgReq(regs unfilteredRegs: HLTypeKinds, args unfilteredArgs: HLTypeKinds)
        -> (Int16, HLTypeKinds)
    {
        let regs = unfilteredRegs.filter { $0 != .void }
        let args = unfilteredArgs.filter { $0 != .void }
        guard regs.prefix(args.count) == args.prefix(args.count) else {
            fatalError(
                "Args must match the first registers (got \(args) and \(regs) respectively)"
            )
        }
        let stackArgs = Array( /* regs should be aligned with args */
            // IMPORTANT: args must come first
            args.prefix(ARG_REGISTER_COUNT) + regs.dropFirst(args.count)
        )
        printerr("args needing space: \(stackArgs.map { $0.debugDescription })")
        let result = stackArgs.reduce(0) { $0 + Int16($1.hlRegSize) }
        return (roundUpStackReservation(result), stackArgs)
    }

    /*
    First 7 args are in registers [x0;x7]. Others are on the stack.
    Stack should be extended to account for data which is in registers.
    Subsequently data should be moved from regs to stack, to enable use of all registers.
*/
    @discardableResult func appendStackInit(
        _ unfilteredRegs: HLTypeKinds,
        args unfilteredArgs: HLTypeKinds,
        builder: OpBuilder
    ) throws -> Int16 {
        // test mismatched before filtering
        let unfilteredArgRegCount = min(unfilteredArgs.count, ARG_REGISTER_COUNT)
        guard
            unfilteredRegs.prefix(unfilteredArgRegCount)
                == unfilteredArgs.prefix(unfilteredArgRegCount)
        else {
            throw GlobalError.invalidOperation(
                "Up to first \(ARG_REGISTER_COUNT) registers must be the same for a function and its type.args"
            )
        }

        // filter after testing for mismatched
        let regs = unfilteredRegs.filter { $0 != .void }
        let args = unfilteredArgs.filter { $0 != .void }
        // only the first args need stack space, because we want to move them from the registers
        // into stack
        let (neededExtraStackSize, stackArgs) = calcStackArgReq(regs: regs, args: args)

        guard neededExtraStackSize > 0 else {
            builder.append(PseudoOp.debugMarker("No extra stack space needed"))
            return neededExtraStackSize
        }

        builder.append(
            PseudoOp.debugMarker("Reserving \(neededExtraStackSize) bytes for stack"),
            M1Op.sub(X.sp, X.sp, try .i(neededExtraStackSize))
        )

        var offset: ByteCount = 0
        for (ix, reg) in stackArgs.enumerated() {
            builder.append(
                PseudoOp.debugMarker(
                    "Moving \(Register64(rawValue: UInt8(ix))!) to \(offset)"
                ),
                M1Op.str(
                    Register64(rawValue: UInt8(ix))!,
                    .reg64offset(Register64.sp, offset, nil)
                )
            )
            print("Inc offset by \(reg.hlRegSize) from \(reg)")
            offset += reg.hlRegSize
        }

        return neededExtraStackSize
    }

    func appendDebugPrintAligned4(_ val: String, builder: OpBuilder) {
        var adr = RelativeDeferredOffset()
        var jmpTarget = RelativeDeferredOffset()
        let str = "[jitdebug] \(val)\n"
        builder.append(PseudoOp.debugMarker("Printing debug message: \(val)"))
        guard stripDebugMessages == false else {
            builder.append(PseudoOp.debugMarker("(debug message printing stripped)"))
            return
        }
        builder.append(
            // Stash registers we'll use (so we can reset)
            .str(Register64.x0, .reg64offset(.sp, -32, .pre)),
            .str(Register64.x1, .reg64offset(.sp, 8, nil)),
            .str(Register64.x2, .reg64offset(.sp, 16, nil)),
            .str(Register64.x16, .reg64offset(.sp, 24, nil)),

            // unix write system call
            .movz64(.x0, 1, nil)
        )
        adr.start(at: builder.byteSize)
        builder.append(
            .adr64(.x1, adr),
            .movz64(.x2, UInt16(str.count), nil),
            .movz64(.x16, 4, nil),
            .svc(0x80),
            // restore
            .ldr(Register64.x16, .reg64offset(.sp, 24, nil)),
            .ldr(Register64.x2, .reg64offset(.sp, 16, nil)),
            .ldr(Register64.x1, .reg64offset(.sp, 8, nil)),
            .ldr(Register64.x0, .reg64offset(.sp, 32, .post))
        )
        jmpTarget.start(at: builder.byteSize)
        builder.append(.b(jmpTarget))
        adr.stop(at: builder.byteSize)
        builder.append(ascii: str).align(4)

        jmpTarget.stop(at: builder.byteSize)
    }

    func appendSystemExit(_ code: UInt8, builder: OpBuilder) {
        builder.append(
            .movz64(.x0, UInt16(code), nil),
            .movz64(.x16, 1, nil),
            .svc(0x80)
        )
    }

    func appendPrologue(builder: OpBuilder) {
        builder.append(
            PseudoOp.debugMarker("Starting prologue"),
            M1Op.stp((.x29_fp, .x30_lr), .reg64offset(.sp, -16, .pre)),
            M1Op.movr64(.x29_fp, .sp)
        )
    }

    func appendEpilogue(builder: OpBuilder) {
        builder.append(
            PseudoOp.debugMarker("Starting epilogue"),
            M1Op.ldp((.x29_fp, .x30_lr), .reg64offset(.sp, 16, .post))
        )
    }
}

typealias Registers = [Resolvable<HLType>]

class M1Compiler {
    /*
    stp    x29, x30, [sp, #-16]!
    mov    x29, sp
    */let emitter = EmitterM1()

    let stripDebugMessages: Bool
    
    func assertEnoughRegisters(_ ix: Reg, regs: Registers) {
        guard ix < regs.count else {
            fatalError("Not enough registers. Expected \(ix) to be available. Got: \(regs)")
        }
    }
    
    func requireTypeMemory(reg: Reg, regs: Registers) -> UnsafePointer<HLType_CCompat> {
        assertEnoughRegisters(reg, regs: regs)
        
        guard let mem = regs[Int(reg)].memory else {
            fatalError("Register \(reg) has no type address available.")
        }
        
        return UnsafePointer(OpaquePointer(mem))
    }
    
    func requireFieldOffset(fieldRef: Int, objIx: Reg, regs: Registers) -> Int64 {
        let mem = requireTypeMemory(reg: objIx, regs: regs)
  
        switch(mem.pointee.kind) {
        case .obj:
            fallthrough
        case .struct:
            guard let rt = mem.pointee.obj.pointee.rt else {
                fatalError("Can not get field offset for obj without rt initialized")
            }
            return Int64(rt.pointee.fields_indexes.advanced(by: fieldRef).pointee)
        default:
            fatalError("Can not get field offset for obj type \(mem.pointee.kind)")
        }
    }
    
    func requireTypeAddress(reg: Reg, from regs: [Resolvable<HLType>]) -> UnsafeRawPointer {
        guard reg < regs.count else {
            fatalError("requireTypeAddress(reg:from:): Not enough registers. Expected \(reg) to be available. Got: \(regs)")
        }
        guard let mem = regs[Int(reg)].memory else {
            fatalError("requireTypeAddress(reg:from:): Register \(reg) has no type address available.")
        }

        return mem
    }
    
    func requireTypeKind(reg: Reg, from resolvedRegs: HLTypeKinds) -> HLTypeKind {
        guard reg < resolvedRegs.count else {
            fatalError("requireType(reg:from:): Not enough registers. Expected \(reg) to be available. Got: \(resolvedRegs)")
        }

        return resolvedRegs[Int(reg)]
    }

    func assertKind(_ op: HLOpCode, _ actual: HLTypeKind, _ expected: HLTypeKind) {
        guard expected == actual else {
            fatalError("\(op): type kind must be \(expected) but got \(actual)")
        }
    }

    func assert(reg: Reg, from: HLTypeKinds, matchesCallArg argReg: Reg, inFun callable: any Callable) {
        guard from.count > reg else {
            fatalError(
                "Register with index \(reg) does not exist. Available registers: \(from)."
            )
        }
        let regKind = from[Int(reg)]
        let argKind = callable.args[Int(argReg)].value.kind
        guard regKind == argKind else {
            fatalError(
                "Register \(reg) kind \(regKind) expected to match arg \(argReg) but arg was \(argKind) "
            )
        }
    }

    func assert(reg: Reg, from: HLTypeKinds, matches type: HLType) {
        guard from.count > reg else {
            fatalError(
                "Register with index \(reg) does not exist. Available registers: \(from)."
            )
        }
        guard from[Int(reg)] == type.kind else {
            fatalError(
                "Register \(reg) expected to be \(type.kind) but is \(from[Int(reg)])"
            )
        }
    }
    
    func assert(reg: Reg, from: HLTypeKinds, is target: HLTypeKind) {
        guard from.count > reg else {
            fatalError(
                "Register with index \(reg) does not exist. Available registers: \(from)."
            )
        }
        guard target == from[Int(reg)] else {
            fatalError(
                "Register \(reg) expected to be \(target) but is \(from[Int(reg)])"
            )
        }
    }

    init(stripDebugMessages: Bool = false) {
        self.stripDebugMessages = stripDebugMessages
    }

    func getRegStackOffset(_ regs: HLTypeKinds, _ ix: Reg) -> ByteCount {
        var result = ByteCount(0)
        for i in 0..<ix { result += regs[Int(i)].hlRegSize }
        return result
    }

    func getFieldOffset(_ obj: HLType, _ field: Int) -> ByteCount {
        guard let objData = obj.objData else {
            fatalError("Field offset only supports .obj and .struct (got \(obj))")
        }
        var startOffset = ByteCount(8)  // hl_type* pointer at start of obj/struct
        for ix in 0..<field {
            let fieldToSkip = objData.fields[ix].value.type.value.kind.hlRegSize
            startOffset += fieldToSkip
        }
        return startOffset
    }

    func push(to buffer: ByteBuffer, _ ops: M1Op...) throws {
        for op in ops { buffer.push(try emitter.emit(for: op)) }
    }

    /// Will compile and update the JIT context with the functions addresses.
    /// - Parameters:
    ///   - findex: 
    ///   - mem: 
    /// - Returns: 
    func compile(findex: Int32, into mem: OpBuilder) throws
    {
        let ctx = mem.ctx
        
        let entry = ctx.callTargets.get(Int(findex))
        
        guard case .compilable(let compilable) = entry else {
            fatalError("Call target fix==\(findex) is not compilable (got \(entry))")
        }
        
        guard !compilable.entrypoint.hasUsableValue else {
            throw GlobalError.invalidOperation(
                "Function \(findex) has already been compiled and assigned an address."
            )
        }
        
        try compile(compilable: compilable, into: mem)
    }
    
    func compile(compilable: Compilable, into mem: OpBuilder) throws
    {
        let ctx = mem.ctx

        // grab it before it changes from prologue
        // let memory = mem.getDeferredPosition()
        let regs = compilable.regs
        let regKinds = regs.map { $0.value.kind }
        
        let findex = compilable.getFindex()
        
        let relativeBaseAddr = mem.getDeferredPosition()
        compilable.entrypoint.update(from: relativeBaseAddr)

        // if we need to return early, we jump to these
        var retTargets: [RelativeDeferredOffset] = []
        
        print("Compiling function \(findex) at deferred address \(compilable.entrypoint)")
        print("REGS: \n--" + regs.map { String(reflecting: $0) }.joined(separator: "\n--"))
//        print("OPS: \n--" + funPtr.pointee.ops.map { String(reflecting: $0) }.joined(separator: "\n--"))

        let argKinds = compilable.args.map { $0.value.kind }

        mem.append(PseudoOp.debugMarker("==> STARTING FUNCTION \(findex)"))
        appendPrologue(builder: mem)
        let reservedStackBytes = try appendStackInit(
            regKinds,
            args: argKinds,
            builder: mem
        )

        appendDebugPrintAligned4(
            "Entering function \(findex)@\(relativeBaseAddr.offsetFromBase)",
            builder: mem
        )
        
        for op in compilable.ops {
            appendDebugPrintAligned4("Executing \(op.debugDescription)", builder: mem)

            switch op {
            case .ORet(let dst):
                // store
                let regStackOffset = getRegStackOffset(regKinds, dst)
                mem.append(
                    PseudoOp.debugMarker("Returning stack offset \(regStackOffset)"),
                    M1Op.ldr(X.x0, .reg64offset(.sp, regStackOffset, nil))
                )

                // jmp to end
                var retTarget = RelativeDeferredOffset()
                retTarget.start(at: mem.byteSize)
                retTargets.append(retTarget)
                mem.append(
                    PseudoOp.debugMarker("Jumping to epilogue"),
                    M1Op.b(retTarget)
                )

            case .OCall0(let dst, let funRef):
                let fn = ctx.callTargets.get(funRef)
                
                assert(
                    reg: dst,
                    from: regKinds,
                    matches: fn.ret.value
                )
            
                let regStackOffset = getRegStackOffset(regKinds, dst)

                mem.append(
                    PseudoOp.debugMarker("Call0 fn@\(funRef) -> \(dst)"),
                    PseudoOp.mov(.x10, fn.entrypoint),
                    M1Op.blr(.x10),
                    M1Op.str(X.x0, .reg64offset(X.sp, regStackOffset, nil))
                )
            case .OCall3(let dst, let fun, let arg0, let arg1, let arg2):
                let callTarget = ctx.callTargets.get(fun)
                
                assert(reg: dst, from: regKinds, matches: callTarget.ret.value)
                assert(reg: arg0, from: regKinds, matchesCallArg: 0, inFun: callTarget)
                assert(reg: arg1, from: regKinds, matchesCallArg: 1, inFun: callTarget)
                assert(reg: arg2, from: regKinds, matchesCallArg: 2, inFun: callTarget)

                let fnAddr = callTarget.entrypoint
                let regStackOffset = getRegStackOffset(regKinds, dst)
                let arg0StackOffset = getRegStackOffset(regKinds, arg0)
                let arg1StackOffset = getRegStackOffset(regKinds, arg1)
                let arg2StackOffset = getRegStackOffset(regKinds, arg2)

                mem.append(
                    PseudoOp.debugMarker("Call3 fn@\(fun)(\(arg0), \(arg1), \(arg2)) -> \(dst)"),
                    M1Op.ldr(X.x0, .reg64offset(X.sp, arg0StackOffset, nil)),
                    M1Op.ldr(X.x1, .reg64offset(X.sp, arg1StackOffset, nil)),
                    M1Op.ldr(X.x2, .reg64offset(X.sp, arg2StackOffset, nil)),
                    PseudoOp.mov(.x10, fnAddr),
                    M1Op.blr(.x10),
                    M1Op.str(X.x0, .reg64offset(X.sp, regStackOffset, nil))
                )
            // fatalError("Jumping to \(fn) for funIx \(fun)")
            // fatalError("OCall0")
            case .ONew(let dst):
                appendDebugPrintAligned4("Entering ONew", builder: mem)
                // LOOK AT: https://github.com/HaxeFoundation/hashlink/blob/284301f11ea23d635271a6ecc604fa5cd902553c/src/jit.c#L3263
                let typeToAllocate = requireTypeKind(reg: dst, from: regKinds)
                
                let allocFunc_jumpTarget: UnsafeRawPointer
                
                // Do we need to move the dst value in X0 for the alloc func (e.g. hl_alloc_dynobj
                // takes no args, but hl_alloc_obj does)
                var needX0 = true
                
                switch(typeToAllocate) {
                case .struct:
                fallthrough
                case .obj:
                    mem.append(
                        PseudoOp.debugMarker("Using hl_alloc_obj to allocate reg \(dst))")
                    )
                    allocFunc_jumpTarget = unsafeBitCast(LibHl._hl_alloc_obj, to: UnsafeRawPointer.self)
                case .dynobj:
                    mem.append(
                        PseudoOp.debugMarker("Using hl_alloc_dynobj to allocate reg \(dst))")
                    )
                    allocFunc_jumpTarget = unsafeBitCast(LibHl._hl_alloc_dynobj, to: UnsafeRawPointer.self)
                    needX0 = false
                case .virtual:
                    mem.append(
                        PseudoOp.debugMarker("Using hl_alloc_virtual to allocate reg \(dst))")
                    )
                    allocFunc_jumpTarget = unsafeBitCast(LibHl._hl_alloc_virtual, to: UnsafeRawPointer.self)
                default:
                fatalError("ONew not implemented for \(typeToAllocate)")
                }
                
                if needX0 {
                    let dstTypeAddr = requireTypeAddress(reg: dst, from: regs)
                    mem.append(
                        PseudoOp.debugMarker("Moving reg address \(dstTypeAddr) in .x0"),
                        PseudoOp.mov(.x0, dstTypeAddr)
                    )
                } else {
                    mem.append(
                        PseudoOp.debugMarker("Not moving reg \(dst) in x0 b/c alloc func doesn't need it")
                        )
                }
                
                let dstOffset = getRegStackOffset(regKinds, dst)
                
                mem.append(
                    PseudoOp.debugMarker("Moving alloc address in x1"),
                    PseudoOp.mov(.x1, allocFunc_jumpTarget),
                    PseudoOp.debugMarker("Jumping to the alloc func"),
                    PseudoOp.debugPrint(self, "Jumping to alloc func and storing result"),
                    M1Op.blr(.x1),
                    M1Op.str(X.x0, .reg64offset(X.sp, dstOffset, nil))
                )
                
                // let g = ctx.storage.globalResolver.get(Int(global!))
                // fatalError("No ONew yet. Allocating: \(typeToAllocate) -> global \(global) \(g)")
            case .OGetThis(let regDst, let fieldRef):
                guard regKinds.count > 0 && regDst < regKinds.count else {
                    fatalError("Not enough registers. Expected register 0 and \(regDst) to be available. Got: \(regKinds)")
                }
                let sourceObjectType = regs[0]
                assertKind(op, sourceObjectType.value.kind, .obj)
                
                guard case .obj(let objData) = sourceObjectType.value else {
                    fatalError("source object should be .obj but was \(sourceObjectType)")
                }
                
                guard fieldRef < objData.fields.count else {
                    fatalError("OGetThis: expected field \(fieldRef) to exist but got \(objData.fields)")
                } 
                let sourceObjectFieldType = objData.fields[fieldRef]
                
                assert(reg: regDst, from: regKinds, is: sourceObjectFieldType.value.type.value.kind)

                let objOffset = getRegStackOffset(regKinds, 0)
                let dstOffset = getRegStackOffset(regKinds, regDst)
                let fieldOffset = getFieldOffset(sourceObjectType.value, fieldRef)

                /* We should:
                - load x0 from reg0
                - access (x0 + 8) + fieldN (accounting for N-1 field sizes)
                - move to regN
                */

                mem.append(
                    PseudoOp.debugMarker("Loading x0 from SP + \(objOffset) (offset for reg0)"),
                    M1Op.ldr(X.x0, .reg64offset(X.sp, objOffset, nil)),     // point x0 to reg0 (i.e. obj)
                    PseudoOp.debugMarker("Moving x0 by \(fieldOffset) (offset for field \(fieldRef))"),
                    M1Op.ldr(X.x0, .reg64offset(X.x0, fieldOffset, nil)),   // move field (via offset) into x0
                    PseudoOp.debugMarker("Storing x0 at SP + \(dstOffset) (offset for reg\(regDst))"),
                    M1Op.str(X.x0, .reg64offset(X.sp, dstOffset, nil))      // store x0 back in sp
                )
            case .OInt(let dst, let iRef):
                assert(reg: dst, from: regKinds, is: .i32)
                let c = ctx.storage.int32Resolver.get(iRef)
                let regStackOffset = getRegStackOffset(regKinds, dst)
                mem.append(
                    PseudoOp.debugMarker(
                        "Mov \(c) into \(X.x0) and store in stack for HL reg \(iRef) at offset \(regStackOffset)"
                    ),
                    PseudoOp.mov(X.x0, c),
                    M1Op.str(X.x0, .reg64offset(X.sp, regStackOffset, nil))
                )
            case .OSetField(let objReg, let fieldRef, let srcReg):
                appendDebugPrintAligned4("Entering OSetField", builder: mem)
                let objRegKind = requireTypeKind(reg: objReg, from: regKinds)

                /**
                 field indexes are fetched from runtime_object,
                 and match what you might expect. E.g. for String:
                 
                    Example offsets for 0) bytes, 1) i32
                 
                 (lldb) p typePtr.pointee.obj.pointee.rt?.pointee.fields_indexes.pointee
                 (Int32?) $R0 = 8   // <----- first is 8 offset, on account of hl_type* at the top
                 (lldb) p typePtr.pointee.obj.pointee.rt?.pointee.fields_indexes.advanced(by: 1).pointee
                 (Int32?) $R1 = 16 // <----- second is 8 more offset, cause bytes is a pointer
                 (lldb)
                 
                    NOTE: keep alignment in mind. E.g. 0) int32 and 1) f64 will have 8 and 16 offsets respectively.
                        But 0) int32, 1) u8, 2) u8, 3) u16, 4) f64 will have 8, 12, 13, 14, 16 offsets respectively.
                 
                    See below:
                 
                 (lldb) p typePtr.pointee.obj.pointee.rt?.pointee.fields_indexes.advanced(by: 0).pointee
                 (Int32?) $R0 = 8
                 (lldb) p typePtr.pointee.obj.pointee.rt?.pointee.fields_indexes.advanced(by: 1).pointee
                 (Int32?) $R1 = 12
                 (lldb) p typePtr.pointee.obj.pointee.rt?.pointee.fields_indexes.advanced(by: 2).pointee
                 (Int32?) $R2 = 13
                 (lldb) p typePtr.pointee.obj.pointee.rt?.pointee.fields_indexes.advanced(by: 3).pointee
                 (Int32?) $R3 = 14
                 (lldb) p typePtr.pointee.obj.pointee.rt?.pointee.fields_indexes.advanced(by: 4).pointee
                 (Int32?) $R4 = 16
                 */
                
                switch(objRegKind) {
                case .obj: fallthrough
                case .struct:
                    // offset from obj address
                    let fieldOffset = requireFieldOffset(fieldRef: fieldRef, objIx: objReg, regs: regs)
                    
                    // offsets from .sp
                    let srcOffset = getRegStackOffset(regKinds, srcReg)
                    let objAddressOffset = getRegStackOffset(regKinds, objReg)
                    
                    mem.append(
                        M1Op.ldr(X.x0, .reg64offset(.sp, srcOffset, nil)),
                        M1Op.ldr(X.x1, .reg64offset(.sp, objAddressOffset, nil)),
                        M1Op.str(X.x0, .reg64offset(.x1, fieldOffset, nil))
                    )
                    // nop
//                        {
//                            hl_runtime_obj *rt = hl_get_obj_rt(dst->t);
//                            preg *rr = alloc_cpu(ctx, dst, true);
//                            copy_from(ctx, pmem(&p, (CpuReg)rr->id, rt->fields_indexes[o->p2]), rb);
//                        }
//                        break;
                default:
                    fatalError("OSetField not implemented for \(objRegKind)")
                }
            default: fatalError("Can't compile \(op.debugDescription)")
            }
        }

        // initialize targets for 'ret' jumps
        for var retTarget in retTargets { retTarget.stop(at: mem.byteSize) }

        if reservedStackBytes > 0 {
            mem.append(
                PseudoOp.debugMarker("Free \(reservedStackBytes) bytes"),
                (try M1Op._add(X.sp, X.sp, ByteCount(reservedStackBytes)))
            )
        }
        else {
            mem.append(
                PseudoOp.debugMarker(
                    "Skipping freeing stack because \(reservedStackBytes) bytes were needed"
                )
            )
        }

        appendEpilogue(builder: mem)
        mem.append(.ret)
    }
}
