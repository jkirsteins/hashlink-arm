// x0 through x7
private let ARG_REGISTER_COUNT = 8
extension M1Compiler {
    // Get offset into stack for a given register. This should skip void registers
    func getRegStackOffset(_ regs: [HLType], args: [HLType], reg: Int) -> ByteCount {
        let base: ByteCount
        let ptr: ArraySlice<HLType>
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
        for ix in (skip..<reg) { result += ptr[ix].neededBytes }
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
    func calcStackArgReq(regs unfilteredRegs: [HLType], args unfilteredArgs: [HLType])
        -> (Int16, [HLType])
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
        print("args needing space", stackArgs.map { $0.debugName })
        let result = stackArgs.reduce(0) { $0 + Int16($1.neededBytes) }
        return (roundUpStackReservation(result), stackArgs)
    }

    /*
    First 7 args are in registers [x0;x7]. Others are on the stack.
    Stack should be extended to account for data which is in registers.
    Subsequently data should be moved from regs to stack, to enable use of all registers.
*/
    @discardableResult func appendStackInit(
        _ unfilteredRegs: [HLType],
        args unfilteredArgs: [HLType],
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
            print("Inc offset by \(reg.neededBytes) from \(reg)")
            offset += reg.neededBytes
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

class M1Compiler {
    /*
    stp    x29, x30, [sp, #-16]!
    mov    x29, sp
    */let emitter = EmitterM1()

    let stripDebugMessages: Bool

    func requireType(reg: Reg, from resolvedRegs: [HLType]) -> HLType {
        guard reg < resolvedRegs.count else {
            fatalError("requireType(reg:from:): Not enough registers. Expected \(reg) to be available. Got: \(resolvedRegs.map { $0.debugName })")
        }

        return resolvedRegs[Int(reg)]
    }

    func assert(reg: Reg, from: [HLType], matchesCallArg argReg: Reg, inFun funType: HLType) {
        guard from.count > reg else {
            fatalError(
                "Register with index \(reg) does not exist. Available registers: \(from)."
            )
        }
        guard case .fun(let funData) = funType else {
            fatalError("Expected HLNative to have .fun type (got \(funType)")
        }
        guard argReg < funData.args.count else {
            fatalError("Expected HLNative to have a valid call reg ix \(argReg) but args are is \(funData.args). Fun: \(funData)")
        }
        guard from[Int(reg)] == funData.args[Int(argReg)].value else {
            fatalError(
                "Register \(reg) expected to be \(funType) but is \(from[Int(reg)])"
            )
        }
    }

    func assert(reg: Reg, from: [HLType], matchesReturn funType: Resolvable<HLType>) {
        assert(reg: reg, from: from, matchesReturn: funType.value)
    }

    func assert(reg: Reg, from: [HLType], matchesReturn funType: HLType) {
        guard from.count > reg else {
            fatalError(
                "Register with index \(reg) does not exist. Available registers: \(from)."
            )
        }
        guard case .fun(let funData) = funType else {
            fatalError("Expected HLNative to have .fun type (got \(funType)")
        }
        guard from[Int(reg)] == funData.ret.value else {
            fatalError(
                "Register \(reg) expected to be \(funType) but is \(from[Int(reg)])"
            )
        }
    }

    func assert(reg: Reg, from: [HLType], is target: HLType) {
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

    func getRegStackOffset(_ regs: [HLType], _ ix: Reg) -> ByteCount {
        var result = ByteCount(0)
        for i in 0..<ix { result += regs[Int(i)].neededBytes }
        return result
    }

    func getFieldOffset(_ obj: HLType, _ field: Int) -> ByteCount {
        guard case .obj(let objData) = obj else {
            fatalError("Field offset only supports .obj (got \(obj.debugName))")
        }
        var startOffset = ByteCount(8)  // hl_type* pointer
        for ix in 0..<field {
            let fieldToSkip = objData.fields[ix].type.value.neededBytes
            startOffset += fieldToSkip
        }
        return startOffset
    }

    func push(to buffer: ByteBuffer, _ ops: M1Op...) throws {
        for op in ops { buffer.push(try emitter.emit(for: op)) }
    }

    /// Deprecated.
    /// - Parameters:
    ///   - findex: 
    ///   - mem: 
    ///   - ctx: 
    /// - Returns: 
    @discardableResult
    func compile(findex: Int32, into mem: OpBuilder, ctx: JitContext) throws
        -> HLCompiledFunction
    {
        try compile(findex: findex, into: mem)
    }

    /// Will compile and update the JIT context with the functions addresses.
    /// - Parameters:
    ///   - findex: 
    ///   - mem: 
    /// - Returns: 
    @discardableResult
    func compile(findex: Int32, into mem: OpBuilder) throws
        -> HLCompiledFunction
    {
        let ctx = mem.ctx

        guard let fun = ctx.compiledFunctions.first(where: { $0.findex == findex })
        else {
            throw GlobalError.invalidValue(
                "Function \(findex) not found in the whole functions table."
            )
        }

        guard !fun.memory.hasUsableValue else {
            print("Memory tested \(fun.memory)")
            throw GlobalError.invalidOperation(
                "Function \(findex) has already been compiled and assigned an address."
            )
        }

        // grab it before it changes from prologue
        // let memory = mem.getDeferredPosition()
        let resolvedRegs = fun.regs.map { $0.value }

        let relativeBaseAddr = mem.getDeferredPosition()
        fun.memory.update(from: relativeBaseAddr)

        // if we need to return early, we jump to these
        var retTargets: [RelativeDeferredOffset] = []

        print("Compiling function \(fun.findex) at deferred address \(fun.memory)")
        print("OPS: \n--" + fun.ops.map { $0.debugDescription }.joined(separator: "\n--"))

        guard case .fun(let funData) = fun.type.value else {
            fatalError("HLCompiledFunction type should be function")
        }

        let resolvedArgs = funData.args.map { $0.value }

        mem.append(PseudoOp.debugMarker("==> STARTING FUNCTION \(fun.findex)"))
        appendPrologue(builder: mem)
        let reservedStackBytes = try appendStackInit(
            resolvedRegs,
            args: resolvedArgs,
            builder: mem
        )

        appendDebugPrintAligned4(
            "Entering function \(fun.findex)@\(relativeBaseAddr.offsetFromBase)",
            builder: mem
        )
        for op in fun.function.ops {
            appendDebugPrintAligned4("Executing \(op.debugDescription)", builder: mem)

            switch op {
            case .ORet(let dst):
                // store
                let regStackOffset = getRegStackOffset(resolvedRegs, dst)
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

            case .OCall0(let dst, let fun):
                let callTarget = try ctx.wft.get(fun)
                if let callTargetNative = callTarget as? HLNative {
                    assert(
                        reg: dst,
                        from: resolvedRegs,
                        matchesReturn: callTargetNative.type
                    )
                }
                else if let callTargetFun = callTarget as? HLCompiledFunction {
                    assert(
                        reg: dst,
                        from: resolvedRegs,
                        matchesReturn: callTargetFun.type
                    )
                }
                else {
                    fatalError("Unknown call target \(callTarget)")
                }

                let fnAddr = try ctx.wft.getAddr(fun)
                let regStackOffset = getRegStackOffset(resolvedRegs, dst)

                mem.append(
                    PseudoOp.debugMarker("Call0 fn@\(fun) -> \(dst)"),
                    PseudoOp.mov(.x10, fnAddr),
                    M1Op.blr(.x10),
                    M1Op.str(X.x0, .reg64offset(X.sp, regStackOffset, nil))
                )
            case .OCall3(let dst, let fun, let arg0, let arg1, let arg2):
                let callTarget = try ctx.wft.get(fun)
                let callTargetType: HLType
                if let callTargetNative = callTarget as? HLNative {
                    callTargetType = callTargetNative.type.value
                }
                else if let callTargetFun = callTarget as? HLCompiledFunction {
                    callTargetType = callTargetFun.type.value
                }
                else {
                    fatalError("OCall3 unknown call target \(callTarget)")
                }

                assert(reg: dst, from: resolvedRegs, matchesReturn: callTargetType)
                assert(reg: arg0, from: resolvedRegs, matchesCallArg: 0, inFun: callTargetType)
                assert(reg: arg1, from: resolvedRegs, matchesCallArg: 1, inFun: callTargetType)
                assert(reg: arg2, from: resolvedRegs, matchesCallArg: 2, inFun: callTargetType)

                let fnAddr = try ctx.wft.getAddr(fun)
                let regStackOffset = getRegStackOffset(resolvedRegs, dst)
                let arg0StackOffset = getRegStackOffset(resolvedRegs, arg0)
                let arg1StackOffset = getRegStackOffset(resolvedRegs, arg1)
                let arg2StackOffset = getRegStackOffset(resolvedRegs, arg2)

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
                // LOOK AT: https://github.com/HaxeFoundation/hashlink/blob/284301f11ea23d635271a6ecc604fa5cd902553c/src/jit.c#L3263
                let typeToAllocate = requireType(reg: dst, from: resolvedRegs)
                fatalError("No ONew yet. Allocating: \(typeToAllocate)")
            case .OGetThis(let regDst, let fieldRef):
                guard resolvedRegs.count > 0 && regDst < resolvedRegs.count else {
                    fatalError("Not enough registers. Expected register 0 and \(regDst) to be available. Got: \(resolvedRegs)")
                }
                let sourceObjectType = resolvedRegs[0]
                guard case .obj(let sourceObjData) = sourceObjectType else {
                    fatalError("OGetThis: source object data must be obj but got \(sourceObjectType)")
                }
                guard fieldRef < sourceObjData.fields.count else {
                    fatalError("OGetThis: expected field \(fieldRef) to exist but got \(sourceObjData.fields)")
                }
                let sourceObjectFieldType = sourceObjData.fields[fieldRef]
                
                assert(reg: regDst, from: resolvedRegs, is: sourceObjectFieldType.type.value)

                let objOffset = getRegStackOffset(resolvedRegs, 0)
                let dstOffset = getRegStackOffset(resolvedRegs, regDst)
                let fieldOffset = getFieldOffset(sourceObjectType, fieldRef)

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
                assert(reg: dst, from: resolvedRegs, is: .i32)
                let c = ctx.storage.int32Resolver.get(iRef)
                let regStackOffset = getRegStackOffset(resolvedRegs, dst)
                mem.append(
                    PseudoOp.debugMarker(
                        "Mov \(c) into \(X.x0) and store in stack for HL reg \(iRef) at offset \(regStackOffset)"
                    ),
                    PseudoOp.mov(X.x0, c),
                    M1Op.str(X.x0, .reg64offset(X.sp, regStackOffset, nil))
                )
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

        return fun
    }
}
