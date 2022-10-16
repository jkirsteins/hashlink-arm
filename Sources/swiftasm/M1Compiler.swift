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
    func calcStackArgReq(regs unfilteredRegs: [HLType], args unfilteredArgs: [HLType]) -> (Int16, [HLType]) {
        let regs = unfilteredRegs.filter { $0 != .void }
        let args = unfilteredArgs.filter { $0 != .void }
        guard regs.prefix(args.count) == args.prefix(args.count) else {
            fatalError("Args must match the first registers (got \(args) and \(regs) respectively)")
        }
        let stackArgs = Array(/* regs should be aligned with args */regs.dropFirst(args.count) + args.prefix(ARG_REGISTER_COUNT))
        print("args needing space", stackArgs)
        let result = stackArgs.reduce(0) { $0 + Int16($1.neededBytes) }
        return (roundUpStackReservation(result), stackArgs)
    }

    /*
    First 7 args are in registers [x0;x7]. Others are on the stack.
    Stack should be extended to account for data which is in registers.
    Subsequently data should be moved from regs to stack, to enable use of all registers.
*/
    @discardableResult
    func appendStackInit(
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
            throw GlobalError.expectationViolation(
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
            builder.append(
                PseudoOp.debugMarker("No extra stack space needed")
            )    
            return neededExtraStackSize
        }

        builder.append(
            PseudoOp.debugMarker("Reserving \(neededExtraStackSize) bytes for stack"),
            M1Op.sub(X.sp, X.sp, try .i(neededExtraStackSize)))

        var offset: ByteCount = 0
        for (ix, reg) in stackArgs.enumerated() {
            builder.append(
                PseudoOp.debugMarker("Moving \(Register64(rawValue: UInt8(ix))!) to \(offset)"),
                M1Op.str(
                    Register64(rawValue: UInt8(ix))!,
                    .reg64offset(Register64.sp, offset, nil)
                )
            )
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
            M1Op.ldp((.x29_fp, .x30_lr), .reg64offset(.sp, 16, .post)))
    }
}

class M1Compiler {
    /*
    stp    x29, x30, [sp, #-16]!
    mov    x29, sp
    */let emitter = EmitterM1()

    let stripDebugMessages: Bool

    func assert(reg: Reg, from: [HLType], is target: HLType) {
        guard from.count > reg else {
            fatalError("Register with index \(reg) does not exist. Available registers: \(from).")
        }
        guard target == from[Int(reg)] else {
            fatalError("Register \(reg) expected to be \(target) but is \(from[Int(reg)])")
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
            var fieldToSkip = objData.fields[ix].type.value.neededBytes
            startOffset += fieldToSkip
        }
        return startOffset
    }

    func push(to buffer: ByteBuffer, _ ops: M1Op...) throws {
        for op in ops { buffer.push(try emitter.emit(for: op)) }
    }

    func compileFake(native: HLFunction, into mem: OpBuilder, ctx: JitContext) throws
        -> HLCompiledFunction
    {
        let relativeBaseAddr = mem.getDeferredPosition()
        let currentFuncAddr = ctx.wft.getAddr(Int(native.findex))
        currentFuncAddr.update(from: relativeBaseAddr)

        mem.append(
            PseudoOp.mov(X.x0, 123),
            M1Op.ret
        )

        let result = HLCompiledFunction(function: native, memory: currentFuncAddr)

        // Register as we go.
        ctx.wft.functions.table.append(result)

        return result
    }

    /*
    Will compile and update the JIT context with new known deferred addresses.
    */
    func compile(native: HLFunction, into mem: OpBuilder, ctx: JitContext) throws
        -> HLCompiledFunction
    {
        // grab it before it changes from prologue
        // let memory = mem.getDeferredPosition()
        let resolvedRegs = native.regs.map { $0.value }

        let relativeBaseAddr = mem.getDeferredPosition()
        let currentFuncAddr = ctx.wft.getAddr(Int(native.findex))
        currentFuncAddr.update(from: relativeBaseAddr)

        // if we need to return early, we jump to these
        var retTargets: [RelativeDeferredOffset] = []

        print(
            "Compiling function \(native.findex) at deferred address \(currentFuncAddr)"
        )

        guard case .fun(let funData) = native.type.value else {
            fatalError("HLFunction type should be function")
        }

        let resolvedArgs = funData.args.map { $0.value }

        appendPrologue(builder: mem)
        let reservedStackBytes = try appendStackInit(resolvedRegs, args: resolvedArgs, builder: mem)

        appendDebugPrintAligned4(
            "Entering function \(native.findex)@\(relativeBaseAddr.offsetFromBase)",
            builder: mem
        )
        for op in native.ops {
            appendDebugPrintAligned4("Executing \(op.debugDescription)", builder: mem)

            switch op {
            case .ORet(let dst):
                
                // store
                let regStackOffset = getRegStackOffset(resolvedRegs, dst)
                mem.append(
                    PseudoOp.debugMarker("Returning stack offset \(regStackOffset)"),
                    M1Op.ldr(X.x0, .reg64offset(.sp, regStackOffset, nil)))

                // jmp to end
                var retTarget = RelativeDeferredOffset()
                retTarget.start(at: mem.byteSize)
                retTargets.append(retTarget)
                mem.append(
                    PseudoOp.debugMarker("Jumping to epilogue"),
                    M1Op.b(retTarget))

            case .OCall0(let dst, let fun):
                let fnAddr = try ctx.wft.getAddr(fun)
                mem.append(PseudoOp.mov(.x10, fnAddr), M1Op.blr(.x10))
            // fatalError("Jumping to \(fn) for funIx \(fun)")
            // fatalError("OCall0")
            case .ONew: fatalError("No ONew yet. What's up with types?")
            case .OGetThis(let reg1, let fieldRef):
                let regStackOffset = getRegStackOffset(resolvedRegs, reg1)
                let sourceObjectType = resolvedRegs[0]
                let fieldOffset = getFieldOffset(sourceObjectType, fieldRef)
                // TODO: is x0 loaded from method args?
                mem.append(
                    try ._add(X.x1, X.x0, fieldOffset)  // point x1 to field

                )

                print("Reg stack offset for reg\(reg1) is \(regStackOffset)")
                fatalError("Fetching \(fieldRef) for \(sourceObjectType)")
            case .OInt(let dst, let iRef):
                assert(reg: dst, from: resolvedRegs, is: .i32)
                let c = ctx.storage.int32Resolver.get(iRef)
                let regStackOffset = getRegStackOffset(resolvedRegs, dst)
                
                mem.append(
                    PseudoOp.debugMarker("Mov \(c) into \(X.x0) and store in stack for HL reg \(iRef) at offset \(regStackOffset)"),
                    PseudoOp.mov(X.x0, c),
                    M1Op.str(X.x0, .reg64offset(X.sp, regStackOffset, nil))
                )
            default: fatalError("Can't compile \(op.debugDescription)")
            }
        }

        // initialize targets for 'ret' jumps
        for var retTarget in retTargets { 
            retTarget.stop(at: mem.byteSize) 
        }

        if reservedStackBytes > 0 {
            mem.append(
                PseudoOp.debugMarker("Free \(reservedStackBytes) bytes"),
                (try M1Op._add(X.sp, X.sp, ByteCount(reservedStackBytes)))
            )
        } else {
            mem.append(
                PseudoOp.debugMarker("Skipping freeing stack because \(reservedStackBytes) bytes were needed")
            )
        }

        appendEpilogue(builder: mem)
        mem.append(.ret)
        

        let result = HLCompiledFunction(function: native, memory: currentFuncAddr)

        // Register as we go.
        ctx.wft.functions.table.append(result)

        return result
    }
}
