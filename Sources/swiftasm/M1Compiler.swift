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
            let (stackRes, _) = calcStackArgReq(args)
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

    func calcStackArgReq(_ args: [HLType]) -> (Int16, ArraySlice<HLType>) {
        let stackArgs = args.prefix(ARG_REGISTER_COUNT)
        let result = stackArgs.reduce(0) { $0 + Int16($1.neededBytes) }
        return (roundUpStackReservation(result), stackArgs)
    }

    /*
    First 7 args are in registers [x0;x7]. Others are on the stack.
    Stack should be extended to account for data which is in registers.
    Subsequently data should be moved from regs to stack, to enable use of all registers.
*/
    func appendStackInit(
        _ unfilteredRegs: [HLType],
        args unfilteredArgs: [HLType],
        builder: OpBuilder
    ) throws {
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
        let args = unfilteredArgs.filter { $0 != .void }
        // only the first args need stack space, because we want to move them from the registers
        // into stack
        let (neededExtraStackSize, stackArgs) = calcStackArgReq(args)

        guard neededExtraStackSize > 0 else { return }

        builder.append(.sub(X.sp, X.sp, try .i(neededExtraStackSize)))

        var offset: ByteCount = 0
        for (ix, reg) in stackArgs.enumerated() {
            builder.append(
                M1Op.str(
                    Register64(rawValue: UInt8(ix))!,
                    .reg64offset(Register64.sp, offset, nil)
                )
            )
            offset += reg.neededBytes
        }
    }

    func appendDebugPrintAligned4(_ val: String, builder: OpBuilder) {
        var adr = RelativeDeferredOffset()
        var jmpTarget = RelativeDeferredOffset()
        let str = "[jitdebug] \(val)"
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
            .stp((.x29_fp, .x30_lr), .reg64offset(.sp, -16, .pre)),
            .movr64(.x29_fp, .sp)
        )
    }

    func appendEpilogue(builder: OpBuilder) {
        builder.append(.ldp((.x29_fp, .x30_lr), .reg64offset(.sp, 16, .post)))
    }
}

class M1Compiler {
    /*
    stp    x29, x30, [sp, #-16]!
    mov    x29, sp
    */let emitter = EmitterM1()

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

        print(
            "Compiling function \(native.findex) at deferred address \(currentFuncAddr)"
        )

        // currentFuncAddr.wrappedValue =

        appendPrologue(builder: mem)

        guard case .fun(let funData) = native.type.value else {
            fatalError("HLFunction type should be function")
        }

        let resolvedArgs = funData.args.map { $0.value }

        try appendStackInit(resolvedRegs, args: resolvedArgs, builder: mem)
        appendDebugPrintAligned4(
            "Entering function \(native.findex)@\(relativeBaseAddr.offsetFromBase)\n",
            builder: mem
        )
        for op in native.ops {
            appendDebugPrintAligned4("Executing \(op.debugDescription)\n", builder: mem)

            switch op {
            case .ORet(let dst):
                mem.append(

                    )
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
            default: fatalError("Can't compile \(op.debugDescription)")
            }
        }

        appendEpilogue(builder: mem)

        // tmp return
        mem.append(.movz64(.x0, UInt16(native.findex), ._0), .ret)

        // mem.appendSystemExit(44)

        let result = HLCompiledFunction(function: native, memory: currentFuncAddr)

        // Register as we go.
        ctx.wft.functions.table.append(result)

        return result

        /*
        Epilogue (excluding return)
        ldp x29, x30, [sp], 16
        */

        /*
        ldr    X1, [x0, #0]     // point to field
        ldr    x2, #1           // length
        mov    x0, #1           // 1 = stdout
        mov    X16, #4          // Unix write system call
        svc    #0x80            // Call kernel to output the string
        */

        // tmp dummy
        // x0 contains address for string obj
        // string obj has first field bytes
        // let offset = 0
        // EmitterM1.emit(for: .ldr(._64(.x0, 1)))

        // return result
    }
}
