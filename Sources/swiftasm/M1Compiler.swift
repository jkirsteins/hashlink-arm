class M1Compiler {
    /*
    stp    x29, x30, [sp, #-16]!
    mov    x29, sp
    */
    let emitter = EmitterM1()

    func getRegStackOffset(_ regs: [HLType], _ ix: Reg) -> ByteCount {
        var result = ByteCount(0)
        for i in 0..<ix {
            result += regs[Int(i)].neededBytes
        }
        return result
    }

    func getFieldOffset(_ obj: HLType, _ field: Int) -> ByteCount {
        guard case .obj(let objData) = obj else { 
            fatalError("Field offset only supports .obj (got \(obj.debugName))") 
        }
        var startOffset = ByteCount(8) // hl_type* pointer
        for ix in 0..<field {
            var fieldToSkip = objData.fields[ix].type.value.neededBytes
            startOffset += fieldToSkip
        }
        return startOffset
    }

    func push(to buffer: ByteBuffer, _ ops: M1Op...) throws {
        for op in ops { buffer.push(try emitter.emit(for: op)) }
    }

    func compile(
        native: HLFunction, 
        into mem: OpBuilder, 
        ctx: JitContext) throws -> HLCompiledFunction {
        
        // grab it before it changes from prologue
        let memory = mem.getDeferredPosition()
        let resolvedRegs = native.regs.map { $0.value }

        print("Compiling function \(native.findex) at \(memory.offsetFromBase)")
        
        try mem.appendPrologue()

        try mem.appendStackReservation(resolvedRegs)
        
        mem.appendDebugPrintAligned4("Entering function \(native.findex)@\(memory.offsetFromBase)\n")
        
        for op in native.ops {
            mem.appendDebugPrintAligned4("Executing \(op.debugDescription)\n")

            switch op {
                case .OCall0(let dst, let fun):
                    let fnAddr = try ctx.wft.getAddr(fun)
                    // mem.append(
                    //     PseudoOp.mov(.x10, fnAddr.wrappedValue),
                    //     .blr(.x10)
                    // )
                    // fatalError("Jumping to \(fn) for funIx \(fun)")
                    fatalError("OCall0")
                case .ONew: fatalError("No ONew yet. What's up with types?")
                case .OGetThis(let reg1, let fieldRef):
                    let regStackOffset = getRegStackOffset(resolvedRegs, reg1)
                    let sourceObjectType = resolvedRegs[0]
                    let fieldOffset = getFieldOffset(sourceObjectType, fieldRef)
                    
                    // TODO: is x0 loaded from method args?
                    mem.append(
                        try ._add(X.x1, X.x0, fieldOffset) // point x1 to field

                    )

                    print("Reg stack offset for reg\(reg1) is \(regStackOffset)")
                    fatalError("Fetching \(fieldRef) for \(sourceObjectType)")
                default: fatalError("Can't compile \(op.debugDescription)")
            }
        }

        try mem.appendEpilogue()

        // tmp return 
        mem.append(
            .movz64(.x0, UInt16(native.findex), ._0),
            .ret
        )

        // mem.appendSystemExit(44)

        return HLCompiledFunction(
            function: native, 
            memory: memory)

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
