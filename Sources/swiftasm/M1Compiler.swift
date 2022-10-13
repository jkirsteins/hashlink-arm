class M1Compiler {
    /*
    stp    x29, x30, [sp, #-16]!
    mov    x29, sp
    */
    let emitter = EmitterM1()

    func push(to buffer: ByteBuffer, _ ops: M1Op...) throws {
        for op in ops { buffer.push(try emitter.emit(for: op)) }
    }

    func compile(native: HLFunction, into mem: OpBuilder) throws -> HLCompiledFunction {
        
        // grab it before it changes from prologue
        let memory = mem.getDeferredPosition()

        print("Compiling function \(native.findex) at \(memory.offsetFromBase)")
        
        try mem.appendPrologue()
        mem.appendDebugPrintAligned4("==-(from jit)-==> Executing function \(native.findex)@\(memory.offsetFromBase)\n")
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

        for op in native.ops {
            switch op {
            default: fatalError("Can't compile \(op.debugDescription)")
            }
        }

        // return result
    }
}
