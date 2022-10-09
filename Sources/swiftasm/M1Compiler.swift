class M1Compiler {
    func compile(native: HLFunction) -> ByteBuffer {
        let result = ByteBuffer(incrementSize: 32)

        /*
        Prologue
        stp    x29, x30, [sp, #-16]!
        mov    x29, sp
        */

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

        guard result.buffer.count > 0 else { fatalError("Empty compilation result") }
        return result
    }
}
