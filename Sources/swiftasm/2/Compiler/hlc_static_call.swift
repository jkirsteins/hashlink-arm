import Foundation

extension M1Compiler2 {
    /// Result:
    ///     - Should return whatever the callee closure returns (if anything)
    ///
    /// C signature:
    ///
    ///     void *hlc_static_call( void *fun, hl_type *t, void **args, vdynamic *out ) {
    static let hlc_static_call: (@convention(c) (_ fun: OpaquePointer, _ t: OpaquePointer, _ args: OpaquePointer, _ out: OpaquePointer)->(OpaquePointer?)) = {
        funPtr, tPtr, argPtr, outPtr in
        
        let t: hlTypePointer = .init(tPtr)
        let out: UnsafeMutablePointer<vdynamic> = .init(outPtr)

        guard let funProvider = t.funProvider else {
            fatal("hlc_static_call fails, invalid type passed", logger)
        }

        /* we'll JIT the call
         *
         * x20 will hold the arg offset
         * x21 will hold the target func offset
         */
        let mem = CpuOpBuffer()
        
        // no-stack-prologue
        mem.append(
            M1Op.subImm12(X.sp, X.sp, Imm12Lsl12(16)),
            M1Op.stp((X.x29_fp, X.x30_lr), .reg64offset(.sp, 0, nil))
        )
        
        appendDebugPrintAligned4("[hlc_static_call] entering...", builder: mem)
        
        // set x20/x21
        mem.append(
            PseudoOp.mov(X.x20, argPtr),
            
            // test
            M1Op.ldr(X.x20, .reg(X.x20, .imm(0, nil))),
            
            PseudoOp.mov(X.x21, funPtr)
        )
        
        appendDebugPrintAligned4("[hlc_static_call] entering 2...", builder: mem)

        var offset: ByteCount = 0
        var gpRegisterIx: Int = 0
        var fpRegisterIx: Int = 0
        for (argIx, arg) in funProvider.argsProvider.enumerated() {
            guard !isFP(vreg: Reg(argIx), kinds: funProvider.argsProvider) else {
                fatal("floating point arguments not implemented", logger)
            }

            // only general purpose registers from here on
            guard gpRegisterIx < ARG_REGISTER_COUNT else {
                fatal("hlc_static_call does not support more than \(ARG_REGISTER_COUNT) arguments", logger)
            }
            defer { gpRegisterIx += 1 }

            let gpRegister = Register64(rawValue: UInt8(gpRegisterIx))!

            M1Compiler2.appendLoad(
                reg: gpRegister,
                as: Reg(argIx),
                fromAddressFrom: X.x20,
                offsetFromAddress: offset,
                kinds: funProvider.argsProvider,
                mem: mem)

            offset += arg.hlRegSize
        }

        appendDebugPrintRegisterAligned4(X.x21, prepend: "[hlc_static_call] jumping...", builder: mem)
        
        mem.append(
            M1Op.blr(X.x21)
        )
        
        appendDebugPrintAligned4("[hlc_static_call] jumped...", builder: mem)
        
        // no-stack-epilogue
        mem.append(
            M1Op.ldp((X.x29_fp, X.x30_lr), .reg64offset(.sp, 0, nil)),
            M1Op.addImm12(X.sp, X.sp, Imm12Lsl12(16)),
            M1Op.ret
        )

        // JIT
        let emittedBytes = try! mem.ops.flatMap { try $0.emit() }

        let execMem = mmap(
            nil,
            Int(mem.byteSize),
            PROT_WRITE | PROT_EXEC,
            MAP_ANONYMOUS | MAP_PRIVATE | MAP_JIT,
            -1,
            0
        )

        guard let execMem = execMem else {
            fatal("mmap returned nil: \(errno)", logger)
        }

        if execMem == MAP_FAILED {
            fatal("mmap failed: \(errno)", logger)
        }

        Swift.assert(mem.byteSize == Int64(emittedBytes.count))

        pthread_jit_write_protect_np(0)
        memcpy(execMem, emittedBytes, emittedBytes.count)
        pthread_jit_write_protect_np(1)
        // End JIT

        switch(funProvider.retProvider.kind) {
        case .void:
            return nil
        case .f32:
            let _jitFunc = unsafeBitCast(execMem, to: (@convention(c) ()->Float32).self)
            let result = _jitFunc()
            
            vdynamic.set(f: result, in: out)
            Swift.assert(out.pointee.f == result)
        case .f64:
            let _jitFunc = unsafeBitCast(execMem, to: (@convention(c) ()->Float64).self)
            let result = _jitFunc()
            
            vdynamic.set(d: result, in: out)
            Swift.assert(out.pointee.d == result)
        case .u8, .bool:
            let _jitFunc = unsafeBitCast(execMem, to: (@convention(c) ()->UInt8).self)
            let result = _jitFunc()
            
            vdynamic.set(ui8: result, in: out)
            Swift.assert(out.pointee.ui8 == result)
        case .u16:
            let _jitFunc = unsafeBitCast(execMem, to: (@convention(c) ()->UInt16).self)
            let result = _jitFunc()
            
            vdynamic.set(ui16: result, in: out)
            Swift.assert(out.pointee.ui16 == result)
        case .i32:
            let _jitFunc = unsafeBitCast(execMem, to: (@convention(c) ()->Int32).self)
            let result = _jitFunc()
            
            vdynamic.set(i: result, in: out)
            Swift.assert(out.pointee.i == result)
        case .i64:
            let _jitFunc = unsafeBitCast(execMem, to: (@convention(c) ()->Int64).self)
            let result = _jitFunc()
            
            vdynamic.set(i64: result, in: out)
            Swift.assert(out.pointee.i == result)
        case .obj:
            let _jitFunc = unsafeBitCast(execMem, to: (@convention(c) ()->OpaquePointer).self)
            let result = _jitFunc()
            return result
        default:
            fatal("hlc_static_call does not support return type \(funProvider.retProvider.kind)", logger)
        }
        
        let res: OpaquePointer = withUnsafeMutablePointer(to: &out.pointee.union) { res in
            return .init(res)
        }
        return res
    }
}
