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
         * x11 will hold the arg offset
         * x10 will hold the target func offset
         */
        let mem = CpuOpBuffer()
        
        // no-stack-prologue
        mem.append(
            M1Op.subImm12(X.sp, X.sp, try! Imm12Lsl12(16, signed: false)),
            M1Op.stp((X.x29_fp, X.x30_lr), .reg64offset(.sp, 0, nil))
        )
        
        // set x10/x11
        mem.append(
            // argPtr is `void *vargs[10]` from hl_wrapper_call (i.e. `void**`)
            // for non-pointer values, we need to advance, then dereference
            // for pointer values, we need to advance, then use that address
            PseudoOp.mov(X.x11, argPtr),
            
            PseudoOp.mov(X.x10, funPtr)
        )
        
        
        #if DEBUG
        appendDebugPrintRegisterAligned4(X.x11, prepend: "base in hlc_static_call", builder: mem)
        #endif

        var offset: ByteCount = 0
        var gpRegisterIx: RegisterRawValue = 0
        var fpRegisterIx: RegisterRawValue = 0
        
        // args are memory addresses, see `hl_wrapper_call` which prepares these
        for (argIx, arg) in funProvider.argsProvider.enumerated() {
            let regToUseIx: RegisterRawValue
            if isFP(vreg: Reg(argIx), kinds: funProvider.argsProvider) {
                regToUseIx = fpRegisterIx
            } else {
                regToUseIx = gpRegisterIx
            }
            
            // only general purpose registers from here on
            guard regToUseIx < ARG_REGISTER_COUNT else {
                fatal("hlc_static_call does not support more than \(ARG_REGISTER_COUNT) arguments of a single register type", logger)
            }
            defer {
                if isFP(vreg: Reg(argIx), kinds: funProvider.argsProvider) {
                    fpRegisterIx += 1
                } else {
                    gpRegisterIx += 1
                }
            }
            
            let argKind = M1Compiler2.requireTypeKind(reg: Reg(argIx), from: funProvider.argsProvider)
            let reg = getRegister(regToUseIx, kind: argKind)

            if argKind.isPointer {
                // hold address
                guard let regGP = reg.i else {
                    fatalError("Pointer values must use GP registers")
                }
                mem.append(
                    M1Op.add(regGP, X.x11, .imm(offset, nil)),
                    M1Op.ldr(regGP, .reg(regGP, .imm(0, nil)))
                )
            } else {
                // hold value
                M1Compiler2.appendLoad(
                    reg: X.x12,
                    as: 0,
                    addressRegister: X.x11,
                    offset: offset,
                    kinds: [HLTypeKind.dyn],    // force an address load
                    mem: mem)
#if DEBUG
                M1Compiler2.appendDebugPrintRegisterAligned4(X.x11, prepend: "tdebug LOADING arg for \(reg) base", builder: mem)
                M1Compiler2.appendDebugPrintRegisterAligned4(X.x12, prepend: "tdebug LOADING arg for \(reg) address (offset \(offset))", builder: mem)
#endif
                M1Compiler2.appendLoad(
                    regToUseIx,
                    as: Reg(argIx),
                    addressRegister: X.x12,
                    offset: 0,
                    kinds: funProvider.argsProvider,
                    mem: mem)
#if DEBUG
                M1Compiler2.appendDebugPrintRegisterAligned4(regToUseIx, kind: argKind, prepend: "tdebug LOADED arg for \(reg)", builder: mem)
#endif
            }
            
            #if DEBUG
            M1Compiler2.appendDebugPrintRegisterAligned4(regToUseIx, kind: argKind, prepend: "hl_dyn_call_obj arg \(reg) in hlc_static_call (offset \(offset))", builder: mem)
            #endif
            
            offset += Int64(MemoryLayout<OpaquePointer>.stride)    // args are memory addresses
        }

#if DEBUG
        appendDebugPrintRegisterAligned4(X.x10, prepend: "[hlc_static_call] jumping...", builder: mem)
#endif
        
        mem.append(
            M1Op.blr(X.x10)
        )
        
#if DEBUG
        appendDebugPrintAligned4("[hlc_static_call] jumped...", builder: mem)
#endif
        
        // no-stack-epilogue
        mem.append(
            M1Op.ldp((X.x29_fp, X.x30_lr), .reg64offset(.sp, 0, nil)),
            M1Op.addImm12(X.sp, X.sp, try! Imm12Lsl12(16, signed: false)),
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

        switch(funProvider.retProvider.kind, funProvider.retProvider.kind.isPointer) {
        case (.void, false):
            return nil
        case (.f32, false):
            let _jitFunc = unsafeBitCast(execMem, to: (@convention(c) ()->Float32).self)
            let result = _jitFunc()
            
            vdynamic.set(f: result, in: out)
            Swift.assert(out.pointee.f == result)
        case (.f64, false):
            let _jitFunc = unsafeBitCast(execMem, to: (@convention(c) ()->Float64).self)
            let result = _jitFunc()
            
            vdynamic.set(d: result, in: out)
            Swift.assert(out.pointee.d == result)
        case (.u8, false), (.bool, false):
            let _jitFunc = unsafeBitCast(execMem, to: (@convention(c) ()->UInt8).self)
            let result = _jitFunc()
            
            vdynamic.set(ui8: result, in: out)
            Swift.assert(out.pointee.ui8 == result)
        case (.u16, false):
            let _jitFunc = unsafeBitCast(execMem, to: (@convention(c) ()->UInt16).self)
            let result = _jitFunc()
            
            vdynamic.set(ui16: result, in: out)
            Swift.assert(out.pointee.ui16 == result)
        case (.i32, false):
            let _jitFunc = unsafeBitCast(execMem, to: (@convention(c) ()->Int32).self)
            let result = _jitFunc()
            
            vdynamic.set(i: result, in: out)
            Swift.assert(out.pointee.i == result)
        case (.i64, false):
            let _jitFunc = unsafeBitCast(execMem, to: (@convention(c) ()->Int64).self)
            let result = _jitFunc()
            
            vdynamic.set(i64: result, in: out)
            Swift.assert(out.pointee.i == result)
        case (_, true):
            let _jitFunc = unsafeBitCast(execMem, to: (@convention(c) ()->OpaquePointer).self)
            let result = _jitFunc()
            return result
        default:
            print(t._overrideDebugDescription)
            fatal("hlc_static_call does not support return type \(funProvider.retProvider.kind)", logger)
        }
        
        let res: OpaquePointer = withUnsafeMutablePointer(to: &out.pointee.union) { res in
            return .init(res)
        }
        return res
    }
}
