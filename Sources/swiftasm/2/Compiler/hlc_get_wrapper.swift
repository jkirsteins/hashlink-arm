import Foundation

extension M1Compiler2 {
    /// Result:
    ///     - Should return whatever the callee closure returns (if anything)
    ///
    /// C signature:
    ///
    ///     void *hlc_get_wrapper( hl_type *t )
    static let hlc_get_wrapper: (@convention(c) (OpaquePointer)->(OpaquePointer)) = {
        _topaque in
        
        let t: hlTypePointer = .init(_topaque)
        let out: UnsafeMutableBufferPointer<vdynamic> = .allocate(capacity: 1)
        
        guard let funProvider = t.funProvider else {
            fatal("hlc_get_wrapper fails, invalid type passed. Expected .fun but got \(t._overrideDebugDescription)", logger)
        }
        
        let dynArgs: UnsafeMutableBufferPointer<OpaquePointer> = .allocate(capacity: funProvider.argsProvider.count)
        logger.debug("[hlc_get_wrapper] argc: \(dynArgs.count)")
        
        let hl_wrapper_call_addr = unsafeBitCast(LibHl._hl_wrapper_call, to: OpaquePointer.self)
        
        // We'll jit a function specifically for the given type
        // TODO: reuse the same JIT for multiple invocations?
        let mem = CpuOpBuffer()
        
        // TODO: is this ever called? Placing this exit to spot if so, and add test coverage
        appendSystemExit(19, builder: mem)
        
        // 8 byte stack prologue (rounded to 32) for storing result
        mem.append(
            M1Op.subImm12(X.sp, X.sp, Imm12Lsl12(32)),
            M1Op.stp((X.x29_fp, X.x30_lr), .reg64offset(.sp, 0, nil))
        )
        
        // move *value into X.x21 for safe keeping
        mem.append(M1Op.movr64(X.x21, X.x0))
        
        #if DEBUG
        appendDebugPrintAligned4("[hlc_get_wrapper] entered...", builder: mem)
        appendDebugPrintRegisterAligned4(X.x0, prepend: "[hlc_get_wrapper] value...", builder: mem)
        #endif
        
        var gpIX: RegisterRawValue = 1  // X.x0 is how we received *value
        var fpIX: RegisterRawValue = 0
        
        for (argIx, argType) in funProvider.argsProvider.enumerated() {
            let argKind = argType.kind
            
#if DEBUG
            appendSystemExit(10, builder: mem, message: "Not implemented/tested")
#endif
            
            guard fpIX < ARG_REGISTER_COUNT && gpIX < ARG_REGISTER_COUNT else {
                fatal("hlc_get_wrapper does not support more than \(ARG_REGISTER_COUNT) arguments (i.e. stack args)", logger)
            }
            
            let isFP = FP_TYPE_KINDS.contains(argKind)
            let regIX: RegisterRawValue
            if isFP {
                regIX = fpIX
            } else {
                regIX = gpIX
            }
            
            defer {
                if isFP {
                    fpIX += 1
                } else {
                    gpIX += 1
                }
            }
            
            let argPointer: OpaquePointer
            switch(argKind, argKind.isPointer) {
            case (.u8, false):
                let u8Pointer: UnsafeMutablePointer<UInt8> = .allocate(capacity: 1)
                mem.append(PseudoOp.mov(X.x20, OpaquePointer(u8Pointer)))
                appendStore(regIX, as: 0, intoAddressFrom: X.x20, offsetFromAddress: 0, kinds: [argKind], mem: mem)
                argPointer = .init(u8Pointer)
                // TODO: dealloc these
            case (.u16, false):
                let u16Pointer: UnsafeMutablePointer<UInt16> = .allocate(capacity: 1)
                mem.append(PseudoOp.mov(X.x20, OpaquePointer(u16Pointer)))
                appendStore(regIX, as: 0, intoAddressFrom: X.x20, offsetFromAddress: 0, kinds: [argKind], mem: mem)
                argPointer = .init(u16Pointer)
            case (.i32, false):
                let i32Pointer: UnsafeMutablePointer<Int32> = .allocate(capacity: 1)
                mem.append(PseudoOp.mov(X.x20, OpaquePointer(i32Pointer)))
                appendStore(regIX, as: 0, intoAddressFrom: X.x20, offsetFromAddress: 0, kinds: [argKind], mem: mem)
                argPointer = .init(i32Pointer)
            case (_, true):
                let opPointer: UnsafeMutablePointer<OpaquePointer> = .allocate(capacity: 1)
                mem.append(PseudoOp.mov(X.x20, OpaquePointer(opPointer)))
                appendStore(regIX, as: 0, intoAddressFrom: X.x20, offsetFromAddress: 0, kinds: [argKind], mem: mem)
                argPointer = .init(opPointer)
            case(.f32, false):
                appendSystemExit(11, builder: mem)
                let i32Pointer: UnsafeMutablePointer<Int32> = .allocate(capacity: 1)
                mem.append(PseudoOp.mov(X.x20, OpaquePointer(i32Pointer)))
                appendStore(regIX, as: 0, intoAddressFrom: X.x20, offsetFromAddress: 0, kinds: [argKind], mem: mem)
                argPointer = .init(i32Pointer)
            case (.f64, false):
                appendSystemExit(11, builder: mem)
                let i32Pointer: UnsafeMutablePointer<Int32> = .allocate(capacity: 1)
                mem.append(PseudoOp.mov(X.x20, OpaquePointer(i32Pointer)))
                appendStore(regIX, as: 0, intoAddressFrom: X.x20, offsetFromAddress: 0, kinds: [argKind], mem: mem)
                argPointer = .init(i32Pointer)
            default:
                fatal("hlc_get_wrapper does not support \(argKind)", logger)
            }
            
            dynArgs[argIx] = argPointer
        }
        
        let needDyn = (funProvider.retProvider.kind.isPointer)
        let retPointer: OpaquePointer?
        if needDyn {
            let mRetPointer: UnsafeMutablePointer<vdynamic> = .allocate(capacity: 1)
            retPointer = .init(mRetPointer)
        } else {
            retPointer = nil
        }
        
        // call hl_wrapper_call
        mem.append(
            M1Op.movr64(X.x0, X.x21),                   // value
            PseudoOp.mov(X.x1, OpaquePointer(dynArgs.baseAddress!))   // args
        )
        if let retPointer = retPointer {
            mem.append(PseudoOp.mov(X.x2, retPointer))
        } else {
            mem.append(M1Op.movz64(X.x2, 0, nil))
        }
        
        appendFuncCall(
            hl_wrapper_call_addr,
            via: X.x20,
            mem: mem)
        
        // stash results on the stack
        if funProvider.retProvider.kind != .void {
            appendStore(0, as: 0, intoAddressFrom: .sp, offsetFromAddress: 16, kinds: [funProvider.retProvider.kind], mem: mem)
        }
        
        // deallocate the arg memory after call
        class _Ctx {
            let v: UnsafeMutableBufferPointer<OpaquePointer>
            init(v: UnsafeMutableBufferPointer<OpaquePointer>) {
                self.v = v
            }
        }
        let ctx = _Ctx(v: dynArgs)
        let ctxRaw = Unmanaged.passRetained(ctx).toOpaque()
        let _c: (@convention(c) (UnsafeRawPointer)->()) = {
            ctxRawPtr in
            
            let ctxDeserialized: _Ctx = Unmanaged.fromOpaque(ctxRawPtr).takeRetainedValue()
            for item in ctxDeserialized.v {
                let mbp: UnsafeMutableRawPointer = .init(item)
                
                mbp.deallocate()
            }
            
            ctxDeserialized.v.deallocate()
        }
        
        // pop results from the stack
        if funProvider.retProvider.kind != .void {
            appendLoad(0, as: 0, addressRegister: .sp, offset: 16, kinds: [funProvider.retProvider.kind], mem: mem)
        }
        
        // no-stack-epilogue
        mem.append(
            M1Op.ldp((X.x29_fp, X.x30_lr), .reg64offset(.sp, 0, nil)),
            M1Op.addImm12(X.sp, X.sp, Imm12Lsl12(32)),
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
        
        return OpaquePointer(execMem)
    }
}
