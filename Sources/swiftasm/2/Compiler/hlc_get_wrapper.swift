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
        
        let args: UnsafeMutableBufferPointer<OpaquePointer> = .allocate(capacity: funProvider.argsProvider.count)
        logger.debug("[hlc_get_wrapper] argc: \(args.count)")
        
        let hl_wrapper_call_addr = unsafeBitCast(LibHl._hl_wrapper_call, to: OpaquePointer.self)
        
        // We'll jit a function specifically for the given type
        // TODO: reuse the same JIT for multiple invocations?
        let mem = CpuOpBuffer()
        
        // no-stack-prologue
        mem.append(
            M1Op.subImm12(X.sp, X.sp, Imm12Lsl12(16)),
            M1Op.stp((X.x29_fp, X.x30_lr), .reg64offset(.sp, 0, nil))
        )
        
        fatal("hlc_get_wrapper not implemented", logger)
    }
}
