import Darwin
import Foundation

class BufferMapper {
    let ctx: CCompatJitContext
    let buffer: CpuOpBuffer
    
    static let logger = LoggerFactory.create(BufferMapper.self)
    
    init(ctx: CCompatJitContext, buffer: CpuOpBuffer) {
        self.ctx = ctx
        self.buffer = buffer
    }
    
    var mapped: UnsafeMutableRawPointer? { self.ctx.jitBase.wrappedValue }
    
    func getMemory() throws -> UnsafeMutableRawPointer {
        try ensureMemory()
    }
    
    func freeMemory() throws {
        guard let mem = mapped else {
            throw GlobalError.invalidOperation("Can't free unallocated memory")
        }
        
        munmap(mem, Int(buffer.byteSize))
    }
    
    func emitMachineCode() throws -> [UInt8] {
        return try buffer.ops.flatMap { try $0.emit() }
    }
    
    deinit {
        try? freeMemory()
    }
    
    fileprivate func ensureMemory() throws -> UnsafeMutableRawPointer {
        guard mapped == nil else {
            /* We might need to call this multiple times (e.g. if we add a hexPrint() debug statement ahead of proper emitting)

            This is an issue only if we try to append new instructions once memory is already locked in place.
            */
            return mapped!
        }
        
        let missing = self.ctx.funcTracker.refs.subtracting(self.ctx.funcTracker.comps)
        guard missing.isEmpty else {
            let message = "These functions are referenced but not compiled: \(missing)"
            Self.logger.debug("\(message)")
            throw GlobalError.invalidOperation(message)
        }
        
        
        // we need to map the memory before emitting instructions
        // as the instructions might refer to absolute memory addresses
        self.ctx.jitBase.wrappedValue = mmap(
            nil,
            Int(buffer.byteSize),
            PROT_WRITE | PROT_EXEC,
            MAP_ANONYMOUS | MAP_PRIVATE | MAP_JIT,
            -1,
            0
        )
        
        // this must happen after setting jitBase
        let mc = try emitMachineCode()
        assert(buffer.byteSize == Int64(mc.count))
        
        // we should make the function addresses available to CCompat
        // (the addresses are only known now, after mmap)
        for functionRealIx in (0..<Int(ctx.nfunctions)) {
            guard
                let fun = ctx.mainContext.pointee.code?.pointee.functions.advanced(by: functionRealIx)
            else {
                fatalError("Can't get findex for function #\(functionRealIx)")
            }
            let funIndex = fun.pointee.findex
            guard let addr = ctx.mainContext.pointee.m?.pointee.functions_ptrs.advanced(by: Int(funIndex)) else {
                fatalError("Can't get function address for findex \(funIndex)")
            }
            let callable = try ctx.getCallable(findex: Int(funIndex))
            guard let realAddress = callable?.address, realAddress.hasUsableValue else {
                #if DEBUG
                // This is only valid for tests
                Self.logger.warning("Not setting function address for findex \(funIndex)")
                #else
                fatal("Could not set final function address for findex \(funIndex)", Self.logger)
                #endif
                continue
            }
            
            let mAddr = UnsafeMutablePointer(mutating: addr)
            mAddr.pointee = .init(realAddress.value)
            Self.logger.debug("Set function address for findex \(String(describing: funIndex)) to \(String(describing: mAddr.pointee))")
        }
        
        for nativeRealIx in (0..<Int(ctx.nnatives)) {
            guard
                let nat = ctx.mainContext.pointee.code?.pointee.natives.advanced(by: nativeRealIx)
            else {
                fatalError("Can't get findex for native #\(nativeRealIx)")
            }
            let funIndex = nat.pointee.findex
            guard let addr = ctx.mainContext.pointee.m?.pointee.functions_ptrs.advanced(by: Int(funIndex)) else {
                fatalError("Can't get function address for findex \(funIndex)")
            }
            let callable = try ctx.getCallable(findex: Int(funIndex))
            guard let realAddress = callable?.address, realAddress.hasUsableValue else {
                #if DEBUG
                // This is only valid for tests
                Self.logger.warning("Not setting function address for findex \(funIndex)")
                #else
                fatal("Could not set final function address for findex \(funIndex)", Self.logger)
                #endif
                continue
            }
            
            let mAddr = UnsafeMutablePointer(mutating: addr)
            mAddr.pointee = .init(realAddress.value)
            Self.logger.debug("Set native address for findex \(String(describing: funIndex)) to \(String(describing: mAddr.pointee))")
        }
        
        guard let mapped = mapped else {
            throw GlobalError.unexpected("mmap returned nil: \(errno)")
        }
        
        if mapped == MAP_FAILED {
            throw GlobalError.unexpected("mmap failed: \(errno)")
        }
        
        pthread_jit_write_protect_np(0)
        memcpy(mapped, mc, mc.count)
        pthread_jit_write_protect_np(1)
        
        return mapped
    }
}
