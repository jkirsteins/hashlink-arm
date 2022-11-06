import Darwin
import Foundation

class BufferMapper {
    let ctx: CCompatJitContext
    let buffer: CpuOpBuffer
    
    init(ctx: CCompatJitContext, buffer: CpuOpBuffer) {
        self.ctx = ctx
        self.buffer = buffer
    }
    
    var mapped: UnsafeMutableRawPointer? { self.ctx.jitBase.wrappedValue }
    
    func getMemory() throws -> UnsafeMutableRawPointer {
        try ensureMemory()
    }
    
    fileprivate func emitMachineCode() throws -> [UInt8] {
        return try buffer.ops.flatMap { try $0.emit() }
    }
    
    fileprivate func ensureMemory() throws -> UnsafeMutableRawPointer {
        guard mapped == nil else {
            /* We might need to call this multiple times (e.g. if we add a hexPrint() debug statement ahead of proper emitting)

            This is an issue only if we try to append new instructions once memory is already locked in place.
            */
            return mapped!
        }
        
        let mc = try emitMachineCode()

        self.ctx.jitBase.wrappedValue = mmap(
            nil,
            mc.count,
            PROT_WRITE | PROT_EXEC,
            MAP_ANONYMOUS | MAP_PRIVATE | MAP_JIT,
            -1,
            0
        )
        
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
