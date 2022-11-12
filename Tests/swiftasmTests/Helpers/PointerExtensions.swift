@testable import swiftasm

extension UnsafeMutableRawPointer {
    func calljit(ctx: CCompatJitContext, fix: Int, arg0: UnsafeRawPointer?) throws -> Int32 {
        return try jit(ctx: ctx, fix: fix) { (ep: (@convention(c) (UnsafeRawPointer?) -> Int32)) in
            return ep(arg0)
        }
    }
    
    func calljit(ctx: CCompatJitContext, fix: Int, arg0: Int) throws -> Int32 {
        return try jit(ctx: ctx, fix: fix) { (ep: (@convention(c) (Int) -> Int32)) in
            return ep(arg0)
        }
    }
    
    func calljit(ctx: CCompatJitContext, fix: Int, arg0: UInt8) throws -> Int32 {
        return try jit(ctx: ctx, fix: fix) { (ep: (@convention(c) (UInt8) -> Int32)) in
            return ep(arg0)
        }
    }
    
    func calljit(ctx: CCompatJitContext, fix: Int, arg0: Int32, arg1: Int32) throws -> Int32 {
        return try jit(ctx: ctx, fix: fix) { (ep: (@convention(c) (Int32, Int32) -> Int32)) in
            return ep(arg0, arg1)
        }
    }
    
    func calljit_i32(ctx: CCompatJitContext, fix: Int, u8_0: UInt8, u8_1: UInt8) throws -> Int32 {
        return try jit(ctx: ctx, fix: fix) { (ep: (@convention(c) (UInt8, UInt8) -> Int32)) in
            return ep(u8_0, u8_1)
        }
    }
    
    func calljit_i32(ctx: CCompatJitContext, fix: Int, u16_0: UInt16, u16_1: UInt16) throws -> Int32 {
        return try jit(ctx: ctx, fix: fix) { (ep: (@convention(c) (UInt16, UInt16) -> Int32)) in
            return ep(u16_0, u16_1)
        }
    }
    
    func calljit_i32(ctx: CCompatJitContext, fix: Int, i32_0: Int32, i32_1: Int32) throws -> Int32 {
        return try jit(ctx: ctx, fix: fix) { (ep: (@convention(c) (Int32, Int32) -> Int32)) in
            return ep(i32_0, i32_1)
        }
    }
    
    func calljit_u8(ctx: CCompatJitContext, fix: Int, u8_0: UInt8) throws -> UInt8 {
        return try jit(ctx: ctx, fix: fix) { (ep: (@convention(c) (UInt8) -> UInt8)) in
            return ep(u8_0)
        }
    }
    
    func calljit_u8(ctx: CCompatJitContext, fix: Int, u8_0: UInt8, u8_1: UInt8) throws -> UInt8 {
        return try jit(ctx: ctx, fix: fix) { (ep: (@convention(c) (UInt8, UInt8) -> UInt8)) in
            return ep(u8_0, u8_1)
        }
    }
    
    func jit<U, R>(ctx: CCompatJitContext, fix: Int, _ callback: (U)->R) throws -> R {
        guard let c = try ctx.getCallable(findex: fix) else {
            throw TestError.unexpected("Function (fix=\(fix)) not found")
        }
        
        let entrypoint = unsafeBitCast(c.address.value, to: U.self)
        return callback(entrypoint)
    }
}
