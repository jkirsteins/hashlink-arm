import Darwin

typealias HLTypeKinds = [HLTypeKind]

typealias Registers = [Resolvable<HLType>]

let _trap: (@convention(c) (Int64, Int64) -> ()) = { (_ exc: Int64, _ offset: Int64) in
    
    let __tinf = UnsafeMutablePointer(mutating: LibHl.hl_get_thread())
    
    let newCtx: UnsafeMutablePointer<HLTrapCtx_CCompat> = .allocate(capacity: 1)
    
    newCtx.initialize(to: HLTrapCtx_CCompat(
        buf: (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        prev: __tinf.pointee.trap_current,
        tcheck: nil))
    
    __tinf.pointee.trap_current = .init(newCtx)
    
    let capacity = Mirror(reflecting: newCtx.pointee.buf).children.count
    
    let testtest: UnsafeMutableBufferPointer<Int32> = .allocate(capacity: 96)
    testtest.initialize(repeating: 0)
    
    let testbuf: UnsafeMutablePointer<jmp_buf> = .allocate(capacity: 1)
    
    var yo: Int32 = 0
    
    print("before", Array(testtest))
    let res = LibSystem.setjmp(.init(testtest.baseAddress!))
    if res != 0 {
        fatalError("Exception triggered, lessgo catch it")
    }
    print("after", Array(testtest))

    
    // res
    var resv = vdynamic(t: .init(bitPattern: 1)!, union: nil)
//    LibHl.hl_throw(&resv)
    longjmp(testtest.baseAddress!, 123)
    
    
    fatalError("Exception not triggered, hoopla")
    
    fatalError("Got thread info")
//    hl_thread_info *__tinf = hl_get_thread();
//        ctx.tcheck = NULL;
//        ctx.prev = __tinf->trap_current;
//        __tinf->trap_current = &ctx;
//        if( setjmp(ctx.buf) ) {
//            r = __tinf->exc_value;
//            goto label;
//        }
    
}
let _endTrap: (@convention(c) (Int64) -> ()) = { (_ exc: Int64) in
    print("Ending trap \(exc)")
}
let _trapAddress = unsafeBitCast(_trap, to: UnsafeMutableRawPointer.self)
let _endTrapAddress = unsafeBitCast(_endTrap, to: UnsafeMutableRawPointer.self)
