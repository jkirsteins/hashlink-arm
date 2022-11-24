/**
 Thread info
 
        typedef struct {
             int thread_id;
             // gc vars
             volatile int gc_blocking;
             void *stack_top;
             void *stack_cur;
             // exception handling
             hl_trap_ctx *trap_current;
             hl_trap_ctx *trap_uncaught;
             vclosure *exc_handler;
             vdynamic *exc_value;
             int flags;
             int exc_stack_count;
             // extra
             jmp_buf gc_regs;
             void *exc_stack_trace[HL_EXC_MAX_STACK];
             void *extra_stack_data[HL_MAX_EXTRA_STACK];
             int extra_stack_size;
         } hl_thread_info;
 */
import Darwin

let HL_EXC_MAX_STACK = 0x100
let HL_MAX_EXTRA_STACK = 64

struct HLThreadInfo_CCompat {
    let thread_id: Int32
    // gc vars
    let gc_blocking: Int32
    let stack_top: OpaquePointer
    let stack_cur: OpaquePointer
    // exception handling
    let trap_current: UnsafePointer<HLTrapCtx_CCompat>?
    let trap_uncaught: UnsafePointer<HLTrapCtx_CCompat>?
    let exc_handler: UnsafePointer<vclosure>?
    let exc_value: UnsafePointer<vdynamic>?
    let flags: Int32
    let exc_stack_count: Int32
    // extra
    let gc_regs: jmp_buf
    let exc_stack_trace: (OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer)
    let extra_stack_data: (OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer, OpaquePointer)

    let extra_stack_size: Int32
    
    // padding to align size w C struct
    let __padding: Int32
}
