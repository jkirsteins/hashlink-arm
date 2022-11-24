/**
 Trap context
 
     struct _hl_trap_ctx {
         jmp_buf buf;
         hl_trap_ctx *prev;
         vdynamic *tcheck;
     };
 */

import Darwin

struct HLTrapCtx_CCompat {
    let buf: jmp_buf
    let prev: UnsafePointer<HLTrapCtx_CCompat>?
    let tcheck: UnsafePointer<vdynamic>?
}
