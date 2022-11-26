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
    var buf: jmp_buf
    var prev: UnsafePointer<HLTrapCtx_CCompat>?
    var tcheck: UnsafePointer<vdynamic>?
}
