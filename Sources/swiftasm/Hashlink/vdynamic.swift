//
//  File.swift
//  
//
//  Created by Janis Kirsteins on 23/10/2022.
//

import Foundation

/**
 typedef struct {
     hl_type *t;
 #    ifndef HL_64
     int __pad; // force align on 16 bytes for double
 #    endif
     union {
         bool b;
         unsigned char ui8;
         unsigned short ui16;
         int i;
         float f;
         double d;
         vbyte *bytes;
         void *ptr;
         int64 i64;
     } v;
 } vdynamic;
 */
struct vdynamic {
    let t: UnsafePointer<HLType_CCompat>
    let union: UnsafeMutableRawPointer
}
