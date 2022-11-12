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
    let union: UnsafeMutableRawPointer?
    
    var b: Bool { ui8 > 0 }
    var ui8: UInt8 { UInt8(truncatingIfNeeded: Int8(truncatingIfNeeded: Int(bitPattern: union))) }
    var ui16: UInt16 { UInt16(truncatingIfNeeded: Int16(truncatingIfNeeded: Int(bitPattern: union))) }
    var i: Int32 { Int32(truncatingIfNeeded: Int(bitPattern: union)) }
    var f: Float32 { Float32(bitPattern: UInt32(bitPattern: Int32(truncatingIfNeeded: Int(bitPattern: union)))) }
    var d: Float64 { Float64(bitPattern: UInt64(bitPattern: Int64(truncatingIfNeeded: Int(bitPattern: union)))) }
    var bytes: UnsafeRawPointer? { .init(union) }
    var ptr: UnsafeRawPointer? { .init(union) }
    var i64: Int64 { Int64(truncatingIfNeeded: Int(bitPattern: union)) }
}
