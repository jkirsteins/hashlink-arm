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
    var union: UnsafeMutableRawPointer?
    
    var b: Bool { ui8 > 0 }
    var ui8: UInt8 { UInt8(truncatingIfNeeded: Int8(truncatingIfNeeded: Int(bitPattern: union))) }
    var ui16: UInt16 { UInt16(truncatingIfNeeded: Int16(truncatingIfNeeded: Int(bitPattern: union))) }
    var i: Int32 { Int32(truncatingIfNeeded: Int(bitPattern: union)) }
    var f: Float32 { Float32(bitPattern: UInt32(bitPattern: Int32(truncatingIfNeeded: Int(bitPattern: union)))) }
    var d: Float64 { Float64(bitPattern: UInt64(bitPattern: Int64(truncatingIfNeeded: Int(bitPattern: union)))) }
    var bytes: UnsafeRawPointer? { .init(union) }
    var ptr: UnsafeRawPointer? { .init(union) }
    var i64: Int64 { Int64(truncatingIfNeeded: Int(bitPattern: union)) }
    
    static func set(d: Float64, in ptr: UnsafePointer<vdynamic>) {
        let ui64 = d.bitPattern
        let i64 = Int64(bitPattern: ui64)
        let i: Int = Int(i64)
        let ui: UInt = UInt(bitPattern: i)
        
        Self.set(union: UnsafeMutableRawPointer(bitPattern: ui), in: ptr)
    }
    
    static func set(f: Float32, in ptr: UnsafePointer<vdynamic>) {
        let ui32 = f.bitPattern
        let i32 = Int32(bitPattern: ui32)
        let i: Int = Int(i32)
        let ui: UInt = UInt(bitPattern: i)
        
        Self.set(union: UnsafeMutableRawPointer(bitPattern: ui), in: ptr)
    }
    
    static func set(ui8: UInt8, in ptr: UnsafePointer<vdynamic>) {
        let ui: UInt = UInt(ui8)
        
        Self.set(union: UnsafeMutableRawPointer(bitPattern: ui), in: ptr)
    }
    
    static func set(ui16: UInt16, in ptr: UnsafePointer<vdynamic>) {
        let ui: UInt = UInt(ui16)
        
        Self.set(union: UnsafeMutableRawPointer(bitPattern: ui), in: ptr)
    }
    
    static func set(i: Int32, in ptr: UnsafePointer<vdynamic>) {
        let i = Int(i)
        let ui: UInt = UInt(bitPattern: i)
        
        Self.set(union: UnsafeMutableRawPointer(bitPattern: ui), in: ptr)
    }
    
    static func set(i64: Int64, in ptr: UnsafePointer<vdynamic>) {
        let i = Int(i64)
        let ui: UInt = UInt(bitPattern: i)
        
        Self.set(union: UnsafeMutableRawPointer(bitPattern: ui), in: ptr)
    }
    
    static func set(union newUnion: UnsafeMutableRawPointer?, in ptr: UnsafePointer<vdynamic>) {
        guard let offset = MemoryLayout<vdynamic>.offset(of: \vdynamic.union) else {
            fatalError("Couldn't calculate offset to vdynamic.union")
        }
        
        let ptrToUnion = UnsafeRawPointer(ptr).advanced(by: offset).bindMemory(to: UnsafeMutableRawPointer?.self, capacity: 1)
            
        let mutablePtr: UnsafeMutablePointer<UnsafeMutableRawPointer?> = .init(mutating: ptrToUnion)
        mutablePtr.pointee = newUnion
        
    }
}
