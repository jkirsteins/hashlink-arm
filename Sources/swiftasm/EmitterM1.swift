public protocol Register {
    associatedtype Shift
}

public enum Shift64 : Int {
    case _0 = 0
    case _16 = 16
    case _32 = 32
    case _48 = 48
}

public enum Shift32 : Int {
    case _0 = 0
    case _16 = 16
}

public enum Register64 : UInt8, Register {
    public typealias Shift = Shift64

    case x0 = 0
    case x1 = 1
    case x2 = 2
    case x3 = 3
    case x4 = 4
    case x5 = 5
    case x6 = 6
    case x7 = 7
    case x8 = 8
    case x9 = 9
    case x10 = 10
    case x11 = 11
    case x12 = 12
    case x13 = 13
    case x14 = 14
    case x15 = 15
    case x16 = 16
    case x17 = 17
    case x18 = 18
    case x19 = 19
    case x20 = 20
    case x21 = 21
    case x22 = 22
    case x23 = 23
    case x24 = 24
    case x25 = 25
    case x26 = 26
    case x27 = 27
    case x28 = 28
    case x29 = 29
    case x30 = 30
    case x31 = 31
}

public enum Register32 : Register {
    public typealias Shift = Shift32

    case w0
    case w1
    case w2
    case w3
    case w4
    case w5
    case w6
    case w7
    case w8
    case w9
    case w10
    case w11
    case w12
    case w13
    case w14
    case w15
    case w16
    case w17
    case w18
    case w19
    case w20
    case w21
    case w22
    case w23
    case w24
    case w25
    case w26
    case w27
    case w28
    case w29
    case w30
    case w31
}

public enum Op {
    case nop
    case ret 

    // https://developer.arm.com/documentation/dui0802/a/A64-General-Instructions/MOVZ
    case movz32(Register32, UInt16, Register32.Shift?)
    case movz64(Register64, UInt16, Register64.Shift?)
}

public enum EmitterM1Error: Error {
    case invalidShift
    case unsupportedOp
}

// good movz x0, 0
// 0x00      0x00      0x80      0xd2     
// 0000 0000 0000 0000 1000 0000 1101 0010

// bad movz x0, 0
// 0x00      0x00      0x80      0x42     
// 0000 0000 0000 0000 1000 0000 0100 0010

// ref 
// x10x 0010 1xxi iiii iiii iiii iiid dddd  -  movz Rd HALF

// good movz x1, 0
// 0x01      0x00      0x80      0xd2
// 0000 0001 0000 0000 1000 0000 1101 0010
    
// good movz x0, 1
// 0x20      0x00      0x80      0xd2
// 0010 0000 0000 0000 1000 0000 1101 0010

// bad movz x0, 1
// 0x20      0x00      0x80      0x42
// 0010 0000 0000 0000 1000 0000 0100 0010

// bad movz x0, 1 (2)
// 0x20      0x00      0xe0      0xd2
// 0010 0000 0000 0000 1110 0000 1101 0010

// good movz64 x15, 65535, lsl 0
// 0xef      0xff      0x9f      0xd2
// 1100 1111 1111 1111 1001 1111 1101 0010

// good movz64 x15, 65535, lsl 0
// 0xef      0xff      0x9f      0xd2
// 1100 1111 111-1111110011111110-1 0010
            
// bad movz64 x15, 65535, lsl 0
// 0xef      0xff      0x80      0xd2
// 1110 1111 111-1111110000000110-1 0010

public class EmitterM1
{
    public static func emit(for op: Op) throws -> [UInt8] {
        switch(op) {
            case .nop:
            return [0x1f, 0x20, 0x03, 0xd5]
            case .ret:
            return [0xc0, 0x03, 0x5f, 0xd6]
            case .movz64(let register, let val, let shift):
                // https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/MOVZ--Move-wide-with-zero-?lang=en#MOVZ_32_movewide

                // x10x 0010 1xxi iiii iiii iiii iiid dddd  -  movz Rd HALF
                let encodedR: Int64 = (Int64)(0b11111 & register.rawValue)
                let encodedVal: Int64 = (Int64(val) << 5) & 0b0001_1111_1111_1111_1110_0000
                let mask: Int64     = 0b1101_0010_1000_0000_0000_0000_0000_0000
                let hwMask: Int64   = 0b0000_0000_0110_0000_0000_0000_0000_0000
                
                let shiftVal: Int64

                if let shift = shift {
                    let shiftValPre = (Int64)((shift.rawValue / 16) << 21) 
                    shiftVal = shiftValPre & hwMask
                    // print("ShiftvalPre:")
                    // print(String(shiftValPre, radix: 2).leftPadding(toLength: 32, withPad: "0"))
                } else {
                    shiftVal = 0
                }

                let encoded: Int64  = encodedR | encodedVal | shiftVal | mask                
                let length: Int = 4 * MemoryLayout<UInt8>.size  
                let a = withUnsafeBytes(of: encoded) { bytes in
                    Array(bytes.prefix(length))
                }

                let result = Array(a)//.reversed()) 
                return result
            default:
            throw EmitterM1Error.unsupportedOp
        }
    }
}

extension String {
    func splitString(_ withSize: Int) -> [String] {
        
        let a = self[self.startIndex..<self.index(self.startIndex, offsetBy: withSize)]
        let b = self[self.index(self.startIndex, offsetBy: withSize)..<self.endIndex]
        return [String(a), String(b)]
    }

    func leftPadding(toLength: Int, withPad: String = " ") -> String {

        guard toLength > self.count else { return self }

        let padding = String(repeating: withPad, count: toLength - self.count)
        return padding + self
    }
}