extension BinaryInteger {
    var leadingOneBitCount: Int {
        let mask: UInt64 = 1 << (self.bitWidth - 1)
        var result = 0
        var comp = UInt64(self)
        while comp != 0 && (mask & comp) > 0 {
            comp = comp << 1
            result += 1
        }
        return result
    }
    
    var trailingOneBitCount: Int {
        (~self).trailingZeroBitCount
    }
}

struct BitmaskImmediate : Equatable, Hashable {
    let n: UInt8
    let immr: UInt8
    let imms: UInt8
    
    /// Is this number's binary representation all 1s?
    static private func isMask(_ imm: UInt64) -> Bool {
        ((imm + 1) & imm) == 0
    }

    /// Is this number's binary representation one or more 1s followed by
    /// one or more 0s?
    static private func isShiftedMask(_ imm: UInt64) -> Bool {
        isMask((imm - 1) | imm)
    }
    
    init(n: UInt8, immr: UInt8, imms: UInt8) {
        self.n = n
        self.imms = imms
        self.immr = immr
    }
    
    /// https://kddnewton.com/2022/08/11/aarch64-bitmask-immediates.html
    init(_ value: UInt64) throws {
        guard value != 0 else {
            throw EmitterM1Error.invalidValue("Bitmask immediate can not encode all 0s.")
        }
        guard value != UInt64.max else {
            throw EmitterM1Error.invalidValue("Bitmask immediate can not encode all 1s.")
        }
        
        var imm: UInt64 = value
        var size: UInt64 = 64

        while (true) {
            size >>= 1;
            let mask: UInt64 = (1 << size) - 1;

            if (imm & mask) != ((imm >> size) & mask) {
              size <<= 1;
              break;
            }

            if size <= 2 {
                break;
            }
        }
        
        var trailing_ones: UInt32
        var left_rotations: UInt32

        let mask = UInt64.max >> (64 - size);
        imm &= mask;

        if Self.isShiftedMask(imm) {
            left_rotations = UInt32(imm.trailingZeroBitCount)
            trailing_ones = UInt32((imm >> left_rotations).trailingOneBitCount)
        } else {
            imm |= ~mask;
            if !Self.isShiftedMask(~imm) {
                throw EmitterM1Error.invalidValue("Could not encoded bitmask immediate")
            }

            let leading_ones = imm.leadingOneBitCount
            left_rotations = UInt32(64 - leading_ones)
            trailing_ones = UInt32(leading_ones) + UInt32(imm.trailingOneBitCount) - (64 - UInt32(size))

        }
        
        // immr is the number of right rotations it takes to get from the
        // matching unrotated pattern to the target value.
        let immr = (size - UInt64(left_rotations)) & (size - 1)
        
        // imms is encoded as the size of the pattern, a 0, and then one less
        // than the number of sequential 1s.
        let imms = (~(size &- 1) << 1) | (UInt64(trailing_ones) &- 1);
        
        // n is 1 if the element size is 64-bits, and 0 otherwise.
        let n = ((imms >> 6) & 1) ^ 1;
        
        self.n = UInt8(n)
        self.imms = UInt8((imms & 0x3f))
        self.immr = UInt8((immr & 0x3f))
    }
}
