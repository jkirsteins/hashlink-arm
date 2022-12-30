import Foundation 

protocol Immediate : Equatable, Hashable {
    var bits: Int64 { get }
    var immediate: Int64 { get }

    // deferred immediates might not have a value set
    var hasUsableValue: Bool { get }

    init(_ val: Int64, bits: Int64) throws
}

extension Immediate {
    var hasUsableValue: Bool { true }
}

func truncateOffsetGlobal(_ val: Int64, divisor: Int64, bits: Int64) throws
    -> Int64
{
    try truncateOffset(val, divisor: divisor, bits: bits)
}

func truncateOffset(_ val: Int64, divisor: Int64, bits: Int64, signed: Bool = true) throws
    -> Int64
{
    if val % divisor != 0 {
        throw EmitterM1Error.invalidOffset(
            "truncateOffset: offset immediate must be a multiple of \(divisor) but was \(val)"
        )
    }

    let divided: Int64 = val / divisor
    let mask: UInt64 = ((1 << bits) - 1)
    
    let leadingBitsMask: UInt64
    if val >= 0 {
        leadingBitsMask = 0
    } else {
        leadingBitsMask = (UInt64(0)..<(64-UInt64(bits))).reduce(0) { $0 | (1 << (63 - $1)) }
    }
    
    let compare: UInt64 = UInt64(bitPattern: divided) & mask
    guard
            (leadingBitsMask | compare) == UInt64(bitPattern: divided),
            // if value is negative, most significant bit must be 1 (we
            // can't rely on bits set outside of our range)
            !signed || (divided >= 0 || (compare >> (bits - 1) == 1)),
            // if value is positive, most significant bit must be 0
            !signed || (divided <= 0 || (compare >> (bits - 1) != 1))
    else {
        throw EmitterM1Error.invalidValue(
            "Immediate \(val) must fit in \(bits) bits"
        )
    }
    
    return Int64(bitPattern: mask) & divided
}

struct VariableImmediate: Immediate {
    let bits: Int64
    let value: Int64 

    var immediate: Int64 { value }

    init(_ val: Int64, bits: Int64, divisor: Int64, signed: Bool) throws {
        // ensure
        self.value = try truncateOffset(val, divisor: divisor, bits: bits, signed: signed)
        self.bits = bits
    }
    
    init(_ val: Int64, bits: Int64, divisor: Int64) throws {
        try self.init(val, bits: bits, divisor: divisor, signed: true)
    }
    
    init(_ val: Int64, bits: Int64, signed: Bool) throws {
        try self.init(val, bits: bits, divisor: 1, signed: signed)
    }
    
    init(_ val: Int64, bits: Int64) throws {
        try self.init(val, bits: bits, signed: true)
    }
}

struct DeferredImmediateSum : Immediate, RelativeOffset, Equatable, Hashable {
    var debugDescription: String {
        "DeferredImmediateSum<\(a), \(b)>"
    }
    
    static func == (lhs: DeferredImmediateSum, rhs: DeferredImmediateSum) -> Bool {
        lhs.immediate == rhs.immediate
    }
    
    func hash(into hasher: inout Hasher) {
        hasher.combine(immediate)
    }
    
    var bits: Int64 { a.bits }
    
    let a: any Immediate
    let b: any Immediate
    let bMul: Int
    let bAdd: Int
    
    var immediate: Int64 {
        a.immediate + b.immediate*Int64(bMul)+Int64(bAdd)
    }
    
    var value: Int64 { immediate }
    
    init(_ a: any Immediate, _ b: any Immediate, _ bMul: Int = 1, _ bAdd: Int = 0) throws {
        self.a = a
        self.b = b
        self.bMul = bMul
        self.bAdd = bAdd
    }
    
    init(_ val: Int64, bits: Int64) throws {
        fatalError("not supported")
    }
    
    init(integerLiteral: Int32) {
        fatalError("not supported")
    }
}

struct DeferredImmediate<T: Immediate> : Immediate {
    let ptr: SharedStorage<T?> = SharedStorage(wrappedValue: nil)

    func finalize(_ val: T) {
        guard ptr.wrappedValue == nil else { fatalError("Can't finalize DeferredImmediate twice") }
        ptr.wrappedValue = val
    }

    func require<R>(_ c: (T)->R) -> R {
        guard let val = self.ptr.wrappedValue else {
            fatalError("DeferredImmediate not finalized before access")
        }
        return c(val)
    }

    func `try`<R>(_ c: (T)->R) throws -> R {
        guard let val = self.ptr.wrappedValue else {
            throw GlobalError.immediateMissingValue("Trying to access DeferredImmediate value \(ptr)")
        }
        return c(val)
    }

    func get() throws -> T {
        try `try` {
            return $0
        }
    }

    var hasUsableValue: Bool { 
        ptr.wrappedValue != nil
    }

    var bits: Int64 {
        require {
            return $0.bits
        } 
    }

    var immediate: Int64 {
        require {
            return $0.immediate
        }
    }

    init() {
    
    }

    init(_ val: Int64, bits: Int64) throws {
        fatalError("wait what")
    }
}

extension Immediate {
    func shiftedLeft(_ lsl: any BinaryInteger) -> Int64 {
        self.immediate << lsl
    }
    
    func signedShiftedRight(_ lsr: any BinaryInteger) -> Int64 {
        signedTruncate(lsr: lsr, capBits: bits)
    }
    
    func signedTruncate(lsr: any BinaryInteger, capBits: Int64) -> Int64 {
        let mask: Int64 = (1 << capBits) - 1
        var res = self.immediate
        for _ in 0..<Int(lsr) {
            res = (res >> 1) | (self.isNegative ? (1 << (self.bits-1)) : 0)
        }
        return res & mask
    }

    var signMask: Int64 {
        1 << (self.bits - 1)
    }

    var isNegative: Bool {
        return (self.immediate & signMask) > 0
    }

    var isPositive: Bool {
        !isNegative
    }

    var flippedSign: Self {
        let mask: Int64 = ((1 << bits) - 1)
        let inverted: Int64 = mask - self.immediate
        let flipped: Int64 = inverted + 1

        // try! because it must fit. Only way it doesn't fit is
        // if there's some bug.
        return try! Self(flipped, bits: self.bits)
    }
}

struct Immediate26: Immediate, ExpressibleByIntegerLiteral {
    let bits: Int64 = 26

    let wrapped: VariableImmediate

    var immediate: Int64 { wrapped.immediate }

    init(integerLiteral: Int32) {
        self.wrapped = try! VariableImmediate(Int64(integerLiteral), bits: bits)
    }

    init(_ val: Int64, bits: Int64) throws {
        self.wrapped = try VariableImmediate(val, bits: bits)
    }

    init(_ val: any BinaryInteger) throws {
        let i = Int(val)
        self.wrapped = try VariableImmediate(Int64(i), bits: bits)
    }
}

struct Immediate12: Immediate, ExpressibleByIntegerLiteral {
    let bits: Int64 = 12

    let wrapped: VariableImmediate

    var immediate: Int64 { wrapped.immediate }

    init(integerLiteral: Int32) {
        self.wrapped = try! VariableImmediate(Int64(integerLiteral), bits: bits)
    }

    init(_ val: Int64, bits: Int64) throws {
        self.wrapped = try VariableImmediate(val, bits: bits)
    }

    init(_ val: any BinaryInteger) throws {
        let i = Int(val)
        self.wrapped = try VariableImmediate(Int64(i), bits: bits)
    }
}

struct Immediate9: Immediate, ExpressibleByIntegerLiteral {
    let bits: Int64 = 9

    let wrapped: VariableImmediate

    var immediate: Int64 { wrapped.immediate }

    init(integerLiteral: Int32) {
        self.wrapped = try! VariableImmediate(Int64(integerLiteral), bits: bits)
    }

    init(_ val: Int64, bits: Int64) throws {
        self.wrapped = try VariableImmediate(val, bits: bits)
    }

    init(_ val: any BinaryInteger) throws {
        let i = Int(val)
        self.wrapped = try VariableImmediate(Int64(i), bits: bits)
    }
}

struct Immediate16: Immediate, ExpressibleByIntegerLiteral {
    let bits: Int64 = 16

    let wrapped: VariableImmediate

    var immediate: Int64 { wrapped.immediate }

    init(integerLiteral: Int32) {
        self.wrapped = try! VariableImmediate(Int64(integerLiteral), bits: bits)
    }

    init(_ val: Int64, bits: Int64) throws {
        self.wrapped = try VariableImmediate(val, bits: bits)
    }

    init(_ val: any BinaryInteger) throws {
        let i = Int(val)
        self.wrapped = try VariableImmediate(Int64(i), bits: bits)
    }
}

struct Immediate6: Immediate, ExpressibleByIntegerLiteral {
    let bits: Int64 = 6

    let wrapped: VariableImmediate

    var immediate: Int64 { wrapped.immediate }

    init(integerLiteral: Int32) {
        self.wrapped = try! VariableImmediate(Int64(integerLiteral), bits: bits)
    }
    
    init(unsigned: UInt8) {
        self.wrapped = try! VariableImmediate(Int64(unsigned), bits: bits, signed: false)
    }

    init(_ val: Int64, bits: Int64) throws {
        self.wrapped = try VariableImmediate(val, bits: bits)
    }

    init(_ val: any BinaryInteger) throws {
        let i = Int(val)
        self.wrapped = try VariableImmediate(Int64(i), bits: bits)
    }
}

struct UImmediate6: Immediate, ExpressibleByIntegerLiteral {
    let bits: Int64 = 6

    let wrapped: VariableImmediate

    var immediate: Int64 { wrapped.immediate }

    init(integerLiteral: Int32) {
        self.wrapped = try! VariableImmediate(Int64(integerLiteral), bits: bits, signed: false)
    }
    
    init(_ val: Int64, bits: Int64) throws {
        self.wrapped = try VariableImmediate(val, bits: bits, signed: false)
    }

    init(_ val: any BinaryInteger) throws {
        let i = Int(val)
        self.wrapped = try VariableImmediate(Int64(i), bits: bits, signed: false)
    }
}

extension Immediate {
    var signedImmediate: Int64 {
        guard self.isNegative else { return immediate }
        
        var result = self.immediate
        for ix in self.bits..<64 {
            result = result | (1 << ix)
        }
        return result
    }
}

struct Immediate21: Immediate, ExpressibleByIntegerLiteral {
    let bits: Int64 = 21

    let wrapped: VariableImmediate

    var immediate: Int64 { wrapped.immediate }
    
    init(integerLiteral: Int32) {
        self.wrapped = try! VariableImmediate(Int64(integerLiteral), bits: bits)
    }
    
    init(_ val: Int64, bits: Int64) throws {
        self.wrapped = try VariableImmediate(val, bits: bits)
    }

    init(_ val: any BinaryInteger) throws {
        let i = Int(val)
        self.wrapped = try VariableImmediate(Int64(i), bits: bits)
    }
}

struct Immediate19: Immediate, ExpressibleByIntegerLiteral {
    let bits: Int64 = 19

    let wrapped: VariableImmediate

    var immediate: Int64 { wrapped.immediate }
    
    init(integerLiteral: Int32) {
        self.wrapped = try! VariableImmediate(Int64(integerLiteral), bits: bits)
    }
    
    init(_ val: Int64, bits: Int64) throws {
        self.wrapped = try VariableImmediate(val, bits: bits)
    }

    init(_ val: any BinaryInteger) throws {
        let i = Int(val)
        self.wrapped = try VariableImmediate(Int64(i), bits: bits)
    }
}

extension Int: Immediate {
    var bits: Int64 { 
        guard Int(0).bitWidth == 64 else {
            fatalError("Int should be 64-bits")
        }
        return 64
    }
    var immediate: Int64 { Int64(self) }

    init(_ val: Int64, bits: Int64) throws {
        self = Self(val)
        guard bits == self.bits else { fatalError("\(type(of: self)) can only be initialized with \(self.bits) bits") }
    }
}

extension Int64: Immediate {
    var bits: Int64 { 64 }
    var immediate: Int64 { self }

    init(_ val: Int64, bits: Int64) throws {
        self = Self(val)
        guard bits == self.bits else { fatalError("\(type(of: self)) can only be initialized with \(self.bits) bits") }
    }
}

extension UInt64: Immediate {
    var bits: Int64 { 64 }
    var immediate: Int64 { Int64(bitPattern: self) }

    init(_ val: Int64, bits: Int64) throws {
        self = Self(val)
        guard bits == self.bits else { fatalError("\(type(of: self)) can only be initialized with \(self.bits) bits") }
    }
}

extension Int32: Immediate {
    var bits: Int64 { 32 }
    var immediate: Int64 { Int64(self) }

    init(_ val: Int64, bits: Int64) throws {
        self = Self(val)
        guard bits == self.bits else { fatalError("\(type(of: self)) can only be initialized with \(self.bits) bits") }
    }
}

extension Int16: Immediate {
    var bits: Int64 { 16 }
    var immediate: Int64 { Int64(self) }

    init(_ val: Int64, bits: Int64) throws {
        self = Self(val)
        guard bits == self.bits else { fatalError("\(type(of: self)) can only be initialized with \(self.bits) bits") }
    }
}


struct Imm12Lsl12 : Immediate, CustomAsmStringConvertible, ExpressibleByIntegerLiteral {
    enum Lsl12 {
        case _0 
        case _12
    }

    var bits: Int64 { imm.bits }
    var immediate: Int64 { imm.immediate }
    
    let imm: Immediate12
    let lsl: Lsl12

    var asmDescription: String {
        if lsl == ._0 {
            return "#\(imm.immediate)"
        } else {
            return "#\(imm.immediate), lsl 12"
        }
    }

    init(integerLiteral val: Int16)
    {
        do {
            self.imm = try Immediate12(val)
            self.lsl = ._0
        } catch {
            guard val == ((val >> 12) << 12) else {
                fatalError("Value must fit in either high or low 12 bits of a 24 bit value")
            }

            self.imm = try! Immediate12(Int64(val) >> 12)
            self.lsl = ._12
        }
    }

    init(_ imm: Immediate12, lsl: Imm12Lsl12.Lsl12 = ._0) throws {
        self.imm = imm
        self.lsl = lsl
    }

    init(_ val: any BinaryInteger) throws {
        try self.init(try Immediate12(val))
    }

    static func i(_ val: any BinaryInteger) throws -> Self {
        try Self(val)
    }

    init(_ val: Int64, bits: Int64) throws {
        guard bits == 12 else { fatalError("bits must be 12 for \(type(of: self))") }
        do {
            self.imm = try Immediate12(val, bits: 12)
            self.lsl = ._0
        } catch {
            guard val == ((val >> 12) << 12) else {
                fatalError("Value must fit in either high or low 12 bits of a 24 bit value")
            }

            self.imm = try Immediate12(val >> 12, bits: 12)
            self.lsl = ._12
        }
    }
}
