protocol Immediate : Equatable {
    var bits: Int64 { get }
    var immediate: Int64 { get }

    // deferred immediates might not have a value set
    var hasUsableValue: Bool { get }

    init(_ val: Int64, bits: Int64) throws
}

extension Immediate {
    var hasUsableValue: Bool { true }
}

fileprivate func truncateOffset(_ val: Int64, divisor: Int64, bits: Int64) throws
    -> Int64
{
    if val % divisor != 0 {
        throw EmitterM1Error.invalidOffset(
            "Offset immediate must be a multiple of \(divisor) but was \(val)"
        )
    }

    let divided = val / divisor
    let mask: Int64 = ((1 << bits) - 1)
    // Check if we fit in required number of bits
    let compare: Int64
    if divided >= 0 {
        compare = divided & mask
    }
    else {
        let rmask: Int64 = (~mask | 0b1000000)
        compare = (divided & mask) | rmask
    }
    guard compare == divided else {
        throw EmitterM1Error.invalidValue(
            "Immediate \(val) must fit in \(bits) bits"
        )
    }

    // apply mask otherwise a negative value will contain leading 1s,
    // which can mess up when shifting left later
    return (mask & divided)
}

struct VariableImmediate: Immediate {
    let bits: Int64
    let value: Int64 

    var immediate: Int64 { value }

    init(_ val: Int64, bits: Int64) throws {
        // ensure 
        self.value = try truncateOffset(val, divisor: 1, bits: bits)
        self.bits = bits
    }
}

struct AbsoluteAddressImmediate: Immediate, ExpressibleByIntegerLiteral {
    let bits: Int64
    let value: Int64 

    var immediate: Int64 { value }

    init(_ val: UnsafeMutableRawPointer) {
        self.value = Int64(Int(bitPattern: val))
        self.bits = 64
    }

    init(_ val: Int64, bits: Int64) throws {
        guard bits == 64 else { throw GlobalError.invalidValue("\(type(of: self)) expects 64 bits") }
        self.value = val
        self.bits = bits
    }

    init(integerLiteral: Int64) {
        self.value = integerLiteral
        self.bits = 64
    }
}

typealias DeferredAddressImmediate = DeferredImmediate<AbsoluteAddressImmediate>

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
            throw GlobalError.immediateMissingValue
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
        try require {
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


struct Imm12Lsl12 : Immediate, CustomDebugStringConvertible, ExpressibleByIntegerLiteral {
    enum Lsl12 {
        case _0 
        case _12
    }

    var bits: Int64 { imm.bits }
    var immediate: Int64 { imm.immediate }
    
    let imm: Immediate12
    let lsl: Lsl12

    var debugDescription: String {
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
