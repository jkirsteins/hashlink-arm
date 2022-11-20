import Foundation 

protocol MemoryAddress : Equatable, Immediate, Hashable {
    var value: UnsafeMutableRawPointer { get }
    
    func isEqual(_ to: any MemoryAddress) -> Bool 
    func update(from: DeferredBaseRelativeAddress)
}

extension UnsafeRawPointer: MemoryAddress, DeferredMemoryAddress {
    var value: UnsafeMutableRawPointer { UnsafeMutableRawPointer(mutating: self) }
    
    func isEqual(_ to: any MemoryAddress) -> Bool {
        self.value == to.value
    }
}

extension OpaquePointer: MemoryAddress, DeferredMemoryAddress {
    var value: UnsafeMutableRawPointer { UnsafeMutableRawPointer(mutating: .init(self)) }
    
    func isEqual(_ to: any MemoryAddress) -> Bool {
        self.value == to.value
    }
}

extension UnsafeMutableRawPointer: MemoryAddress, DeferredMemoryAddress {
    var value: UnsafeMutableRawPointer { self }
    
    func isEqual(_ to: any MemoryAddress) -> Bool {
        guard let to = to as? UnsafeMutableRawPointer else {
            return false 
        }
        return self == to
    }
}

typealias DeferredAbsoluteAddress = SharedStorage<UnsafeMutableRawPointer?>

extension UnsafeMutableRawPointer: ExpressibleByIntegerLiteral {
    public init(integerLiteral: Int64) {
        self = UnsafeMutableRawPointer(bitPattern: Int(integerLiteral))!
    }
}

extension SharedStorage<UnsafeMutableRawPointer?>: Immediate, DeferredMemoryAddress, MemoryAddress {
    var value: UnsafeMutableRawPointer {
        guard let res = self.wrappedValue else {
            fatalError("Deferred absolute address not available")
        }
        return res 
    }

    static func == (lhs: Self, rhs: Self) -> Bool {
        lhs.wrappedValue == rhs.wrappedValue
    }

    func isEqual(_ to: any MemoryAddress) -> Bool {
        guard let to = to as? SharedStorage<UnsafeMutableRawPointer?> else {
            return false 
        }
        return self == to
    }

    var hasUsableValue: Bool { self.wrappedValue != nil }

    var bits: Int64 { 64 }
    var immediate: Int64 {Int64(Int(bitPattern: self.value)) }

    init(_ val: Int64, bits: Int64) {
        fatalError("Not implemented")
    }
}

protocol DeferredMemoryAddress: MemoryAddress, Immediate {
    func update(from: DeferredBaseRelativeAddress) 
}

// extension DeferredMemoryAddress {
//     var bits: Int64 { 64 }
//     var immediate: Int64 {Int64(Int(bitPattern: self.value)) }

//     init(_ val: Int64, bits: Int64) {
//         fatalError("Not implemented")
//     }

//     func update(from: DeferredBaseRelativeAddress) {
//         fatalError("\(type(of: self)) can not merge with DeferredBaseRelativeAddress")    
//     }
// }

extension MemoryAddress {
    var bits: Int64 { 64 }
    var immediate: Int64 {Int64(Int(bitPattern: self.value)) }

    init(_ val: Int64, bits: Int64) {
        fatalError("Not implemented")
    }

    func update(from: DeferredBaseRelativeAddress) {
        fatalError("\(type(of: self)) can not merge with DeferredBaseRelativeAddress")    
    }
}

// neither base nor offset known
struct FullyDeferredRelativeAddress: Equatable, DeferredMemoryAddress, LinkableAddress, MemoryAddress, Hashable, CustomDebugStringConvertible {
    let jitBase: SharedStorage<UnsafeMutableRawPointer?>
    let offsetFromBase: SharedStorage<ByteCount?> = SharedStorage(wrappedValue: nil)

    var hasBase: Bool { self.jitBase.wrappedValue != nil }
    var hasOffset: Bool { self.offsetFromBase.wrappedValue != nil }

    var value: UnsafeMutableRawPointer {
        guard let base = self.jitBase.wrappedValue, let offsetFromBase = self.offsetFromBase.wrappedValue else {
            fatalError("Fully deferred address not available (base: \(hasBase); offset: \(hasOffset)). At: \(Thread.callStackSymbols.joined(separator: "\n"))")
        }
        return base.advanced(by: Int(offsetFromBase))
    }

    var debugDescription: String {
        "FullyDeferredRelativeAddress<hasBase: \(hasBase); hasOffset: \(hasOffset)>"
    }

    var hasUsableValue: Bool {
        self.hasBase && self.hasOffset
    }

    static func == (lhs: Self, rhs: Self) -> Bool {
        lhs.jitBase == rhs.jitBase &&
        lhs.offsetFromBase == rhs.offsetFromBase
    }

    func isEqual(_ to: any MemoryAddress) -> Bool {
        guard let to = to as? FullyDeferredRelativeAddress else {
            return false 
        }
        return self == to 
    }

    func update(from: DeferredBaseRelativeAddress) {
        guard self.jitBase.isSameStorage(from.jitBase) else {
            fatalError("Can only merge addresses with same jit base")
        }
        self.setOffset(from.offsetFromBase)
    }
    
    func setOffset(_ offset: ByteCount) {
        self.offsetFromBase.wrappedValue = offset
    }
    
    
}

// base is deferred, but offset is known
struct DeferredBaseRelativeAddress: Equatable, MemoryAddress, Hashable {
    let jitBase: SharedStorage<UnsafeMutableRawPointer?>
    let offsetFromBase: ByteCount

    var value: UnsafeMutableRawPointer {
        guard let base = self.jitBase.wrappedValue else {
            fatalError("Deferred address not available")
        }
        return base.advanced(by: Int(offsetFromBase))
    }

    static func == (lhs: Self, rhs: Self) -> Bool {
        lhs.jitBase == rhs.jitBase &&
        lhs.offsetFromBase == rhs.offsetFromBase
    }

    func isEqual(_ to: any MemoryAddress) -> Bool {
        guard let to = to as? DeferredBaseRelativeAddress else {
            return false 
        }
        return self == to
    }
}
