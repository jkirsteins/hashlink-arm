protocol MemoryAddress : Equatable {
    var value: UnsafeMutableRawPointer { get }
    
    func isEqual(_ to: any MemoryAddress) -> Bool 
}

extension UnsafeMutableRawPointer: MemoryAddress {
    var value: UnsafeMutableRawPointer { self }
    
    func isEqual(_ to: any MemoryAddress) -> Bool {
        guard let to = to as? UnsafeMutableRawPointer else {
            return false 
        }
        return self == to
    }
}

typealias DeferredAbsoluteAddress = SharedStorage<UnsafeMutableRawPointer?>

extension SharedStorage<UnsafeMutableRawPointer?>: MemoryAddress {
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
}

struct DeferredAddress: Equatable, MemoryAddress {
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
        guard let to = to as? DeferredAddress else {
            return false 
        }
        return self == to
    }
}