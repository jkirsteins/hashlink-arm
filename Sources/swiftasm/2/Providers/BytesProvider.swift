// These should be used to facilitate tests, but otherwise not

protocol BytesProvider {
    func getBytes(count: Int) throws -> [UInt8]
    var ccompatAddress: OpaquePointer { get }
}

extension Array : BytesProvider where Element == UInt8 {
    func getBytes(count: Int) throws -> [UInt8] {
        guard count <= self.count else {
            throw GlobalError.invalidOperation("Can't fetch \(count) bytes. Only \(self.count) available.")
        }
        return Array(self.dropLast(self.count - count))
    }
    
    var ccompatAddress: OpaquePointer {
        fatalError("Can't get address from an array")
    }
}

extension UnsafeRawPointer : BytesProvider {
    func getBytes(count: Int) throws -> [UInt8] {
        let p: UnsafeBufferPointer<UInt8> = UnsafeBufferPointer(start: .init(OpaquePointer(self)), count: Int(count))
        return p.map { $0 }
    }
    
    var ccompatAddress: OpaquePointer { .init(self) }
}

extension UnsafePointer : BytesProvider where Pointee == CChar {
    func getBytes(count: Int) throws -> [UInt8] {
        let p: UnsafeBufferPointer<UInt8> = UnsafeBufferPointer(start: .init(OpaquePointer(self)), count: Int(count))
        return p.map { $0 }
    }
    
    var ccompatAddress: OpaquePointer { .init(self) }
}

extension BytesProvider {
    /// Meant for use in v simple tests, doesn't need to cover every case.
    func isEquivalent(_ other: any BytesProvider) -> Bool {
        if let la = self as? Array<UInt8>, let ra = other as? Array<UInt8> {
            return la == ra
        }
        return self.ccompatAddress == other.ccompatAddress
    }
}
