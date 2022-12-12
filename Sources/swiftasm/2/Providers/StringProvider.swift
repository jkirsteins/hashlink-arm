protocol StringProvider {
    var stringValue: String { get }
    var ccompatCCharAddress: UnsafePointer<CChar> { get }
}

extension UnsafePointer : StringProvider {
    var stringValue: String {
        if Pointee.self == CChar16.self {
            return String._wrapUtf16(from: self as! UnsafePointer<CChar16>)
        } else if Pointee.self == CChar.self {
            return String.wrapUtf8(from: self as! UnsafePointer<CChar>)
        } else {
            fatalError("UnsafePointer<\(Pointee.self)> can not be used as StringProvider")
        }
    }
    
    var ccompatCCharAddress: UnsafePointer<CChar> {
        if Pointee.self == CChar.self {
            return self as! UnsafePointer<CChar>
        } else {
            fatalError("UnsafePointer<\(Pointee.self)> can not be used as StringProvider")
        }
    }
}

extension String : StringProvider {
    var stringValue: String { self }
    var ccompatCCharAddress: UnsafePointer<CChar> {
        fatalError("ccompatCCharAddress unavailable in String")
    }
}

extension StringProvider {
    /// Meant for use in v simple tests, doesn't need to cover every case.
    func isEquivalent(_ other: any StringProvider) -> Bool {
        return self.stringValue == other.stringValue
    }
}
