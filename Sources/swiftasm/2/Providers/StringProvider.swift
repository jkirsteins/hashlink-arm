protocol StringProvider {
    var stringValue: String { get }
}

extension UnsafePointer<CChar16> : StringProvider {
    var stringValue: String { String._wrapUtf16(from: self) }
}

extension String : StringProvider {
    var stringValue: String { self }
}

extension StringProvider {
    /// Meant for use in v simple tests, doesn't need to cover every case.
    func isEquivalent(_ other: any StringProvider) -> Bool {
        return self.stringValue == other.stringValue
    }
}
