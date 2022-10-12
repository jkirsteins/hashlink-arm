protocol RelativeOffset {
    var value: Int64 { get }
}
typealias RelativeLiteralOffset = Int64
typealias RelativeDeferredOffset = SharedStorage<Int64>

extension RelativeLiteralOffset : RelativeOffset {
    var value: Int64 { self }
}

extension RelativeDeferredOffset : RelativeOffset {
    var value: Int64 { self.wrappedValue }
}
