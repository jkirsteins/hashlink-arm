protocol RelativeOffset : CustomDebugStringConvertible {
    var value: Int64 { get }
}
typealias RelativeLiteralOffset = Int64

extension RelativeLiteralOffset : RelativeOffset {
    var value: Int64 { self }
}

struct RelativeDeferredOffset : RelativeOffset, CustomDebugStringConvertible {
    let storage = SharedStorage(wrappedValue: Int64(0))
    var value: Int64 { self.storage.wrappedValue }

    var startStop: Int64? = nil

    mutating func start(at position: Int64) {
        guard startStop == nil else { fatalError("Already started") }
        self.startStop = position
    }

    mutating func stop(at position: Int64) {
        guard let startStop = startStop else { fatalError("Already started") }
        self.storage.wrappedValue = position - startStop
        self.startStop = nil
    }

    public var debugDescription: String {
        "\(self.storage.wrappedValue)"
    }
}

extension RelativeLiteralOffset : CustomDebugStringConvertible {
    public var debugDescription: String {
        "\(self)"
    }
}


