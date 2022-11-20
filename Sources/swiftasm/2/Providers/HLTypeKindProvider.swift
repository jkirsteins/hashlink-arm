

protocol HLTypeKindProvider : OverrideCustomDebugStringConvertible, Equatable, Hashable {
    var kind: HLTypeKind { get }
    var hlRegSize: ByteCount { get }
}

extension HLTypeKindProvider {
    var debugDescription: String {
        self.kind.debugDescription
    }
}

extension HLTypeKind : HLTypeKindProvider {
    var kind: HLTypeKind { self }
}
