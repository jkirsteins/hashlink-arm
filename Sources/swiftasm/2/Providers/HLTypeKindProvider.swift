

protocol HLTypeKindProvider : OverrideCustomDebugStringConvertible, Equatable, Hashable {
    var kind: HLTypeKind { get }
    var hlRegSize: ByteCount { get }
}

extension HLTypeKind {
    var isPointer: Bool {
        switch(self) {
        case .i64, .i32, .u16, .u8: return false
        case .f64, .f32: return false
        case .bool: return false
        case .void: return false
        default: return true
        }
    }
    
    var isSigned: Bool? {
        switch(self) {
        case .i64, .i32: return true
        case .u16, .u8: return false
        case .f64, .f32: return true
        default: return nil
        }
    }
}

extension HLTypeKindProvider {
    var debugDescription: String {
        self.kind.debugDescription
    }
}

extension HLTypeKind : HLTypeKindProvider {
    var kind: HLTypeKind { self }
}
