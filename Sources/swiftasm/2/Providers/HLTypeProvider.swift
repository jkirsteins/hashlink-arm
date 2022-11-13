
protocol HLTypeProvider: HLTypeKindProvider, OverrideCustomDebugStringConvertible, Equatable, Hashable {
    var kind: HLTypeKind { get }
    var funProvider: (any HLTypeFunProvider)? { get }
    var objProvider: (any HLTypeObjProvider)? { get }
    
    var ccompatAddress: UnsafeRawPointer { get }
}


extension HLTypeProvider {
    /// Meant for use in v simple tests, doesn't need to cover every case.
    func isEquivalent(_ other: any HLTypeProvider) -> Bool {
        guard self.kind == other.kind else { return false }
        switch(self.kind) {
        case .i32, .u8, .u16, .i64, .bool, .void, .dyn, .bytes, .type:
            break
        case .fun:
            guard let lhs = self.funProvider, let rhs = other.funProvider else {
                fatalError("fun type must have funProvider set")
            }
            return lhs.isEquivalent(rhs)
        case .obj:
            guard let lhs = self.objProvider, let rhs = other.objProvider else {
                fatalError("obj type must have objProvider set")
            }
            
            let res = lhs.isEquivalent(rhs)
            print("HLTypeProvider: true")
            return res
        default:
            fatalError("HLTypeProvider.isEquivalent not implemented for \(self.kind)")
        }
        
        return true
    }
}
