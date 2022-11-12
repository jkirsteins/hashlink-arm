extension UnsafePointer<HLType_CCompat> : HLTypeProvider, HLTypeKindProvider {
    var ccompatAddress: UnsafeRawPointer { .init(self) }
    var kind: HLTypeKind { self.pointee.kind }
    var hlRegSize: ByteCount { kind.hlRegSize }
    var funProvider: (any HLTypeFunProvider)? {
        switch(self.kind) {
        case .fun:
            return self.pointee.fun
        default:
            return nil
        }
    }
    var objProvider: (any HLTypeObjProvider)? {
        switch(self.kind) {
        case .obj:
            return self.pointee.obj
        default:
            return nil
        }
    }
}
