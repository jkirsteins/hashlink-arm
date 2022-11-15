@testable import swiftasm

struct Test_HLTypeFun : HLTypeProvider, HLTypeFunProvider, Equatable, Hashable, CustomDebugStringConvertible {
    static func == (lhs: Test_HLTypeFun, rhs: Test_HLTypeFun) -> Bool {
        fatalError("wip")
    }
    
    func hash(into hasher: inout Hasher) {
        fatalError("hash")
    }
    
    var hlRegSize: ByteCount { self.kind.hlRegSize }
    var kind: HLTypeKind { .fun }
    var ccompatAddress: UnsafeRawPointer { fatalError("Don't use this outside of tests.") }
    var argsProvider: [any HLTypeProvider]
    var retProvider: any HLTypeProvider
    
    var funProvider: (any HLTypeFunProvider)? { self }
    var objProvider: (any HLTypeObjProvider)? { nil }
    var tparamProvider: (any HLTypeProvider)? { nil }
    var tenumProvider: (any HLTypeEnumProvider)? { nil }
    
    var debugDescription: String {
        "Test_HLTypeFun(\(argsProvider.map { $0.debugDescription }.joined(separator: ", "))) -> (\(retProvider.debugDescription))"
    }
}
