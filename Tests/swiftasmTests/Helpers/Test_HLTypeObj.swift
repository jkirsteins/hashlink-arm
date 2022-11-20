import XCTest

@testable import swiftasm

struct Test_HLTypeObj : HLTypeProvider, HLTypeObjProvider, Equatable, Hashable, CustomDebugStringConvertible {
    
    static func == (lhs: Test_HLTypeObj, rhs: Test_HLTypeObj) -> Bool {
        fatalError("")
    }
    
    func hash(into hasher: inout Hasher) {
        fatalError("")
    }
    
    var hlRegSize: ByteCount { self.kind.hlRegSize }
    var kind: HLTypeKind { .obj }
    var ccompatAddress: UnsafeRawPointer { fatalError("Don't use this outside of tests.") }
    
    var funProvider: (any HLTypeFunProvider)? { nil }
    var objProvider: (any HLTypeObjProvider)? { self }
    var tparamProvider: (any HLTypeProvider)? { nil }
    var tenumProvider: (any HLTypeEnumProvider)? { nil }
    
    let fieldsProvider: [swiftasm.HLObjFieldProvider]
    let nameProvider: any StringProvider
    let superTypeProvider: (any HLTypeProvider)?
    
    var debugDescription: String {
        "HLTypeObj(\(self.nameProvider.stringValue))"
    }
    
    init(
        fieldsProvider: [swiftasm.HLObjFieldProvider],
        nameProvider: any StringProvider = "testObject",
        superTypeProvider: (any HLTypeProvider)? = nil
    ) {
        self.fieldsProvider = fieldsProvider
        self.nameProvider = nameProvider
        self.superTypeProvider = superTypeProvider
    }
}
