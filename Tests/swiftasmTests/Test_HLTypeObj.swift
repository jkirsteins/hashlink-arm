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
    
    let fieldsProvider: [swiftasm.HLObjFieldProvider]
    let nameProvider: any StringProvider
    
    var debugDescription: String {
        "HLTypeObj"
    }
    
    init(fieldsProvider: [swiftasm.HLObjFieldProvider], nameProvider: any StringProvider = "testObject") {
        self.fieldsProvider = fieldsProvider
        self.nameProvider = nameProvider
    }
}
