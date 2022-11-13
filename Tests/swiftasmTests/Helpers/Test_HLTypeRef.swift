import XCTest

@testable import swiftasm

struct Test_HLTypeRef : HLTypeProvider, Equatable, Hashable, CustomDebugStringConvertible {
    
    var funProvider: (any HLTypeFunProvider)? { nil }
    var objProvider: (any HLTypeObjProvider)? { nil }
    
    static func == (lhs: Test_HLTypeRef, rhs: Test_HLTypeRef) -> Bool {
        fatalError("")
    }
    
    func hash(into hasher: inout Hasher) {
        fatalError("")
    }
    
    var hlRegSize: ByteCount { self.kind.hlRegSize }
    var kind: HLTypeKind { .ref }
    var ccompatAddress: UnsafeRawPointer { fatalError("Don't use this outside of tests.") }
    
    let tparamProvider: (any HLTypeProvider)?
    
    var debugDescription: String {
        "HLTypeRef(\(self.tparamProvider!._overrideDebugDescription))"
    }
    
    init(tparamProvider: any HLTypeProvider) {
        self.tparamProvider = tparamProvider
    }
}
