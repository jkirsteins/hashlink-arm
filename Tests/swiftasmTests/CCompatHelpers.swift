import XCTest

@testable import swiftasm

extension HLTypeKind : HLTypeProvider {
    public var ccompatAddress: UnsafeRawPointer {
        fatalError("Don't use this outside of tests.")
    }
    
    public var kind: HLTypeKind { self }
    
    public var funProvider: (any HLTypeFunProvider)? {
        switch(self) {
        case .fun:
            fatalError("HLTypeKind can not be used as a function type")
        default:
            return nil
        }
        
    }
    
    public var objProvider: (any HLTypeObjProvider)? {
        switch(self) {
        case .obj:
            fatalError("HLTypeKind can not be used as a function type")
        default:
            return nil
        }
        
    }
}


