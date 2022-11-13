import XCTest

@testable import swiftasm

struct Test_HLObjField : HLObjFieldProvider, CustomDebugStringConvertible {
    let nameProvider: any StringProvider
    let typeProvider: any HLTypeProvider
    
    var debugDescription: String {
        "Test_HLObjField(\(nameProvider.stringValue) \(typeProvider.debugDescription)"
    }
}
