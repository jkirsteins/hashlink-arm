import Foundation

class AnyHLTypeEnumProvider : Equatable, Hashable, CustomDebugStringConvertible, HLTypeEnumProvider {
    var debugDescription: String { fatalError("not implemented") }
    
    var nameProvider: any StringProvider { fatalError("not implemented") }
    
    var constructsProvider: [any HLEnumConstructProvider] { fatalError("not implemented") }
    
    var global_value: UnsafeRawPointer { fatalError("not implemented") }
    
    static func == (lhs: AnyHLTypeEnumProvider, rhs: AnyHLTypeEnumProvider) -> Bool {
        fatalError("not implemented")
    }
    
    func hash(into hasher: inout Hasher) {
        fatalError("not implemented")
    }
    
    init(_ wrapped: any HLTypeEnumProvider) {
        fatalError("not implemented")
    }
}
