import Foundation

class AnyHLEnumConstructProvider : Equatable, Hashable, CustomDebugStringConvertible, HLEnumConstructProvider {
    var nameProvider: StringProvider { fatalError("not implemented") }
    
    var params: UnsafePointer<UnsafePointer<HLType_CCompat>> { fatalError("not implemented") }
    
    var size: Int32 { fatalError("not implemented") }
    
    var hasptr: Bool { fatalError("not implemented") }
    
    var offsets: UnsafePointer<Int32> { fatalError("not implemented") }
    
    var debugDescription: String { fatalError("not implemented") }
    
    static func == (lhs: AnyHLEnumConstructProvider, rhs: AnyHLEnumConstructProvider) -> Bool {
        fatalError("not implemented")
    }
    
    func hash(into hasher: inout Hasher) {
        fatalError("not implemented")
    }
    
}
