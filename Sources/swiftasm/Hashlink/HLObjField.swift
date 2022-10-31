import Foundation

struct HLObjField: Equatable, CustomDebugStringConvertible, Hashable {
    let name: Resolvable<String>
    let type: Resolvable<HLType>

    var debugDescription: String {
        "\(name.value): <\(type.value.debugName)>@\(type.ix)"
    }
    
    init(name: Resolvable<String>, type: Resolvable<HLType>) {
        self.name = name
        self.type = type
    }
    
    init(name: String, type: HLType) {
        self.name = Resolvable(name, memory: nil)
        self.type = Resolvable(type, memory: nil)
    }
    
    init(_ ccompat: UnsafePointer<HLObjField_CCompat>) {
        let name = Resolvable(ccompat.pointee.name, memory: ccompat.pointee.namePtr)
        self.name = name
        self.type = .type(fromUnsafe: ccompat.pointee.tPtr)
    }
}
