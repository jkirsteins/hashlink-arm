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
    
    init(_ ccompat: HLObjField_CCompat) {
        self.name = Resolvable(ccompat.name, memory: nil)
        self.type = Resolvable(HLType(ccompat.tPtr.pointee), memory: ccompat.tPtr)
    }
}
