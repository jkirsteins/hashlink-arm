///
/// Reference to writing the data: https://github.com/HaxeFoundation/haxe/blob/c35bbd4472c3410943ae5199503c23a2b7d3c5d6/src/generators/genhl.ml#L3840
class HLTypeObj: Equatable, CustomDebugStringConvertible, Hashable {
    static func == (lhs: HLTypeObj, rhs: HLTypeObj) -> Bool {
        return lhs.name == rhs.name &&
            lhs.superType == rhs.superType &&
            lhs.global == rhs.global &&
            lhs.fields == rhs.fields &&
            lhs.proto == rhs.proto &&
            lhs.bindings == rhs.bindings
    }
    
    func hash(into hasher: inout Hasher) {
        hasher.combine(name)
        hasher.combine(superType)
        hasher.combine(global)
        hasher.combine(fields)
        hasher.combine(proto)
        hasher.combine(bindings)
    }
    
    let name: Resolvable<String>
    let superType: Resolvable<HLType>?
    let global: Int32?

    let fields: [Resolvable<HLObjField>]
    let proto: [Resolvable<HLObjProto>]
    let bindings: [HLTypeBinding]
    
    init(name: Resolvable<String>, superType: Resolvable<HLType>?, global: Int32?, fields: [Resolvable<HLObjField>], proto: [Resolvable<HLObjProto>], bindings: [HLTypeBinding]) {
        self.name = name
        self.superType = superType
        self.global = global
        self.fields = fields
        self.proto = proto
        self.bindings = bindings
    }

    var debugDescription: String {
        """
        \(name.debugDescription) \(superType == nil ? "" : "extends \(superType!.debugDescription)")
        global: \(global?.debugDescription ?? "nil")
        fields: \(fields.count > 0 ? "\n" : "")\((fields.map { "  \($0.debugDescription)" }).joined(separator: "\n"))
        protos: \(proto.count > 0 ? "\n" : "")\(proto.map { "  \($0.debugDescription)" }.joined(separator: "\n"))
        bindings: \(bindings.count > 0 ? "\n" : "")\(bindings.map { "  \($0.debugDescription)" }.joined(separator: "\n"))
        """
    }
}
