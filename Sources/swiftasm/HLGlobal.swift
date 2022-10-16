struct HLGlobal : Equatable, CustomDebugStringConvertible, Hashable {
    let type: Resolvable<HLType>

    var debugDescription: String {
        return "global<\(type.debugDescription)>"
    }
}