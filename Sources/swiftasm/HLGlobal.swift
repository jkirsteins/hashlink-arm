struct HLGlobal : CustomDebugStringConvertible {
    let type: Resolvable<HLType>

    var debugDescription: String {
        return "global<\(type.debugDescription)>"
    }
}