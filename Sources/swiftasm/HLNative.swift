struct HLNative: Equatable, WholeFunction, CustomDebugStringConvertible {
    let lib: Resolvable<String>
    let name: Resolvable<String>
    let type: Resolvable<HLType>
    let findex: Int32
    let memory: any MemoryAddress

    var wholeFunctionDebugDescription: String {
        "native/\(lib.value)/\(name.value)/\(findex)"
    }

    var debugDescription: String {
        "HLNative<\(lib.debugDescription), \(name.debugDescription), \(type.debugDescription), \(findex), mem \(memory)>"
    }

    static func == (lhs: Self, rhs: Self) -> Bool {
        return lhs.memory.isEqual(rhs.memory)
    }
}