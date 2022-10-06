struct HLNative: CustomDebugStringConvertible {
    let lib: Resolvable<String>
    let name: Resolvable<String>
    let type: Resolvable<HLType>
    let findex: Int32
    let memory: UnsafeMutableRawPointer

    var debugDescription: String {
        "HLNative<\(lib.debugDescription), \(name.debugDescription), \(type.debugDescription), \(findex), mem \(memory)>"
    }
}