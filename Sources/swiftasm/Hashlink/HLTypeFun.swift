struct HLTypeFun: Equatable, CustomDebugStringConvertible, Hashable {
    let args: [Resolvable<HLType>]
    let ret: Resolvable<HLType>

    var debugDescription: String {
        return
            "(\(args.map { $0.debugDescription }.joined(separator: ", "))) -> (\(ret.debugDescription))"
    }
}
