struct HLConstant : Equatable, CustomDebugStringConvertible {
    let global: Resolvable<HLGlobal>
    let	fields: [TableIndex]

    var debugDescription: String {
        return "Constant(global \(global) fields: \(fields))"
    }
}