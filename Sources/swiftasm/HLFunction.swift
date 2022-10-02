struct HLFunction: CustomDebugStringConvertible {

    let type: Resolvable<HLType>
    let findex: Int32 
    let regs: [Resolvable<HLType>]
    let ops: [HLOpCode]

    var debugDescription: String {
        "fun<\(type.debugDescription)@\(findex) \(regs.count) regs \(ops.count) ops>"
    }
}