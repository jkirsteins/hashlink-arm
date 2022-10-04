struct HLFunction: CustomDebugStringConvertible {

    let type: Resolvable<HLType>
    let findex: Int32 
    let regs: [Resolvable<HLType>]
    let ops: [HLOpCode]
    let assigns: [HLFunctionAssign]

    var debugDescription: String {
        "fun@\(findex)<\(type.debugDescription) \(regs.count) regs \(ops.count) ops \(assigns.count) assigns>"
    }
}