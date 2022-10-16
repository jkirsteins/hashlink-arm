struct HLCompiledFunction : Equatable, WholeFunction, CustomDebugStringConvertible, Hashable {
    let function: HLFunction
    let memory: any MemoryAddress

    var type: Resolvable<HLType> { function.type }
    var regs: [Resolvable<HLType>] { function.regs }
    var ops: [HLOpCode] { function.ops }
    var findex: Int32 { function.findex }
    
    var debugDescription: String {
        function.debugDescription
    }

    var wholeFunctionDebugDescription: String {
        "compiled/ops:\(function.ops.count)/regs:\(function.regs.count)/\(findex)"
    }

    func hash(into hasher: inout Hasher) {
        function.hash(into: &hasher)
        memory.hash(into: &hasher)
    }

    static func == (lhs: Self, rhs: Self) -> Bool {
        return lhs.function == rhs.function  &&
            lhs.memory.isEqual(rhs.memory) && 
            lhs.type == rhs.type && 
            lhs.findex == rhs.findex 
    }
}