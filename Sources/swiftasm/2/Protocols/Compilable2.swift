protocol Compilable2: Callable2 {
    var ops: [HLOpCode] { get }
    var regsProvider: [any HLTypeProvider] { get }
    var linkableAddress: any LinkableAddress { get }
}
