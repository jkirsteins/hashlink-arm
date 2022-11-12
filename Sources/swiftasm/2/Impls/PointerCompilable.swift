
struct PointerCompilable : Compilable2 {
    
    let findex: RefFun
    let ops: [HLOpCode]
    let linkableAddress: any LinkableAddress
    
    let regsProvider: [any HLTypeProvider]
    
    var address: any MemoryAddress { linkableAddress }
    
    var typeProvider: any HLTypeProvider
}
