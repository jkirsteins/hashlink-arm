protocol Callable2 {
    var findex: RefFun { get }
    var address: any MemoryAddress { get }
    var typeProvider: any HLTypeProvider { get }
}

extension Callable2 {
    var retProvider: any HLTypeProvider { self.typeProvider.funProvider!.retProvider }
    var argsProvider: [any HLTypeProvider] { self.typeProvider.funProvider!.argsProvider }
}
