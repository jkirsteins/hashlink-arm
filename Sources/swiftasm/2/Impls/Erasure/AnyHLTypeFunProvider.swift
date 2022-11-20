
class AnyHLTypeFunProvider : Equatable, Hashable, CustomDebugStringConvertible, HLTypeFunProvider {
    static func == (lhs: AnyHLTypeFunProvider, rhs: AnyHLTypeFunProvider) -> Bool {
        lhs.isEquivalent(rhs)
    }
    
    func hash(into hasher: inout Hasher) {
        hasher.combine(_args)
        hasher.combine(_ret)
    }
    
    let _args: [AnyHLTypeProvider]
    var argsProvider: [any HLTypeProvider] { _args }
    
    let _ret: AnyHLTypeProvider
    var retProvider: any HLTypeProvider { _ret }
    
    let debugDescription: String
    
    init(_ wrapped: any HLTypeFunProvider) {
        self.debugDescription = wrapped.debugDescription
        self._args = wrapped.argsProvider.map { AnyHLTypeProvider($0) }
        self._ret = AnyHLTypeProvider(wrapped.retProvider)
    }
    
}
