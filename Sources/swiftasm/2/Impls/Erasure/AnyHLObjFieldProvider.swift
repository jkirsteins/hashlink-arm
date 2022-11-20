
class AnyHLObjFieldProvider : Equatable, Hashable, HLObjFieldProvider {
    var nameProvider: any StringProvider { _nameProvider() }
    var typeProvider: any HLTypeProvider { _typeProvider() }
    
    var _nameProvider: ()->any StringProvider
    var _typeProvider: ()->any HLTypeProvider
    
    static func == (lhs: AnyHLObjFieldProvider, rhs: AnyHLObjFieldProvider) -> Bool {
        lhs.isEquivalent(rhs)
    }
    
    func hash(into hasher: inout Hasher) {
        
    }
    
    init(_ wrapped: any HLObjFieldProvider) {
        self._nameProvider = { wrapped.nameProvider }
        self._typeProvider = { wrapped.typeProvider }
    }
}
