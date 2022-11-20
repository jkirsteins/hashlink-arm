
class AnyHLTypeObjProvider : Equatable, Hashable, CustomDebugStringConvertible, HLTypeObjProvider {
    var debugDescription: String { _debugDescription() }
    var fieldsProvider: [any HLObjFieldProvider] { _fieldsProvider() }
    var nameProvider: any StringProvider { _nameProvider() }
    var superTypeProvider: (any HLTypeProvider)? { _superTypeProvider() }
    
    var _fieldsProvider: ()->[any HLObjFieldProvider]
    var _nameProvider: ()->any StringProvider
    var _superTypeProvider: ()->(any HLTypeProvider)?
    let _debugDescription: ()->String
    
    static func == (lhs: AnyHLTypeObjProvider, rhs: AnyHLTypeObjProvider) -> Bool {
        let res = lhs.isEquivalent(rhs)
        print("AnyHLTypeObjProvider isEquivalent: \(res)")
        return res
    }
    
    func hash(into hasher: inout Hasher) {
        
    }
    
    init(_ wrapped: any HLTypeObjProvider) {
        self._debugDescription = { wrapped.debugDescription }
        self._fieldsProvider = { wrapped.fieldsProvider }
        self._nameProvider = { wrapped.nameProvider }
        self._superTypeProvider = { wrapped.superTypeProvider }
    }
}
