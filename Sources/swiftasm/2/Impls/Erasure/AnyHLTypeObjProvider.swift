
class AnyHLTypeObjProvider : Equatable, Hashable, CustomDebugStringConvertible, HLTypeObjProvider {
    var debugDescription: String { _debugDescription() }
    var fieldsProvider: [any HLObjFieldProvider] { _fieldsProvider() }
    var bindingsProvider: [(Int32, Int32)] { _bindingsProvider() }
    var protoProvider: [any HLObjProtoProvider] { _protoProvider() }
    var nameProvider: any StringProvider { _nameProvider() }
    var superTypeProvider: (any HLTypeProvider)? { _superTypeProvider() }
    
    var _bindingsProvider: ()->[(Int32, Int32)]
    var _fieldsProvider: ()->[any HLObjFieldProvider]
    var _protoProvider: ()->[any HLObjProtoProvider]
    var _nameProvider: ()->any StringProvider
    var _superTypeProvider: ()->(any HLTypeProvider)?
    let _debugDescription: ()->String
    
    static func == (lhs: AnyHLTypeObjProvider, rhs: AnyHLTypeObjProvider) -> Bool {
        let res = lhs.isEquivalent(rhs)
        return res
    }
    
    func hash(into hasher: inout Hasher) {
        
    }
    
    init(_ wrapped: any HLTypeObjProvider) {
        self._debugDescription = { wrapped.debugDescription }
        self._fieldsProvider = { wrapped.fieldsProvider }
        self._protoProvider = { wrapped.protoProvider }
        self._nameProvider = { wrapped.nameProvider }
        self._superTypeProvider = { wrapped.superTypeProvider }
        self._bindingsProvider = { wrapped.bindingsProvider }
    }
}
