
class AnyHLTypeProvider : Equatable, Hashable, CustomDebugStringConvertible, HLTypeProvider {
    static func == (lhs: AnyHLTypeProvider, rhs: AnyHLTypeProvider) -> Bool {
        lhs.isEquivalent(rhs)
    }
    
    func hash(into hasher: inout Hasher) {
        hasher.combine(kind)
        hasher.combine(_funProvider)
    }
    
    var kind: HLTypeKind { _kind() }
    var hlRegSize: ByteCount { _hlRegSize() }
    var debugDescription: String { _debugDescription() }
    var ccompatAddress: UnsafeRawPointer { _ccompatAddress() }
    var funProvider: (any HLTypeFunProvider)? { _funProvider }
    var objProvider: (any HLTypeObjProvider)? { _objProvider }
    var tparamProvider: (any HLTypeProvider)? { _tparamProvider }
    
    let _ccompatAddress: ()->UnsafeRawPointer
    let _kind: ()->HLTypeKind
    let _hlRegSize: ()->ByteCount
    let _debugDescription: ()->String
    let _funProvider: AnyHLTypeFunProvider?
    let _objProvider: AnyHLTypeObjProvider?
    let _tparamProvider: AnyHLTypeProvider?
    
    init(_ wrapped: any HLTypeProvider) {
        self._kind = { wrapped.kind }
        self._hlRegSize = { wrapped.hlRegSize }
        self._debugDescription = { wrapped.debugDescription }
        self._ccompatAddress = { wrapped.ccompatAddress }
        
        if let fp = wrapped.funProvider {
            self._funProvider = AnyHLTypeFunProvider(fp)
        } else {
            self._funProvider = nil
        }
        
        if let op = wrapped.objProvider {
            self._objProvider = AnyHLTypeObjProvider(op)
        } else {
            self._objProvider = nil
        }
        
        if let tparam = wrapped.tparamProvider {
            self._tparamProvider = AnyHLTypeProvider(tparam)
        } else {
            self._tparamProvider = nil
        }
    }
}
