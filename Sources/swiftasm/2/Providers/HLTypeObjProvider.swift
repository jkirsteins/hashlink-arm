
protocol HLTypeObjProvider: CustomDebugStringConvertible, Equatable, Hashable  {
    var nameProvider: any StringProvider { get }
    var superTypeProvider: (any HLTypeProvider)? { get }
    var fieldsProvider: [any HLObjFieldProvider] { get }
}

extension UnsafePointer<HLTypeObj_CCompat> : HLTypeObjProvider {
    var fieldsProvider: [HLObjFieldProvider] {
        Array(UnsafeBufferPointer(start: self.pointee.fieldsPtr, count: Int(self.pointee.nfields)))
    }
    
    var nameProvider: any StringProvider {
        self.pointee.namePtr
    }
    
    var superTypeProvider: (any HLTypeProvider)? {
        self.pointee.superPtr
    }
}

extension HLTypeObjProvider {
    /// Meant for use in v simple tests, doesn't need to cover every case.
    func isEquivalent(_ other: any HLTypeObjProvider) -> Bool {
        
        guard self.nameProvider.isEquivalent(other.nameProvider) else {
            return false
        }
        
        if let lhsSuper = self.superTypeProvider, let rhsSuper = other.superTypeProvider {
            guard lhsSuper.isEquivalent(rhsSuper) else {
                return false
            }
        } else if self.superTypeProvider == nil && other.superTypeProvider == nil {
            // both nil is cool
        } else {
            return false
        }
        
        guard self.fieldsProvider.count == other.fieldsProvider.count else {
            return false }
        guard self.fieldsProvider.enumerated().allSatisfy({ (ix, lhsI) in
            lhsI.isEquivalent(other.fieldsProvider[ix])
        }) else {
            return false
        }
        
        return true
    }
}
