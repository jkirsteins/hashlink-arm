
protocol HLTypeObjProvider: CustomDebugStringConvertible, Equatable, Hashable  {
    var nameProvider: any StringProvider { get }
    var fieldsProvider: [any HLObjFieldProvider] { get }
}

extension UnsafePointer<HLTypeObj_CCompat> : HLTypeObjProvider {
    var fieldsProvider: [HLObjFieldProvider] {
        Array(UnsafeBufferPointer(start: self.pointee.fieldsPtr, count: Int(self.pointee.nfields)))
    }
    
    var nameProvider: any StringProvider {
        self.pointee.namePtr
    }
}

extension HLTypeObjProvider {
    /// Meant for use in v simple tests, doesn't need to cover every case.
    func isEquivalent(_ other: any HLTypeObjProvider) -> Bool {
        
        guard self.nameProvider.isEquivalent(other.nameProvider) else {
            print("failed")
            return false
        }
        
        for f in self.fieldsProvider {
            print("lhs fields: \(f)")
        }
        
        print("lhs fields \(self.fieldsProvider.count)")
        print("rhs fields \(other.fieldsProvider.count)")
        guard self.fieldsProvider.count == other.fieldsProvider.count else {
            return false }
        guard self.fieldsProvider.enumerated().allSatisfy({ (ix, lhsI) in
            lhsI.isEquivalent(other.fieldsProvider[ix])
        }) else {
            return false
        }
        
        print("HLTypeObjProvider: true")
        return true
    }
}
