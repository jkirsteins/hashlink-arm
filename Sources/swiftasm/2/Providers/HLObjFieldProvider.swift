
protocol HLObjFieldProvider {
    var nameProvider: any StringProvider { get }
    var typeProvider: any HLTypeProvider { get }
}

extension UnsafePointer<HLObjField_CCompat> : HLObjFieldProvider {
    var nameProvider: any StringProvider { self.pointee.nameProvider }
    var typeProvider: any HLTypeProvider { self.pointee.typeProvider }
}

extension HLObjField_CCompat : HLObjFieldProvider {
    var nameProvider: any StringProvider { self.namePtr }
    var typeProvider: any HLTypeProvider { self.tPtr }
}

extension HLObjFieldProvider {
    /// Meant for use in v simple tests, doesn't need to cover every case.
    func isEquivalent(_ other: any HLObjFieldProvider) -> Bool {
        
        guard self.typeProvider.isEquivalent(other.typeProvider) else {
            return false
        }
        guard self.nameProvider.isEquivalent(other.nameProvider) else {
            return false
        }
        
        return true
    }
}
