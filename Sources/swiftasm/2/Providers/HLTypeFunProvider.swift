
protocol HLTypeFunProvider: CustomDebugStringConvertible, Equatable, Hashable  {
    var argsProvider: [any HLTypeProvider] { get }
    var retProvider: any HLTypeProvider { get }
}

extension HLTypeFunProvider {
    /// Meant for use in v simple tests, doesn't need to cover every case.
    func isEquivalent(_ other: any HLTypeFunProvider) -> Bool {
        guard self.retProvider.isEquivalent(other.retProvider) else {
            return false
        }
        guard self.argsProvider.count == other.argsProvider.count else {
            return false
        }
        guard self.argsProvider.enumerated().allSatisfy({ (ix, lhsI) in
            lhsI.isEquivalent(other.argsProvider[ix])
        }) else {
            return false
        }
        
        return true
    }
}
