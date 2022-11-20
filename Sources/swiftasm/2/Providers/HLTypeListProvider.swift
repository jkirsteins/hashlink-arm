
protocol HLTypeListProvider {
    var ntypes: UInt32 { get }
    
    /// Get type from index.
    func getType(_ ix: Int) throws -> any HLTypeProvider
}
