
protocol HLGlobalListProvider {
    var nglobals: UInt32 { get }
    
    // hlcode needs to contain types, to facilitate allocating memory
    // it does not contain the actual object (must be initialized at runtime)
    func getGlobal(_ globalRef: Ref) throws -> (any HLTypeProvider)?
}
