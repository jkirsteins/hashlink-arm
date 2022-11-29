protocol HLStringListProvider {
    var nstrings: UInt32 { get }
    
    func getString(_ ix: Int) throws -> any StringProvider
}
