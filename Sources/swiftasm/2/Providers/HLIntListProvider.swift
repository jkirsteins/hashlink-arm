protocol HLIntListProvider {
    var nints: UInt32 { get }
    
    func getInt(_ ix: Int) throws -> Int32
}
