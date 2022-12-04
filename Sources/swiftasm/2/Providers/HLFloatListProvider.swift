protocol HLFloatListProvider {
    var nfloats: UInt32 { get }
    
    func getFloat(_ ix: Int) throws -> Float64
}
