protocol HLByteListProvider {
    var nbytes: UInt32 { get }
    
    func getBytes(_ ix: Int) throws -> any BytesProvider
    
    
    /// Don't use in normal circumstances. This is meant to facilitate generating
    /// the `HLCode*` structure for tests.
    ///
    /// The second value of the return tuple is mapping byte-resource-indexes to offsets
    /// in the first value of the return tuple.
    /// - Returns: (continuous bytes, byte offsets)
    func getAllBytes_forWriters() throws -> ([UInt8], [Int32])
}
