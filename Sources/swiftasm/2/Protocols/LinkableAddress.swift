protocol LinkableAddress : MemoryAddress {
    func setOffset(_ offset: ByteCount)
    var hasOffset: Bool { get }
}
