/// Like `FullyDeferredRelativeAddress` that also updates the address in CCompat `function_ptrs`.
struct CCompatUpdatingLinkableAddress : LinkableAddress {
    var hasOffset: Bool { self.wrapped.hasOffset }
    
    var value: UnsafeMutableRawPointer { self.wrapped.value }
    
    func isEqual(_ to: any MemoryAddress) -> Bool {
        self.wrapped.isEqual(to)
    }
    
    let wrapped: FullyDeferredRelativeAddress
    let ccompat: UnsafeMutablePointer<UnsafeRawPointer?>
    
    init(jitBase: JitBase, ccompat: UnsafeMutablePointer<UnsafeRawPointer?>) {
        self.wrapped = FullyDeferredRelativeAddress(jitBase: jitBase)
        self.ccompat = ccompat
    }
    
    func setOffset(_ offset: ByteCount) {
        self.wrapped.setOffset(offset)
    }
    
    
}
