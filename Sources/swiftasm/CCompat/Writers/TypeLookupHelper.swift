class TypeLookupHelper {
    let wrapped: UnsafeBufferPointer<HLType_CCompat>
    let voidIndex: UInt32
    
    init(start: UnsafePointer<HLType_CCompat>, count: Int, voidIndex: UInt32) {
        self.wrapped = .init(start: start, count: count)
        self.voidIndex = voidIndex
    }
    
    /// Get the canonical representation of a type.
    ///
    /// We can't look for the type in ctx, as the context might be still being initialized.
    func getCCompatType(type: any HLTypeProvider) throws -> UnsafePointer<HLType_CCompat>? {
        print("Looking for \(type) in \(wrapped.count)")
        
        for ix in 0..<wrapped.count {
            let candidate = wrapped.baseAddress!.advanced(by: ix)
            let res = type.isEquivalent(candidate as any HLTypeProvider)
            print("-- comparing against \(candidate._overrideDebugDescription) (res: \(res))")
            
            // .void can mean a) not initialized yet, or b) .void; so we need to match against a known index
            if res && (type.kind != .void || ix == voidIndex) {
                return candidate
            }
        }
        
        return nil
    }
}
