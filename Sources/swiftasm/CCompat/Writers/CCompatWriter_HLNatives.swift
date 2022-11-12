import Foundation

class CCompatWriter_HLNatives {
    let ctx: any JitContext2
    
    let writers: [CCompatWriter_HLNative]
    
    init(_ ctx: any JitContext2, typeLookup: TypeLookupHelper) throws {
        self.ctx = ctx
        self.writers = try ctx.getOrderedNativesByRealIx__slow().map {
            try CCompatWriter_HLNative(ctx, callable: $0, typeLookup: typeLookup)
        }
    }
    
    deinit {
        
    }
    
    func initialize(target: UnsafeMutablePointer<HLNative_CCompat>) throws {
        // first pass
        for (ix, w) in self.writers.enumerated() {
            try w.initialize(target: target.advanced(by: ix))
        }
    }
}

class CCompatWriter_HLNative {
    let ctx: any JitContext2
    let callable: any NativeCallable2
    
    // Not for serialization
    let typeLookup: TypeLookupHelper
    
    let libNameData: Data
    let funcNameData: Data
    
    let libNamePtr: UnsafeMutableBufferPointer<UInt8>
    let funcNamePtr: UnsafeMutableBufferPointer<UInt8>
    
    init(
        _ ctx: any JitContext2,
        callable: any NativeCallable2,
        typeLookup: TypeLookupHelper) throws {
            self.ctx = ctx
            self.callable = callable
            self.typeLookup = typeLookup
            
            self.libNameData = (callable.libProvider.stringValue + "\0").data(using: .utf8)!
            self.funcNameData = (callable.nameProvider.stringValue + "\0").data(using: .utf8)!
            
            self.libNamePtr = .allocate(capacity: self.libNameData.count)
            self.funcNamePtr = .allocate(capacity: self.funcNameData.count)
            
    }
    
    deinit {
        libNamePtr.deallocate()
        funcNamePtr.deallocate()
    }
    
    func initialize(target: UnsafeMutablePointer<HLNative_CCompat>) throws {
        guard let funType = try typeLookup.getCCompatType(type: callable.typeProvider) else {
            throw GlobalError.invalidOperation("Callable type \(callable.typeProvider) not serialized.")
        }
        
        _ = libNamePtr.initialize(from: self.libNameData)
        _ = funcNamePtr.initialize(from: self.funcNameData)
        
        target.initialize(to: HLNative_CCompat(
            libPtr: .init(OpaquePointer(libNamePtr.baseAddress!)),
            namePtr: .init(OpaquePointer(funcNamePtr.baseAddress!)),
            typePtr: funType,
            findex: Int64(callable.findex))
        )
    }
}
