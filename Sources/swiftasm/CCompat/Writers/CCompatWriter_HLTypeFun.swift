class CCompatWriter_HLTypeFun {
    let ctx: any JitContext2
    let funDataIn: any HLTypeFunProvider
    let argsPtr: UnsafeMutableBufferPointer<UnsafePointer<HLType_CCompat>>
    let typeLookup: TypeLookupHelper
    
    init(_ ctx: any JitContext2, fun: any HLTypeFunProvider, typeLookup: TypeLookupHelper) throws {
        self.ctx = ctx
        self.funDataIn = fun
        self.typeLookup = typeLookup
        self.argsPtr = .allocate(capacity: funDataIn.argsProvider.count)
    }
    
    deinit {
        self.argsPtr.deallocate()
    }
    
    func initialize(target: UnsafeMutablePointer<HLTypeFun_CCompat>) throws {
        // We might have a missing dependency if we're serializing a type that depends
        // on a yet-unserialized type. Throw, skip, and do a second pass later.
        
        guard let retPtr = try self.typeLookup.getCCompatType(type: funDataIn.retProvider) else {
            throw CCompatWriterError.missingDependency("HLTypeFun has unserialized return type \(funDataIn.retProvider)")
        }
        
        let args = try funDataIn.argsProvider.map { argProv in
            guard let found = try self.typeLookup.getCCompatType(type: argProv) else {
                throw CCompatWriterError.missingDependency("HLTypeFun has unserialized argument type \(argProv)")
            }
            return found
        }
        _ = argsPtr.initialize(from: args)
        
        target.initialize(to: HLTypeFun_CCompat(
            argsPtr: argsPtr.baseAddress!,
            retPtr: retPtr,
            nargs: UInt32(funDataIn.argsProvider.count),
            parent: nil,
            // TODO: closure values?
            closure_type: HLType_CCompat_Fun_ClosureType(kind: .void, p: nil),
            closure: HLType_CCompat_Fun_Closure(argsPtr: nil, ret: nil, nargs: 0, parent: nil))
        )
    }
}
