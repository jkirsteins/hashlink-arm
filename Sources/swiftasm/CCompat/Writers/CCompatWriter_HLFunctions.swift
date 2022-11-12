class CCompatWriter_HLFunctions {
    let ctx: any JitContext2
    
    let writers: [CCompatWriter_HLFunction]
    
    init(_ ctx: any JitContext2, typeLookup: TypeLookupHelper) throws {
        self.ctx = ctx
        self.writers = try ctx.getOrderedCompilablesByRealIx__slow().map {
            try CCompatWriter_HLFunction(ctx, function: $0, typeLookup: typeLookup)
        }
    }
    
    deinit {
        
    }
    
    func initialize(target: UnsafeMutablePointer<HLFunction_CCompat>) throws {
        // first pass
        for (ix, w) in self.writers.enumerated() {
            try w.initialize(target: target.advanced(by: ix))
        }
    }
}


class CCompatWriter_HLFunction {
    let ctx: any JitContext2
    let function: any Compilable2
    
    // Pointers
    let ops: UnsafeMutableBufferPointer<HLOpCode_CCompat>
    let regs: UnsafeMutableBufferPointer<UnsafePointer<HLType_CCompat>>
    
    // Not for serialization
    let typeLookup: TypeLookupHelper
    
    // Writers
    let opsWriter: CCompatWriter_HLOpCodes
    
    init(
        _ ctx: any JitContext2,
        function: any Compilable2,
        typeLookup: TypeLookupHelper) throws {
            self.ctx = ctx
            self.function = function
            self.typeLookup = typeLookup
            
            ops = .allocate(capacity: function.ops.count)
            regs = .allocate(capacity: function.regsProvider.count)
            
            opsWriter = try CCompatWriter_HLOpCodes(ctx, ops: function.ops)
        }
    
    deinit {
        ops.deallocate()
        regs.deallocate()
    }
    
    func initialize(target: UnsafeMutablePointer<HLFunction_CCompat>) throws {
        guard let funType = try typeLookup.getCCompatType(type: function.typeProvider) else {
            throw GlobalError.invalidOperation("Ret type \(function.typeProvider) not serialized.")
        }
        
        opsWriter.initialize(target: ops)
        
        guard let regBase = regs.baseAddress else {
            fatalError("No base addre for regs")
        }
        
        
        for (ix, swiftRegType) in self.function.regsProvider.enumerated() {
            guard let mem = try typeLookup.getCCompatType(type: swiftRegType) else {
                fatalError("No memory for \(swiftRegType)")
            }
            regBase.advanced(by: ix).initialize(to: mem)
        }
        
        target.initialize(to: HLFunction_CCompat(
            findex: Int32(function.findex),
            nregs: Int32(function.regsProvider.count),
            nops: Int32(function.ops.count),
            ref: 0,
            typePtr: funType,
            regsPtr: regBase,
            opsPtr: ops.baseAddress,
            debug: nil,
            objPtr: nil,
            unionPtr: nil)
        )
    }
}
