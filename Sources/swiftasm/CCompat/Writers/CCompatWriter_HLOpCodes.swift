class CCompatWriter_HLOpCodes {
    let ctx: any JitContext2
    
    let writers: [CCompatWriter_HLOpCode]
    
    init(_ ctx: any JitContext2, ops: [HLOpCode]) throws {
        self.ctx = ctx
        self.writers = try ops.map { op in
            try CCompatWriter_HLOpCode(ctx, op)
        }
    }
    
    deinit {
        
    }
    
    func initialize(target: UnsafeMutableBufferPointer<HLOpCode_CCompat>) {
        guard let baseAddr = target.baseAddress else {
            fatalError("No base address")
        }
        for (ix, w) in self.writers.enumerated() {
            w.initialize(target: baseAddr.advanced(by: ix))
        }
    }
}
