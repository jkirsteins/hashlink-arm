class CCompatWriter_HLObjFields {
    let ctx: any JitContext2
    
    let writers: [CCompatWriter_HLObjField]
    
    init(_ ctx: any JitContext2, fields: [any HLObjFieldProvider], typeLookup: TypeLookupHelper) throws {
        self.ctx = ctx
        self.writers = try fields.map {
            try CCompatWriter_HLObjField(ctx, field: $0, typeLookup: typeLookup)
        }
    }
    
    deinit {
        
    }
    
    func initialize(target: UnsafeMutablePointer<HLObjField_CCompat>) throws {
        // first pass
        for (ix, w) in self.writers.enumerated() {
            print("Writing obj field \(target.advanced(by: ix))")
            try w.initialize(target: target.advanced(by: ix))
        }
    }
}
