
class CCompatWriter_HLOpCode {
    let ctx: any JitContext2
    let op: HLOpCode
    let parsedOp: HLOpCode_CCompat
    let extra: UnsafeMutableBufferPointer<Int32>?
    
    init(_ ctx: any JitContext2, _ opIn: HLOpCode) throws {
        self.ctx = ctx
        self.op = opIn
        
        var extra: UnsafeMutableBufferPointer<Int32>? = nil
        self.parsedOp = HLOpCode_CCompat(op, &extra)
        self.extra = extra
    }
    
    deinit {
        self.extra?.deallocate()
    }
    
    func initialize(target: UnsafeMutablePointer<HLOpCode_CCompat>) {
        target.initialize(to: parsedOp)
    }
}
