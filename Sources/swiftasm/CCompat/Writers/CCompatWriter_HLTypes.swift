
class CCompatWriter_HLType {
    
    let ctx: any JitContext2
    let typeIn: any HLTypeProvider
    
    /// For dependency lookup (e.g. for fun types)
    let typeLookup: TypeLookupHelper
    
    var union: UnsafeMutableRawPointer?
    
    let logger = LoggerFactory.create(CCompatWriter_HLType.self)
    
    // MARK: kind == .fun
    let funPtr: UnsafeMutablePointer<HLTypeFun_CCompat>?
    let funWriter: CCompatWriter_HLTypeFun?
    
    // MARK: kind == .obj
    let objPtr: UnsafeMutablePointer<HLTypeObj_CCompat>?
    let objWriter: CCompatWriter_HLTypeObj?
    
    init(_ ctx: any JitContext2, type: any HLTypeProvider, typeLookup: TypeLookupHelper) throws {
        self.ctx = ctx
        self.typeIn = type
        self.typeLookup = typeLookup
        
        switch(typeIn.kind)
        {
        case .fun:
            guard let funData = type.funProvider else {
                fatalError("Invalid")
            }
            let funPtr: UnsafeMutablePointer<HLTypeFun_CCompat> = .allocate(capacity: 1)
            self.funPtr = funPtr
            self.funWriter = try .init(ctx, fun: funData, typeLookup: typeLookup)
            self.objPtr = nil
            self.objWriter = nil
            self.union = .init(funPtr)
        case .obj:
            guard let objData = type.objProvider else {
                fatalError("Invalid")
            }
            let objPtr: UnsafeMutablePointer<HLTypeObj_CCompat> = .allocate(capacity: 1)
            self.objPtr = objPtr
            self.objWriter = try .init(ctx, obj: objData, typeLookup: typeLookup)
            self.funPtr = nil
            self.funWriter = nil
            self.union = .init(objPtr)
        case .u8, .u16, .i32, .i64, .bool, .void, .dyn, .bytes, .type, .f32, .f64:
            funPtr = nil
            funWriter = nil
            objPtr = nil
            objWriter = nil
            union = nil
        default:
            fatalError("Unsupported type serialization \(typeIn.kind)")
        }
    }
    
    deinit {
        switch(typeIn.kind) {
        case .fun:
            guard let funPtr = funPtr else { fatalError("Expected funPtr to be allocated") }
            funPtr.deinitialize(count: 1)
            funPtr.deallocate()
        case .obj:
            guard let objPtr = objPtr else { fatalError("Expected objPtr to be allocated") }
            objPtr.deinitialize(count: 1)
            objPtr.deallocate()
        case .u8, .u16, .i32, .i64, .bool, .void, .dyn, .bytes, .type, .f32, .f64:
            break
        default:
            fatalError("Type kind \(typeIn.kind) deinit not implemented")
        }
    }
    
    func initialize(target: UnsafeMutablePointer<HLType_CCompat>) throws {
        logger.info("Serializing \(String(describing: self.typeIn))")
        defer { self.logger.info("Finished serializing \(String(describing: self.typeIn))") }
        
        if let funPtr = self.funPtr, let funWriter = funWriter {
            try funWriter.initialize(target: funPtr)
        }
        
        if let objPtr = self.objPtr, let objWriter = objWriter {
            try objWriter.initialize(target: objPtr)
        }
        
        target.initialize(to: HLType_CCompat(
            kind: typeIn.kind,
            union: union,
            vobjProto: nil,
            markBits: nil)
        )
    }
}


class CCompatWriter_HLTypes {
    let ctx: any JitContext2
    
    let writers: [CCompatWriter_HLType]
    
    let logger = LoggerFactory.create(CCompatWriter_HLTypes.self)
    
    init(_ ctx: any JitContext2, typeLookup: TypeLookupHelper) throws {
        self.ctx = ctx
        self.writers = try (0..<ctx.ntypes).map { ix in
            try CCompatWriter_HLType(ctx, type: try ctx.getType(Int(ix)), typeLookup: typeLookup)
        }
    }
    
    deinit {
        
    }
    
    func initialize(target: UnsafeMutablePointer<HLType_CCompat>) throws {
        
        let RETRY_PASSES = 1
        
        var toIterateOver: [(Int, CCompatWriter_HLType)] = self.writers.enumerated().map { ($0, $1) }
        
        for pass in 0..<RETRY_PASSES+1 {
            logger.info("Starting pass #\(pass)")
            var failedInPass: [(Int, CCompatWriter_HLType)] = []
            for (ix, w) in toIterateOver {
                do {
                    let dest = target.advanced(by: ix)
                    logger.info("[Pass #\(pass)] Writing #\(ix): \(String(describing: w.typeIn._overrideDebugDescription)) at \(String(describing: dest))")
                    try w.initialize(target: dest)
                } catch CCompatWriterError.missingDependency(let msg) {
                    guard pass < RETRY_PASSES else {
                        logger.critical("[Pass #\(pass)] Failed to serialize type \(String(describing: w.typeIn)): \(msg))")
                        throw CCompatWriterError.missingDependency(msg)
                    }
                    logger.info("[Pass #\(pass)] Skipping \(String(describing: w.typeIn)) in pass #\(pass): \(msg)")
                    failedInPass.append((ix, w))
                }
            }
            toIterateOver = failedInPass
        }
    }
}
