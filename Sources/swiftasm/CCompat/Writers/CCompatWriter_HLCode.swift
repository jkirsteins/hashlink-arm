
class CCompatWriter_HLCode {
    let ctx: any JitContext2
    
    let logger = LoggerFactory.create(CCompatWriter_HLCode.self)
    
    let ints: UnsafeMutableBufferPointer<Int32>
    let floats: UnsafeMutablePointer<Double>
    let strings: UnsafeMutablePointer<UnsafePointer<CChar>>
    let string_lens: UnsafeMutablePointer<UInt32>
    let bytes: UnsafeMutablePointer<Int8>
    let bytes_pos: UnsafeMutablePointer<Int32>
    let debugfiles: UnsafeMutablePointer<UnsafePointer<CChar>>
    let debugfiles_lens: UnsafeMutablePointer<UInt32>
    let ustrings: UnsafeMutablePointer<UnsafePointer<CChar16>>
    let types: UnsafeMutablePointer<HLType_CCompat>
    let globals: UnsafeMutablePointer<UnsafePointer<HLType_CCompat>>
    let natives: UnsafeMutablePointer<HLNative_CCompat>
    let functions: UnsafeMutablePointer<HLFunction_CCompat>
    let constants: UnsafeMutablePointer<HLConstant_CCompat>
    
    let typeWriter: CCompatWriter_HLTypes
    let funWriter: CCompatWriter_HLFunctions
    let nativesWriter: CCompatWriter_HLNatives
    
    // Workaround to not overwrite the void type during initialization. All types are .void initially,
    // so when doing type lookup for .void, we also need to match the expected index.
    let voidIndex: UInt32
    
    init(_ ctx: any JitContext2) throws {
        //
        let types: UnsafeMutablePointer<HLType_CCompat> = .allocate(capacity: Int(ctx.ntypes))
        // Initialize to dummies to allow lookup to not crash (as we initialize it, we might need to
        // look for dependent types within)
        types.initialize(repeating: HLType_CCompat(kind: .void), count: Int(ctx.ntypes))
        //
        
        // workaround for .void
        var voidIndex: UInt32? = nil
        for ix in 0..<ctx.ntypes {
            if try ctx.getType(Int(ix)).kind == .void {
                voidIndex = ix
                break
            }
        }
        guard let voidIndex = voidIndex else {
            fatalError(".void must be one of the types")
        }
        self.voidIndex = voidIndex
        //
        
        let typeLookup: TypeLookupHelper = .init(start: types, count: Int(ctx.ntypes), voidIndex: voidIndex)
        self.ctx = ctx
        self.typeWriter = try CCompatWriter_HLTypes(ctx, typeLookup: typeLookup)
        self.funWriter = try CCompatWriter_HLFunctions(ctx, typeLookup: typeLookup)
        self.nativesWriter = try CCompatWriter_HLNatives(ctx, typeLookup: typeLookup)
        
        self.ints = .allocate(capacity: Int(ctx.nints))
        self.floats = .allocate(capacity: 0)
        self.strings = .allocate(capacity: 0)
        self.string_lens = .allocate(capacity: 0)
        self.bytes = .allocate(capacity: 0)
        self.bytes_pos = .allocate(capacity: 0)
        self.debugfiles = .allocate(capacity: 0)
        self.debugfiles_lens = .allocate(capacity: 0)
        self.ustrings = .allocate(capacity: 0)
        self.types = types
        self.globals = .allocate(capacity: 0)
        self.natives = .allocate(capacity: Int(ctx.nnatives))
        self.functions = .allocate(capacity: Int(ctx.nfunctions))
        self.constants = .allocate(capacity: 0)
    }
    
    deinit {
        self.floats.deinitialize(count: 0)
        self.strings.deinitialize(count: 0)
        self.string_lens.deinitialize(count: 0)
        self.bytes.deinitialize(count: 0)
        self.bytes_pos.deinitialize(count: 0)
        self.debugfiles.deinitialize(count: 0)
        self.debugfiles_lens.deinitialize(count: 0)
        self.ustrings.deinitialize(count: 0)
        self.types.deinitialize(count: Int(ctx.ntypes))
        self.globals.deinitialize(count: 0)
        self.functions.deinitialize(count: Int(ctx.nfunctions))
        self.constants.deinitialize(count: 0)
        self.natives.deinitialize(count: Int(ctx.nnatives))
        
        self.ints.deallocate()
        self.floats.deallocate()
        self.strings.deallocate()
        self.string_lens.deallocate()
        self.bytes.deallocate()
        self.bytes_pos.deallocate()
        self.debugfiles.deallocate()
        self.debugfiles_lens.deallocate()
        self.ustrings.deallocate()
        self.types.deallocate()
        self.globals.deallocate()
        self.functions.deallocate()
        self.constants.deallocate()
        self.natives.deallocate()
    }
    
    func initialize(target: UnsafeMutablePointer<HLCode_CCompat>) throws {
        print(types)
        try typeWriter.initialize(target: types)
        try funWriter.initialize(target: functions)
        try nativesWriter.initialize(target: natives)
        
        let intArray = try (0..<ctx.nints).map { try ctx.getInt(Int($0)) }
        _ = self.ints.initialize(from: intArray)
        
        // TODO: remove
        for ix in (0..<ctx.nfunctions) {
            let f = functions.advanced(by: Int(ix)).pointee
            print("Found f \(f)")
        }
        // TODO: remove
        
        print("Functions base \(functions)")
        target.initialize(to: HLCode_CCompat(
            version: 4,
            nints: ctx.nints,
            nfloats: 0,
            nstrings: 0,
            nbytes: 0,
            ntypes: ctx.ntypes,
            nglobals: 0,
            nnatives: ctx.nnatives,
            nfunctions: ctx.nfunctions,
            nconstants: 0,
            entrypoint: 0,
            ndebugfiles: 0,
            hasdebug: false,
            ints: ints.baseAddress!,
            floats: floats,
            strings: strings,
            string_lens: string_lens,
            bytes: bytes,
            bytes_pos: bytes_pos,
            debugfiles: debugfiles,
            debuffiles_lens: debugfiles_lens,
            ustrings: ustrings,
            types: types,
            globals: globals,
            natives: natives,
            functions: functions,
            constants: constants,
            alloc: 0,
            falloc: 0))
    }
}
