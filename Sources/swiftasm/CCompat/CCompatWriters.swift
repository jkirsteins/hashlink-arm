class CCompatWriter_MainContext {
    let ctx: any JitContext2
    let code: UnsafeMutablePointer<HLCode_CCompat> = .allocate(capacity: 1)
    let mPtr: UnsafeMutablePointer<UnsafeMutablePointer<HLModule_CCompat>> = .allocate(capacity: 1)
    let file: String
    
    let codeWriter: CCompatWriter_HLCode
    
    init(_ ctx: any JitContext2, file: String) throws {
        self.ctx = ctx
        self.file = file
        self.codeWriter = try CCompatWriter_HLCode(ctx)
    }
    
    deinit {
        mPtr.deinitialize(count: 1)
        mPtr.deallocate()
        
        code.deinitialize(count: 1)
        code.deallocate()
    }
    
    func initialize(target: UnsafeMutablePointer<MainContext_CCompat>) throws {
        guard target.pointee.code == nil else {
            fatalError("Target already written: \(target.pointee.code)")
        }
        
        try codeWriter.initialize(target: self.code)
        mPtr.pointee = .init(mutating: LibHl.hl_module_alloc(.init(self.code)))
        
        file.withCString { fileCstr in
            target.initialize(to: MainContext_CCompat(
                code: self.code,
                m: mPtr.pointee,
                ret: nil,
                file: fileCstr,
                file_time: 0
            ))
        }
        
        // This initializes the function indexes etc.
        guard LibHl.hl_module_init(mPtr.pointee, false) == 1 else {
            throw GlobalError.unexpected("Failed to initialize the hl_module* after writing hl_code*")
        }
        
        print("Wrote all")
    }
}

class CCompatWriter_HLCode {
    let ctx: any JitContext2
    
    let ints: UnsafeMutablePointer<Int32>
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
    
    init(_ ctx: any JitContext2) throws {
        //
        let types: UnsafeMutablePointer<HLType_CCompat> = .allocate(capacity: Int(ctx.ntypes))
        // Initialize to dummies to allow lookup to not crash (as we initialize it, we might need to
        // look for dependent types within)
        types.initialize(repeating: HLType_CCompat(kind: .void), count: Int(ctx.ntypes))
        //
        
        let typeLookup: UnsafeBufferPointer<HLType_CCompat> = .init(start: types, count: Int(ctx.ntypes))
        self.ctx = ctx
        self.typeWriter = try CCompatWriter_HLTypes(ctx, typeLookup: typeLookup)
        self.funWriter = try CCompatWriter_HLFunctions(ctx, typeLookup: typeLookup)
        
        self.ints = .allocate(capacity: 0)
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
        self.natives = .allocate(capacity: 0)
        self.functions = .allocate(capacity: Int(ctx.nfunctions))
        self.constants = .allocate(capacity: 0)
    }
    
    deinit {
        self.ints.deinitialize(count: 0)
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
    }
    
    func initialize(target: UnsafeMutablePointer<HLCode_CCompat>) throws {
        print(types)
        try typeWriter.initialize(target: types)
        try funWriter.initialize(target: functions)
        
        // TODO: remove
        for ix in (0..<ctx.nfunctions) {
            let f = functions.advanced(by: Int(ix)).pointee
            print("Found f \(f)")
        }
        // TODO: remove
        
        print("Functions base \(functions)")
        target.initialize(to: HLCode_CCompat(
            version: 4,
            nints: 0,
            nfloats: 0,
            nstrings: 0,
            nbytes: 0,
            ntypes: ctx.ntypes,
            nglobals: 0,
            nnatives: 0,
            nfunctions: ctx.nfunctions,
            nconstants: 0,
            entrypoint: 0,
            ndebugfiles: 0,
            hasdebug: false,
            ints: ints,
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

extension UnsafeBufferPointer<HLType_CCompat> {
    /// Get the canonical representation of a type.
    ///
    /// We can't look for the type in ctx, as the context might be still being initialized.
    func getCCompatType(type: any HLTypeProvider) throws -> UnsafePointer<HLType_CCompat>? {
        print("Looking for \(type) in \(self.count)")
        
        for ix in 0..<self.count {
            let candidate = self.baseAddress!.advanced(by: ix)
            print("-- comparing \(type) to \(candidate)")
            let res = type.isEquivalent(candidate as any HLTypeProvider)
            print("   res: \(res)")
            if res { return candidate }
        }
        
        return nil
    }
}

class CCompatWriter_HLFunctions {
    let ctx: any JitContext2
    
    let writers: [CCompatWriter_HLFunction]
    
    init(_ ctx: any JitContext2, typeLookup: UnsafeBufferPointer<HLType_CCompat>) throws {
        self.ctx = ctx
        self.writers = try ctx.getOrderedCompilablesByRealIx__slow().map {
            try CCompatWriter_HLFunction(ctx, function: $0, types: typeLookup)
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
    let typeLookup: UnsafeBufferPointer<HLType_CCompat>
    
    // Writers
    let opsWriter: CCompatWriter_HLOpCodes
    
    init(
        _ ctx: any JitContext2,
        function: any Compilable2,
        types: UnsafeBufferPointer<HLType_CCompat>) throws {
        self.ctx = ctx
        self.function = function
        self.typeLookup = types
        
        ops = .allocate(capacity: function.ops.count)
        regs = .allocate(capacity: function.regs.count)
        
        opsWriter = try CCompatWriter_HLOpCodes(ctx, ops: function.ops)
    }
    
    deinit {
        ops.deallocate()
        regs.deallocate()
    }
    
    func initialize(target: UnsafeMutablePointer<HLFunction_CCompat>) throws {
        guard let funType = try typeLookup.getCCompatType(type: function.type) else {
            throw GlobalError.invalidOperation("Ret type \(function.type) not serialized.")
        }
        
        opsWriter.initialize(target: ops)
        
        guard let regBase = regs.baseAddress else {
            fatalError("No base addre for regs")
        }
        
        
        for (ix, swiftRegType) in self.function.regs.enumerated() {
            guard let mem = try typeLookup.getCCompatType(type: swiftRegType) else {
                fatalError("No memory for \(swiftRegType)")
            }
            regBase.advanced(by: ix).initialize(to: mem)
        }
        
        target.initialize(to: HLFunction_CCompat(
            findex: Int32(function.findex),
            nregs: Int32(function.regs.count),
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

enum CCompatWriterError : Error {
    case missingDependency(_ reason: String)
}

class CCompatWriter_HLType {
    
    let ctx: any JitContext2
    let typeIn: any HLTypeProvider
    
    /// For dependency lookup (e.g. for fun types)
    let typeLookup: UnsafeBufferPointer<HLType_CCompat>
    
    var union: UnsafeMutableRawPointer?
    
    // MARK: kind == .fun
    let funPtr: UnsafeMutablePointer<HLTypeFun_CCompat>?
    let funWriter: CCompatWriter_HLTypeFun?
    
    init(_ ctx: any JitContext2, type: any HLTypeProvider, typeLookup: UnsafeBufferPointer<HLType_CCompat>) throws {
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
            self.union = .init(funPtr)
        case .i32, .bool, .i64, .f32, .f64, .u8, .u16:
            funPtr = nil
            funWriter = nil
            union = nil
        default:
            fatalError("Unsupported type serialization \(typeIn.kind)")
        }
    }
    
    deinit {
        switch(typeIn.kind) {
        case .fun:
            guard let funPtr = funPtr else { fatalError("Expected funArgs to be allocated") }
            funPtr.deinitialize(count: 1)
            funPtr.deallocate()
        case .u8, .u16, .i32, .i64, .bool:
            break
        default:
            fatalError("Type kind \(typeIn.kind) deinit not implemented")
        }
    }
    
    func initialize(target: UnsafeMutablePointer<HLType_CCompat>) throws {
        print("Got type in: \(typeIn) fun:\(typeIn.funProvider)")
        print("CCompatWriter writing type: \(typeIn)")
        
        if let funPtr = self.funPtr, let funWriter = funWriter {
            try funWriter.initialize(target: funPtr)
        }
        
        print("Fun ptr: \(funPtr)")
        print("Union: \(union)")
        
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
    
    init(_ ctx: any JitContext2, typeLookup: UnsafeBufferPointer<HLType_CCompat>) throws {
        self.ctx = ctx
        self.writers = try (0..<ctx.ntypes).map { ix in
            try CCompatWriter_HLType(ctx, type: try ctx.getType(Int(ix)), typeLookup: typeLookup)
        }
    }
    
    deinit {
        
    }
    
    func initialize(target: UnsafeMutablePointer<HLType_CCompat>) throws {
        var failedFirstPass: [Int:CCompatWriter_HLType] = [:]
        
        // first pass
        for (ix, w) in self.writers.enumerated() {
            do {
                try w.initialize(target: target.advanced(by: ix))
            } catch CCompatWriterError.missingDependency(let msg) {
                print("Skipping \(w.typeIn) in first pass: \(msg)")
                failedFirstPass[ix] = w
            }
        }
        
        // second pass (in case dependencies were missed on first pass)
        for (ix, w) in failedFirstPass {
            try w.initialize(target: target.advanced(by: ix))
        }
    }
}

class CCompatWriter_HLTypeFun {
    let ctx: any JitContext2
    let funDataIn: any HLTypeFunProvider
    let argsPtr: UnsafeMutableBufferPointer<UnsafePointer<HLType_CCompat>>
    let typeLookup: UnsafeBufferPointer<HLType_CCompat>
    
    init(_ ctx: any JitContext2, fun: any HLTypeFunProvider, typeLookup: UnsafeBufferPointer<HLType_CCompat>) throws {
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

class CCompatWriter_HLOpCode {
    let ctx: any JitContext2
    let op: HLOpCode
    
    init(_ ctx: any JitContext2, _ opIn: HLOpCode) throws {
        self.ctx = ctx
        self.op = opIn
    }
    
    deinit {
        
    }
    
    func initialize(target: UnsafeMutablePointer<HLOpCode_CCompat>) {
        target.initialize(to: HLOpCode_CCompat(op))
    }
}

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
