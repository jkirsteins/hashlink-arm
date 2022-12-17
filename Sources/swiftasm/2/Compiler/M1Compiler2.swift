import Darwin

extension M1Compiler2 : CompilerUtilities2 {
    
}

struct _StringX {
    let t: UnsafePointer<HLType_CCompat>
    let bytes: UnsafePointer<CChar16>
    let length: Int32
}

let FP_TYPE_KINDS = [HLTypeKind.f32, HLTypeKind.f64]
let INTEGER_TYPE_KINDS = [HLTypeKind.u8, HLTypeKind.u16, HLTypeKind.i32, HLTypeKind.i64]
let NUMERIC_TYPE_KINDS = FP_TYPE_KINDS + INTEGER_TYPE_KINDS

protocol CompilerUtilities2 {
    func appendDebugPrintAligned4(_ val: String, builder: CpuOpBuffer);
}

typealias Registers2 = [any HLTypeProvider]

let OEnumIndex_impl: (@convention(c)(OpaquePointer)->(Int32)) = {
    _enum in
    
    let enumPtr: UnsafePointer<venum> = .init(_enum)
    return enumPtr.pointee.index
}

// MARK: Determine dyn set/get/cast
func get_dynset( from kind: HLTypeKind ) -> OpaquePointer {
    switch( kind ) {
    case .f32:
        return unsafeBitCast(LibHl._hl_dyn_setf, to: OpaquePointer.self)
    case .f64:
        return unsafeBitCast(LibHl._hl_dyn_setd, to: OpaquePointer.self)
    case .i32, .u16, .u8, .bool:
        return unsafeBitCast(LibHl._hl_dyn_seti, to: OpaquePointer.self)
    default:
        return unsafeBitCast(LibHl._hl_dyn_setp, to: OpaquePointer.self)
    }
}

func get_dynget( to kind: HLTypeKind ) -> OpaquePointer {
    switch( kind ) {
    case .f32:
        return unsafeBitCast(LibHl._hl_dyn_getf, to: OpaquePointer.self)
    case .f64:
        return unsafeBitCast(LibHl._hl_dyn_getd, to: OpaquePointer.self)
    case .i32, .u16, .u8, .bool:
        return unsafeBitCast(LibHl._hl_dyn_geti, to: OpaquePointer.self)
    default:
        return unsafeBitCast(LibHl._hl_dyn_getp, to: OpaquePointer.self)
    }
}

fileprivate func get_dyncast( to kind: HLTypeKind ) -> OpaquePointer {
    switch( kind ) {
    case .f32:
        return unsafeBitCast(LibHl._hl_dyn_castf, to: OpaquePointer.self)
    case .f64:
        return unsafeBitCast(LibHl._hl_dyn_castd, to: OpaquePointer.self)
    case .i32, .u16, .u8, .bool:
        return unsafeBitCast(LibHl._hl_dyn_casti, to: OpaquePointer.self)
    default:
        return unsafeBitCast(LibHl._hl_dyn_castp, to: OpaquePointer.self)
    }
}

// MARK: Enum impl methods

let OEnumField_impl: (@convention(c)(OpaquePointer, Int32, Int32)->(OpaquePointer)) = {
    _enum, constructIndex, fieldIndex in
    
    let enumPtr: UnsafePointer<venum> = .init(_enum)
    let constructPtr = enumPtr.pointee.t.pointee.tenum.pointee.constructs.advanced(by: Int(constructIndex))
    let result = constructPtr.pointee.offsets.advanced(by: Int(fieldIndex))
    return .init(result)
}

let OSetEnumField_impl: (@convention(c) (
    OpaquePointer,  // type
    Int32,          // field index
    Int32           // source
)->()) = {
    (_type, fieldIndex, source) in
    let type: UnsafePointer<venum> = .init(_type)
    let fieldPtr = type.pointee.t.pointee.tenum.pointee.constructs.pointee.offsets.advanced(by: Int(fieldIndex))
    let mFieldPtr: UnsafeMutablePointer<Int32> = .init(mutating: fieldPtr)
    mFieldPtr.pointee = source
}

let OEnumAlloc_impl: (@convention(c) (
    OpaquePointer,  // type
    Int32           // construct index
)->(OpaquePointer)) = {
    _type, index in
    
    let type: UnsafePointer<HLType_CCompat> = .init(_type)
    let result = LibHl.hl_alloc_enum(.init(type), index)
    
    return .init(result)
}

let OMakeEnum_impl: (@convention(c) (
    OpaquePointer,  // type
    Int32,          // construct index
    Int32,          // arg count
    OpaquePointer,  // args (reg)
    OpaquePointer   // args (values in int64)
)->(OpaquePointer)) = {
    _type, index, argCount, _argRegs, _argValues in
    
    let argRegs: UnsafeMutableBufferPointer<Reg> =  .init(start: .init(_argRegs), count: Int(argCount))
    let argValues: UnsafeMutableBufferPointer<Int64> = .init(start: .init(_argValues), count: Int(argCount))
    let type: UnsafePointer<HLType_CCompat> = .init(_type)
    defer {
        argRegs.deallocate()
        argValues.deallocate()
    }
    
    let result = LibHl.hl_alloc_enum(.init(type), index)
    
    let cPtr = type.pointee.tenum.pointee.constructs.advanced(by: Int(index))
    assert(cPtr.pointee.nparams == argCount)
    
    let mutatingConstruct: UnsafeMutablePointer<HLEnumConstruct_CCompat> = .init(mutating: cPtr)
    for paramIx in (0..<Int(cPtr.pointee.nparams)) {
        let argValueInt32 = Int32(argValues[paramIx])
        let offsetPtr = cPtr.pointee.offsets.advanced(by: paramIx)
        let mutatingOffsetPtr: UnsafeMutablePointer<Int32> = .init(mutating: offsetPtr)
        mutatingOffsetPtr.pointee = argValueInt32
    }
    
    return .init(result)
}

// MARK: Dyn impl

let OToDyn_impl: (@convention(c) (/*dstType*/UnsafeRawPointer, /*srcType*/UnsafeRawPointer, /*dstVal*/Int64, /*srcVal*/Int64)->(UnsafeRawPointer)) = {
    (dstType, srcType, dst, src) in
    
    let dstTypeB = dstType.bindMemory(to: HLType_CCompat.self, capacity: 1)
    let srcTypeB = srcType.bindMemory(to: HLType_CCompat.self, capacity: 1)
    
    /*
     dst must be:
     - dyn
     - null (see mod2 fn@3)
     */
    assert(dstTypeB.kind == .dyn || dstTypeB.kind == .null)
    let res = LibHl.hl_alloc_dynamic(srcTypeB)  // use the source type
    
    var mutatingRes: UnsafeMutableRawPointer = .init(mutating: res)
      
    switch(srcTypeB.pointee.kind) {
    case .i32:
        let srcI32 = Int32(truncatingIfNeeded: src)
        mutatingRes.advanced(by: 8).bindMemory(to: Int32.self, capacity: 1).pointee = srcI32
        mutatingRes.advanced(by: 8).bindMemory(to: Int32.self, capacity: 1).pointee = srcI32
        assert(res.pointee.i == srcI32)
    case .bool, .u8:
        let srcU8 = UInt8(truncatingIfNeeded: src)
        mutatingRes.advanced(by: 8).bindMemory(to: UInt8.self, capacity: 1).pointee = srcU8
        assert(res.pointee.b == (srcU8 > 0))
        assert(res.pointee.ui8 == srcU8)
    default:
        fatalError("Casting ToDyn from \(srcTypeB.pointee.kind) not implemented")
    }
    
    assert(res.pointee.t.pointee.kind == srcTypeB.pointee.kind)
    
    return .init(res)
}

// MARK: Compiler extension

// x0 through x7
let ARG_REGISTER_COUNT = 8
extension M1Compiler2 {
    /// Stack space should be allocated for first 8 function args (will be filled from registers) and
    /// any register, which is not part of args.
    /// - Parameters:
    ///   - regs:
    ///   - args:
    /// - Returns:
    func calcStackArgReq(regs unfilteredRegs: [any HLTypeKindProvider], args unfilteredArgs: [any HLTypeKindProvider])
    -> (Int16, [any HLTypeKindProvider])
    {
        let regs = unfilteredRegs.filter { $0.kind != .void }
        let args = unfilteredArgs.filter { $0.kind != .void }
        guard regs.prefix(args.count).map({ $0.kind }) == args.prefix(args.count).map({$0.kind}) else {
            fatalError(
                "Args must match the first registers (got \(args) and \(regs) respectively)"
            )
        }
        let stackArgs = Array( /* regs should be aligned with args */
            // IMPORTANT: args must come first
            args.prefix(ARG_REGISTER_COUNT) + regs.dropFirst(args.count)
        )
        
        let result = stackArgs.reduce(0) { $0 + Int16($1.hlRegSize) }
        return (StackInfo.roundUpStackReservation(result), stackArgs)
    }
    
    func isNumeric(vreg: Reg, kinds: [any HLTypeKindProvider]) -> Bool {
        Self.isNumeric(vreg: vreg, kinds: kinds)
    }
    
    static func isNumeric(vreg: Reg, kinds: [any HLTypeKindProvider]) -> Bool {
        return NUMERIC_TYPE_KINDS.contains( (kinds[Int(vreg)] as (any HLTypeKindProvider)).kind )
    }
    
    func isInteger(vreg: Reg, kinds: [any HLTypeKindProvider]) -> Bool {
        Self.isInteger(vreg: vreg, kinds: kinds)
    }
    
    static func isInteger(vreg: Reg, kinds: [any HLTypeKindProvider]) -> Bool {
        return INTEGER_TYPE_KINDS.contains( (kinds[Int(vreg)] as (any HLTypeKindProvider)).kind )
    }
    
    func isFP(vreg: Reg, kinds: [any HLTypeKindProvider]) -> Bool {
        Self.isFP(vreg: vreg, kinds: kinds)
    }
    
    static func isFP(vreg: Reg, kinds: [any HLTypeKindProvider]) -> Bool {
        return FP_TYPE_KINDS.contains( (kinds[Int(vreg)] as (any HLTypeKindProvider)).kind )
    }
    
    /// Result:
    ///     - Should return whatever the callee closure returns (if anything)
    ///
    /// C signature:
    ///
    ///     void *hlc_static_call( void *fun, hl_type *t, void **args, vdynamic *out ) {
    static let hlc_static_call: (@convention(c) (_ fun: OpaquePointer, _ t: OpaquePointer, _ args: OpaquePointer, _ out: OpaquePointer)->(OpaquePointer?)) = {
        funPtr, tPtr, argPtr, outPtr in
        
        let t: hlTypePointer = .init(tPtr)
        let out: UnsafeMutablePointer<vdynamic> = .init(outPtr)

        guard let funProvider = t.funProvider else {
            fatal("hlc_static_call fails, invalid type passed", logger)
        }

        /* we'll JIT the call
         *
         * x20 will hold the arg offset
         * x21 will hold the target func offset
         */
        let mem = CpuOpBuffer()
        
        // no-stack-prologue
        mem.append(
            M1Op.subImm12(X.sp, X.sp, Imm12Lsl12(16)),
            M1Op.stp((X.x29_fp, X.x30_lr), .reg64offset(.sp, 0, nil))
        )
        
        // set x20/x21
        mem.append(
            PseudoOp.mov(X.x20, argPtr),
            
            // test
            M1Op.ldr(X.x20, .reg(X.x20, .imm(0, nil))),
            
            PseudoOp.mov(X.x21, funPtr)
        )

        var offset: ByteCount = 0
        var gpRegisterIx: Int = 0
        var fpRegisterIx: Int = 0
        for (argIx, arg) in funProvider.argsProvider.enumerated() {
            guard !isFP(vreg: Reg(argIx), kinds: funProvider.argsProvider) else {
                fatal("floating point arguments not implemented", logger)
            }

            // only general purpose registers from here on
            guard gpRegisterIx < ARG_REGISTER_COUNT else {
                fatal("hlc_static_call does not support more than \(ARG_REGISTER_COUNT) arguments", logger)
            }
            defer { gpRegisterIx += 1 }

            let gpRegister = Register64(rawValue: UInt8(gpRegisterIx))!

            M1Compiler2.appendLoad(
                reg: gpRegister,
                as: Reg(argIx),
                fromAddressFrom: X.x20,
                offsetFromAddress: offset,
                kinds: funProvider.argsProvider,
                mem: mem)

            print("argIx", argIx, "into", gpRegister)
            print("arg", arg)
            print("arg kind", arg.kind)
            print("offset", offset)
            offset += arg.hlRegSize
        }

        mem.append(
            M1Op.blr(X.x21)
        )
        
        // no-stack-epilogue
        mem.append(
            M1Op.ldp((X.x29_fp, X.x30_lr), .reg64offset(.sp, 0, nil)),
            M1Op.addImm12(X.sp, X.sp, Imm12Lsl12(16)),
            M1Op.ret
        )

        // JIT
        let emittedBytes = try! mem.ops.flatMap { try $0.emit() }

        let execMem = mmap(
            nil,
            Int(mem.byteSize),
            PROT_WRITE | PROT_EXEC,
            MAP_ANONYMOUS | MAP_PRIVATE | MAP_JIT,
            -1,
            0
        )

        guard let execMem = execMem else {
            fatal("mmap returned nil: \(errno)", logger)
        }

        if execMem == MAP_FAILED {
            fatal("mmap failed: \(errno)", logger)
        }

        Swift.assert(mem.byteSize == Int64(emittedBytes.count))

        pthread_jit_write_protect_np(0)
        memcpy(execMem, emittedBytes, emittedBytes.count)
        pthread_jit_write_protect_np(1)
        // End JIT

        switch(funProvider.retProvider.kind) {
        case .void:
            return nil
        case .f32:
            // TODO: test
            let _jitFunc = unsafeBitCast(execMem, to: (@convention(c) ()->Float32).self)
            let result = _jitFunc()
            out.pointee.set(f: result)
            Swift.assert(out.pointee.f == result)
            return withUnsafeMutablePointer(to: &out.pointee.union) { res in
                print(res, "vs", out)
                print(res.pointee)
                fatalError("Not tested")
                return .init(res)
            }
        case .f64:
            mem.hexPrint()
            let _jitFunc = unsafeBitCast(execMem, to: (@convention(c) ()->Float64).self)
            let result = _jitFunc()
            out.pointee.set(d: result)
            Swift.assert(out.pointee.d == result)
            print("Got result", result)
            return withUnsafeMutablePointer(to: &out.pointee.union) { res in
                print(res, "vs", out)
                print(res.pointee)
                fatalError("Not tested")
                return .init(res)
            }
        default:
            fatal("hlc_static_call does not support return type \(funProvider.retProvider.kind)", logger)
        }
    }
    
    func appendPrepareDoubleForStore(reg: RegisterFP64, to vreg: Reg, kinds: [any HLTypeKindProvider], mem: CpuOpBuffer) {
        let vregKind = requireTypeKind(reg: vreg, from: kinds)
        switch(vregKind.hlRegSize) {
        case 8:
            // no op
            break
        case 4:
            mem.append(M1Op.fcvt(reg.to32, reg))
        default:
            fatalError("Can't convert floating point to size \(vregKind.hlRegSize)")
        }
    }
    
    func appendFPRegToDouble(reg: RegisterFP64, from vreg: Reg, kinds: [any HLTypeKindProvider], mem: CpuOpBuffer) {
        Self.appendFPRegToDouble(reg: reg, from: vreg, kinds: kinds, mem: mem)
    }
    
    static func appendFPRegToDouble(reg: RegisterFP64, from vreg: Reg, kinds: [any HLTypeKindProvider], mem: CpuOpBuffer) {
        let vregKind = requireTypeKind(reg: vreg, from: kinds)
        switch(vregKind.hlRegSize) {
        case 8:
            // no op
            break
        case 4:
            mem.append(M1Op.fcvt(reg, reg.to32))
        case 2, 1:
            mem.append(M1Op.fcvt(reg, reg.to16))
        default:
            fatalError("Can't convert floating point to size \(vregKind.hlRegSize)")
        }
    }
    
    func appendLoadNumeric(reg: Int, from vreg: Reg, kinds: [any HLTypeKindProvider], mem: CpuOpBuffer) {
        assertNumeric(reg: vreg, from: kinds)
        let vregKind = requireTypeKind(reg: vreg, from: kinds)
        if INTEGER_TYPE_KINDS.contains(vregKind) {
            let reg = Register64(rawValue: UInt8(reg))!
            appendLoad(reg: reg, from: vreg, kinds: kinds, mem: mem)
        } else if FP_TYPE_KINDS.contains(vregKind) {
            let reg = RegisterFP64(rawValue: UInt8(reg))!
            appendLoad(reg: reg, from: vreg, kinds: kinds, mem: mem)
        } else {
            fatalError("Can't append numeric for \(vregKind)")
        }
    }
    
    /// Load numeric values into FP registers. If source is an integer (non-FP value) then it will be
    /// loaded into a GP register first, then converted to a 64-bit FP value.
    /// - Parameters:
    ///   - reg: target register
    ///   - vreg: virtual register index to load
    ///   - kinds: list of available virtual register types
    ///   - mem: op buffer
    func appendLoadNumericAsFP(reg: RegisterFP64, from vreg: Reg, kinds: [any HLTypeKindProvider], mem: CpuOpBuffer)
    {
        appendLoadNumeric(reg: Int(reg.rawValue), from: vreg, kinds: kinds, mem: mem)
        let vregKind = requireTypeKind(reg: vreg, from: kinds)
        if INTEGER_TYPE_KINDS.contains(vregKind) {
            let regGP = Register64(rawValue: reg.rawValue)!
            appendLoad(reg: regGP, from: vreg, kinds: kinds, mem: mem)
            appendDebugPrintRegisterAligned4(regGP, prepend: "appendLoadNumericAsFP", builder: mem)
            
            // sign-extend -> convert to FP? -> convert FP? to FP64
            appendSignMode(true, reg: regGP, from: vreg, kinds: kinds, mem: mem)
            appendScvtf(reg: regGP, to: reg, target: vreg, kinds: kinds, mem: mem)
            appendFPRegToDouble(reg: reg, from: vreg, kinds: kinds, mem: mem)
            
            appendDebugPrintRegisterAligned4(reg, prepend: "appendLoadNumericAsFP", builder: mem)
            
        } else if FP_TYPE_KINDS.contains(vregKind) {
            appendFPRegToDouble(reg: reg, from: vreg, kinds: kinds, mem: mem)
        } else {
            fatalError("Can't append numeric for \(vregKind)")
        }
    }
    
    /// Store FP register as either integer or FP value, depending on the vreg kind.
    ///
    /// - Parameters:
    ///   - reg: source FP register
    ///   - vreg: vreg that determines the HL kind
    ///   - addrReg: GP register holding the target address where to store the value
    ///   - offsetFromAddress: offset from the value in `addrReg`
    ///   - kinds: known virtual (HL) register kinds
    ///   - mem: mem
    func appendStoreFPAsNumeric(reg: RegisterFP64, as vreg: Reg, intoAddressFrom addrReg: Register64, offsetFromAddress: Int64, kinds: [any HLTypeKindProvider], mem: CpuOpBuffer) {
        let vregKind = requireTypeKind(reg: vreg, from: kinds)
        
        if FP_TYPE_KINDS.contains(vregKind) {
            // convert FP to right size, and store
            appendPrepareDoubleForStore(reg: reg, to: vreg, kinds: kinds, mem: mem)
            appendStore(reg: reg, as: vreg, intoAddressFrom: addrReg, offsetFromAddress: offsetFromAddress, kinds: kinds, mem: mem)
        } else {
            // convert FP to int, and store
            let regI = Register64(rawValue: reg.rawValue)!
            appendFcvtzs(reg: reg, to: regI, target: vreg, kinds: kinds, mem: mem)
            appendStore(reg: regI, as: vreg, intoAddressFrom: addrReg, offsetFromAddress: offsetFromAddress, kinds: kinds, mem: mem)
            appendDebugPrintRegisterAligned4(reg, prepend: "store (FP)", builder: mem)
            appendDebugPrintRegisterAligned4(regI, prepend: "store (GP)", builder: mem)
        }
    }
    
    func appendStoreFPAsNumeric(reg: RegisterFP64, as vreg: Reg, kinds: [any HLTypeKindProvider], mem: CpuOpBuffer) {
        let offset = getRegStackOffset(kinds, vreg)
        appendStoreFPAsNumeric(reg: reg, as: vreg, intoAddressFrom: .sp, offsetFromAddress: offset, kinds: kinds, mem: mem)
    }
    
    func appendLoad(reg: Register64, from vreg: Reg, kinds: [any HLTypeKindProvider], mem: CpuOpBuffer) {
        let offset = getRegStackOffset(kinds, vreg)
        appendLoad(reg: reg, from: vreg, kinds: kinds, offset: offset, mem: mem)
    }
    
    func appendUcvtf(reg: Register64, to fp: RegisterFP64, target vreg: Reg, kinds: [any HLTypeKindProvider], mem: CpuOpBuffer) {
        let tk = requireTypeKind(reg: vreg, from: kinds)
        switch(tk.hlRegSize) {
        case 8:
            mem.append(M1Op.ucvtf(fp, reg))
        case 4:
            mem.append(M1Op.ucvtf(fp.to32, reg))
        default:
            fatalError("appendUcvtf not implemented for size \(tk.hlRegSize)")
        }
    }
    
    func appendScvtf(reg: Register64, to fp: RegisterFP64, target vreg: Reg, kinds: [any HLTypeKindProvider], mem: CpuOpBuffer) {
        let tk = requireTypeKind(reg: vreg, from: kinds)
        switch(tk.hlRegSize) {
        case 8:
            mem.append(M1Op.scvtf(fp, reg))
        case 4:
            mem.append(M1Op.scvtf(fp.to32, reg))
        case 2, 1:
            mem.append(M1Op.scvtf(fp.to16, reg))
        default:
            fatalError("appendScvtf not implemented for size \(tk.hlRegSize)")
        }
    }
    
    func appendFcvtzs(reg: RegisterFP64, to gp: Register64, target vreg: Reg, kinds: [any HLTypeKindProvider], mem: CpuOpBuffer) {
        let tk = requireTypeKind(reg: vreg, from: kinds)
        switch(tk.hlRegSize) {
        case 8:
            mem.append(M1Op.fcvtzs(gp, reg))
        case 4, 2, 1:
            mem.append(M1Op.fcvtzs(gp.to32, reg))
        default:
            fatalError("appendFcvtzs not implemented for size \(tk.hlRegSize)")
        }
    }
    
    func appendLoad(reg: RegisterFP64, from vreg: Reg, kinds: [any HLTypeKindProvider], mem: CpuOpBuffer) {
        let offset = getRegStackOffset(kinds, vreg)
        appendLoad(reg: reg, from: vreg, kinds: kinds, offset: offset, mem: mem)
    }
    
    func appendLoadAndConvertToDouble(reg: RegisterFP64, from vreg: Reg, kinds: [any HLTypeKindProvider], mem: CpuOpBuffer)
    {
        appendLoad(reg: reg, from: vreg, kinds: kinds, mem: mem)
        appendFPRegToDouble(reg: reg, from: vreg, kinds: kinds, mem: mem)
    }
    
    func appendStoreDoubleToRightSize(reg: RegisterFP64, into vreg: Reg, kinds: [any HLTypeKindProvider], mem: CpuOpBuffer)
    {
        appendPrepareDoubleForStore(reg: reg, to: vreg, kinds: kinds, mem: mem)
        appendStore(reg: reg, into: vreg, kinds: kinds, mem: mem)
    }
    
    func appendSignMode(_ signed: Bool, reg: Register64, from vreg: Reg, kinds: [any HLTypeKindProvider], mem: CpuOpBuffer) {
        let vregKind = requireTypeKind(reg: vreg, from: kinds)
        switch(vregKind.hlRegSize, signed) {
        case (8, _):
            break
        case (4, true):
            mem.append(M1Op.sxtw(reg, reg.to32))
        case (4, false):
            mem.append(M1Op.uxtw(reg.to32, reg.to32))
        case (2, true):
            mem.append(M1Op.sxth(reg, reg.to32))
        case (2, false):
            mem.append(M1Op.uxth(reg.to32, reg.to32))
        case (1, true):
            mem.append(M1Op.sxtb(reg, reg.to32))
        case (1, false):
            mem.append(M1Op.uxtb(reg.to32, reg.to32))
        default:
            fatalError("Unknown size for setting size mode modifier")
        }
    }
    
    static func appendLoad(reg: Register64, as vreg: Reg, fromAddressFrom addrReg: Register64, offsetFromAddress offset: ByteCount, kinds: [any HLTypeKindProvider], mem: CpuOpBuffer) {
        // TODO: combine appendLoads
        let vregKind = requireTypeKind(reg: vreg, from: kinds)
        if vregKind.hlRegSize == 8 {
            mem.append(
                M1Op.ldr(reg, .reg64offset(addrReg, offset, nil))
            )
        } else if vregKind.hlRegSize == 4 {
            mem.append(
                M1Op.ldr(reg.to32, .reg64offset(addrReg, offset, nil))
            )
        } else if vregKind.hlRegSize == 2 {
            mem.append(
                M1Op.ldrh(reg.to32, .imm64(addrReg, offset, nil))
            )
        } else if vregKind.hlRegSize == 1 {
            mem.append(
                M1Op.ldrb(reg.to32, .imm64(addrReg, offset, nil))
            )
        } else if vregKind.hlRegSize == 0 {
            // nop
        } else {
            fatalError("Size must be 8, 4, 2, 1, or 0")
        }
    }
    
    func appendLoad(reg: any Register, from vreg: Reg, kinds: [any HLTypeKindProvider], offset: ByteCount, mem: CpuOpBuffer) {
        let vregKind = requireTypeKind(reg: vreg, from: kinds)
        if vregKind.hlRegSize == 8 {
            mem.append(
                M1Op.ldr(reg, .reg64offset(.sp, offset, nil))
            )
        } else if vregKind.hlRegSize == 4 {
            guard let reg32: any Register = (reg.i?.to32 ?? reg.fp?.to32) else {
                fatalError("Can't convert \(reg) to 32-bit variant")
            }
            
            mem.append(
                M1Op.ldr(reg32, .reg64offset(.sp, offset, nil))
            )
        } else if vregKind.hlRegSize == 2 {
            guard let regGP32 = reg.i?.to32 else {
                fatal("Not supported 2 byte FP load", Self.logger)
            }
            
            mem.append(
                M1Op.ldrh(regGP32, .imm64(.sp, offset, nil))
            )
        } else if vregKind.hlRegSize == 1 {
            guard let regGP32 = reg.i?.to32 else {
                fatal("Not supported 1 byte FP load", Self.logger)
            }
            
            mem.append(
                M1Op.ldrb(regGP32, .imm64(.sp, offset, nil))
            )
        } else if vregKind.hlRegSize == 0 {
            // nop
        } else {
            fatalError("Size must be 8, 4, 2, 1, or 0")
        }
    }
    
    func appendStore(reg: any Register, as vreg: Reg, intoAddressFrom addrReg: Register64, offsetFromAddress: Int64, kinds: [any HLTypeKindProvider], mem: CpuOpBuffer) {
        let vregKind = requireTypeKind(reg: vreg, from: kinds)
        
        Swift.assert(reg.is64)
        
        guard let reg32: any Register = (reg.i?.to32 ?? reg.fp?.to32) else {
            fatalError("Can't convert \(reg) to 32-bit variant")
        }
        
        if vregKind.hlRegSize == 8 {
            mem.append(
                PseudoOp.debugMarker("Storing 8 bytes in vreg \(vreg)"),
                M1Op.str(reg, .reg64offset(addrReg, offsetFromAddress, nil))
            )
        } else if vregKind.hlRegSize == 4 {
            mem.append(
                PseudoOp.debugMarker("Storing 4 bytes in vreg \(vreg)"),
                M1Op.str(reg32, .reg64offset(addrReg, offsetFromAddress, nil))
            )
        } else if vregKind.hlRegSize == 2 {
            guard let reg32gp = reg32.i?.to32 else {
                fatalError("store for 2-byte FP registers not implemented")
            }
            mem.append(
                PseudoOp.debugMarker("Storing 2 bytes in vreg \(vreg)"),
                M1Op.strh(reg32gp, .imm64(addrReg, offsetFromAddress, nil))
            )
        } else if vregKind.hlRegSize == 1 {
            guard let reg32gp = reg32.i?.to32 else {
                fatalError("store for 1-byte FP registers not implemented")
            }
            mem.append(
                PseudoOp.debugMarker("Storing 1 byte in vreg \(vreg)"),
                M1Op.strb(reg32gp, .imm64(addrReg, offsetFromAddress, nil))
            )
        } else if vregKind.hlRegSize == 0 {
            // nop
        } else {
            fatalError("Size must be 8, 4, 2, 1, or 0")
        }
    }
    
    func appendStore(reg: Register64, into vreg: Reg, kinds: [any HLTypeKindProvider], mem: CpuOpBuffer) {
        let vregKind = requireTypeKind(reg: vreg, from: kinds)
        let offset = getRegStackOffset(kinds, vreg)
        
        if vregKind.hlRegSize == 8 {
            mem.append(
                PseudoOp.debugMarker("Storing 8 bytes in vreg \(vreg)"),
                M1Op.str(reg, .reg64offset(.sp, offset, nil))
            )
        } else if vregKind.hlRegSize == 4 {
            mem.append(
                PseudoOp.debugMarker("Storing 4 bytes in vreg \(vreg)"),
                M1Op.str(reg.to32, .reg64offset(.sp, offset, nil))
            )
        } else if vregKind.hlRegSize == 2 {
            mem.append(
                PseudoOp.debugMarker("Storing 2 bytes in vreg \(vreg)"),
                M1Op.strh(reg.to32, .imm64(.sp, offset, nil))
            )
        } else if vregKind.hlRegSize == 1 {
            mem.append(
                PseudoOp.debugMarker("Storing 1 byte in vreg \(vreg)"),
                M1Op.strb(reg.to32, .imm64(.sp, offset, nil))
            )
        } else if vregKind.hlRegSize == 0 {
            // nop
        } else {
            fatalError("Size must be 8, 4, 2, 1, or 0")
        }
    }
    
    func appendStore(reg: RegisterFP64, into vreg: Reg, kinds: [any HLTypeKindProvider], mem: CpuOpBuffer) {
        let vregKind = requireTypeKind(reg: vreg, from: kinds)
        let offset = getRegStackOffset(kinds, vreg)
        
        if vregKind.hlRegSize == 8 {
            mem.append(
                PseudoOp.debugMarker("Storing 8 bytes in vreg \(vreg)"),
                M1Op.str(reg, .reg64offset(.sp, offset, nil))
            )
        } else if vregKind.hlRegSize == 4 {
            mem.append(
                PseudoOp.debugMarker("Storing 4 bytes in vreg \(vreg)"),
                M1Op.str(reg.to32, .reg64offset(.sp, offset, nil))
            )
        } else if vregKind.hlRegSize == 2 {
            fatalError("16-bit str not implemented for fp registers")
//            mem.append(
//                PseudoOp.debugMarker("Storing 2 bytes in vreg \(vreg)"),
//                M1Op.strh(reg.to32, .imm64(.sp, offset, nil))
//            )
        } else if vregKind.hlRegSize == 1 {
            fatalError("8-bit str not implemented for fp registers")
//            mem.append(
//                PseudoOp.debugMarker("Storing 1 byte in vreg \(vreg)"),
//                M1Op.strb(reg.to32, .imm64(.sp, offset, nil))
//            )
        } else if vregKind.hlRegSize == 0 {
            // nop
        } else {
            fatalError("Size must be 8, 4, 2, 1, or 0")
        }
    }
    
    /// Holds information about the stack reservations for a function.
    struct StackInfo {
        /// Unrounded space used for virtual regs
        let reservedForVreg: ByteCount
        
        /// Unrounded stack space used for trap contexts
        let reservedForTrapContexts: ByteCount
        
        var unroundedTotal: ByteCount {
            reservedForVreg + reservedForTrapContexts
        }
        
        /// Rounded value that can be used to move the stack pointer
        var total: ByteCount {
            ByteCount(Self.roundUpStackReservation(Int16(unroundedTotal)))
        }
        
        var trapContextOffset: Int16 {
            Int16(reservedForVreg)
        }
        
        /// Stack pointer (SP) movements must be aligned to 16-bytes
        /// - Parameter val: unrounded byte count needed for the stack
        /// - Returns: value rounded up to nearest multiple of 16
        static func roundUpStackReservation(
            _ val: Int16
        ) -> Int16 {
            guard val % 16 != 0 else { return val }
            return (val + (16 &- 1)) & (0 &- 16)
        }
    }
    
    /*
     First 7 args are in registers [x0;x7]. Others are on the stack.
     Stack should be extended to account for data which is in registers.
     Subsequently data should be moved from regs to stack, to enable use of all registers.
     */
    @discardableResult func appendStackInit(
        _ unfilteredRegs: [any HLTypeKindProvider],
        args unfilteredArgs: [any HLTypeKindProvider],
        builder: CpuOpBuffer,
        prologueSize: ByteCount,
        trapContextsNeeded: Int = 0
    ) throws -> StackInfo {
        // test mismatched before filtering
        let unfilteredArgRegCount = min(unfilteredArgs.count, ARG_REGISTER_COUNT)
        guard
            unfilteredRegs.prefix(unfilteredArgRegCount).map({ $0.kind })
        == unfilteredArgs.prefix(unfilteredArgRegCount).map({ $0.kind })
        else {
            throw GlobalError.invalidOperation(
                "Up to first \(ARG_REGISTER_COUNT) registers must be the same for a function and its type.args"
            )
        }
        
        // move ALL args in continuous space (either from regs, or on sp)
        let vregStackSize_unr = unfilteredRegs.reduce(Int16(0)) { $0 + Int16($1.hlRegSize) }
        
        let trapCtxStackSize_unr = Int16(trapContextsNeeded) * Int16(MemoryLayout<HLTrapCtx_CCompat>.size)
        
        let stackInfo = StackInfo(
            reservedForVreg: ByteCount(vregStackSize_unr),
            reservedForTrapContexts: ByteCount(trapCtxStackSize_unr))
        
        guard stackInfo.total > 0 else {
            builder.append(PseudoOp.debugMarker("No extra stack space needed"))
            return stackInfo
        }
        
        builder.append(
            PseudoOp.debugMarker("Reserving \(stackInfo.total) bytes for entire stack"),
            M1Op.subImm12(X.sp, X.sp, try .i(stackInfo.total))
        )
        
        var offset: ByteCount = 0
        var overflowOffset: ByteCount = stackInfo.total + prologueSize // for regs passed in via stack
        
        // Now move all data from (stack/registers) to (stack) in the expected layout
        // Keep track of general-purpose and floating-point registers separately
        var gpIx = 0
        var fpIx = 0
        for (rix, reg) in unfilteredRegs.filter({ $0.hlRegSize > 0 }).enumerated() {

            Swift.assert(reg.hlRegSize > 0, "empty registers have to be filtered out earlier to not affect register index")
            
            let isFpReg = FP_TYPE_KINDS.contains(reg.kind)
            let needLoad = (isFpReg && fpIx >= ARG_REGISTER_COUNT) || (!isFpReg && gpIx >= ARG_REGISTER_COUNT)
            
            let regToUse = needLoad ? (1) : (isFpReg ? fpIx : gpIx)
            defer {
                if isFpReg {
                    fpIx+=1
                } else {
                    gpIx+=1
                }
            }
            
            switch (needLoad, isFpReg, reg.hlRegSize) {
            case (false, _, _):
                break
            case (true, true/*is fp*/, let regSize):
                // floating point register
                fatalError("TODO: add a test for loading FP properly")
                builder.append(PseudoOp.ldrVregFP(RegisterFP64(rawValue: UInt8(regToUse))!, X.x15, overflowOffset, regSize))
                overflowOffset += regSize
            case (true, false/*is !fp*/, let regSize):
                // general purpose register
                builder.append(PseudoOp.ldrVreg(Register64(rawValue: UInt8(regToUse))!, overflowOffset, regSize))
                overflowOffset += regSize
            }

            if isFpReg {
                let fpreg = RegisterFP64(rawValue: UInt8(regToUse))!
                
                // we can't use 64-bit or 32-bit interchangeably
                // convert ?argsize? to 64-bit register
                appendFPRegToDouble(reg: fpreg, from: Reg(rix), kinds: unfilteredRegs, mem: builder)
                
                // now that we know the value is in 64-bit,
                // we can store in stack
                appendPrepareDoubleForStore(reg: fpreg, to: Reg(rix), kinds: unfilteredRegs, mem: builder)
                builder.append(PseudoOp.strVregFP(fpreg, X.x15, offset, reg.hlRegSize))
            } else {
                builder.append(PseudoOp.strVreg(Register64(rawValue: UInt8(regToUse))!, X.x15, offset, reg.hlRegSize))
            }

            offset += reg.hlRegSize
        }
        
        return stackInfo
    }
    
    func appendDebugPrintRegisterAligned4(_ reg: any Register, prepend: String? = nil, builder: CpuOpBuffer)
    {
        if let regI = reg.i?.to64 {
            appendDebugPrintRegisterAligned4(regI, prepend: prepend, builder: builder)
        } else if let regFP = reg.fp?.to64 {
            appendDebugPrintRegisterAligned4(regFP, prepend: prepend, builder: builder)
        } else {
            fatal("Can't debug print register \(reg)", Self.logger)
        }
    }
    
    func appendDebugPrintRegisterAligned4(_ reg: Register64, prepend: String? = nil, builder: CpuOpBuffer) {
        guard stripDebugMessages == false else {
            builder.append(PseudoOp.debugMarker("(debug message printing stripped)"))
            return
        }
        
        Self.appendDebugPrintRegisterAligned4(reg, prepend: prepend, builder: builder)
    }
    
    static func appendDebugPrintRegisterAligned4(_ reg: Register64, prepend: String? = nil, builder: CpuOpBuffer) {
        guard let printfAddr = dlsym(dlopen(nil, RTLD_LAZY), "printf") else {
            fatalError("No printf addr")
        }
        
        var adr = RelativeDeferredOffset()
        var jmpTarget = RelativeDeferredOffset()
        let str: String
        if let prepend = prepend {
            str = "[jitdebug] [\(prepend)] Register \(reg): %p\n\0"
        } else {
            str = "[jitdebug] Register \(reg): %p\n\0"
        }
        
        builder.append(PseudoOp.debugMarker("Printing debug register: \(reg)"))
        
        guard reg.rawValue <= X.x18.rawValue else {
            fatalError("reg \(reg) not supported")
        }
        
        builder.append(
            // Stash registers we'll use (so we can reset)
            M1Op.subImm12(X.sp, X.sp, Imm12Lsl12(256)),
            
            M1Op.str(Register64.x0, .reg64offset(.sp, 8, nil)),
            M1Op.str(reg, .reg64offset(.sp, 0, nil)),
            M1Op.stp((Register64.x1, Register64.x2), .reg64offset(.sp, 16, nil)),
            M1Op.stp((Register64.x3, Register64.x4), .reg64offset(.sp, 32, nil)),
            M1Op.stp((Register64.x5, Register64.x6), .reg64offset(.sp, 48, nil)),
            M1Op.stp((Register64.x7, Register64.x8), .reg64offset(.sp, 64, nil)),
            M1Op.stp((Register64.x9, Register64.x10), .reg64offset(.sp, 80, nil)),
            M1Op.stp((Register64.x11, Register64.x12), .reg64offset(.sp, 96, nil)),
            M1Op.stp((Register64.x13, Register64.x14), .reg64offset(.sp, 112, nil)),
            M1Op.stp((Register64.x15, Register64.x16), .reg64offset(.sp, 128, nil)),
            M1Op.stp((Register64.x17, Register64.x18), .reg64offset(.sp, 144, nil)),
            M1Op.stp((Register64.x29_fp, Register64.x30_lr), .reg64offset(.sp, 160, nil)),
            
            M1Op.stp((D.d0, D.d1), .reg64offset(.sp, 176, nil)),
            M1Op.stp((D.d2, D.d3), .reg64offset(.sp, 192, nil)),
            M1Op.stp((D.d4, D.d5), .reg64offset(.sp, 208, nil)),
            M1Op.stp((D.d6, D.d7), .reg64offset(.sp, 224, nil)),
            M1Op.stp((D.d8, D.d9), .reg64offset(.sp, 240, nil))

        )
        adr.start(at: builder.byteSize)
        builder.append(M1Op.adr64(.x0, adr))
        
        builder.append(
            PseudoOp.mov(.x16, printfAddr),
            M1Op.blr(.x16),
//            // restore
            M1Op.ldr(Register64.x0, .reg64offset(.sp, 8, nil)),
            M1Op.ldp((Register64.x1, Register64.x2), .reg64offset(.sp, 16, nil)),
            M1Op.ldp((Register64.x3, Register64.x4), .reg64offset(.sp, 32, nil)),
            M1Op.ldp((Register64.x5, Register64.x6), .reg64offset(.sp, 48, nil)),
            M1Op.ldp((Register64.x7, Register64.x8), .reg64offset(.sp, 64, nil)),
            M1Op.ldp((Register64.x9, Register64.x10), .reg64offset(.sp, 80, nil)),
            M1Op.ldp((Register64.x11, Register64.x12), .reg64offset(.sp, 96, nil)),
            M1Op.ldp((Register64.x13, Register64.x14), .reg64offset(.sp, 112, nil)),
            M1Op.ldp((Register64.x15, Register64.x16), .reg64offset(.sp, 128, nil)),
            M1Op.ldp((Register64.x17, Register64.x18), .reg64offset(.sp, 144, nil)),
            M1Op.ldp((Register64.x29_fp, Register64.x30_lr), .reg64offset(.sp, 160, nil)),
            
            M1Op.ldp((D.d0, D.d1), .reg64offset(.sp, 176, nil)),
            M1Op.ldp((D.d2, D.d3), .reg64offset(.sp, 192, nil)),
            M1Op.ldp((D.d4, D.d5), .reg64offset(.sp, 208, nil)),
            M1Op.ldp((D.d6, D.d7), .reg64offset(.sp, 224, nil)),
            M1Op.ldp((D.d8, D.d9), .reg64offset(.sp, 240, nil)),

            try! M1Op._add(X.sp, X.sp, 256)
        )
        
        jmpTarget.start(at: builder.byteSize)
        builder.append(M1Op.b(jmpTarget))
        adr.stop(at: builder.byteSize)
        builder.append(PseudoOp.ascii(str)).align(4)

        Swift.assert(builder.byteSize % 4 == 0)
        jmpTarget.stop(at: builder.byteSize)
    }
    
    func appendDebugPrintRegisterAligned4(_ reg: RegisterFP64, prepend: String? = nil, builder: CpuOpBuffer) {
        guard stripDebugMessages == false else {
            builder.append(PseudoOp.debugMarker("(debug message printing stripped)"))
            return
        }
        
        Self.appendDebugPrintRegisterAligned4(reg, prepend: prepend, builder: builder)
    }
    
    static func appendDebugPrintRegisterAligned4(_ reg: RegisterFP64, prepend: String? = nil, builder: CpuOpBuffer) {
        guard let printfAddr = dlsym(dlopen(nil, RTLD_LAZY), "printf") else {
            fatalError("No printf addr")
        }
        
        var adr = RelativeDeferredOffset()
        var jmpTarget = RelativeDeferredOffset()
        let str: String
        if let prepend = prepend {
            str = "[jitdebug] [\(prepend)] Register \(reg): %f\n\0"
        } else {
            str = "[jitdebug] Register \(reg): %f\n\0"
        }
        
        builder.append(PseudoOp.debugMarker("Printing debug register: \(reg)"))
        
        guard reg.rawValue <= D.d9.rawValue else {
            fatalError("reg \(reg) not supported")
        }
        
        builder.append(
            // Stash registers we'll use (so we can reset)
            M1Op.subImm12(X.sp, X.sp, Imm12Lsl12(256)),
            
            M1Op.str(Register64.x0, .reg64offset(.sp, 8, nil)),
            M1Op.str(reg, .reg64offset(.sp, 0, nil)),
            M1Op.stp((Register64.x1, Register64.x2), .reg64offset(.sp, 16, nil)),
            M1Op.stp((Register64.x3, Register64.x4), .reg64offset(.sp, 32, nil)),
            M1Op.stp((Register64.x5, Register64.x6), .reg64offset(.sp, 48, nil)),
            M1Op.stp((Register64.x7, Register64.x8), .reg64offset(.sp, 64, nil)),
            M1Op.stp((Register64.x9, Register64.x10), .reg64offset(.sp, 80, nil)),
            M1Op.stp((Register64.x11, Register64.x12), .reg64offset(.sp, 96, nil)),
            M1Op.stp((Register64.x13, Register64.x14), .reg64offset(.sp, 112, nil)),
            M1Op.stp((Register64.x15, Register64.x16), .reg64offset(.sp, 128, nil)),
            M1Op.stp((Register64.x17, Register64.x18), .reg64offset(.sp, 144, nil)),
            M1Op.stp((Register64.x29_fp, Register64.x30_lr), .reg64offset(.sp, 160, nil)),
            
            M1Op.stp((D.d0, D.d1), .reg64offset(.sp, 176, nil)),
            M1Op.stp((D.d2, D.d3), .reg64offset(.sp, 192, nil)),
            M1Op.stp((D.d4, D.d5), .reg64offset(.sp, 208, nil)),
            M1Op.stp((D.d6, D.d7), .reg64offset(.sp, 224, nil)),
            M1Op.stp((D.d8, D.d9), .reg64offset(.sp, 240, nil))
        )
        adr.start(at: builder.byteSize)
        builder.append(M1Op.adr64(.x0, adr))
        
        builder.append(
            PseudoOp.mov(.x16, printfAddr),
            M1Op.blr(.x16),
//            // restore
            M1Op.ldr(Register64.x0, .reg64offset(.sp, 8, nil)),
            M1Op.ldp((Register64.x1, Register64.x2), .reg64offset(.sp, 16, nil)),
            M1Op.ldp((Register64.x3, Register64.x4), .reg64offset(.sp, 32, nil)),
            M1Op.ldp((Register64.x5, Register64.x6), .reg64offset(.sp, 48, nil)),
            M1Op.ldp((Register64.x7, Register64.x8), .reg64offset(.sp, 64, nil)),
            M1Op.ldp((Register64.x9, Register64.x10), .reg64offset(.sp, 80, nil)),
            M1Op.ldp((Register64.x11, Register64.x12), .reg64offset(.sp, 96, nil)),
            M1Op.ldp((Register64.x13, Register64.x14), .reg64offset(.sp, 112, nil)),
            M1Op.ldp((Register64.x15, Register64.x16), .reg64offset(.sp, 128, nil)),
            M1Op.ldp((Register64.x17, Register64.x18), .reg64offset(.sp, 144, nil)),
            M1Op.ldp((Register64.x29_fp, Register64.x30_lr), .reg64offset(.sp, 160, nil)),
            
            M1Op.ldp((D.d0, D.d1), .reg64offset(.sp, 176, nil)),
            M1Op.ldp((D.d2, D.d3), .reg64offset(.sp, 192, nil)),
            M1Op.ldp((D.d4, D.d5), .reg64offset(.sp, 208, nil)),
            M1Op.ldp((D.d6, D.d7), .reg64offset(.sp, 224, nil)),
            M1Op.ldp((D.d8, D.d9), .reg64offset(.sp, 240, nil)),
            
            try! M1Op._add(X.sp, X.sp, 256)
        )
        
        jmpTarget.start(at: builder.byteSize)
        builder.append(M1Op.b(jmpTarget))
        adr.stop(at: builder.byteSize)
        builder.append(PseudoOp.ascii(str)).align(4)

        Swift.assert(builder.byteSize % 4 == 0)
        jmpTarget.stop(at: builder.byteSize)
    }
    
    func appendDebugPrintAligned4(_ val: String, builder: CpuOpBuffer) {
        var adr = RelativeDeferredOffset()
        var jmpTarget = RelativeDeferredOffset()
        let str = "[jitdebug] \(val)\n"
        builder.append(PseudoOp.debugMarker("Printing debug message: \(val)"))
        guard stripDebugMessages == false else {
            builder.append(PseudoOp.debugMarker("(debug message printing stripped)"))
            return
        }
        builder.append(
            // Stash registers we'll use (so we can reset)
            M1Op.subImm12(X.sp, X.sp, Imm12Lsl12(240)),
            M1Op.str(Register64.x0, .reg64offset(.sp, 8, nil)),
            M1Op.str(Register64.x19, .reg64offset(.sp, 0, nil)),
            M1Op.stp((Register64.x1, Register64.x2), .reg64offset(.sp, 16, nil)),
            M1Op.stp((Register64.x3, Register64.x4), .reg64offset(.sp, 32, nil)),
            M1Op.stp((Register64.x5, Register64.x6), .reg64offset(.sp, 48, nil)),
            M1Op.stp((Register64.x7, Register64.x8), .reg64offset(.sp, 64, nil)),
            M1Op.stp((Register64.x9, Register64.x10), .reg64offset(.sp, 80, nil)),
            M1Op.stp((Register64.x11, Register64.x12), .reg64offset(.sp, 96, nil)),
            M1Op.stp((Register64.x13, Register64.x14), .reg64offset(.sp, 112, nil)),
            M1Op.stp((Register64.x15, Register64.x16), .reg64offset(.sp, 128, nil)),
            M1Op.stp((Register64.x17, Register64.x18), .reg64offset(.sp, 144, nil)),
            
            M1Op.stp((D.d0, D.d1), .reg64offset(.sp, 160, nil)),
            M1Op.stp((D.d2, D.d3), .reg64offset(.sp, 176, nil)),
            M1Op.stp((D.d4, D.d5), .reg64offset(.sp, 192, nil)),
            M1Op.stp((D.d6, D.d7), .reg64offset(.sp, 208, nil)),
            M1Op.stp((D.d8, D.d9), .reg64offset(.sp, 224, nil))
        )
        
        builder.append(
            // unix write system call
            M1Op.movz64(.x0, 1, nil)
        )
        adr.start(at: builder.byteSize)
        builder.append(
            M1Op.adr64(.x1, adr),
            M1Op.movz64(.x2, UInt16(str.count), nil),
            M1Op.movz64(.x16, 4, nil),
            M1Op.svc(0x80),
            
            // restore
            M1Op.ldr(Register64.x0, .reg64offset(.sp, 8, nil)),
            M1Op.ldr(Register64.x19, .reg64offset(.sp, 0, nil)),
            M1Op.ldp((Register64.x1, Register64.x2), .reg64offset(.sp, 16, nil)),
            M1Op.ldp((Register64.x3, Register64.x4), .reg64offset(.sp, 32, nil)),
            M1Op.ldp((Register64.x5, Register64.x6), .reg64offset(.sp, 48, nil)),
            M1Op.ldp((Register64.x7, Register64.x8), .reg64offset(.sp, 64, nil)),
            M1Op.ldp((Register64.x9, Register64.x10), .reg64offset(.sp, 80, nil)),
            M1Op.ldp((Register64.x11, Register64.x12), .reg64offset(.sp, 96, nil)),
            M1Op.ldp((Register64.x13, Register64.x14), .reg64offset(.sp, 112, nil)),
            M1Op.ldp((Register64.x15, Register64.x16), .reg64offset(.sp, 128, nil)),
            M1Op.ldp((Register64.x17, Register64.x18), .reg64offset(.sp, 144, nil)),
            
            M1Op.ldp((D.d0, D.d1), .reg64offset(.sp, 160, nil)),
            M1Op.ldp((D.d2, D.d3), .reg64offset(.sp, 176, nil)),
            M1Op.ldp((D.d4, D.d5), .reg64offset(.sp, 192, nil)),
            M1Op.ldp((D.d6, D.d7), .reg64offset(.sp, 208, nil)),
            M1Op.ldp((D.d8, D.d9), .reg64offset(.sp, 224, nil)),
            
            try! M1Op._add(X.sp, X.sp, 240)
        )
        jmpTarget.start(at: builder.byteSize)
        builder.append(M1Op.b(jmpTarget))
        adr.stop(at: builder.byteSize)
        builder.append(PseudoOp.ascii(str)).align(4)
        
        jmpTarget.stop(at: builder.byteSize)
    }
    
    func appendSystemExit(_ code: UInt8, builder: CpuOpBuffer) {
        builder.append(
            M1Op.movz64(.x0, UInt16(code), nil),
            M1Op.movz64(.x16, 1, nil),
            M1Op.svc(0x80)
        )
    }
    
    func appendSystemExit_CodeInX0(builder: CpuOpBuffer) {
        builder.append(
            M1Op.movz64(.x16, 1, nil),
            M1Op.svc(0x80)
        )
    }
    
    /// Returns the amount of change for SP
    func appendPrologue(builder: CpuOpBuffer) -> ByteCount {
        appendDebugPrintAligned4("Starting prologue", builder: builder)

        let stackReservation: ByteCount = 304
        
        // Storing all non-corruptible registers so we don't have to keep track of them
        // during the execution. Potential optimization here.
        builder.append(
            M1Op.subImm12(X.sp, X.sp, try! Imm12Lsl12(stackReservation)),
            
            M1Op.stp((X.x15, X.x16), .reg64offset(.sp, 0, nil)),
            M1Op.stp((X.x17, X.x18), .reg64offset(.sp, 16, nil)),
            M1Op.stp((X.x19, X.x20), .reg64offset(.sp, 32, nil)),
            M1Op.stp((X.x21, X.x22), .reg64offset(.sp, 48, nil)),
            M1Op.stp((X.x23, X.x24), .reg64offset(.sp, 64, nil)),
            M1Op.stp((X.x25, X.x26), .reg64offset(.sp, 80, nil)),
            M1Op.stp((X.x27, X.x28), .reg64offset(.sp, 96, nil)),
            M1Op.stp((X.x29_fp, X.x30_lr), .reg64offset(.sp, 112, nil)),
            
            M1Op.stp((D.d9, D.d10), .reg64offset(.sp, 128, nil)),
            M1Op.stp((D.d11, D.d12), .reg64offset(.sp, 144, nil)),
            M1Op.stp((D.d13, D.d14), .reg64offset(.sp, 160, nil)),
            M1Op.stp((D.d15, D.d16), .reg64offset(.sp, 176, nil)),
            M1Op.stp((D.d17, D.d18), .reg64offset(.sp, 192, nil)),
            M1Op.stp((D.d19, D.d20), .reg64offset(.sp, 208, nil)),
            M1Op.stp((D.d21, D.d22), .reg64offset(.sp, 224, nil)),
            M1Op.stp((D.d23, D.d24), .reg64offset(.sp, 240, nil)),
            M1Op.stp((D.d25, D.d26), .reg64offset(.sp, 256, nil)),
            M1Op.stp((D.d27, D.d28), .reg64offset(.sp, 272, nil)),
            M1Op.stp((D.d29, D.d30), .reg64offset(.sp, 288, nil)),
            
            M1Op.movr64(.x29_fp, .sp)
        )
        return stackReservation
    }
    
    func appendEpilogue(builder: CpuOpBuffer) {
        appendDebugPrintAligned4("Starting epilogue", builder: builder)
        builder.append(
            M1Op.ldp((X.x15, X.x16), .reg64offset(.sp, 0, nil)),
            M1Op.ldp((X.x17, X.x18), .reg64offset(.sp, 16, nil)),
            M1Op.ldp((X.x19, X.x20), .reg64offset(.sp, 32, nil)),
            M1Op.ldp((X.x21, X.x22), .reg64offset(.sp, 48, nil)),
            M1Op.ldp((X.x23, X.x24), .reg64offset(.sp, 64, nil)),
            M1Op.ldp((X.x25, X.x26), .reg64offset(.sp, 80, nil)),
            M1Op.ldp((X.x27, X.x28), .reg64offset(.sp, 96, nil)),
            M1Op.ldp((X.x29_fp, X.x30_lr), .reg64offset(.sp, 112, nil)),
            
            M1Op.ldp((D.d9, D.d10), .reg64offset(.sp, 128, nil)),
            M1Op.ldp((D.d11, D.d12), .reg64offset(.sp, 144, nil)),
            M1Op.ldp((D.d13, D.d14), .reg64offset(.sp, 160, nil)),
            M1Op.ldp((D.d15, D.d16), .reg64offset(.sp, 176, nil)),
            M1Op.ldp((D.d17, D.d18), .reg64offset(.sp, 192, nil)),
            M1Op.ldp((D.d19, D.d20), .reg64offset(.sp, 208, nil)),
            M1Op.ldp((D.d21, D.d22), .reg64offset(.sp, 224, nil)),
            M1Op.ldp((D.d23, D.d24), .reg64offset(.sp, 240, nil)),
            M1Op.ldp((D.d25, D.d26), .reg64offset(.sp, 256, nil)),
            M1Op.ldp((D.d27, D.d28), .reg64offset(.sp, 272, nil)),
            M1Op.ldp((D.d29, D.d30), .reg64offset(.sp, 288, nil)),
            
            try! M1Op._add(X.sp, X.sp, 304)
        )
        appendDebugPrintAligned4("Finished epilogue", builder: builder)
    }
}

// MARK: Compiler

class M1Compiler2 {
    let emitter = EmitterM1()
    let ctx: CCompatJitContext
    let stripDebugMessages: Bool
    
    static let logger = LoggerFactory.create(M1Compiler2.self)
    
    init(ctx: CCompatJitContext, stripDebugMessages: Bool = false) {
        self.stripDebugMessages = stripDebugMessages
        self.ctx = ctx
    }
    
    func assertEnoughRegisters(_ ix: Reg, regs: Registers2) {
        guard ix < regs.count else {
            fatalError("Not enough registers. Expected \(ix) to be available. Got: \(regs)")
        }
    }
    
    func requireTypeMemory(reg: Reg, regs: Registers2) -> UnsafeRawPointer /*UnsafePointer<HLType_CCompat>*/ {
        assertEnoughRegisters(reg, regs: regs)
        
        let reg = regs[Int(reg)]
        return reg.ccompatAddress
    }
    
    func requireType(reg: Reg, regs: Registers2) -> any HLTypeProvider {
        assertEnoughRegisters(reg, regs: regs)
        
        return regs[Int(reg)]
    }
    
    func requireFieldOffset(fieldRef: Int, objIx: Reg, regs: Registers2) -> Int64 {
        let typ = requireType(reg: objIx, regs: regs)
        
        let ptr: UnsafePointer<HLType_CCompat> = .init(OpaquePointer(typ.ccompatAddress))
        
        switch(typ.kind) {
        case .obj:
            fallthrough
        case .struct:
            // if this is not set, the module has not
            // been initialized
            let rt = ptr.pointee.obj.pointee.getRt(ptr)
            return Int64(rt.pointee.fields_indexes.advanced(by: fieldRef).pointee)
        default:
            fatalError("Can not get field offset for obj type \(ptr.pointee.kind)")
        }
    }
    
    func requireTypeAddress(reg: Reg, from regs: [any HLTypeProvider]) -> UnsafeRawPointer {
        guard reg < regs.count else {
            let _debug = regs as [any OverrideCustomDebugStringConvertible]
            fatalError("requireTypeAddress(reg:from:): Not enough registers. Expected \(reg) to be available. Got: \(_debug._overrideDebugDescription)")
        }
        
        return regs[Int(reg)].ccompatAddress
    }
    
    func requireTypeKind(reg: Reg, from resolvedRegs: [any HLTypeKindProvider]) -> HLTypeKind {
        Self.requireTypeKind(reg: reg, from: resolvedRegs)
    }
    
    static func requireTypeKind(reg: Reg, from resolvedRegs: [any HLTypeKindProvider]) -> HLTypeKind {
        guard reg < resolvedRegs.count else {
            let _debug = resolvedRegs as [any OverrideCustomDebugStringConvertible]
            fatalError("requireType(reg:from:): Not enough registers. Expected \(reg) to be available. Got: \(_debug._overrideDebugDescription)")
        }
        
        return resolvedRegs[Int(reg)].kind
    }
    
    func requireTypeSizeLsl(reg: Reg, from resolvedRegs: [any HLTypeKindProvider]) -> UInt8 {
        let kind = requireTypeKind(reg: reg, from: resolvedRegs)
        switch(kind.hlRegSize) {
        case 8: return 3
        case 4: return 2
        case 2: return 1
        case 1: return 0
        default:
            fatalError("Unsupported size")
        }
    }
    
    func requireTypeKind(reg: Reg, from resolvedRegs: [any HLTypeKindProvider], shouldMatch: any HLTypeKindProvider) -> any HLTypeKindProvider {
        let kind = requireTypeKind(reg: reg, from: resolvedRegs)
        guard kind.kind == shouldMatch.kind else {
            fatalError("Expected reg \(reg) to be \(shouldMatch) but was \(kind)")
        }
        return kind
    }
    
    func assertKind(_ op: HLOpCode, _ actual: any HLTypeKindProvider, _ expected: HLTypeKind) {
        guard expected.kind == actual.kind else {
            fatalError("\(op): type kind must be \(expected) but got \(actual)")
        }
    }
    
    func assert(reg: Reg, from: [any HLTypeKindProvider], matchesCallArg argReg: Reg, inFun callable: any Callable2) {
        assert(reg: reg, from: from, matchesCallArg: argReg, inFunArgs: callable.argsProvider)
    }
    
    func assert(reg: Reg, from: [any HLTypeKindProvider], matchesCallArg argReg: Reg, inFunArgs argsProvider: [any HLTypeProvider]) {
        guard from.count > reg else {
            fatalError(
                "Register with index \(reg) does not exist. Available registers: \(from)."
            )
        }
        guard argsProvider.count > argReg else {
            fatalError("Expected args to have index \(argReg) but got \(argsProvider)")
        }
        let regKind = from[Int(reg)]
        let argKind = argsProvider[Int(argReg)].kind
        guard argKind == .dyn || regKind.kind == .dyn || regKind.kind == argKind.kind else {
            fatalError(
                "Register \(reg) kind \(regKind) expected to match arg \(argReg) but arg was \(argKind) "
            )
        }
    }
    
    func assert(reg: Reg, from: [any HLTypeKindProvider], matches type: any HLTypeKindProvider) {
        guard from.count > reg else {
            fatalError(
                "Register with index \(reg) does not exist. Available registers: \(from)."
            )
        }
        guard from[Int(reg)].kind == type.kind else {
            fatalError(
                "Register \(reg) expected to be \(type.kind) but is \(from[Int(reg)].kind)"
            )
        }
    }
    
    func assertFP(reg: Reg, from: [any HLTypeKindProvider]) {
        assert(reg: reg, from: from, in: FP_TYPE_KINDS)
    }
    
    func assertNumeric(reg: Reg, from: [any HLTypeKindProvider]) {
        assert(reg: reg, from: from, in: NUMERIC_TYPE_KINDS)
    }
    
    func assertInteger(reg: Reg, from: [any HLTypeKindProvider]) {
        assert(reg: reg, from: from, in: INTEGER_TYPE_KINDS)
    }
    
    func assert(reg: Reg, from: [any HLTypeKindProvider], is target: any HLTypeKindProvider) {
        guard from.count > reg else {
            fatalError(
                "Register with index \(reg) does not exist. Available registers: \(from)."
            )
        }
        guard target.kind == .dyn || from[Int(reg)].kind == .dyn || target.kind == from[Int(reg)].kind else {
            fatalError(
                "Register \(reg) expected to be \(target.kind) but is \(from[Int(reg)].kind)"
            )
        }
    }
    
    func assertDynamic(reg: Reg, from regs: [any HLTypeKindProvider]) {
        assert(reg: reg, from: regs, in: [
            HLTypeKind.dyn,
            HLTypeKind.fun,
            HLTypeKind.obj,
            HLTypeKind.array,
            HLTypeKind.virtual,
            HLTypeKind.dynobj,
            HLTypeKind.enum,
            HLTypeKind.null
        ])
    }
    
    func assertCallable(reg: Reg, from regs: [any HLTypeKindProvider]) {
        /* HLTypeKind.fun
           HLTypeKind.dyn can also be callable (see String#call_toString)
         */
        assert(reg: reg, from: regs, in: [HLTypeKind.fun, HLTypeKind.dyn])
    }
    
    func assertNumeric(dst: Reg, bigEnoughFor src: Reg, from kinds: [any HLTypeKindProvider]) throws {
        
        let dstKind = requireTypeKind(reg: dst, from: kinds)
        let srcKind = requireTypeKind(reg: src, from: kinds)
        
        // if src or target is dynamic, let's assume good faith
        guard srcKind != .dyn && dstKind != .dyn else {
            return
        }
        
        let bothFP = FP_TYPE_KINDS.contains(dstKind) && FP_TYPE_KINDS.contains(srcKind)
        let bothI = INTEGER_TYPE_KINDS.contains(dstKind) && INTEGER_TYPE_KINDS.contains(srcKind)
        
        guard bothFP || bothI else {
            throw GlobalError.invalidOperation("Src register \(srcKind) and dst register \(dstKind) should both be either floating-point or integer")
        }
        
        guard dstKind.hlRegSize >= srcKind.hlRegSize else {
            throw GlobalError.invalidOperation("Src register \(srcKind) (size \(srcKind.hlRegSize)) does not fit in dst register \(dstKind) (size \(dstKind.hlRegSize))")
        }
    }
    
    func assert(reg: Reg, from: [any HLTypeKindProvider], in targets: [any HLTypeKindProvider]) {
        guard from.count > reg else {
            fatalError(
                "Register with index \(reg) does not exist. Available registers: \(from)."
            )
        }
        
        guard targets.contains(where: { $0.kind == from[Int(reg)].kind }) else {
            fatalError(
                "Register \(reg) expected to be one of \(targets) but is \(from[Int(reg)].kind)"
            )
        }
    }
    
    func getRegStackOffset(_ regs: [any HLTypeKindProvider], _ ix: Reg) -> ByteCount {
        var result = ByteCount(0)
        for i in 0..<ix {
            result += regs[Int(i)].hlRegSize
        }
        return result
    }
    
    func getFieldOffset(_ objData: any HLTypeObjProvider, _ field: Int) -> ByteCount {
        var startOffset = ByteCount(8)  // hl_type* pointer at start of obj/struct
        for ix in 0..<field {
            let fieldToSkip = objData.fieldsProvider[ix].typeProvider.kind.hlRegSize
            startOffset += fieldToSkip
        }
        return startOffset
    }
    
    func push(to buffer: ByteBuffer, _ ops: M1Op...) throws {
        for op in ops { buffer.push(try emitter.emit(for: op)) }
    }
    
    /// Will compile and update the JIT context with the functions addresses.
    /// - Parameters:
    ///   - findex:
    ///   - mem:
    /// - Returns:
    func compile(findex fix: RefFun, into mem: CpuOpBuffer) throws
    {
        guard let compilable = try ctx.getCompilable(findex: fix) else {
            throw GlobalError.invalidValue("Function (findex=\(fix)) not found.")
        }
        
        // TODO: mark as compiled
        try compile(compilable: compilable, into: mem)
    }
    
    // MARK: compile
    func compile(compilable: any Compilable2, into mem: CpuOpBuffer) throws
    {
        defer {
            ctx.funcTracker.compiled(compilable.findex)
        }
        
        /* we need to allocate trap contexts on the stack
         
         Number of OTrap ops determines how many trap contexts we'll need.
         
         It might not match OEndTrap (as a single OTrap might have multiple
         corresponding OEndTrap).
         */
        var availableTrapIx: Int64 = 0  // each OTrap will increment, and OEndTrap will decrement
        let trapCount = compilable.ops.filter({
            guard case .OTrap(_, _) = $0 else {
                return false
            }
            return true
        }).count
        
        // grab it before it changes from prologue
        // let memory = mem.getDeferredPosition()
        let regs = compilable.regsProvider
        let args = compilable.argsProvider
        
        let fix = compilable.findex

        guard !compilable.linkableAddress.hasOffset else {
            throw GlobalError.functionAlreadyCompiled("Can not compile function (findex=\(fix)) because it already has been compiled and linked. \(compilable.address)")
        }
        
        compilable.linkableAddress.setOffset(mem.byteSize)

        // if we need to return early, we jump to these
        var retTargets: [RelativeDeferredOffset] = []

        appendDebugPrintAligned4("Entering fix \(fix)", builder: mem)
        
        mem.append(PseudoOp.debugMarker("==> STARTING FUNCTION \(fix)"))
        let prologueSize = appendPrologue(builder: mem)
        let stackInfo = try appendStackInit(
            regs,
            args: compilable.argsProvider,
            builder: mem,
            prologueSize: prologueSize,
            trapContextsNeeded: trapCount
        )

        appendDebugPrintAligned4(
            "Entering function \(fix)@\(compilable.address)",
            builder: mem
        )
        
        /* Relative conditional jumps will need to fit in 19 bits and absolute jumps in 26 bits.
         
         But the absolute offsets can be stored with more bits (in case the relative
         offset is smaller). */
        let addrBetweenOps: [DeferredImmediate<Immediate26>] = (0..<compilable.ops.count).map { _ in
            return DeferredImmediate()
        }

        for (currentInstruction, op) in compilable.ops.enumerated() {

            Self.logger.debug("f\(compilable.findex): #\(currentInstruction) (offset: \(mem.byteSize))")
            addrBetweenOps[currentInstruction].finalize(try Immediate26(mem.byteSize))

            mem.append(
                PseudoOp.debugMarker("Marking position for \(currentInstruction) at \(mem.byteSize)")
            )

            appendDebugPrintAligned4("f\(compilable.findex): #\(currentInstruction): \(op.debugDescription)", builder: mem)
            
            switch op {
            case .OAssert:
                appendDebugPrintAligned4("OAssert is a no-op?", builder: mem)
            case .ORet(let dst):
                // store
                let dstStackOffset = getRegStackOffset(regs, dst)
                let dstKind = requireTypeKind(reg: dst, from: regs)
                
                if dstKind.hlRegSize > 0 {
                    if isFP(vreg: dst, kinds: regs) {
                        appendDebugPrintAligned4("Returning FP stack offset \(dstStackOffset)", builder: mem)
                        appendLoad(reg: D.d0, from: dst, kinds: regs, mem: mem)
                    } else {
                        appendDebugPrintAligned4("Returning I stack offset \(dstStackOffset)", builder: mem)
                        appendLoad(reg: X.x0, from: dst, kinds: regs, mem: mem)
                        appendDebugPrintRegisterAligned4(X.x0, prepend: "ORet", builder: mem)
                    }
                }

                // jmp to end (NOTE: DO NOT ADD ANYTHING BETWEEN .start() and mem.append()
                var retTarget = RelativeDeferredOffset()
                retTarget.start(at: mem.byteSize)
                retTargets.append(retTarget)
                mem.append(M1Op.b(retTarget)
                )

            case .OCall0(let dst, let funRef):
                let fn = try ctx.requireCallable(findex: funRef)
                
                ctx.funcTracker.referenced2(fn)
  
                assert(
                    reg: dst,
                    from: regs,
                    matches: fn.retProvider
                )

                let dstStackOffset = getRegStackOffset(regs, dst)
                let dstKind = requireTypeKind(reg: dst, from: regs)

                mem.append(
                    PseudoOp.debugMarker("Call0 fn@\(funRef) -> \(dst)"),
                    PseudoOp.mov(.x10, fn.address),
                    M1Op.blr(.x10),
                    PseudoOp.strVreg(X.x0, X.x15, dstStackOffset, dstKind.hlRegSize)
                )
            case .OCall1(let dst, let fun, let arg0):
                if currentInstruction == 1 && compilable.findex == 231 {
                    let c: (@convention(c) (OpaquePointer)->()) = {
                        strP in
                        
                        let str: UnsafePointer<_StringX> = .init(strP)
                        print("__passing string", str.pointee.bytes.stringValue)
                        return
                    }
                    appendLoad(reg: X.x0, from: arg0, kinds: regs, mem: mem)
                    mem.append(
                        PseudoOp.mov(X.x10, unsafeBitCast(c, to: OpaquePointer.self)),
                        M1Op.blr(X.x10)
                    )
                }
                try __ocalln(
                    dst: dst,
                    funIndex: fun,
                    regs: regs,
                    args: [arg0],
                    reservedStackBytes: stackInfo.total,
                    mem: mem)
            case .OCall3(let dst, let fun, let arg0, let arg1, let arg2):
                try __ocalln(
                    dst: dst,
                    funIndex: fun,
                    regs: regs,
                    args: [arg0, arg1, arg2],
                    reservedStackBytes: stackInfo.total,
                    mem: mem)
            case .OCall4(let dst, let fun, let arg0, let arg1, let arg2, let arg3):
                try __ocalln(
                    dst: dst,
                    funIndex: fun,
                    regs: regs,
                    args: [arg0, arg1, arg2, arg3],
                    reservedStackBytes: stackInfo.total,
                    mem: mem)
            case .OCallClosure(let dst, let closureObject, let args):
                assertCallable(reg: closureObject, from: regs)
                
                let dstType = self.requireType(reg: dst, regs: regs)
                let clType = self.requireType(reg: closureObject, regs: regs)
                
                // vclosure offsets
                let funOffset: Int64 = Int64(MemoryLayout<vclosure>.offset(of: \vclosure.fun)!)
                let hasValueOffset: Int64 = Int64(MemoryLayout<vclosure>.offset(of: \vclosure.hasValue)!)
                let valueOffset: Int64 = Int64(MemoryLayout<vclosure>.offset(of: \vclosure.value)!)
                
                if (dstType.kind == .dyn) {
                        
                    /*
                     needs:
                     // ASM for {
                     //     vdynamic *args[] = {args};
                     //     vdynamic *ret = hl_dyn_call(closure,args,nargs);
                     //     dst = hl_dyncast(ret,t_dynamic,t_dst);  // OSafeCast
                     // }
                     */
                    let nargs = args.count
                    let dynArgs: UnsafeMutableBufferPointer<vdynamicPointer> = .allocate(capacity: nargs)
                    mem.append(PseudoOp.mov(X.x0, OpaquePointer(dynArgs.baseAddress!)))
                    for (ix, argRegister) in args.enumerated() {
                        /* Every arg must be a dyn, see jit.c:
                               if( !hl_is_dynamic(a->t) ) ASSERT(0);
                        */
                        assertDynamic(reg: argRegister, from: regs)
                        
                        appendLoad(reg: X.x1, from: argRegister, kinds: regs, mem: mem)
                        let offset: Int64 = Int64(ix * MemoryLayout<UnsafePointer<vdynamic>>.stride)
                        appendStore(reg: X.x1, as: argRegister, intoAddressFrom: X.x0, offsetFromAddress: offset, kinds: regs, mem: mem)
                    }
                    
                    // call hl_dyn_call
                    appendLoad(reg: X.x0, from: closureObject, kinds: regs, mem: mem)
                    mem.append(PseudoOp.mov(X.x1, OpaquePointer(dynArgs.baseAddress!)))
                    mem.append(M1Op.movz64(X.x2, UInt16(nargs), nil))
                    mem.append(
                        PseudoOp.mov(X.x10, unsafeBitCast(LibHl._hl_dyn_call, to: OpaquePointer.self)),
                        M1Op.blr(X.x10)
                    )
                    
                    // MARK: TMP
                    let _c: (@convention(c) (OpaquePointer)->()) = {
                        optr in
                        
                        let x: vdynamicPointer = .init(optr)
                        print(x)
                    }
                    mem.append(
                        PseudoOp.mov(X.x2, unsafeBitCast(_c, to: OpaquePointer.self)),
                        PseudoOp.mov(X.x1, nargs),
                        M1Op.blr(X.x2))
                    appendSystemExit(123, builder: mem)
                    // MARK: TMP
                    
                    
                } else {
                    // ASM for  if( c->hasValue ) c->fun(value,args) else c->fun(args)
                    
                    appendLoad(reg: X.x10, from: closureObject, kinds: regs, mem: mem)
                    appendDebugPrintRegisterAligned4(X.x10, prepend: "OCallClosure obj", builder: mem)
                    
                    var jmpTargetHasValue = RelativeDeferredOffset()
                    var jmpTargetFinish = RelativeDeferredOffset()

                    mem.append(
                        M1Op.ldr(X.x0, .reg64offset(X.x10, hasValueOffset, nil)),
                        M1Op.movz64(X.x1, 0, nil),
                        M1Op.cmp(X.x0, X.x1)
                    )

                    appendDebugPrintAligned4("CHECKING TARGET VALUE", builder: mem)
                    mem.append(
                        PseudoOp.withOffset(
                            offset: &jmpTargetHasValue,
                            mem: mem,
                            M1Op.b_ne(try! Immediate21(jmpTargetHasValue.value))
                        )
                    )
                    appendDebugPrintAligned4("TARGET HAS NO VALUE", builder: mem)
                    // MARK: no target value
                    try __ocall_impl(
                        dst: dst,
                        funType: clType,
                        appendCall: { buff in
                            appendLoad(reg: X.x10, from: closureObject, kinds: regs, mem: buff)
                            appendDebugPrintAligned4("[__ocall_impl] Call", builder: buff)
                            buff.append(
                                M1Op.ldr(X.x15, .reg64offset(X.x10, funOffset, nil))
                                )
                            appendDebugPrintRegisterAligned4(X.x15, prepend: "__ocall_impl loaded fun", builder: buff)
                            buff.append(
                                M1Op.blr(X.x15)
                            )
                        },
                        regs: regs,
                        preArgs: [],
                        args: args,
                        reservedStackBytes: stackInfo.total,
                        mem: mem
                    )
                    
                    mem.append(
                        PseudoOp.withOffset(
                            offset: &jmpTargetFinish,
                            mem: mem,
                            M1Op.b(jmpTargetFinish)
                        )
                    )

                    jmpTargetHasValue.stop(at: mem.byteSize)
                    appendDebugPrintAligned4("TARGET HAS VALUE", builder: mem)
                    try __ocall_impl(
                        dst: dst,
                        funType: clType,
                        appendCall: { buff in
                            appendLoad(reg: X.x10, from: closureObject, kinds: regs, mem: buff)
                            buff.append(
                                M1Op.ldr(X.x15, .reg64offset(X.x10, funOffset, nil)),
                                M1Op.blr(X.x15)
                            )
                        },
                        regs: regs,
                        preArgs: [
                            ({
                                buff, reg in
                                
                                guard let regI = reg.i?.to64 else {
                                    fatal("Non-integer registers not supported", Self.logger)
                                }
                                
                                guard regI != X.x19 else { fatalError("X.x19 can not be loaded") }
                                
                                self.appendLoad(reg: X.x19, from: closureObject, kinds: regs, mem: buff)
                                buff.append(
                                    M1Op.ldr(regI, .reg64offset(X.x19, valueOffset, nil))
                                )
                            },
                             HLTypeKind.dyn
                             )
                        ],
                        args: args,
                        reservedStackBytes: stackInfo.total,
                        mem: mem
                    )
                    
                    jmpTargetFinish.stop(at: mem.byteSize)
                    appendDebugPrintAligned4("Exiting OCallClosure", builder: mem)
                }
            case .OCallN(let dst, let fun, let args):
                try __ocalln(
                    dst: dst,
                    funIndex: fun,
                    regs: regs,
                    args: args,
                    reservedStackBytes: stackInfo.total,
                    mem: mem)
            case .OCall2(let dst, let fun, let arg0, let arg1):
                try __ocalln(
                    dst: dst,
                    funIndex: fun,
                    regs: regs,
                    args: [arg0, arg1],
                    reservedStackBytes: stackInfo.total,
                    mem: mem)
            case .ONew(let dst):
                appendDebugPrintAligned4("Entering ONew", builder: mem)
                // LOOK AT: https://github.com/HaxeFoundation/hashlink/blob/284301f11ea23d635271a6ecc604fa5cd902553c/src/jit.c#L3263
                let typeToAllocate = requireTypeKind(reg: dst, from: regs)

                let allocFunc_jumpTarget: UnsafeRawPointer

                // Do we need to move the dst value in X0 for the alloc func (e.g. hl_alloc_dynobj
                // takes no args, but hl_alloc_obj does)
                var needX0 = true

                switch(typeToAllocate.kind) {
                case .struct:
                    fallthrough
                case .obj:
                    mem.append(
                        PseudoOp.debugMarker("Using hl_alloc_obj to allocate reg \(dst))")
                    )
                    allocFunc_jumpTarget = unsafeBitCast(LibHl._hl_alloc_obj, to: UnsafeRawPointer.self)
                case .dynobj:
                    mem.append(
                        PseudoOp.debugMarker("Using hl_alloc_dynobj to allocate reg \(dst))")
                    )
                    allocFunc_jumpTarget = unsafeBitCast(LibHl._hl_alloc_dynobj, to: UnsafeRawPointer.self)
                    needX0 = false
                case .virtual:
                    mem.append(
                        PseudoOp.debugMarker("Using hl_alloc_virtual to allocate reg \(dst))")
                    )
                    allocFunc_jumpTarget = unsafeBitCast(LibHl._hl_alloc_virtual, to: UnsafeRawPointer.self)
                default:
                    fatalError("ONew not implemented for \(typeToAllocate)")
                }

                if needX0 {
                    let dstTypeAddr = requireTypeAddress(reg: dst, from: regs)
                    mem.append(
                        PseudoOp.debugMarker("Moving reg address \(dstTypeAddr) in .x0"),
                        PseudoOp.mov(.x0, dstTypeAddr)
                    )
                } else {
                    mem.append(
                        PseudoOp.debugMarker("Not moving reg \(dst) in x0 b/c alloc func doesn't need it")
                    )
                }

                let dstOffset = getRegStackOffset(regs, dst)

                mem.append(
                    PseudoOp.debugMarker("Moving alloc address in x1"),
                    PseudoOp.mov(.x1, allocFunc_jumpTarget),
                    PseudoOp.debugMarker("Jumping to the alloc func")
                )
                appendDebugPrintAligned4("Jumping to alloc func and storing result", builder: mem)
                mem.append(
                    M1Op.blr(.x1),
                    M1Op.str(X.x0, .reg64offset(X.sp, dstOffset, nil))
                )

            case .OGetThis(let dstReg, let fieldRef):
                try __ogetthis_ofield(
                    dstReg: dstReg,
                    objReg: 0,
                    fieldRef: fieldRef,
                    regs: regs,
                    mem: mem)
            case .OField(let dstReg, let objReg, let fieldRef):
                try __ogetthis_ofield(
                    dstReg: dstReg,
                    objReg: objReg,
                    fieldRef: fieldRef,
                    regs: regs,
                    mem: mem)
            case .OBytes(let dst, let ptr):
                let bytesAddress = try ctx.getBytes(ptr).ccompatAddress
                mem.append(PseudoOp.mov(X.x0, bytesAddress))
                appendStore(reg: X.x0, into: dst, kinds: regs, mem: mem)
            case .OString(let dst, let ptr):
                assert(reg: dst, from: regs, is: HLTypeKind.bytes)
                
                guard let codePtr = ctx.mainContext.pointee.code else {
                    fatalError("No code set")
                }
                mem.append(
                    PseudoOp.mov(X.x0, OpaquePointer(codePtr)),
                    PseudoOp.mov(X.x1, ptr),
                    PseudoOp.mov(X.x2, unsafeBitCast(LibHl._hl_get_ustring, to: OpaquePointer.self)),
                    M1Op.blr(X.x2)
                )
                appendStore(reg: X.x0, into: dst, kinds: regs, mem: mem)
            case .OFloat(let dst, let fref):
                assert(reg: dst, from: regs, in: [HLTypeKind.f32, HLTypeKind.f64])
                let f = try ctx.requireFloat(fref)
                mem.append(PseudoOp.mov(X.x0, f.bitPattern))
                appendStore(reg: X.x0, into: dst, kinds: regs, mem: mem)
            case .OInt(let dst, let iRef):
                assert(reg: dst, from: regs, in: [HLTypeKind.i64, HLTypeKind.i32, HLTypeKind.u8, HLTypeKind.u16])
                let c = try ctx.requireInt(iRef)
                mem.append(PseudoOp.mov(X.x0, c))
                appendStore(reg: X.x0, into: dst, kinds: regs, mem: mem)
            case .OSetThis(field: let fieldRef, src: let srcReg):
                try __osetthis_osetfield(objReg: 0, fieldRef: fieldRef, srcReg: srcReg, regs: regs, mem: mem)
            case .OSetField(let objReg, let fieldRef, let srcReg):
                try __osetthis_osetfield(objReg: objReg, fieldRef: fieldRef, srcReg: srcReg, regs: regs, mem: mem)
            case .OJNotEq(let a, let b, let offset):
                fallthrough
            case .OJNotGte(let a, let b, let offset):
                fallthrough
            case .OJNotLt(let a, let b, let offset):
                fallthrough
            case .OJSGt(let a, let b, let offset):
                fallthrough
            case .OJUGte(let a, let b, let offset):
                fallthrough
            case .OJEq(let a, let b, let offset):
                fallthrough
            case .OJSGte(let a, let b, let offset):
                fallthrough
            case .OJSLte(let a, let b, let offset):
                fallthrough
            case .OJSLt(let a, let b, let offset):
                fallthrough
            case .OJULt(let a, let b, let offset):

                let wordsToSkip = Int(offset) + 1
                let targetInstructionIx = currentInstruction + wordsToSkip
                guard targetInstructionIx < addrBetweenOps.count else {
                    fatalError("Jump going to an invalid op (\(targetInstructionIx))")
                }

                let regOffsetA = getRegStackOffset(regs, a)
                let regOffsetB = getRegStackOffset(regs, b)

                let kindA = requireTypeKind(reg: a, from: regs)
                let kindB = requireTypeKind(reg: b, from: regs)

                let sizeA = kindA.hlRegSize
                let sizeB = kindB.hlRegSize

                appendDebugPrintAligned4("\(op.id) <\(a)@\(regOffsetA), \(b)@\(regOffsetB)> --> \(offset) (target instruction: \(targetInstructionIx))", builder: mem)
                
                appendLoad(reg: X.x0, from: a, kinds: regs, mem: mem)
                appendLoad(reg: X.x1, from: b, kinds: regs, mem: mem)

                switch(op) {
                case .OJSGte:
                    fallthrough
                case .OJSLte:
                    fallthrough
                case .OJSGt:
                    fallthrough
                case .OJSLt:
                    switch(kindA) {
                    case .i32:
                        mem.append(M1Op.sxtw(.x0, .w0))
                    case .u16:
                        mem.append(M1Op.sxth(.x0, .w0))
                    case .u8:
                        mem.append(M1Op.sxtb(.x0, .w0))
                    default:
                        break
                    }
                    switch(kindB) {
                    case .i32:
                        mem.append(M1Op.sxtw(.x1, .w1))
                    case .u16:
                        mem.append(M1Op.sxth(.x1, .w1))
                    case .u8:
                        mem.append(M1Op.sxtb(.x1, .w1))
                    default:
                        break
                    }
                default:
                    break
                }

                appendDebugPrintRegisterAligned4(X.x0, prepend: "\(op.id)", builder: mem)
                appendDebugPrintRegisterAligned4(X.x1, prepend: "\(op.id)", builder: mem)

                mem.append(M1Op.cmp(X.x0, X.x1))

                
                // calculate what to skip
                let jumpOffset_partA = try Immediate21(mem.byteSize)
                let jumpOffset_partB = addrBetweenOps[targetInstructionIx]
                let jumpOffset = try DeferredImmediateSum(
                    jumpOffset_partB,
                    jumpOffset_partA,
                    -1,
                    -Int(0))
                //


                mem.append(
                    PseudoOp.deferred(4) {
                        switch(op.id) {
                        case .OJSGt:
                            return M1Op.b_gt(try Immediate21(jumpOffset.immediate))
                        case .OJSLt:
                            fallthrough
                        case .OJULt:
                            fallthrough
                        case .OJNotGte:
                            return M1Op.b_lt(try Immediate21(jumpOffset.immediate))
                        case .OJSLte:
                            return M1Op.b_le(try Immediate21(jumpOffset.immediate))
                        case .OJSGte:
                            fallthrough
                        case .OJUGte:
                            fallthrough
                        case .OJNotLt:
                            return M1Op.b_ge(try Immediate21(jumpOffset.immediate))
                        case .OJEq:
                            return M1Op.b_eq(try Immediate21(jumpOffset.immediate))
                        case .OJNotEq:
                            return M1Op.b_ne(try Immediate21(jumpOffset.immediate))
                        default:
                            fatalError("Unsupported jump id \(op.id)")
                        }
                    })
                
                appendDebugPrintAligned4("NOT JUMPING", builder: mem)

            // TODO: somehow combine all the jumps with fallthroughs?
            case .OJNotNull(let reg, let offset):
                fallthrough
            case .OJFalse(let reg, let offset):
                fallthrough
            case .OJTrue(let reg, let offset):
                fallthrough
            case .OJNull(let reg, let offset):

                let wordsToSkip = Int(offset) + 1
                let targetInstructionIx = currentInstruction + wordsToSkip
                guard targetInstructionIx < addrBetweenOps.count else {
                    fatalError("Jump going to an invalid op (\(targetInstructionIx))")
                }

                let regOffset = getRegStackOffset(regs, reg)

                mem.append(
                    PseudoOp.debugMarker("\(op.id) \(reg)@\(regOffset) --> \(offset) (target instruction: \(targetInstructionIx))"))
                mem.append(M1Op.movz64(X.x0, 0, nil))
                appendLoad(reg: X.x1, from: reg, kinds: regs, mem: mem)
                
                appendDebugPrintAligned4("Jump comparing x0 and x1", builder: mem)
                appendDebugPrintRegisterAligned4(X.x0, builder: mem)
                appendDebugPrintRegisterAligned4(X.x1, builder: mem)

                mem.append(M1Op.cmp(X.x0, X.x1))

                // calculate what to skip
                let jumpOffset_partA = try Immediate21(mem.byteSize)
                let jumpOffset_partB = addrBetweenOps[targetInstructionIx]
                let jumpOffset = try DeferredImmediateSum(
                    jumpOffset_partB,
                    jumpOffset_partA,
                    -1,
                    -Int(0))
                //


                mem.append(
                    PseudoOp.deferred(4) {
                        switch(op.id) {
                        case .OJFalse:
                            fallthrough
                        case .OJNull:
                            return M1Op.b_eq(try Immediate21(jumpOffset.immediate))
                        case .OJTrue:
                            fallthrough
                        case .OJNotNull:
                            return M1Op.b_ne(try Immediate21(jumpOffset.immediate))
                        default:
                            fatalError("Unsupported jump id \(op.id)")
                        }
                    })
                
                appendDebugPrintAligned4("NOT JUMPING", builder: mem)
            // TODO: combine with above jumps
            case .OJAlways(let offset):
                let wordsToSkip = Int(offset) + 1
                let targetInstructionIx = currentInstruction + wordsToSkip
                guard targetInstructionIx < addrBetweenOps.count else {
                    fatalError("Jump going to an invalid op (\(targetInstructionIx))")
                }

                // calculate what to skip
                let jumpOffset_partA = try Immediate21(mem.byteSize)
                let jumpOffset_partB = addrBetweenOps[targetInstructionIx]
                let jumpOffset = try DeferredImmediateSum(
                    jumpOffset_partB,
                    jumpOffset_partA,
                    -1,
                    -Int(0))
                //


                mem.append(
                    PseudoOp.deferred(4) {
                        return M1Op.b(jumpOffset.immediate)
                    })
            case .OGetGlobal(let dst, let globalRef):
                let globalInstanceAddress = try ctx.requireGlobalData(globalRef)
                assert(reg: dst, from: regs, in: [HLTypeKind.dyn, HLTypeKind.obj, HLTypeKind.struct, HLTypeKind.abstract, HLTypeKind.enum])
                mem.append(PseudoOp.mov(.x0, UnsafeRawPointer(globalInstanceAddress)))
                mem.append(M1Op.ldr(X.x0, .reg64offset(X.x0, 0, nil)))
                appendStore(reg: X.x0, into: dst, kinds: regs, mem: mem)
                appendDebugPrintRegisterAligned4(X.x0, prepend: "OGetGlobal result", builder: mem)
            case .OSetGlobal(let globalRef, let src):
                let globalInstanceAddress = try ctx.requireGlobalData(globalRef)
                assert(reg: src, from: regs, in: [HLTypeKind.dyn, HLTypeKind.obj, HLTypeKind.struct, HLTypeKind.abstract, HLTypeKind.enum])
                appendLoad(reg: X.x0, from: src, kinds: regs, mem: mem)
                mem.append(PseudoOp.mov(.x1, OpaquePointer(globalInstanceAddress)))
                mem.append(M1Op.str(X.x0, .reg64offset(X.x1, 0, nil)))
            case .OShl(let dst, let a, let b):
                appendLoad(reg: .x0, from: a, kinds: regs, mem: mem)
                appendLoad(reg: .x1, from: b, kinds: regs, mem: mem)
                mem.append(M1Op.lsl_r(X.x2, X.x0, X.x1))
                appendStore(reg: X.x2, into: dst, kinds: regs, mem: mem)
                appendLoad(reg: X.x15, from: a, kinds: regs, mem: mem)
                appendLoad(reg: X.x15, from: b, kinds: regs, mem: mem)
                appendLoad(reg: X.x15, from: dst, kinds: regs, mem: mem)
            case .OSetI8(let bytes, let index, let src):
                fallthrough
            case .OSetI16(let bytes, let index, let src):
                assert(reg: bytes, from: regs, is: HLTypeKind.bytes)
                assert(reg: index, from: regs, is: HLTypeKind.i32)
                
                if op.id == .OSetI16 {
                    assert(reg: src, from: regs, in: [HLTypeKind.u16, HLTypeKind.i32])
                } else if op.id == .OSetI8 {
                    assert(reg: src, from: regs, in: [HLTypeKind.u8, HLTypeKind.u16, HLTypeKind.i32])
                }
                
                appendLoad(reg: X.x0, from: bytes, kinds: regs, mem: mem)
                appendLoad(reg: X.x1, from: index, kinds: regs, mem: mem)
                appendLoad(reg: X.x2, from: src, kinds: regs, mem: mem)
                
                if op.id == .OSetI8 {
                    mem.append(
                        M1Op.strb(W.w2, .reg(X.x0, .r64shift(X.x1, .lsl(0))))
                    )
                } else if op.id == .OSetI16 {
                    mem.append(
                        M1Op.strh(W.w2, .reg(X.x0, .r64shift(X.x1, .lsl(0))))
                    )
                } else {
                    fatalError("Unexpected op \(op.id)")
                }
            case .OGetMem(let dst, let bytes, let index):
                fallthrough
            case .OGetI8(let dst, let bytes, let index):
                fallthrough
            case .OGetI16(let dst, let bytes, let index):
                
                appendLoad(reg: X.x15, from: index, kinds: regs, mem: mem)
                appendDebugPrintRegisterAligned4(X.x15, prepend: "db_OGetI*", builder: mem)
                
                assert(reg: bytes, from: regs, is: HLTypeKind.bytes)
                assert(reg: index, from: regs, is: HLTypeKind.i32)
                if op.id == .OGetI16 {
                    assert(reg: dst, from: regs, in: [HLTypeKind.u16, HLTypeKind.i32])
                } else if op.id == .OGetI8 {
                    assert(reg: dst, from: regs, in: [HLTypeKind.u8, HLTypeKind.u16, HLTypeKind.i32])
                }

                let dstOffset = getRegStackOffset(regs, dst)
                let byteOffset = getRegStackOffset(regs, bytes)
                let indexOffset = getRegStackOffset(regs, index)

                mem.append(
                    // Load byte address (base) in X.x0. It is 8 bytes
                    M1Op.ldr(X.x0, .reg64offset(.sp, byteOffset, nil)),
                    // Load index into X.x1. It is 4 bytes
                    M1Op.ldr(W.w1, .reg64offset(.sp, indexOffset, nil))
                )
                
                if op.id == .OGetI8 {
                    appendDebugPrintAligned4("about to load from bytes (off: \(byteOffset))", builder: mem)
                    appendDebugPrintRegisterAligned4(X.x0, builder: mem)
                    
                    mem.append(
                        M1Op.ldrb(W.w0, .reg(X.x0, .r64shift(X.x1, .lsl(0))))
                    )
                    appendDebugPrintAligned4("loaded", builder: mem)
                } else if op.id == .OGetI16 {
                    mem.append(
                        M1Op.ldrh(W.w0, .reg(X.x0, .r64shift(X.x1, .lsl(0))))
                    )
                } else if op.id == .OGetMem {
                    // TODO: add test
                    assert(reg: dst, from: regs, is: HLTypeKind.i32)
                    mem.append(
                        M1Op.ldr(W.w0, .reg(X.x0, .r64shift(X.x1, .lsl(0))))
                    )
                }
                
                let size = requireTypeKind(reg: dst, from: regs).hlRegSize
                if size == 4 {
                    mem.append(M1Op.str(W.w0, .reg64offset(.sp, dstOffset, nil)))
                } else if size == 8 {
                    mem.append(M1Op.str(X.x0, .reg64offset(.sp, dstOffset, nil)))
                } else if size == 2 {
                    mem.append(M1Op.strh(W.w0, .imm64(.sp, dstOffset, nil)))
                } else if size == 1 {
                    mem.append(M1Op.strb(W.w0, .imm64(.sp, dstOffset, nil)))
                } else {
                    fatalError("Invalid register size")
                }
            case .ONullCheck(let dst):
                let dstOffset = getRegStackOffset(regs, dst)
                let size = requireTypeKind(reg: dst, from: regs).hlRegSize
                if size == 4 {
                    mem.append(M1Op.ldr(W.w0, .reg64offset(.sp, dstOffset, nil)))
                } else if size == 8 {
                    mem.append(M1Op.ldr(X.x0, .reg64offset(.sp, dstOffset, nil)))
                } else if size == 2 {
                    mem.append(M1Op.ldrh(W.w0, .imm64(.sp, dstOffset, nil)))
                } else if size == 1 {
                    mem.append(M1Op.ldrb(W.w0, .imm64(.sp, dstOffset, nil)))
                } else {
                    fatalError("Invalid size for null check")
                }

                mem.append(
                    M1Op.movz64(X.x1, 0, nil),
                    M1Op.cmp(X.x0, X.x1)
                )
                var jumpOverDeath = RelativeDeferredOffset()
                jumpOverDeath.start(at: mem.byteSize)
                mem.append(
                    PseudoOp.deferred(4) {
                        return M1Op.b_ne(try Immediate21(jumpOverDeath.value))
                    }
                )
                appendDebugPrintAligned4("Null access exception", builder: mem)
                
                // tmp crash on null access
                appendSystemExit(1, builder: mem)
                jumpOverDeath.stop(at: mem.byteSize)
            case .OAnd(let dst, let a, let b):
                appendLoad(reg: X.x0, from: a, kinds: regs, mem: mem)
                appendLoad(reg: X.x1, from: b, kinds: regs, mem: mem)
                
                mem.append(
                    //M1Op.ldr(X.x0, .reg64offset(X.sp, aOffset, nil)),
                    //M1Op.ldr(X.x1, .reg64offset(X.sp, bOffset, nil)),
                    M1Op.and(X.x2, X.x0, .r64shift(X.x1, .lsl(0)))
                    //M1Op.str(X.x2, .reg64offset(X.sp, dstOffset, nil))
                )
                appendStore(reg: X.x2, into: dst, kinds: regs, mem: mem)
            case .ORethrow(let exc):
                appendLoad(reg: X.x0, from: exc, kinds: regs, mem: mem)
                mem.append(
                    PseudoOp.mov(X.x1, unsafeBitCast(LibHl._hl_rethrow, to: OpaquePointer.self)),
                    M1Op.blr(X.x1)
                )
            case .OThrow(let exc):
                appendLoad(reg: X.x0, from: exc, kinds: regs, mem: mem)
                mem.append(
                    PseudoOp.mov(X.x1, unsafeBitCast(LibHl._hl_throw, to: OpaquePointer.self)),
                    M1Op.blr(X.x1)
                )
            case .OTrap(let exc, let offset):
                /*
                 Roughly what needs to happen is:
                     
                        #define hl_trap(ctx,r,label) {
                            hl_thread_info *__tinf = hl_get_thread();
                            ctx.tcheck = NULL;
                            ctx.prev = __tinf->trap_current;
                            __tinf->trap_current = &ctx;
                            if( setjmp(ctx.buf) ) {
                                r = __tinf->exc_value;
                                goto label;
                            }
                        }
                 
                 The trap context needs to be on the stack, and setjmp can not be called from
                 within a child function (needs to remain in the overall function context).
                 */
                
                // Offsets into `HLTrapCtx_CCompat`
                let bufOffset: Int64 = Int64(MemoryLayout.offset(of: \HLTrapCtx_CCompat.buf)!)
                let prevOffset: Int64 = Int64(MemoryLayout.offset(of: \HLTrapCtx_CCompat.prev)!)
                let tcheckOffset: Int64 = Int64(MemoryLayout.offset(of: \HLTrapCtx_CCompat.tcheck)!)
                
                // Offsets into HLThreadInfo_CCompat
                let tinf__trapCurrent: Int64 = Int64(MemoryLayout.offset(of: \HLThreadInfo_CCompat.trap_current)!)
                let tinf__excValue: Int64 = Int64(MemoryLayout.offset(of: \HLThreadInfo_CCompat.exc_value)!)
                
                let currentTrapIx: Int64 = availableTrapIx
                availableTrapIx += 1
                
                let trapOffsetInStack = stackInfo.reservedForVreg + currentTrapIx * Int64(MemoryLayout<HLTrapCtx_CCompat>.stride)
                
                // hl_thread_info *__tinf = hl_get_thread();
                appendDebugPrintAligned4(
                    "// hl_thread_info *__tinf = hl_get_thread();", builder: mem
                )
                mem.append(
                    PseudoOp.mov(
                        X.x0,
                        unsafeBitCast(LibHl._hl_get_thread, to: OpaquePointer.self)
                    ),
                    M1Op.blr(X.x0),
                    M1Op.movr64(X.x9, X.x0) // x9 = __tinf
                )
                
                appendDebugPrintAligned4(
                    "// ctx.tcheck = NULL;", builder: mem
                )
                mem.append(
                    M1Op.movz64(X.x1, 0, nil),
                    M1Op.movz64(X.x14, UInt16(trapOffsetInStack + tcheckOffset), nil),
                    M1Op.str(X.x1, .reg(X.sp, .r64ext(X.x14, .sxtx(0))))
                )
                
                // ctx.prev = __tinf->trap_current;
                appendDebugPrintAligned4(
                    "ctx.prev = __tinf->trap_current;", builder: mem
                )
                
                appendDebugPrintAligned4("[traptest] In findex \(compilable.findex); trapoff: \(trapOffsetInStack)", builder: mem)

                // fetch trap_current
                mem.append(M1Op.ldr(X.x2, .reg64offset(X.x9, tinf__trapCurrent, nil)))
                appendDebugPrintRegisterAligned4(X.x2, prepend: "[traptest] Loaded current", builder: mem)
                // store ctx.prev
                let testOff: Int64 = trapOffsetInStack + prevOffset
                mem.append(
                    M1Op.movz64(X.x14, UInt16(testOff), nil),
                    M1Op.str(X.x2, .reg(X.sp, .r64ext(X.x14, .sxtx(0))))
                )
                appendDebugPrintAligned4("[traptest] Offset: \(testOff)", builder: mem)
                //
                
                // __tinf->trap_current = &ctx;
                appendDebugPrintAligned4(
                    "__tinf->trap_current = &ctx;", builder: mem
                )
                mem.append(
                    PseudoOp.debugMarker("x3 = &ctx"),
                    M1Op.movr64(X.x3, .sp),
                    M1Op.add(X.x3, X.x3, .imm(trapOffsetInStack, nil))
                )
                
                appendDebugPrintAligned4("__tinf->trap_current = x3", builder: mem)
                
                mem.append(
                    PseudoOp.debugMarker("__tinf->trap_current = x3"),
                    M1Op.str(X.x3, .reg64offset(X.x9, tinf__trapCurrent, nil))
                )
                
                // setjmp(ctx.buf)
                let _setjmpAddr = unsafeBitCast(LibSystem.setjmp, to: OpaquePointer.self)
                mem.append(
                    PseudoOp.debugMarker("x3 = &ctx.buf"),
                    M1Op.movr64(X.x3, .sp),
                    M1Op.movz64(X.x4, UInt16(trapOffsetInStack) + UInt16(bufOffset), nil),
                    M1Op.add(X.x3, X.x3, .r64shift(X.x4, .lsl(0))),
                    
                    PseudoOp.debugMarker("x1 = x0 (__tinf)"),
                    M1Op.movr64(X.x1, X.x0),
                    
                    PseudoOp.debugMarker("x0 = x3 (&ctx.buf)"),
                    M1Op.movr64(X.x0, X.x3),
                    
                    PseudoOp.debugMarker("setjmp(&ctx.buf)"),
                    PseudoOp.mov(X.x10, _setjmpAddr),
                    M1Op.blr(X.x10)
                )
                
                // compare result
                mem.append(
                    M1Op.movz64(X.x1, 0, nil),
                    M1Op.cmp(X.x0, X.x1)
                )
                
                var setJmpEq0Target = RelativeDeferredOffset()
                mem.append(
                    PseudoOp.withOffset(
                        offset: &setJmpEq0Target,
                        mem: mem,
                        M1Op.b_eq(try! Immediate21(setJmpEq0Target.value))
                    )
                )
                
                appendDebugPrintAligned4("[traptest] SETJMP RETURNED !0", builder: mem)
                
                assert(reg: exc, from: regs, is: HLTypeKind.dyn)
                
                // r = __tinf->exc_value;
                appendDebugPrintAligned4("x0 (__tinf) = hl_get_thread()", builder: mem)
                mem.append(
                    PseudoOp.debugMarker("x0 (__tinf) = hl_get_thread()"),
                    PseudoOp.mov(
                        X.x0,
                        unsafeBitCast(LibHl._hl_get_thread, to: OpaquePointer.self)
                    ),
                    M1Op.blr(X.x0)
                )
                appendDebugPrintRegisterAligned4(X.x0, prepend: "__tinf", builder: mem)
                
                appendDebugPrintAligned4("r = __tinf->exc_value;", builder: mem)
                appendDebugPrintAligned4("loading", builder: mem)
                mem.append(
                    PseudoOp.debugMarker("r = __tinf->exc_value;"),
                    M1Op.ldr(X.x1, .reg64offset(X.x0, tinf__excValue, nil))
                )
                appendDebugPrintRegisterAligned4(X.x1, prepend: "exc_value", builder: mem)
                appendLoad(reg: X.x2, from: exc, kinds: regs, mem: mem)
                appendDebugPrintRegisterAligned4(X.x2, prepend: "EXC (after)", builder: mem)
                
                mem.append(M1Op.movr64(X.x12, .sp))
                appendDebugPrintRegisterAligned4(X.x12, prepend: "SP (after)", builder: mem)
                
                appendStore(reg: X.x2, into: exc, kinds: regs, mem: mem)
                appendDebugPrintAligned4("Stored x2 in x1", builder: mem)
                
                // goto label;
                let wordsToSkip = Int(offset) + 1
                let targetInstructionIx = currentInstruction + wordsToSkip
                
                appendDebugPrintAligned4("Jumping from \(currentInstruction) to \(targetInstructionIx)", builder: mem)
                appendDebugPrintAligned4("Preparing jump (words to skip \(wordsToSkip))...", builder: mem)
                
                let jumpOffset_partA = try Immediate21(mem.byteSize)
                let jumpOffset_partB = addrBetweenOps[targetInstructionIx]
                let jumpOffset = try DeferredImmediateSum(
                    jumpOffset_partB,
                    jumpOffset_partA,
                    -1,
                    -Int(0))
                mem.append(
                    PseudoOp.deferred(4) {
                        M1Op.b_v2(try Immediate26(jumpOffset.immediate))
                    }
                )
                
                // marker for other branch after setjmp()
                setJmpEq0Target.stop(at: mem.byteSize)
                appendDebugPrintAligned4("[traptest] SETJMP RETURNED 0", builder: mem)
                
                // call longjump
//                mem.append(
//                    PseudoOp.debugMarker("x0 = &ctx.buf"),
//                    M1Op.movr64(X.x0, .sp),
//                    M1Op.movz64(X.x1, UInt16(trapOffsetInStack) + UInt16(bufOffset), nil),
//                    M1Op.add(X.x0, X.x0, .r64shift(X.x1, .lsl(0))),
//
//                    PseudoOp.debugMarker("x1 = 10"),
//                    M1Op.movz64(X.x1, 14, nil),
//
//                    PseudoOp.debugMarker("longjmp"),
//                    PseudoOp.mov(X.x10, unsafeBitCast(LibSystem.longjmp, to: OpaquePointer.self)),
//                    M1Op.blr(X.x10)
//                )
//
//                appendSystemExit(15, builder: mem)
                
//
//                if( setjmp(ctx.buf) ) {
//                    r = __tinf->exc_value;
//                    goto label;
//                }
                
                
            case .OEndTrap(let exc):
                availableTrapIx -= 1
                
                let currentTrapIx: Int64 = availableTrapIx
                let trapOffsetInStack = stackInfo.reservedForVreg + currentTrapIx * Int64(MemoryLayout<HLTrapCtx_CCompat>.stride)
                
                appendDebugPrintAligned4("[OEndTrap] hl_get_thread()->trap_current = ctx.prev", builder: mem)
                
                // Offsets into HLThreadInfo_CCompat
                let tinf__trapCurrent: Int64 = Int64(MemoryLayout.offset(of: \HLThreadInfo_CCompat.trap_current)!)
                
                // Offsets into HLTrapCtx_CCompat
                let prevOffset: Int64 = Int64(MemoryLayout.offset(of: \HLTrapCtx_CCompat.prev)!)
                
                mem.append(
                    PseudoOp.debugMarker("// hl_thread_info *__tinf = hl_get_thread();"),
                    PseudoOp.mov(
                        X.x0,
                        unsafeBitCast(LibHl._hl_get_thread, to: OpaquePointer.self)
                    ),
                    M1Op.blr(X.x0),
                    M1Op.movr64(X.x9, X.x0) // x9 = __tinf
                )
                
                mem.append(
                    PseudoOp.debugMarker("// x2 = ctx.prev"),
                    M1Op.movz64(X.x14, UInt16(trapOffsetInStack + prevOffset), nil),
                    M1Op.ldr(X.x2, .reg(X.sp, .r64ext(X.x14, .sxtx(0))))
                )
                
                // TODO: test 2 traps, throw second
                
                mem.append(
                    PseudoOp.debugMarker("// __tinf->trap_current = x2"),
                    M1Op.str(X.x2, .reg64offset(X.x9, tinf__trapCurrent, nil))
                )
            case .ONull(let dst):
                mem.append(
                    M1Op.movz64(X.x0, 0, nil)
                )
                appendStore(reg: X.x0, into: dst, kinds: regs, mem: mem)
            case .OBool(let dst, let value):
                let dstOffset = getRegStackOffset(regs, dst)
                mem.append(
                    M1Op.movz64(X.x0, UInt16(value), nil),
                    M1Op.strb(W.w0, .imm64(.sp, dstOffset, nil))
                )
            case .OUnsafeCast(let dst, let src):
                fallthrough
            case .OMov(let dst, let src) where isInteger(vreg: dst, kinds: regs) && isInteger(vreg: src, kinds: regs):
                try assertNumeric(dst: dst, bigEnoughFor: src, from: regs)
                
                appendLoad(reg: X.x0, from: src, kinds: regs, mem: mem)
                appendStore(reg: X.x0, into: dst, kinds: regs, mem: mem)
            case .OMov(let dst, let src) where isFP(vreg: dst, kinds: regs) && isFP(vreg: src, kinds: regs):
                try assertNumeric(dst: dst, bigEnoughFor: src, from: regs)
                
                appendLoadAndConvertToDouble(reg: D.d0, from: src, kinds: regs, mem: mem)
                appendStoreDoubleToRightSize(reg: D.d0, into: dst, kinds: regs, mem: mem)
            case .OMov(let dst, let src) where !isNumeric(vreg: dst, kinds: regs) && !isNumeric(vreg: src, kinds: regs):
                let srcKind = requireTypeKind(reg: src, from: regs)
                assert(reg: dst, from: regs, in: [srcKind, HLTypeKind.dyn])
                appendLoad(reg: X.x0, from: src, kinds: regs, mem: mem)
                appendStore(reg: X.x0, into: dst, kinds: regs, mem: mem)
            case .OSafeCast(let dst, let src):
                /*
                 safecast [dst], [r] cast register r into register dst, throw an exception if there is no way to perform such operation
                 */
                
                _ = requireTypeKind(reg: src, from: regs)
                let dstType = requireTypeKind(reg: dst, from: regs)
                
                let srcOffset = getRegStackOffset(regs, src)
                
                let castFunc = get_dyncast(to: dstType.kind)
                appendDebugPrintAligned4("Determined cast function", builder: mem)
                
                    
                mem.append(
                    // load &src into x.0
                    M1Op.movr64(X.x0, .sp),
                    M1Op.movz64(X.x1, UInt16(srcOffset), nil),
                    M1Op.add(X.x0, X.x0, .r64shift(X.x1, .lsl(0)))
                )
                mem.append(PseudoOp.mov(X.x1, requireTypeMemory(reg: src, regs: regs)))
                
                appendDebugPrintRegisterAligned4(X.x0, prepend: "OSafeCast", builder: mem)
                appendDebugPrintRegisterAligned4(X.x1, prepend: "OSafeCast", builder: mem)
                if (dstType != .f32 && dstType != .f64) {
                    // float/double casts are the only ones that don't require the third "to" arg
                    mem.append(PseudoOp.mov(X.x2, requireTypeMemory(reg: dst, regs: regs)))
                    appendDebugPrintRegisterAligned4(X.x2, prepend: "OSafeCast", builder: mem)
                }
                appendDebugPrintAligned4("Jumping to cast function", builder: mem)
                mem.append(
                    PseudoOp.mov(X.x15, castFunc),
                    M1Op.blr(X.x15)
                )
                // TODO: check for failed cast result
                appendDebugPrintAligned4("TODO: OSafeCast should check for failed cast result", builder: mem)
                
                if (dstType != .f32 && dstType != .f64) {
                    appendDebugPrintRegisterAligned4(X.x0, prepend: "OSafeCast result (gp)", builder: mem)
                    appendStore(reg: X.x0, into: dst, kinds: regs, mem: mem)
                } else {
                    appendFPRegToDouble(reg: D.d0, from: dst, kinds: regs, mem: mem)
                    appendDebugPrintRegisterAligned4(D.d0, prepend: "OSafeCast result (float)", builder: mem)
                    appendStore(reg: D.d0, into: dst, kinds: regs, mem: mem)
                }
            case .OLabel:
                appendDebugPrintAligned4("OLabel", builder: mem)
            case .OSub(let dst, let a, let b):
                appendLoad(reg: .x0, from: a, kinds: regs, mem: mem)
                appendLoad(reg: .x1, from: b, kinds: regs, mem: mem)

                mem.append(
                    M1Op.sub(X.x2, X.x0, .r64shift(X.x1, .lsl(0)))
                )

                appendStore(reg: .x2, into: dst, kinds: regs, mem: mem)
            case .OAdd(let dst, let a, let b):
                appendLoad(reg: .x0, from: a, kinds: regs, mem: mem)
                appendLoad(reg: .x1, from: b, kinds: regs, mem: mem)
                appendDebugPrintRegisterAligned4(X.x0, builder: mem)
                appendDebugPrintRegisterAligned4(X.x1, builder: mem)
                mem.append(M1Op.add(X.x0, X.x0, .r64shift(X.x1, .lsl(0))))
                appendStore(reg: X.x0, into: dst, kinds: regs, mem: mem)
            case .ONot(let dst, let src):
                assert(reg: dst, from: regs, is: HLTypeKind.bool)
                let _notter: (@convention(c) (UInt8)->(UInt8)) = {
                    inVal in
                    
                    return inVal ^ 1
                }
                appendLoad(reg: X.x0, from: src, kinds: regs, mem: mem)
                mem.append(
                    PseudoOp.mov(X.x1, unsafeBitCast(_notter, to: OpaquePointer.self)),
                        M1Op.blr(X.x1)
                )
                appendStore(reg: X.x0, into: dst, kinds: regs, mem: mem)
            case .ONeg(let dst, let src):
                assert(reg: dst, from: regs, in: [HLTypeKind.i32, HLTypeKind.i64, HLTypeKind.u8, HLTypeKind.u16])
                let _negger: (@convention(c) (UInt64)->(UInt64)) = {
                    inVal in
                    
                    return 0 &- inVal
                }
                appendLoad(reg: X.x0, from: src, kinds: regs, mem: mem)
                mem.append(
                    PseudoOp.mov(X.x1, unsafeBitCast(_negger, to: OpaquePointer.self)),
                        M1Op.blr(X.x1)
                )
                appendStore(reg: X.x0, into: dst, kinds: regs, mem: mem)
            case .OUShr(let dst, let a, let b):
                fallthrough
            case .OSShr(let dst, let a, let b):
                appendLoad(reg: .x0, from: a, kinds: regs, mem: mem)
                appendLoad(reg: .x1, from: b, kinds: regs, mem: mem)

                if case .OSShr = op {
                    appendSignMode(true, reg: .x0, from: a, kinds: regs, mem: mem)
                    mem.append(M1Op.asrv(X.x2, X.x0, X.x1))
                } else if case .OUShr = op {
                    mem.append(M1Op.lsrv(X.x2, X.x0, X.x1))
                } else {
                    fatalError("Unknown shift op")
                }

                appendStore(reg: X.x2, into: dst, kinds: regs, mem: mem)
            case .OArraySize(let dst, let src):
                _ = requireTypeKind(reg: src, from: regs, shouldMatch: HLTypeKind.array)

                appendLoad(reg: .x0, from: src, kinds: regs, mem: mem)
                mem.append(
                    // varray: skip 2 pointers (16 bytes) and load 4 bytes
                    M1Op.ldr(W.w0, .reg(X.x0, .imm(16, nil)))
                )
                appendStore(reg: .x0, into: dst, kinds: regs, mem: mem)
            case .OType(let dst, let ty):
                let typeMemory = try ctx.getType(ty)
                let typeMemoryVal = Int(bitPattern: typeMemory.ccompatAddress)
                mem.append(PseudoOp.mov(.x0, typeMemoryVal))
                appendStore(reg: .x0, into: dst, kinds: regs, mem: mem)

                let _test: (@convention(c) (UnsafeRawPointer) -> ()) = { (_ ptr: UnsafeRawPointer) in
                    let p = UnsafePointer<HLType_CCompat>(OpaquePointer(ptr))
                }
                let _testAddress = unsafeBitCast(_test, to: UnsafeMutableRawPointer.self)
                mem.append(
                    PseudoOp.mov(X.x1, _testAddress),
                    M1Op.blr(X.x1))
            case .OIncr(let dst):
                appendLoad(reg: .x0, from: dst, kinds: regs, mem: mem)
                mem.append(M1Op.add(X.x0, X.x0, .imm(1, nil)))
                appendStore(reg: .x0, into: dst, kinds: regs, mem: mem)
            case .ODecr(let dst):
                appendLoad(reg: .x0, from: dst, kinds: regs, mem: mem)
                mem.append(M1Op.sub(X.x0, X.x0, .imm(1, nil)))
                appendStore(reg: .x0, into: dst, kinds: regs, mem: mem)
            case .OSetArray(let array, let index, let src):
                // x0 -> points to ((*_varray)(array))+1
                appendLoad(reg: X.x0, from: array, kinds: regs, mem: mem)
                appendDebugPrintRegisterAligned4(X.x0, prepend: "OSetArray array", builder: mem)
                
                // x1 point to field base (right after the (varray) content at x0
                mem.append(M1Op.add(X.x1, X.x0, .imm(Int64(MemoryLayout<varray>.stride), nil)))
                appendDebugPrintRegisterAligned4(X.x1, prepend: "OSetArray before deref", builder: mem)
                
                // x2 load index and shift by field size multiplier
                let lsl = requireTypeSizeLsl(reg: src, from: regs)
                appendLoad(reg: X.x2, from: index, kinds: regs, mem: mem)
                appendDebugPrintRegisterAligned4(X.x2, prepend: "OSetArray index before mul", builder: mem)
                mem.append(M1Op.lsl_i(X.x2, X.x2, try UImmediate6(lsl)))
                appendDebugPrintRegisterAligned4(X.x2, prepend: "OSetArray index after mul", builder: mem)
                
                // x3 load value to store
                appendLoad(reg: X.x3, from: src, kinds: regs, mem: mem)
                
                // dereference [x1/*field base*/ + x2/*offset from index*/]
                mem.append(M1Op.str(X.x3, .reg(X.x1, .r64ext(X.x2, .sxtx(0)))))
            case .OGetArray(let dst, let array, let index):
                // x0 -> points to ((*_varray)(array))+1
                appendLoad(reg: X.x0, from: array, kinds: regs, mem: mem)
                appendDebugPrintRegisterAligned4(X.x0, prepend: "OGetArray array", builder: mem)
                
                // x1 point to field base (right after the (varray) content at x0
                mem.append(M1Op.add(X.x1, X.x0, .imm(Int64(MemoryLayout<varray>.stride), nil)))
                appendDebugPrintRegisterAligned4(X.x1, prepend: "OGetArray before deref", builder: mem)
                
                // x2 load index and shift by field size multiplier
                let lsl = requireTypeSizeLsl(reg: dst, from: regs)
                appendLoad(reg: X.x2, from: index, kinds: regs, mem: mem)
                appendDebugPrintRegisterAligned4(X.x2, prepend: "OGetArray index before mul", builder: mem)
                mem.append(M1Op.lsl_i(X.x2, X.x2, try UImmediate6(lsl)))
                appendDebugPrintRegisterAligned4(X.x2, prepend: "OGetArray index after mul", builder: mem)
                
                // dereference [x1/*field base*/ + x2/*offset from index*/]
                mem.append(M1Op.ldr(X.x1, .reg(X.x1, .r64shift(X.x2, .lsl(0)))))
                appendDebugPrintRegisterAligned4(X.x1, prepend: "OGetArray fieldbase after deref", builder: mem)
                
                appendStore(reg: X.x1, into: dst, kinds: regs, mem: mem)
                appendDebugPrintRegisterAligned4(X.x1, prepend: "OGetArray fetched item", builder: mem)
            case .ONop:
                mem.append(M1Op.nop)
            case .OXor(let dst, let a, let b):
                appendLoad(reg: .x0, from: a, kinds: regs, mem: mem)
                appendLoad(reg: .x1, from: b, kinds: regs, mem: mem)
                mem.append(M1Op.eor_r(X.x2, X.x0, X.x1, nil))
                appendStore(reg: X.x2, into: dst, kinds: regs, mem: mem)
            case .OMul(let dst, let a, let b):
                appendLoadNumericAsFP(reg: D.d0, from: a, kinds: regs, mem: mem)
                appendLoadNumericAsFP(reg: D.d1, from: b, kinds: regs, mem: mem)
                
                appendDebugPrintRegisterAligned4(X.x0, prepend: "OMul(a-i)", builder: mem)
                appendDebugPrintRegisterAligned4(X.x1, prepend: "OMul(b-i)", builder: mem)
                
                appendDebugPrintRegisterAligned4(D.d0, prepend: "OMul(a)", builder: mem)
                appendDebugPrintRegisterAligned4(D.d1, prepend: "OMul(b)", builder: mem)
                
                mem.append(M1Op.fmul(D.d0, D.d0, D.d1))
                
                appendStoreFPAsNumeric(reg: D.d0, as: dst, kinds: regs, mem: mem)
            case .OUDiv(let dst, let a, let b):
                // UDiv only defined for integer types
                // See: https://haxe.org/blog/hashlink-in-depth-p2/
                fallthrough
            case .OSDiv(let dst, let a, let b) where isInteger(vreg: a, kinds: regs) && isInteger(vreg: b, kinds: regs):
                assertFP(reg: dst, from: regs)
                assertInteger(reg: a, from: regs)
                assertInteger(reg: b, from: regs)
                
                appendLoadNumeric(reg: 0, from: a, kinds: regs, mem: mem)
                appendLoadNumeric(reg: 1, from: b, kinds: regs, mem: mem)
                appendDebugPrintRegisterAligned4(X.x0, prepend: "\(op.id)", builder: mem)
                appendDebugPrintRegisterAligned4(X.x1, prepend: "\(op.id)", builder: mem)
                
                if case .OUDiv = op {
                    appendSignMode(false, reg: X.x0, from: a, kinds: regs, mem: mem)
                    appendSignMode(false, reg: X.x1, from: b, kinds: regs, mem: mem)
                    mem.append(M1Op.udiv(X.x0, X.x0, X.x1))
                } else if case .OSDiv = op {
                    appendSignMode(true, reg: X.x0, from: a, kinds: regs, mem: mem)
                    appendSignMode(true, reg: X.x1, from: b, kinds: regs, mem: mem)
                    mem.append(M1Op.sdiv(X.x0, X.x0, X.x1))
                } else {
                    fatalError("Invalid op for div (to determine udiv/sdiv)")
                }
                
                appendDebugPrintRegisterAligned4(X.x0, prepend: "\(op.id) res (before float)", builder: mem)
                
                if case .OUDiv = op {
                    appendUcvtf(reg: X.x0, to: D.d0, target: dst, kinds: regs, mem: mem)
                } else if case .OSDiv = op {
                    appendScvtf(reg: X.x0, to: D.d0, target: dst, kinds: regs, mem: mem)
                } else {
                    fatalError("Invalid op for div (to determine how to convert to float)")
                }
                
                appendStore(reg: D.d0, into: dst, kinds: regs, mem: mem)
                
            case .OSDiv(let dst, let a, let b) where isFP(vreg: a, kinds: regs) && isFP(vreg: b, kinds: regs):
                assertFP(reg: dst, from: regs)
                appendLoad(reg: D.d0, from: a, kinds: regs, mem: mem)
                appendLoad(reg: D.d1, from: b, kinds: regs, mem: mem)
                
                appendFPRegToDouble(reg: D.d0, from: a, kinds: regs, mem: mem)
                appendFPRegToDouble(reg: D.d1, from: b, kinds: regs, mem: mem)
                appendDebugPrintRegisterAligned4(D.d0, prepend: "OSDiv a", builder: mem)
                appendDebugPrintRegisterAligned4(D.d1, prepend: "OSDiv b", builder: mem)
                mem.append(M1Op.fdiv(D.d0, D.d0, D.d1))
                appendPrepareDoubleForStore(reg: D.d0, to: dst, kinds: regs, mem: mem)
                appendDebugPrintRegisterAligned4(D.d0, prepend: "OSDiv result", builder: mem)
                appendStore(reg: D.d0, into: dst, kinds: regs, mem: mem)
            case .OSDiv(let dst, let a, let b):
                assertFP(reg: dst, from: regs)
                
                // load ?0 and convert to d0
                appendLoadNumeric(reg: 0, from: a, kinds: regs, mem: mem)
                if isInteger(vreg: a, kinds: regs) {
                    appendScvtf(reg: X.x0, to: D.d0, target: a, kinds: regs, mem: mem)
                }
                appendFPRegToDouble(reg: D.d0, from: a, kinds: regs, mem: mem)
                
                // load ?1 and convert to d1
                appendLoadNumeric(reg: 1, from: b, kinds: regs, mem: mem)
                if isInteger(vreg: b, kinds: regs) {
                    appendScvtf(reg: X.x1, to: D.d1, target: b, kinds: regs, mem: mem)
                }
                appendFPRegToDouble(reg: D.d1, from: b, kinds: regs, mem: mem)
                
                mem.append(M1Op.fdiv(D.d0, D.d0, D.d1))
                
                appendPrepareDoubleForStore(reg: D.d0, to: dst, kinds: regs, mem: mem)
                appendStore(reg: D.d0, into: dst, kinds: regs, mem: mem)
            case .OToSFloat(let dst, let src):
                fallthrough
            case .OToInt(let dst, let src):
                let dstKind = requireTypeKind(reg: dst, from: regs)
                let srcKind = requireTypeKind(reg: src, from: regs)

                appendLoad(reg: X.x0, from: src, kinds: regs, mem: mem)

                switch(srcKind, dstKind) {
                case (let a, let b) where a == b:
                    appendStore(reg: X.x0, into: dst, kinds: regs, mem: mem)
                case (.u8, .i64), (.u16, .i64), (.i32, .i64):
                    appendStore(reg: X.x0, into: dst, kinds: regs, mem: mem)
                case (.u8, .i32), (.u16, .i32), (.i64, .i32):
                    appendDebugPrintAligned4("TODO: .ToInt i64->i32: investigate if size check needed", builder: mem)
                    appendStore(reg: X.x0, into: dst, kinds: regs, mem: mem)
                case (.f64, .i32):
                    mem.append(M1Op.fcvtzs(W.w0, D.d0))
                    appendStore(reg: X.x0, into: dst, kinds: regs, mem: mem)
                case (.i32, .f64):
                    appendDebugPrintRegisterAligned4(X.x0, prepend: "OToSFloat", builder: mem)
                    mem.append(M1Op.scvtf(D.d0, W.w0))
                    appendStore(reg: D.d0, into: dst, kinds: regs, mem: mem)
                default:
                    fatalError("Don't know how to cast \(srcKind) to \(dstKind)")
                }
            case .OToDyn(let dst, let src):
                let addr = unsafeBitCast(OToDyn_impl, to: UnsafeRawPointer.self)
                let dstType = requireType(reg: dst, regs: regs)
                let srcType = requireType(reg: src, regs: regs)
                mem.append(
                    PseudoOp.mov(X.x10, addr),
                    PseudoOp.mov(X.x0, dstType.ccompatAddress),
                    PseudoOp.mov(X.x1, srcType.ccompatAddress)
                )
                appendLoad(reg: .x2, from: dst, kinds: regs, mem: mem)
                appendLoad(reg: .x3, from: src, kinds: regs, mem: mem)
                mem.append(M1Op.blr(.x10))
                appendStore(reg: .x0, into: dst, kinds: regs, mem: mem)
            case .OOr(let dst, let a, let b):
                appendLoad(reg: X.x0, from: a, kinds: regs, mem: mem)
                appendLoad(reg: X.x1, from: b, kinds: regs, mem: mem)
                mem.append(M1Op.orr(X.x0, X.x0, X.x1, nil))
                appendStore(reg: X.x0, into: dst, kinds: regs, mem: mem)
            case .OSwitch(let reg, let offsets, _ /*end*/):
                
                appendLoad(reg: X.x0, from: reg, kinds: regs, mem: mem)
                
                for (expectedValue, jmpOffset) in offsets.enumerated() {
                    appendDebugPrintAligned4("Comparing reg \(reg) against \(expectedValue)", builder: mem)
                    mem.append(
                        PseudoOp.mov(X.x1, expectedValue),
                        M1Op.cmp(X.x0, X.x1)
                    )
                    
                    // calculate what to skip
                    let wordsToSkip = Int(jmpOffset) + 1
                    let targetInstructionIx = currentInstruction + wordsToSkip
                    
                    let jumpOffset_partA = try Immediate21(mem.byteSize)
                    let jumpOffset_partB = addrBetweenOps[targetInstructionIx]
                    let jumpOffset = try DeferredImmediateSum(
                        jumpOffset_partB,
                        jumpOffset_partA,
                        -1,
                        -Int(0))
                    //
                    
                    mem.append(
                        PseudoOp.deferred(4) {
                            M1Op.b_eq(try Immediate21(jumpOffset.immediate))
                        }
                    )
                    
                    appendDebugPrintAligned4("Didn't jump from case \(expectedValue)", builder: mem)
                }
            case .OGetTID(let dst, let src):
                let srcType = requireType(reg: src, regs: regs)
                Swift.assert(srcType.kind == .type)
                
                /* X.x0 <- hl_type*
                 The first 32bits of that address are the kind
                 */
                appendLoad(reg: X.x0, from: src, kinds: regs, mem: mem)
                // M1Op.ldr(reg, .reg64offset(.sp, offset, nil))
                mem.append(M1Op.ldr(W.w0, .reg(X.x0, .imm(0, nil))))
                appendStore(reg: X.x0, into: dst, kinds: regs, mem: mem)
            case .OGetType(let dst, let src):
                let srcType = requireType(reg: src, regs: regs)
                switch(srcType.kind) {
                case .dyn:
                    /* X.x0 <- first 8 bytes (which are hl_type address)
                     */
                    appendLoad(reg: X.x0, from: src, kinds: regs, mem: mem)
                    mem.append(
                        M1Op.ldr(X.x0, .reg64offset(X.x0, 0, nil))
                    )
                default:
                    fatal("OGetType not supported for src \(srcType.kind)", Self.logger)
                }
                appendDebugPrintAligned4("INTTYPE test", builder: mem)
                appendDebugPrintRegisterAligned4(X.x0, builder: mem)
                appendStore(reg: X.x0, into: dst, kinds: regs, mem: mem)
            case .ORef(let dst, let src):
                let dstType = requireType(reg: dst, regs: regs)
                let srcType = requireType(reg: src, regs: regs)
                Swift.assert(dstType.kind == .ref)
                Swift.assert(srcType.kind == dstType.tparamProvider?.kind)
                
                let srcOffset = getRegStackOffset(regs, src)
                
                mem.append(
                    // creates a pointer to the stack value and puts the addr in x0
                    // x0 = [sp + srcOffset]
                    M1Op.movr64(X.x0, .sp),
                    M1Op.movz64(X.x1, UInt16(srcOffset), nil),
                    M1Op.add(X.x0, X.x0, .r64shift(X.x1, .lsl(0)))
                )
                appendStore(reg: X.x0, into: dst, kinds: regs, mem: mem)
            case .OUnref(let dst, let src):
                let dstType = requireType(reg: dst, regs: regs)
                let srcType = requireType(reg: src, regs: regs)
                Swift.assert(srcType.kind == .ref)
                Swift.assert(dstType.kind == srcType.tparamProvider?.kind)
                
                appendLoad(reg: X.x0, from: src, kinds: regs, mem: mem)
                
                switch(dstType.hlRegSize) {
                case 8:
                    mem.append(
                        M1Op.ldr(X.x1, .reg64offset(X.x0, 0, nil))
                    )
                case 4:
                    mem.append(
                        M1Op.ldr(W.w1, .reg64offset(X.x0, 0, nil))
                    )
                case 2:
                    mem.append(
                        M1Op.ldrh(W.w1, .imm64(X.x0, 0, nil))
                    )
                case 1:
                    mem.append(
                        M1Op.ldrb(W.w1, .imm64(X.x0, 0, nil))
                    )
                default:
                    fatalError("Invalid size \(dstType.hlRegSize)")
                }
                appendStore(reg: X.x1, into: dst, kinds: regs, mem: mem)
            case .OSetref(let dst, let src):
                let dstType = requireType(reg: dst, regs: regs)
                let srcType = requireType(reg: src, regs: regs)
                Swift.assert(dstType.kind == .ref)
                Swift.assert(srcType.kind == dstType.tparamProvider?.kind)
                // x0 = address of ref
                appendLoad(reg: X.x0, from: dst, kinds: regs, mem: mem)
                appendLoad(reg: X.x1, from: src, kinds: regs, mem: mem)
                appendStore(reg: X.x1, as: src, intoAddressFrom: X.x0, offsetFromAddress: 0, kinds: regs, mem: mem)
                break
            case .OMakeEnum(let dst, let construct, let args):
                assert(reg: dst, from: regs, is: HLTypeKind.enum)
                let type = requireType(reg: dst, regs: regs)
                guard let _ = type.tenumProvider else {
                    fatalError("Enum provider must be set")
                }
                
                // these need to be deallocated in the callee
                let argBuff: UnsafeMutableBufferPointer<Reg> = .allocate(capacity: args.count)
                _ = argBuff.initialize(from: args)
                
                let argValBuff: UnsafeMutableBufferPointer<Int64> = .allocate(capacity: args.count)
                
                for (ix, arg) in args.enumerated() {
                    appendLoad(reg: X.x0, from: arg, kinds: regs, mem: mem)
                    mem.append(
                        PseudoOp.mov(X.x1, UnsafeRawPointer(argValBuff.baseAddress!.advanced(by: ix))),
                        M1Op.str(X.x0, .reg64offset(X.x1, 0, nil))
                    )
                }
                
                let _implTarget = unsafeBitCast(OMakeEnum_impl, to: UnsafeRawPointer.self)
                
                mem.append(
                    PseudoOp.mov(X.x0, type.ccompatAddress),
                    PseudoOp.mov(X.x1, construct),
                    M1Op.movz64(X.x2, UInt16(args.count), nil),
                    PseudoOp.mov(X.x3, UnsafeRawPointer(argBuff.baseAddress!)),
                    PseudoOp.mov(X.x4, UnsafeRawPointer(argValBuff.baseAddress!)),
                    
                    PseudoOp.mov(.x19, _implTarget),
                    M1Op.blr(.x19)
                )
                appendStore(reg: X.x0, into: dst, kinds: regs, mem: mem)
            case .OEnumIndex(let dst, let value):
                let _implTarget = unsafeBitCast(OEnumIndex_impl, to: UnsafeRawPointer.self)
                
                appendLoad(reg: X.x0, from: value, kinds: regs, mem: mem)
                mem.append(
                    PseudoOp.mov(.x19, _implTarget),
                    M1Op.blr(.x19)
                )
                assert(reg: dst, from: regs, is: HLTypeKind.i32)
                appendStore(reg: X.x0, into: dst, kinds: regs, mem: mem)
            case .OEnumField(let dst, let value, let construct, let field):
                let _implTarget = unsafeBitCast(OEnumField_impl, to: UnsafeRawPointer.self)
                
                // TODO: can enums have other types of assoc data? Need a test
                assert(reg: dst, from: regs, is: HLTypeKind.i32)
                
                appendLoad(reg: X.x0, from: value, kinds: regs, mem: mem)
                
                mem.append(
                    M1Op.movz64(X.x1, UInt16(construct), nil),
                    M1Op.movz64(X.x2, UInt16(field), nil),
                    
                    PseudoOp.mov(.x19, _implTarget),
                    M1Op.blr(.x19),
                    M1Op.ldr(X.x0, .reg64offset(X.x0, 0, nil))
                )
//                assert(reg: dst, from: regs, is: HLTypeKind.i32)
                appendStore(reg: X.x0, into: dst, kinds: regs, mem: mem)
            case .OEnumAlloc(let dst, let construct):
                assert(reg: dst, from: regs, is: HLTypeKind.enum)
                let type = requireType(reg: dst, regs: regs)
                guard let _ = type.tenumProvider else {
                    fatalError("Enum provider must be set")
                }
                
                let _implTarget = unsafeBitCast(OEnumAlloc_impl, to: UnsafeRawPointer.self)
                
                mem.append(
                    PseudoOp.mov(X.x0, type.ccompatAddress),
                    PseudoOp.mov(X.x1, construct),
                    PseudoOp.mov(.x19, _implTarget),
                    M1Op.blr(.x19)
                )
                appendStore(reg: X.x0, into: dst, kinds: regs, mem: mem)
            case .OSetEnumField(let value, let field, let src):
                assert(reg: src, from: regs, is: HLTypeKind.i32)
                
                let _implTarget = unsafeBitCast(OSetEnumField_impl, to: UnsafeRawPointer.self)
                
                appendLoad(reg: X.x2, from: src, kinds: regs, mem: mem)
                appendLoad(reg: X.x0, from: value, kinds: regs, mem: mem)
                mem.append(
                    PseudoOp.mov(X.x1, field),
                    PseudoOp.mov(.x19, _implTarget),
                    M1Op.blr(.x19)
                )
            case .OInstanceClosure(let dst, let fun, let obj):
                
                guard let funIndex = ctx.mainContext.pointee.m?.pointee.functions_indexes.advanced(by: fun).pointee else {
                    fatalError("No real fun index")
                }
                guard let funType = ctx.mainContext.pointee.code?.pointee.functions.advanced(by: Int(funIndex)).pointee.typePtr else {
                    fatalError("No fun type")
                }
                
                Swift.assert(funType.kind == .fun)
                
                let callTarget = try ctx.requireCallable(findex: fun)
                ctx.funcTracker.referenced2(callTarget)
                
                
                let allocClosure_jumpTarget = unsafeBitCast(LibHl._hl_alloc_closure_ptr, to: UnsafeRawPointer.self)
                
                
                appendLoad(reg: X.x2, from: obj, kinds: regs, mem: mem)
                mem.append(
                    PseudoOp.mov(X.x0, funType.ccompatAddress),
                    PseudoOp.mov(X.x1, callTarget.address),
                    
                    PseudoOp.mov(X.x3, allocClosure_jumpTarget),
                    M1Op.blr(X.x3)
                )
                appendStore(reg: X.x0, into: dst, kinds: regs, mem: mem)
                appendDebugPrintRegisterAligned4(X.x0, builder: mem)
            case .OStaticClosure(let dst, let fun):
                let mutatingMod = UnsafeMutablePointer(mutating: ctx.mainContext.pointee.m!)
                let callTarget = try ctx.requireCallable(findex: fun)
                ctx.funcTracker.referenced2(callTarget)
                
                withUnsafeMutablePointer(to: &mutatingMod.pointee.ctx.alloc) {
                    allocPtr in
                    
                    let c: UnsafeMutablePointer<vclosure> = .init(LibHl.hl_malloc(allocPtr, Int32(MemoryLayout<vclosure>.size)))
                    
                    let fidx = mutatingMod.pointee.functions_indexes.advanced(by: fun).pointee
                    
                    let nfuncs = mutatingMod.pointee.code.pointee.nfunctions
                    
                    /* The call target address is not available at compilation time, but will be available at link time.
                     
                     So instead of assigning c.pointee.fun directly (can't - no address), we append instructions to load the address
                     at runtime (at which point it will be available).
                     
                     This is equivalent to: `c.pointee.fun = .init(callTarget.address.value)` except via assembly.
                     */
                    mem.append(PseudoOp.mov(X.x0, callTarget.address))
                    mem.append(PseudoOp.mov(X.x1, OpaquePointer(c)))
                    mem.append(M1Op.str(X.x0, .reg64offset(X.x1, 8 /* offset to `fun` in vclosure */, nil)))
                    
                    c.pointee.value = nil
                    
                    if (fidx >= nfuncs) {
                        // native
                        c.pointee.t = mutatingMod.pointee.code.pointee.natives.advanced(by: Int(UInt32(fidx) - nfuncs)).pointee.typePtr
                    } else {
                        let funcPtr = mutatingMod.pointee.code.pointee.functions.advanced(by: Int(fidx))
                        c.pointee.t = funcPtr.pointee.typePtr!
                    }
                    
                    c.pointee.hasValue = 0
                    
                    mem.append(PseudoOp.mov(X.x0, OpaquePointer(c)))
                    appendStore(reg: X.x0, into: dst, kinds: regs, mem: mem)
                }
            case .OToVirtual(let dst, let src):
                let _ensureInitialized: (@convention(c)(OpaquePointer)->()) = {
                    oDynPtr in
                    
                    let dynPtr: UnsafePointer<vdynamic> = .init(oDynPtr)
                    if dynPtr.pointee.t.kind == .obj {
                        // ensure initialized
                        _ = dynPtr.pointee.t.pointee.obj.pointee.getRt(dynPtr.pointee.t)
                    }
                }
                let dstType = requireTypeMemory(reg: dst, regs: regs)
                
                appendLoad(reg: X.x0, from: src, kinds: regs, mem: mem)
                // ensure src is initialized (if obj)
                mem.append(
                    PseudoOp.mov(X.x1, unsafeBitCast(_ensureInitialized, to: OpaquePointer.self)),
                    M1Op.blr(X.x1)
                )
                // hl_to_virtual
                mem.append(PseudoOp.mov(X.x0, dstType))
                appendLoad(reg: X.x1, from: src, kinds: regs, mem: mem)
                mem.append(
                    PseudoOp.mov(X.x2, unsafeBitCast(LibHl._hl_to_virtual, to: OpaquePointer.self)),
                    M1Op.blr(X.x2)
                )
                appendStore(reg: X.x0, into: dst, kinds: regs, mem: mem)
            case .OCallMethod(let dst, let obj, let field, let args):
                let objType = requireType(reg: obj, regs: regs)
                switch(objType.kind) {
                case .obj:
                    let _getProtoFindex: (@convention(c)(OpaquePointer, Int32)->Int32) = {
                        (objPtr, protoIx) in
                        
                        let vd: UnsafePointer<vdynamic> = .init(objPtr)
                        let protoPtr = vd.pointee.t.pointee.obj.pointee.protoPtr?.advanced(by: Int(protoIx))
                        
                        guard let protoFix = protoPtr?.pointee.findex else {
                            fatalError("OCallMethod failed. Could not find proto findex")
                        }
                        
                        // TODO: remove
                        if protoFix == 0 {
                            fatalError(":(")
                        }
                        //
                        
                        return protoFix
                    }
                    let _getType: (@convention(c)(Int32, OpaquePointer)->OpaquePointer) = {
                        (findex, mPtr) in
                        
                        let mod: UnsafePointer<HLModule_CCompat> = .init(mPtr)
                        
                        let funIndex = mod.pointee.functions_indexes.advanced(by: Int(findex)).pointee
                        let fun = mod.pointee.code.pointee.functions.advanced(by: Int(funIndex))
                        guard let typePtr = fun.pointee.typePtr else {
                            fatalError("OCallMethod encountered a proto without a type")
                        }
                        
                        guard typePtr.pointee.kind == .fun else {
                            fatalError("OCallMethod fetched a proto type that is not .fun")
                        }
                        
                        return .init(typePtr)
                    }
                    let _getCallAddress: (@convention(c)(Int32, OpaquePointer)->OpaquePointer) = {
                        (findex, mPtr) in
                        
                        let mod: UnsafePointer<HLModule_CCompat> = .init(mPtr)
                        
                        guard let funAddr = mod.pointee.functions_ptrs.advanced(by: Int(findex)).pointee else {
                            /* NOTE: If this happens in a test, you might need to specify depHints
                               properly */
                            fatalError("OCallMethod encountered a missing function address (findex: \(findex))")
                        }
                        return .init(funAddr)
                    }
                    guard let m = ctx.mainContext.pointee.m else {
                        fatalError("OCallMethod can't access the module (for function addresses)")
                    }
                    
                    // Fetch proto function index
                    appendLoad(reg: X.x0, from: obj, kinds: regs, mem: mem)
                    mem.append(PseudoOp.mov(X.x1, field))
                    mem.append(
                        PseudoOp.mov(X.x2, unsafeBitCast(_getProtoFindex, to: OpaquePointer.self)),
                        M1Op.blr(X.x2),
                        
                        M1Op.movr64(X.x7, X.x0) // proto findex in x7
                    )
                    
                    // Fetch proto function type
//                    mem.append(M1Op.movr64(X.x0, X.x7))
//                    mem.append(PseudoOp.mov(X.x1, OpaquePointer(m)))
//                    mem.append(
//                        PseudoOp.mov(X.x2, unsafeBitCast(_getType, to: OpaquePointer.self)),
//                        M1Op.blr(X.x2),
//
//                        M1Op.movr64(X.x8, X.x0) // proto function type in x8
//                    )
                    
                    // Fetch proto function address
                    mem.append(M1Op.movr64(X.x0, X.x7))
                    mem.append(PseudoOp.mov(X.x1, OpaquePointer(m)))
                    mem.append(
                        PseudoOp.mov(X.x2, unsafeBitCast(_getCallAddress, to: OpaquePointer.self)),
                        M1Op.blr(X.x2),
                        
                        M1Op.movr64(X.x9, X.x0) // proto function address in x9
                    )

                    try __ocallmethod_impl__addrInX9(
                        dst: dst,
                        regs: regs,
                        preArgs: [
                            ({ (inmem, regForPreArg) in
                                self.appendLoad(reg: regForPreArg, from: obj, kinds: regs, mem: inmem)
                            }, HLTypeKind.obj)
                        ],
                        args: args,
                        reservedStackBytes: stackInfo.total,
                        mem: mem)
                case .virtual:
                    fatalError("Not implemented")
                default:
                    fatalError("Invalid target for OCallMethod")
                }
            case .ODynGet(let dst, let obj, let field):
                let dstType = requireTypeKind(reg: dst, from: regs)
                let dyngetFunc = get_dynget(to: dstType.kind)
                
                // load obj into x0
                appendLoad(reg: X.x0, from: obj, kinds: regs, mem: mem)
                
                // load field name hash into x1
                // note: different from ODynSet
                let fieldName = try ctx.getString(field)
                let fNameHash = LibHl.hl_hash_utf8(fieldName.ccompatCCharAddress)
                mem.append(
                    PseudoOp.mov(X.x1, fNameHash)
                )
                
                // load type into x2 (for non-f32 and non-f64 arguments)
                if (dstType != .f32 && dstType != .f64) {
                    mem.append(PseudoOp.mov(X.x2, requireTypeMemory(reg: dst, regs: regs)))
                }
                
                appendDebugPrintAligned4("Jumping to dynget function", builder: mem)
                mem.append(
                    PseudoOp.mov(X.x15, dyngetFunc),
                    M1Op.blr(X.x15)
                )
                // TODO: check for failed cast result
                appendDebugPrintAligned4("TODO: ODynGet should check for failed cast result", builder: mem)
                
                if (dstType != .f32 && dstType != .f64) {
                    appendDebugPrintRegisterAligned4(X.x0, prepend: "ODynGet result (non float)", builder: mem)
                    appendStore(reg: X.x0, into: dst, kinds: regs, mem: mem)
                } else {
                    // TODO: test coverage
                    appendDebugPrintAligned4("TODO: ODynGet test for returning floats", builder: mem)
                    
                    appendFPRegToDouble(reg: D.d0, from: dst, kinds: regs, mem: mem)
                    appendDebugPrintRegisterAligned4(D.d0, prepend: "ODynGet result (float)", builder: mem)
                    appendStore(reg: D.d0, into: dst, kinds: regs, mem: mem)
                }
            case .ODynSet(let obj, let field, let src):
                let srcType = requireTypeKind(reg: src, from: regs)
                let dynsetFunc = get_dynset(from: srcType.kind)
                
                // load obj into x0
                appendLoad(reg: X.x0, from: obj, kinds: regs, mem: mem)
                
                // load field name hash into x1
                //
                // NOTE: different impl from ODynGet (same result though)
                // keeping it for compatibility with hashlink jit.c just in
                // case there are sideeffects
                guard let hlcode = ctx.mainContext.pointee.code else {
                    fatalError("No hl_code* available")
                }
                
                let nameUstr = LibHl._hl_get_ustring(.init(hlcode), Int32(field))
                let nameHash = LibHl.hl_hash_gen(.init(nameUstr), true)
                
                mem.append(
                    PseudoOp.mov(X.x1, nameHash)
                )
                
                // order of following args is diff for (non-f32, non-f64) and (other) arguments
                // b/c the f* arguments don't need the type
                if (srcType != .f32 && srcType != .f64) {
                    // load type into x2
                    mem.append(PseudoOp.mov(X.x2, requireTypeMemory(reg: src, regs: regs)))
                    
                    // load value into x3
                    appendLoad(reg: X.x3, from: src, kinds: regs, mem: mem)
                    
                    appendDebugPrintRegisterAligned4(X.x3, prepend: "ODynSet value (gp)", builder: mem)
                } else {
                    // load value into d0 (for f32 and f64 arguments)
                    // as it's the first floating point arg)
                    appendLoad(reg: D.d0, from: src, kinds: regs, mem: mem)
                    appendFPRegToDouble(reg: D.d0, from: src, kinds: regs, mem: mem)
                    appendDebugPrintRegisterAligned4(D.d0, prepend: "ODynSet value (float)", builder: mem)
                }
                
                appendDebugPrintAligned4("Jumping to dynset function", builder: mem)
                mem.append(
                    PseudoOp.mov(X.x15, dynsetFunc),
                    M1Op.blr(X.x15)
                )
                // TODO: check for failed cast result
                appendDebugPrintAligned4("TODO: ODynSet should check for failed cast result", builder: mem)
            default:
                fatalError("Can't compile \(op.debugDescription)")
            }
        }

        // initialize targets for 'ret' jumps
        for var retTarget in retTargets {
            retTarget.stop(at: mem.byteSize)
        }

        if stackInfo.total > 0 {
            appendDebugPrintAligned4("Free \(stackInfo.total) bytes (pre-epilogue)", builder: mem)
            mem.append(
                (try M1Op._add(X.sp, X.sp, stackInfo.total))
            )
        }
        else {
            appendDebugPrintAligned4("Skipping freeing stack because \(stackInfo.total) bytes were needed", builder: mem)
        }

        appendEpilogue(builder: mem)
        appendDebugPrintAligned4("Returning", builder: mem)
        mem.append(M1Op.ret)
        appendDebugPrintAligned4("Returned", builder: mem)
    }
}
