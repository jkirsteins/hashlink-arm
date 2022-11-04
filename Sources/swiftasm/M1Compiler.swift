import Darwin

typealias HLTypeKinds = [HLTypeKind]

protocol CompilerUtilities {
    func appendDebugPrintAligned4(_ val: String, builder: OpBuilder);
}

extension M1Compiler : CompilerUtilities {
    
}

// x0 through x7
private let ARG_REGISTER_COUNT = 8
extension M1Compiler {
    /* SP movements must be aligned to 16-bytes */
    func roundUpStackReservation(
        _ val: Int16
    ) -> Int16 {
        guard val % 16 != 0 else { return val }
        return (val + (16 &- 1)) & (0 &- 16)
    }
    
    /// Stack space should be allocated for first 8 function args (will be filled from registers) and
    /// any register, which is not part of args.
    /// - Parameters:
    ///   - regs:
    ///   - args:
    /// - Returns:
    func calcStackArgReq(regs unfilteredRegs: HLTypeKinds, args unfilteredArgs: HLTypeKinds)
    -> (Int16, HLTypeKinds)
    {
        let regs = unfilteredRegs.filter { $0 != .void }
        let args = unfilteredArgs.filter { $0 != .void }
        guard regs.prefix(args.count) == args.prefix(args.count) else {
            fatalError(
                "Args must match the first registers (got \(args) and \(regs) respectively)"
            )
        }
        let stackArgs = Array( /* regs should be aligned with args */
            // IMPORTANT: args must come first
            args.prefix(ARG_REGISTER_COUNT) + regs.dropFirst(args.count)
        )
        
        let result = stackArgs.reduce(0) { $0 + Int16($1.hlRegSize) }
        return (roundUpStackReservation(result), stackArgs)
    }
    
    func appendLoad(reg: Register64, from vreg: Reg, kinds: HLTypeKinds, mem: OpBuilder) {
        let offset = getRegStackOffset(kinds, vreg)
        appendLoad(reg: reg, from: vreg, kinds: kinds, offset: offset, mem: mem)
    }
    
    func appendSignMode(_ signed: Bool, reg: Register64, from vreg: Reg, kinds: HLTypeKinds, mem: OpBuilder) {
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
    
    func appendLoad(reg: Register64, from vreg: Reg, kinds: HLTypeKinds, offset: ByteCount, mem: OpBuilder) {
        let vregKind = requireTypeKind(reg: vreg, from: kinds)
        print("Loading \(reg) from vreg \(vreg) at offset \(offset) size \(vregKind.hlRegSize)")
        if vregKind.hlRegSize == 8 {
            mem.append(
                M1Op.ldr(reg, .reg64offset(.sp, offset, nil))
            )
        } else if vregKind.hlRegSize == 4 {
            mem.append(
                M1Op.ldr(reg.to32, .reg64offset(.sp, offset, nil))
            )
        } else if vregKind.hlRegSize == 2 {
            mem.append(
                M1Op.ldrh(reg.to32, .imm64(.sp, offset, nil))
            )
        } else if vregKind.hlRegSize == 1 {
            mem.append(
                M1Op.ldrb(reg.to32, .imm64(.sp, offset, nil))
            )
        } else if vregKind.hlRegSize == 0 {
            // nop
        } else {
            fatalError("Size must be 8, 4, 2, 1, or 0")
        }
    }
    
    func appendStore(reg: Register64, into vreg: Reg, kinds: HLTypeKinds, mem: OpBuilder) {
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
    
    /*
     First 7 args are in registers [x0;x7]. Others are on the stack.
     Stack should be extended to account for data which is in registers.
     Subsequently data should be moved from regs to stack, to enable use of all registers.
     */
    @discardableResult func appendStackInit(
        _ unfilteredRegs: HLTypeKinds,
        args unfilteredArgs: HLTypeKinds,
        builder: OpBuilder,
        prologueSize: ByteCount
    ) throws -> Int16 {
        // test mismatched before filtering
        let unfilteredArgRegCount = min(unfilteredArgs.count, ARG_REGISTER_COUNT)
        guard
            unfilteredRegs.prefix(unfilteredArgRegCount)
                == unfilteredArgs.prefix(unfilteredArgRegCount)
        else {
            throw GlobalError.invalidOperation(
                "Up to first \(ARG_REGISTER_COUNT) registers must be the same for a function and its type.args"
            )
        }
        
        // move ALL args in continuous space (either from regs, or on sp)
        let vregStackSize_unr = unfilteredRegs.reduce(Int16(0)) { $0 + Int16($1.hlRegSize) }
        let vregStackSize = roundUpStackReservation(vregStackSize_unr)
        
        guard vregStackSize > 0 else {
            builder.append(PseudoOp.debugMarker("No extra stack space needed"))
            return vregStackSize
        }
        
        builder.append(
            PseudoOp.debugMarker("Reserving \(vregStackSize) bytes for entire stack"),
            M1Op.subImm12(X.sp, X.sp, try .i(vregStackSize))
        )
        
        var offset: ByteCount = 0
        var overflowOffset: ByteCount = Int64(vregStackSize) + prologueSize // for regs passed in via stack
        
        for (ix, reg) in unfilteredRegs.filter({ $0.hlRegSize > 0 }).enumerated() {

            Swift.assert(reg.hlRegSize > 0, "empty registers have to be filtered out earlier to not affect register index")

//            builder.append(
//                PseudoOp.debugPrint(
//                    self, "Stack init. Moving \(Register64(rawValue: UInt8(ix))!) to \(offset) (size \(reg.hlRegSize)"
//                )
//            )
            // tmp

//
            let needLoad = ix >= ARG_REGISTER_COUNT
            let regToUse: Register64 = needLoad ? .x1 : Register64(rawValue: UInt8(ix))!
            switch (needLoad, reg.hlRegSize) {
            case (false, _):
                break
            case (true, let regSize):
                builder.append(PseudoOp.ldrVreg(regToUse, overflowOffset, regSize))
                overflowOffset += regSize
            }

            builder.append(PseudoOp.strVreg(regToUse, offset, reg.hlRegSize))

            print("Inc offset by \(reg.hlRegSize) from \(reg)")
            offset += reg.hlRegSize
        }
        
//        builder.append(PseudoOp.mov(X.x0, 2), M1Op.strb(W.w0, .imm64(.sp, 0, nil)))
//        builder.append(PseudoOp.mov(X.x0, 6), M1Op.strh(W.w0, .imm64(.sp, 1, nil)))
//        builder.append(PseudoOp.mov(X.x0, 8), M1Op.str(W.w0, .reg64offset(.sp, 3, nil)))
//
//        builder.append(PseudoOp.ldrVreg(X.x0, 0, 1))
//        appendDebugPrintRegisterAligned4(X.x0, builder: builder)
//        builder.append(PseudoOp.ldrVreg(X.x1, 1, 2))
//        appendDebugPrintRegisterAligned4(X.x1, builder: builder)
//        builder.append(PseudoOp.ldrVreg(X.x2, 3, 4))
//        appendDebugPrintRegisterAligned4(X.x2, builder: builder)
        
        
        return vregStackSize
    }
    
    func appendDebugPrintRegisterAligned4(_ reg: Register64, builder: OpBuilder) {
        guard let printfAddr = dlsym(dlopen(nil, RTLD_LAZY), "printf") else {
            fatalError("No printf addr")
        }
        
        var adr = RelativeDeferredOffset()
        var jmpTarget = RelativeDeferredOffset()
        let str = "[jitdebug] Register \(reg): %llu\n\0"
        
        guard stripDebugMessages == false else {
            builder.append(PseudoOp.debugMarker("(debug message printing stripped)"))
            return
        }
        builder.append(PseudoOp.debugMarker("Printing debug register: \(reg)"))
        
        guard reg.rawValue <= X.x18.rawValue else {
            fatalError("reg \(reg) not supported")
        }
        
        builder.append(
            // Stash registers we'll use (so we can reset)
            .subImm12(X.sp, X.sp, Imm12Lsl12(160)),
            .str(Register64.x0, .reg64offset(.sp, 8, nil)),
            .str(reg, .reg64offset(.sp, 0, nil)),
            .stp((Register64.x1, Register64.x2), .reg64offset(.sp, 16, nil)),
            .stp((Register64.x3, Register64.x4), .reg64offset(.sp, 32, nil)),
            .stp((Register64.x5, Register64.x6), .reg64offset(.sp, 48, nil)),
            .stp((Register64.x7, Register64.x8), .reg64offset(.sp, 64, nil)),
            .stp((Register64.x9, Register64.x10), .reg64offset(.sp, 80, nil)),
            .stp((Register64.x11, Register64.x12), .reg64offset(.sp, 96, nil)),
            .stp((Register64.x13, Register64.x14), .reg64offset(.sp, 112, nil)),
            .stp((Register64.x15, Register64.x16), .reg64offset(.sp, 128, nil)),
            .stp((Register64.x17, Register64.x18), .reg64offset(.sp, 144, nil))
        )
        adr.start(at: builder.byteSize)
        builder.append(.adr64(.x0, adr))
        
        builder.append(
            PseudoOp.mov(.x16, printfAddr),
            M1Op.blr(.x16),
            // restore
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
            try! M1Op._add(X.sp, X.sp, 160)
        )
        
        jmpTarget.start(at: builder.byteSize)
        builder.append(.b(jmpTarget))
        adr.stop(at: builder.byteSize)
        builder.append(ascii: str).align(4)
        
        Swift.assert(builder.byteSize % 4 == 0)
        jmpTarget.stop(at: builder.byteSize)
    }
    
    func appendDebugPrintAligned4(_ val: String, builder: OpBuilder) {
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
            .str(Register64.x0, .reg64offset(.sp, -32, .pre)),
            .str(Register64.x1, .reg64offset(.sp, 8, nil)),
            .str(Register64.x2, .reg64offset(.sp, 16, nil)),
            .str(Register64.x16, .reg64offset(.sp, 24, nil)),
            
            // unix write system call
            .movz64(.x0, 1, nil)
        )
        adr.start(at: builder.byteSize)
        builder.append(
            .adr64(.x1, adr),
            .movz64(.x2, UInt16(str.count), nil),
            .movz64(.x16, 4, nil),
            .svc(0x80),
            // restore
            .ldr(Register64.x16, .reg64offset(.sp, 24, nil)),
            .ldr(Register64.x2, .reg64offset(.sp, 16, nil)),
            .ldr(Register64.x1, .reg64offset(.sp, 8, nil)),
            .ldr(Register64.x0, .reg64offset(.sp, 32, .post))
        )
        jmpTarget.start(at: builder.byteSize)
        builder.append(.b(jmpTarget))
        adr.stop(at: builder.byteSize)
        builder.append(ascii: str).align(4)
        
        jmpTarget.stop(at: builder.byteSize)
    }
    
    func appendSystemExit(_ code: UInt8, builder: OpBuilder) {
        builder.append(
            .movz64(.x0, UInt16(code), nil),
            .movz64(.x16, 1, nil),
            .svc(0x80)
        )
    }
    
    /// Returns the amount of change for SP
    func appendPrologue(builder: OpBuilder) -> ByteCount {
        builder.append(
            PseudoOp.debugPrint(self, "Starting prologue and reserving 16"),
            M1Op.stp((.x29_fp, .x30_lr), .reg64offset(.sp, -16, .pre)),
            M1Op.movr64(.x29_fp, .sp)
        )
        return 16
    }
    
    func appendEpilogue(builder: OpBuilder) {
        builder.append(
            PseudoOp.debugPrint(self, "Starting epilogue"),
            M1Op.ldp((.x29_fp, .x30_lr), .reg64offset(.sp, 16, .post)),
            PseudoOp.debugPrint(self, "Finished epilogue")
        )
    }
}

typealias Registers = [Resolvable<HLType>]

let _throw: (@convention(c) (Int64) -> ()) = { (_ exc: Int64) in
    print("Throwing \(exc)")
}
let _trap: (@convention(c) (Int64, Int64) -> ()) = { (_ exc: Int64, _ offset: Int64) in
    print("Trapping \(exc) \(offset)")
}
let _endTrap: (@convention(c) (Int64) -> ()) = { (_ exc: Int64) in
    print("Ending trap \(exc)")
}
let _throwAddress = unsafeBitCast(_throw, to: UnsafeMutableRawPointer.self)
let _trapAddress = unsafeBitCast(_trap, to: UnsafeMutableRawPointer.self)
let _endTrapAddress = unsafeBitCast(_endTrap, to: UnsafeMutableRawPointer.self)

class M1Compiler {
    /*
     stp    x29, x30, [sp, #-16]!
     mov    x29, sp
     */let emitter = EmitterM1()
    
    let stripDebugMessages: Bool
    
    func assertEnoughRegisters(_ ix: Reg, regs: Registers) {
        guard ix < regs.count else {
            fatalError("Not enough registers. Expected \(ix) to be available. Got: \(regs)")
        }
    }
    
    func requireTypeMemory(reg: Reg, regs: Registers) -> UnsafePointer<HLType_CCompat> {
        assertEnoughRegisters(reg, regs: regs)
        
        guard let mem = regs[Int(reg)].memory else {
            fatalError("Register \(reg) has no type address available.")
        }
        
        return UnsafePointer(OpaquePointer(mem))
    }
    
    func requireFieldOffset(fieldRef: Int, objIx: Reg, regs: Registers) -> Int64 {
        let mem = requireTypeMemory(reg: objIx, regs: regs)
        
        switch(mem.pointee.kind) {
        case .obj:
            fallthrough
        case .struct:
            // if this is not set, the module has not
            // been initialized
            let rt = mem.pointee.obj.pointee.getRt(mem) 
            return Int64(rt.pointee.fields_indexes.advanced(by: fieldRef).pointee)
        default:
            fatalError("Can not get field offset for obj type \(mem.pointee.kind)")
        }
    }
    
    func requireTypeAddress(reg: Reg, from regs: [Resolvable<HLType>]) -> UnsafeRawPointer {
        guard reg < regs.count else {
            fatalError("requireTypeAddress(reg:from:): Not enough registers. Expected \(reg) to be available. Got: \(regs)")
        }
        guard let mem = regs[Int(reg)].memory else {
            fatalError("requireTypeAddress(reg:from:): Register \(reg) has no type address available.")
        }
        
        return mem
    }
    
    func requireTypeKind(reg: Reg, from resolvedRegs: HLTypeKinds) -> HLTypeKind {
        guard reg < resolvedRegs.count else {
            fatalError("requireType(reg:from:): Not enough registers. Expected \(reg) to be available. Got: \(resolvedRegs)")
        }
        
        return resolvedRegs[Int(reg)]
    }
    
    func requireTypeSizeLsl(reg: Reg, from resolvedRegs: HLTypeKinds) -> UInt8 {
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
    
    func requireTypeKind(reg: Reg, from resolvedRegs: HLTypeKinds, shouldMatch: HLTypeKind) -> HLTypeKind {
        let kind = requireTypeKind(reg: reg, from: resolvedRegs)
        guard kind == shouldMatch else {
            fatalError("Expected reg \(reg) to be \(shouldMatch) but was \(kind)")
        }
        return kind
    }
    
    func assertKind(_ op: HLOpCode, _ actual: HLTypeKind, _ expected: HLTypeKind) {
        guard expected == actual else {
            fatalError("\(op): type kind must be \(expected) but got \(actual)")
        }
    }
    
    func assert(reg: Reg, from: HLTypeKinds, matchesCallArg argReg: Reg, inFun callable: any Callable) {
        guard from.count > reg else {
            fatalError(
                "Register with index \(reg) does not exist. Available registers: \(from)."
            )
        }
        guard callable.args.count > argReg else {
            fatalError("Expected args to have index \(argReg) but got \(callable.args)")
        }
        let regKind = from[Int(reg)]
        let argKind = callable.args[Int(argReg)].value.kind
        guard argKind == .dyn || regKind == .dyn || regKind == argKind else {
            fatalError(
                "Register \(reg) kind \(regKind) expected to match arg \(argReg) but arg was \(argKind) "
            )
        }
    }
    
    func assert(reg: Reg, from: HLTypeKinds, matches type: HLType) {
        guard from.count > reg else {
            fatalError(
                "Register with index \(reg) does not exist. Available registers: \(from)."
            )
        }
        guard from[Int(reg)] == type.kind else {
            fatalError(
                "Register \(reg) expected to be \(type.kind) but is \(from[Int(reg)])"
            )
        }
    }
    
    func assert(reg: Reg, from: HLTypeKinds, is target: HLTypeKind) {
        guard from.count > reg else {
            fatalError(
                "Register with index \(reg) does not exist. Available registers: \(from)."
            )
        }
        guard target == .dyn || from[Int(reg)] == .dyn || target == from[Int(reg)] else {
            fatalError(
                "Register \(reg) expected to be \(target) but is \(from[Int(reg)])"
            )
        }
    }
    
    func assert(reg: Reg, from: HLTypeKinds, in targets: HLTypeKinds) {
        guard from.count > reg else {
            fatalError(
                "Register with index \(reg) does not exist. Available registers: \(from)."
            )
        }
        guard targets.contains(from[Int(reg)]) else {
            fatalError(
                "Register \(reg) expected to be one of \(targets) but is \(from[Int(reg)])"
            )
        }
    }
    
    init(stripDebugMessages: Bool = false) {
        self.stripDebugMessages = stripDebugMessages
    }
    
    func getRegStackOffset(_ regs: HLTypeKinds, _ ix: Reg) -> ByteCount {
        var result = ByteCount(0)
        for i in 0..<ix {
            result += regs[Int(i)].hlRegSize
        }
        printerr("Stack offset for \(ix) is \(result)")
        return result
    }
    
    func getFieldOffset(_ obj: HLType, _ field: Int) -> ByteCount {
        guard let objData = obj.objData else {
            fatalError("Field offset only supports .obj and .struct (got \(obj))")
        }
        var startOffset = ByteCount(8)  // hl_type* pointer at start of obj/struct
        for ix in 0..<field {
            let fieldToSkip = objData.fields[ix].value.type.value.kind.hlRegSize
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
    func compile(findex: Int32, into mem: OpBuilder) throws
    {
        let ctx = mem.ctx
        
        let entry = ctx.callTargets.get(Int(findex))
        
        guard case .compilable(let compilable) = entry else {
            fatalError("Call target fix==\(findex) is not compilable (got \(entry))")
        }
        
        guard !compilable.entrypoint.hasUsableValue else {
            throw GlobalError.invalidOperation(
                "Function \(findex) has already been compiled and assigned an address."
            )
        }
        
        try compile(compilable: compilable, into: mem)
    }
    
    func compile(compilable: Compilable, into mem: OpBuilder) throws
    {
        let ctx = mem.ctx
        defer {
            ctx.funcTracker.compiled(compilable.getFindex())
        }
        
        // grab it before it changes from prologue
        // let memory = mem.getDeferredPosition()
        let regs = compilable.regs
        let regKinds = regs.map { $0.value.kind }
        
        let findex = compilable.getFindex()
        
        // Memory might not be set in some tests that don't
        // properly initialize types :(
//        Swift.assert(
//            regs.map { $0.memory }.filter { $0 != nil }.count > 0,
//            "regs must have memory")
        
        let relativeBaseAddr = mem.getDeferredPosition()
        compilable.entrypoint.update(from: relativeBaseAddr)
        
        // if we need to return early, we jump to these
        var retTargets: [RelativeDeferredOffset] = []
        
        print("Compiling function \(findex) at deferred address \(compilable.entrypoint)")
        print("REGS: \n--" + regs.map { String(reflecting: $0) }.joined(separator: "\n--"))
        //        print("OPS: \n--" + funPtr.pointee.ops.map { String(reflecting: $0) }.joined(separator: "\n--"))
        
        let argKinds = compilable.args.map { $0.value.kind }
        
        mem.append(PseudoOp.debugMarker("==> STARTING FUNCTION \(findex)"))
        let prologueSize = appendPrologue(builder: mem)
        let reservedStackBytes = try appendStackInit(
            regKinds,
            args: argKinds,
            builder: mem,
            prologueSize: prologueSize
        )
        
        appendDebugPrintAligned4(
            "Entering function \(findex)@\(relativeBaseAddr.offsetFromBase)",
            builder: mem
        )
        
        let addrBetweenOps: [DeferredImmediate<Immediate19>] = (0..<compilable.ops.count).map { _ in
            return DeferredImmediate()
        }
        
        for (currentInstruction, op) in compilable.ops.enumerated() {
            
            addrBetweenOps[currentInstruction].finalize(try Immediate19(mem.byteSize))
            
            mem.append(
                PseudoOp.debugMarker("Marking position for \(currentInstruction) at \(mem.byteSize)")
            )
            
            print("Compiling \(op)")
            mem.append(
                PseudoOp.debugMarker("#\(currentInstruction): \(op.debugDescription)")
            )
            
            switch op {
            case .ORet(let dst):
                // store
                let dstStackOffset = getRegStackOffset(regKinds, dst)
                let dstKind = requireTypeKind(reg: dst, from: regKinds)
                if dstKind.hlRegSize > 0 {
                    mem.append(
                        PseudoOp.debugPrint(self, "Returning stack offset \(dstStackOffset)"),
                        PseudoOp.ldrVreg(X.x0, dstStackOffset, dstKind.hlRegSize)
                    )
                }
                mem.append(PseudoOp.debugPrint(self, "Jumping to epilogue. Loaded x0 from \(dstStackOffset)"))
                
                // jmp to end (NOTE: DO NOT ADD ANYTHING BETWEEN .start() and mem.append()
                var retTarget = RelativeDeferredOffset()
                print("Starting retTarget at \(mem.byteSize)")
                retTarget.start(at: mem.byteSize)
                retTargets.append(retTarget)
                mem.append(M1Op.b(retTarget)
                )
                
            case .OCall0(let dst, let funRef):
                let fn = ctx.callTargets.get(funRef)
                ctx.funcTracker.referenced(fn)
                
                assert(
                    reg: dst,
                    from: regKinds,
                    matches: fn.ret.value
                )
                
                let dstStackOffset = getRegStackOffset(regKinds, dst)
                let dstKind = requireTypeKind(reg: dst, from: regKinds)
                
                mem.append(
                    PseudoOp.debugMarker("Call0 fn@\(funRef) -> \(dst)"),
                    PseudoOp.mov(.x10, fn.entrypoint),
                    M1Op.blr(.x10),
                    PseudoOp.strVreg(X.x0, dstStackOffset, dstKind.hlRegSize)
                )
            case .OCall1(let dst, let fun, let arg0):
                let callTarget = ctx.callTargets.get(fun)
                ctx.funcTracker.referenced(callTarget)
                
                assert(reg: dst, from: regKinds, matches: callTarget.ret.value)
                assert(reg: arg0, from: regKinds, matchesCallArg: 0, inFun: callTarget)
                
                let fnAddr = callTarget.entrypoint
                let dstStackOffset = getRegStackOffset(regKinds, dst)
                let arg0StackOffset = getRegStackOffset(regKinds, arg0)
                let arg0Kind = requireTypeKind(reg: arg0, from: regKinds)
                let dstKind = requireTypeKind(reg: dst, from: regKinds)
                
                mem.append(
                    PseudoOp.debugMarker("Call1 fn@\(fun)(\(arg0)) -> \(dst)"),
                    PseudoOp.ldrVreg(X.x0, arg0StackOffset, arg0Kind.hlRegSize),
                    PseudoOp.mov(.x10, fnAddr),
                    M1Op.blr(.x10),
                    PseudoOp.strVreg(X.x0, dstStackOffset, dstKind.hlRegSize)
                )
            case .OCall3(let dst, let fun, let arg0, let arg1, let arg2):
                let callTarget = ctx.callTargets.get(fun)
                ctx.funcTracker.referenced(callTarget)
                
                
                assert(reg: dst, from: regKinds, matches: callTarget.ret.value)
                assert(reg: arg0, from: regKinds, matchesCallArg: 0, inFun: callTarget)
                assert(reg: arg1, from: regKinds, matchesCallArg: 1, inFun: callTarget)
                assert(reg: arg2, from: regKinds, matchesCallArg: 2, inFun: callTarget)
                
                let fnAddr = callTarget.entrypoint
                let dstStackOffset = getRegStackOffset(regKinds, dst)
                let arg0StackOffset = getRegStackOffset(regKinds, arg0)
                let arg1StackOffset = getRegStackOffset(regKinds, arg1)
                let arg2StackOffset = getRegStackOffset(regKinds, arg2)
                
                let dstKind = requireTypeKind(reg: dst, from: regKinds)
                let arg0Kind = requireTypeKind(reg: arg0, from: regKinds)
                let arg1Kind = requireTypeKind(reg: arg1, from: regKinds)
                let arg2Kind = requireTypeKind(reg: arg2, from: regKinds)
                
                mem.append(
                    PseudoOp.debugMarker("Call3 fn@\(fun)(\(arg0), \(arg1), \(arg2)) -> \(dst)"),
                    
                    PseudoOp.ldrVreg(X.x0, arg0StackOffset, arg0Kind.hlRegSize),
                    PseudoOp.ldrVreg(X.x1, arg1StackOffset, arg1Kind.hlRegSize),
                    PseudoOp.ldrVreg(X.x2, arg2StackOffset, arg2Kind.hlRegSize),
                    
                    PseudoOp.mov(.x10, fnAddr),
                    M1Op.blr(.x10),
                    
                    PseudoOp.strVreg(X.x0, dstStackOffset, dstKind.hlRegSize)
                )
            case .OCall4(let dst, let fun, let arg0, let arg1, let arg2, let arg3):
                let callTarget = ctx.callTargets.get(fun)
                ctx.funcTracker.referenced(callTarget)
                
                assert(reg: dst, from: regKinds, matches: callTarget.ret.value)
                assert(reg: arg0, from: regKinds, matchesCallArg: 0, inFun: callTarget)
                assert(reg: arg1, from: regKinds, matchesCallArg: 1, inFun: callTarget)
                assert(reg: arg2, from: regKinds, matchesCallArg: 2, inFun: callTarget)
                assert(reg: arg3, from: regKinds, matchesCallArg: 3, inFun: callTarget)
                
                let fnAddr = callTarget.entrypoint
                let regStackOffset = getRegStackOffset(regKinds, dst)
                let arg0StackOffset = getRegStackOffset(regKinds, arg0)
                let arg1StackOffset = getRegStackOffset(regKinds, arg1)
                let arg2StackOffset = getRegStackOffset(regKinds, arg2)
                let arg3StackOffset = getRegStackOffset(regKinds, arg3)
                
                mem.append(
                    PseudoOp.debugMarker("Call4 fn@\(fun)(\(arg0), \(arg1), \(arg2)) -> \(dst)"),
                    M1Op.ldr(X.x0, .reg64offset(X.sp, arg0StackOffset, nil)),
                    M1Op.ldr(X.x1, .reg64offset(X.sp, arg1StackOffset, nil)),
                    M1Op.ldr(X.x2, .reg64offset(X.sp, arg2StackOffset, nil)),
                    M1Op.ldr(X.x3, .reg64offset(X.sp, arg3StackOffset, nil)),
                    PseudoOp.mov(.x10, fnAddr),
                    M1Op.blr(.x10),
                    M1Op.str(X.x0, .reg64offset(X.sp, regStackOffset, nil))
                )
            case .OCallN(let dst, let fun, let args):
                let callTarget = ctx.callTargets.get(fun)
                ctx.funcTracker.referenced(callTarget)
                
                assert(reg: dst, from: regKinds, matches: callTarget.ret.value)
                let dstStackOffset = getRegStackOffset(regKinds, dst)
                let dstKind = requireTypeKind(reg: dst, from: regKinds)
                
                let regWkindToPass = args.enumerated().map {
                    (reg, argReg) in
                    assert(reg: argReg, from: regKinds, matchesCallArg: Reg(reg), inFun: callTarget)
                    return (reg, requireTypeKind(reg: argReg, from: regKinds))
                }
                
                let additionalSizeUnrounded = regWkindToPass.dropFirst(ARG_REGISTER_COUNT).reduce(0) {
                    print("Adding size for \($1.1)")
                    return $0 + Int($1.1.hlRegSize)
                }
                let additionalSize = roundUpStackReservation(Int16(additionalSizeUnrounded))
                
                if additionalSize > 0 {
                    mem.append(
                        PseudoOp.debugPrint(self, "Reserving \(additionalSize) bytes for stack (OCallN)"),
                        M1Op.subImm12(X.sp, X.sp, try .i(additionalSize))
                    )
                }
                
                var argOffset: Int64 = 0
                for (regIx, regKind) in regWkindToPass.dropFirst(ARG_REGISTER_COUNT) {
                    guard args.count > regIx else { break }
                    let argReg = args[regIx]
                    let offset = getRegStackOffset(regKinds, argReg) + Int64(additionalSize)
                    
                    mem.append(
                        PseudoOp.ldrVreg(X.x0, offset, regKind.hlRegSize),
                        PseudoOp.strVreg(X.x0, argOffset, regKind.hlRegSize)
                    )
                    mem.append(PseudoOp.debugPrint(self,
                                                   "Loaded \(offset) -> \(argOffset) -> \(regKind.hlRegSize)"))
                    
                    argOffset += regKind.hlRegSize
                }
                
                mem.append(PseudoOp.debugPrint(self, "CallN fn@\(fun)(\(args)) -> \(dst)"))
                
                for regIx in 0..<ARG_REGISTER_COUNT {
                    guard args.count > regIx else { break }
                    let argReg = args[regIx]
                    
                    puts("Putting varg \(argReg) in nreg \(regIx)")
                    let offset = getRegStackOffset(regKinds, argReg) + Int64(additionalSize)
//
                    appendLoad(reg: Register64(
                        rawValue: UInt8(regIx))!,
                               from: argReg,
                               kinds: regKinds, // careful, pass all kinds, not just the arg ones
                               offset: offset,
                               mem: mem)
                    appendDebugPrintRegisterAligned4(Register64(rawValue: UInt8(regIx))!, builder: mem)
                }
                
                // TODOFIX
                let fnAddr = callTarget.entrypoint
                print("Target entrypoint is \(fun) \(fnAddr)")
                
                mem.append(
                    PseudoOp.mov(.x19, fnAddr),
                    M1Op.blr(.x19)
                    )
                
                mem.append(
                    PseudoOp.strVreg(X.x0, dstStackOffset + Int64(additionalSize), dstKind.hlRegSize),
                    PseudoOp.debugPrint(self, "Got back and put result at offset \(dstStackOffset + Int64(additionalSize))")
                )
                if additionalSize > 0 {
                    mem.append(
                        PseudoOp.debugPrint(self, "Free \(additionalSize) bytes (OCallN)"),
                        (try M1Op._add(X.sp, X.sp, ByteCount(reservedStackBytes)))
                    )
                }
                    
            case .OCall2(let dst, let fun, let arg0, let arg1):
                let callTarget = ctx.callTargets.get(fun)
                ctx.funcTracker.referenced(callTarget)
                
                assert(reg: dst, from: regKinds, matches: callTarget.ret.value)
                assert(reg: arg0, from: regKinds, matchesCallArg: 0, inFun: callTarget)
                assert(reg: arg1, from: regKinds, matchesCallArg: 1, inFun: callTarget)
                
                let fnAddr = callTarget.entrypoint
                let regStackOffset = getRegStackOffset(regKinds, dst)
                let arg0StackOffset = getRegStackOffset(regKinds, arg0)
                let arg1StackOffset = getRegStackOffset(regKinds, arg1)
                
                mem.append(
                    PseudoOp.debugMarker("Call2 fn@\(fun)(\(arg0), \(arg1)) -> \(dst)"),
                    M1Op.ldr(X.x0, .reg64offset(X.sp, arg0StackOffset, nil)),
                    M1Op.ldr(X.x1, .reg64offset(X.sp, arg1StackOffset, nil)),
                    PseudoOp.mov(.x10, fnAddr),
                    M1Op.blr(.x10),
                    M1Op.str(X.x0, .reg64offset(X.sp, regStackOffset, nil))
                )
            case .ONew(let dst):
                appendDebugPrintAligned4("Entering ONew", builder: mem)
                // LOOK AT: https://github.com/HaxeFoundation/hashlink/blob/284301f11ea23d635271a6ecc604fa5cd902553c/src/jit.c#L3263
                let typeToAllocate = requireTypeKind(reg: dst, from: regKinds)
                
                let allocFunc_jumpTarget: UnsafeRawPointer
                
                // Do we need to move the dst value in X0 for the alloc func (e.g. hl_alloc_dynobj
                // takes no args, but hl_alloc_obj does)
                var needX0 = true
                
                switch(typeToAllocate) {
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
                
                let dstOffset = getRegStackOffset(regKinds, dst)
                
                mem.append(
                    PseudoOp.debugMarker("Moving alloc address in x1"),
                    PseudoOp.mov(.x1, allocFunc_jumpTarget),
                    PseudoOp.debugMarker("Jumping to the alloc func"),
                    PseudoOp.debugPrint(self, "Jumping to alloc func and storing result"),
                    M1Op.blr(.x1),
                    M1Op.str(X.x0, .reg64offset(X.sp, dstOffset, nil))
                )
                
                // let g = ctx.storage.globalResolver.get(Int(global!))
                // fatalError("No ONew yet. Allocating: \(typeToAllocate) -> global \(global) \(g)")
            case .OSetThis(field: let fieldRef, src: let srcReg):
                guard regKinds.count > 0 && srcReg < regKinds.count else {
                    fatalError("Not enough registers. Expected register 0 and \(srcReg) to be available. Got: \(regKinds)")
                }
                let sourceObjectType = regs[0]
                assertKind(op, sourceObjectType.value.kind, .obj)
                
                guard case .obj(let objData) = sourceObjectType.value else {
                    fatalError("source object should be .obj but was \(sourceObjectType)")
                }
                
                guard fieldRef < objData.fields.count else {
                    fatalError("OGetThis: expected field \(fieldRef) to exist but got \(objData.fields)")
                }
                let sourceObjectFieldType = objData.fields[fieldRef]
                
                assert(reg: srcReg, from: regKinds, is: sourceObjectFieldType.value.type.value.kind)
                
                let objOffset = getRegStackOffset(regKinds, 0)
                let srcOffset = getRegStackOffset(regKinds, srcReg)
                let fieldOffset = getFieldOffset(sourceObjectType.value, fieldRef)
                
                /* We should:
                 - load x0 from reg0 -- x0 is the base address of the type
                 - load x1 from the source register
                 - store x1 in x0 + field offset
                 */
                
                mem.append(
                    M1Op.ldr(X.x0, .reg64offset(X.sp, objOffset, nil)),
                    M1Op.ldr(X.x1, .reg64offset(X.sp, srcOffset, nil)),
                    M1Op.str(X.x1, .reg64offset(X.x0, fieldOffset, nil))
                )
            case .OGetThis(let regDst, let fieldRef):
                guard regKinds.count > 0 && regDst < regKinds.count else {
                    fatalError("Not enough registers. Expected register 0 and \(regDst) to be available. Got: \(regKinds)")
                }
                let sourceObjectType = regs[0]
                assertKind(op, sourceObjectType.value.kind, .obj)
                
                guard case .obj(let objData) = sourceObjectType.value else {
                    fatalError("source object should be .obj but was \(sourceObjectType)")
                }
                
                guard fieldRef < objData.fields.count else {
                    fatalError("OGetThis: expected field \(fieldRef) to exist but got \(objData.fields)")
                }
                let sourceObjectFieldType = objData.fields[fieldRef]
                
                assert(reg: regDst, from: regKinds, is: sourceObjectFieldType.value.type.value.kind)
                
                let objOffset = getRegStackOffset(regKinds, 0)
                let dstOffset = getRegStackOffset(regKinds, regDst)
                let fieldOffset = getFieldOffset(sourceObjectType.value, fieldRef)
                
                /* We should:
                 - load x0 from reg0
                 - access (x0 + fieldoffset) 
                 - move to regN
                 */
                
                mem.append(
                    PseudoOp.debugMarker("Loading x0 from SP + \(objOffset) (offset for reg0)"),
                    M1Op.ldr(X.x0, .reg64offset(X.sp, objOffset, nil)),     // point x0 to reg0 (i.e. obj)
                    PseudoOp.debugMarker("Moving x0 by \(fieldOffset) (offset for field \(fieldRef))"),
                    M1Op.ldr(X.x0, .reg64offset(X.x0, fieldOffset, nil)),   // move field (via offset) into x0
                    PseudoOp.debugMarker("Storing x0 at SP + \(dstOffset) (offset for reg\(regDst))"),
                    M1Op.str(X.x0, .reg64offset(X.sp, dstOffset, nil))      // store x0 back in sp
                )
            case .OInt(let dst, let iRef):
                assert(reg: dst, from: regKinds, is: .i32)
                let c = ctx.storage.int32Resolver.get(iRef)
                let regStackOffset = getRegStackOffset(regKinds, dst)
                mem.append(
                    PseudoOp.debugPrint(self, "--> Storing int \(iRef) (val \(c)) in \(dst)"),
                    PseudoOp.debugMarker(
                        "Mov \(c) into \(X.x0) and store in stack for HL reg \(iRef) at offset \(regStackOffset)"
                    ),
                    PseudoOp.mov(X.x0, c),
                    M1Op.str(W.w0, .reg64offset(X.sp, regStackOffset, nil))
                )
            case .OSetField(let objReg, let fieldRef, let srcReg):
                appendDebugPrintAligned4("Entering OSetField", builder: mem)
                let objRegKind = requireTypeKind(reg: objReg, from: regKinds)
                
                /**
                 field indexes are fetched from runtime_object,
                 and match what you might expect. E.g. for String:
                 
                 Example offsets for 0) bytes, 1) i32
                 
                 (lldb) p typePtr.pointee.obj.pointee.rt?.pointee.fields_indexes.pointee
                 (Int32?) $R0 = 8   // <----- first is 8 offset, on account of hl_type* at the top
                 (lldb) p typePtr.pointee.obj.pointee.rt?.pointee.fields_indexes.advanced(by: 1).pointee
                 (Int32?) $R1 = 16 // <----- second is 8 more offset, cause bytes is a pointer
                 (lldb)
                 
                 NOTE: keep alignment in mind. E.g. 0) int32 and 1) f64 will have 8 and 16 offsets respectively.
                 But 0) int32, 1) u8, 2) u8, 3) u16, 4) f64 will have 8, 12, 13, 14, 16 offsets respectively.
                 
                 See below:
                 
                 (lldb) p typePtr.pointee.obj.pointee.rt?.pointee.fields_indexes.advanced(by: 0).pointee
                 (Int32?) $R0 = 8
                 (lldb) p typePtr.pointee.obj.pointee.rt?.pointee.fields_indexes.advanced(by: 1).pointee
                 (Int32?) $R1 = 12
                 (lldb) p typePtr.pointee.obj.pointee.rt?.pointee.fields_indexes.advanced(by: 2).pointee
                 (Int32?) $R2 = 13
                 (lldb) p typePtr.pointee.obj.pointee.rt?.pointee.fields_indexes.advanced(by: 3).pointee
                 (Int32?) $R3 = 14
                 (lldb) p typePtr.pointee.obj.pointee.rt?.pointee.fields_indexes.advanced(by: 4).pointee
                 (Int32?) $R4 = 16
                 */
                
                switch(objRegKind) {
                case .obj: fallthrough
                case .struct:
                    // offset from obj address
                    let fieldOffset = requireFieldOffset(fieldRef: fieldRef, objIx: objReg, regs: regs)
                    
                    // offsets from .sp
                    let srcOffset = getRegStackOffset(regKinds, srcReg)
                    let objAddressOffset = getRegStackOffset(regKinds, objReg)
                    
                    mem.append(
                        M1Op.ldr(X.x0, .reg64offset(.sp, srcOffset, nil)),
                        M1Op.ldr(X.x1, .reg64offset(.sp, objAddressOffset, nil)),
                        M1Op.str(X.x0, .reg64offset(.x1, fieldOffset, nil))
                    )
                    // nop
                    //                        {
                    //                            hl_runtime_obj *rt = hl_get_obj_rt(dst->t);
                    //                            preg *rr = alloc_cpu(ctx, dst, true);
                    //                            copy_from(ctx, pmem(&p, (CpuReg)rr->id, rt->fields_indexes[o->p2]), rb);
                    //                        }
                    //                        break;
                default:
                    fatalError("OSetField not implemented for \(objRegKind)")
                }
            case .OJNotEq(let a, let b, let offset):
                fallthrough
            case .OJEq(let a, let b, let offset):
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
                
                let regOffsetA = getRegStackOffset(regKinds, a)
                let regOffsetB = getRegStackOffset(regKinds, b)
                
                let kindA = requireTypeKind(reg: a, from: regKinds)
                let kindB = requireTypeKind(reg: b, from: regKinds)
                
                let sizeA = kindA.hlRegSize
                let sizeB = kindB.hlRegSize
                
                mem.append(
                        PseudoOp.debugMarker("\(op.id) <\(a)@\(regOffsetA), \(b)@\(regOffsetB)> --> \(offset) (target instruction: \(targetInstructionIx))"),
                        M1Op.ldr(sizeA == 4 ? W.w0 : X.x0, .reg64offset(.sp, regOffsetA, nil)),
                        M1Op.ldr(sizeB == 4 ? W.w1 : X.x1, .reg64offset(.sp, regOffsetB, nil))
                    )
                
                switch(op) {
                case .OJSLte:
                    fallthrough
                case .OJSLt:
                    switch(kindA) {
                    case .u16:
                        mem.append(M1Op.sxth(.x0, .w0))
                    case .u8:
                        mem.append(M1Op.sxtb(.x0, .w0))
                    default:
                        break
                    }
                    switch(kindB) {
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
                
                
                appendDebugPrintRegisterAligned4(X.x0, builder: mem)
                appendDebugPrintRegisterAligned4(X.x1, builder: mem)
                
                mem.append(M1Op.cmp(X.x0, X.x1))
                
                
                // calculate what to skip
                let jumpOffset_partA = try Immediate19(mem.byteSize)
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
                        case .OJSLt:
                            fallthrough
                        case .OJULt:
                            return M1Op.b_lt(try Immediate19(jumpOffset.immediate))
                        case .OJSLte:
                            return M1Op.b_le(try Immediate19(jumpOffset.immediate))
                        case .OJEq:
                            return M1Op.b_eq(try Immediate19(jumpOffset.immediate))
                        case .OJNotEq:
                            return M1Op.b_ne(try Immediate19(jumpOffset.immediate))
                        default:
                            fatalError("Unsupported jump id \(op.id)")
                        }
                    })
                mem.append(
                    PseudoOp.debugPrint(self, "NOT JUMPING")
                )
            
            // TODO: somehow combine all the jumps with fallthroughs?
            case .OJNotNull(let reg, let offset):
                fallthrough
            case .OJFalse(let reg, let offset):
                fallthrough
            case .OJNull(let reg, let offset):
                
                let wordsToSkip = Int(offset) + 1
                let targetInstructionIx = currentInstruction + wordsToSkip
                guard targetInstructionIx < addrBetweenOps.count else {
                    fatalError("Jump going to an invalid op (\(targetInstructionIx))")
                }
                
                let regOffset = getRegStackOffset(regKinds, reg)
                
                let size = requireTypeKind(reg: reg, from: regKinds).hlRegSize
                
                mem.append(
                    PseudoOp.debugMarker("\(op.id) \(reg)@\(regOffset) --> \(offset) (target instruction: \(targetInstructionIx))"))
                if size == 8 {
                    mem.append(
                        M1Op.movz64(X.x0, 0, nil),
                        M1Op.ldr(X.x1, .reg64offset(.sp, regOffset, nil))
                    )
                } else if size == 4 {   // bool
                    mem.append(
                        M1Op.movz64(X.x0, 0, nil),
                        M1Op.ldr(W.w1, .reg64offset(.sp, regOffset, nil))
                    )
                } else if size == 2 {   // bool
                    mem.append(
                        M1Op.movz64(X.x0, 0, nil),
                        M1Op.ldrh(W.w1, .imm64(.sp, regOffset, nil))
                    )
                } else if size == 1 {   // bool
                    mem.append(
                        M1Op.movz64(X.x0, 0, nil),
                        M1Op.ldrb(W.w1, .imm64(.sp, regOffset, nil))
                    )
                } else {
                    fatalError("Reg size must be 8 or 4")
                }
                
                appendDebugPrintRegisterAligned4(X.x0, builder: mem)
                appendDebugPrintRegisterAligned4(X.x1, builder: mem)
                
                mem.append(M1Op.cmp(X.x0, X.x1))
                
                // calculate what to skip
                let jumpOffset_partA = try Immediate19(mem.byteSize)
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
                            return M1Op.b_eq(try Immediate19(jumpOffset.immediate))
                        case .OJNotNull:
                            return M1Op.b_ne(try Immediate19(jumpOffset.immediate))
                        default:
                            fatalError("Unsupported jump id \(op.id)")
                        }
                    })
                mem.append(
                    PseudoOp.debugPrint(self, "NOT JUMPING")
                )
            // TODO: combine with above jumps
            case .OJAlways(let offset):
                let wordsToSkip = Int(offset) + 1
                let targetInstructionIx = currentInstruction + wordsToSkip
                guard targetInstructionIx < addrBetweenOps.count else {
                    fatalError("Jump going to an invalid op (\(targetInstructionIx))")
                }
                
                // calculate what to skip
                let jumpOffset_partA = try Immediate19(mem.byteSize)
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
                guard let globalTypePtr = ctx.hlcode?.pointee.globals.advanced(by: globalRef).pointee else {
                    fatalError("Can't resolve global \(globalRef)")
                }
                let dstOffset = getRegStackOffset(regKinds, dst)
                mem.append(
                    PseudoOp.mov(.x0, UnsafeRawPointer(globalTypePtr)),
                    M1Op.str(X.x0, .reg64offset(.sp, dstOffset, nil))
                )
            case .OShl(let dst, let a, let b):
                appendLoad(reg: .x0, from: a, kinds: regKinds, mem: mem)
                appendLoad(reg: .x1, from: b, kinds: regKinds, mem: mem)
                mem.append(M1Op.lsl_r(X.x2, X.x0, X.x1))
                appendStore(reg: X.x2, into: dst, kinds: regKinds, mem: mem)
            case .OGetI8(let dst, let bytes, let index):
                fallthrough
            case .OGetI16(let dst, let bytes, let index):
                assert(reg: bytes, from: regKinds, is: .bytes)
                assert(reg: index, from: regKinds, is: .i32)
                if op.id == .OGetI16 {
                    assert(reg: dst, from: regKinds, in: [.u16, .i32])
                } else if op.id == .OGetI8 {
                    assert(reg: dst, from: regKinds, in: [.u8, .u16, .i32])
                }
                
                let dstOffset = getRegStackOffset(regKinds, dst)
                let byteOffset = getRegStackOffset(regKinds, bytes)
                let indexOffset = getRegStackOffset(regKinds, index)
                
                mem.append(
                    // Load byte address (base) in X.x0. It is 8 bytes
                    M1Op.ldr(X.x0, .reg64offset(.sp, byteOffset, nil)),
                    // Load index into X.x1. It is 4 bytes
                    M1Op.ldr(W.w1, .reg64offset(.sp, indexOffset, nil))
                )
                
                if op.id == .OGetI8 {
                    mem.append(
                        M1Op.ldrb(W.w0, .reg(X.x0, .r64shift(X.x1, .lsl(0))))
                    )
                } else if op.id == .OGetI16 {
                    mem.append(
                        // lsl1 the index register b/c index is
                        //    HL-side: ??? TODO: verify in a test that we need the .lsl(1) here
                        //    M1-side: expected in bytes not half-words
                        M1Op.ldrh(W.w0, .reg(X.x0, .r64shift(X.x1, .lsl(1))))
                    )
                }
                
                let size = requireTypeKind(reg: dst, from: regKinds).hlRegSize
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
                let dstOffset = getRegStackOffset(regKinds, dst)
                let size = requireTypeKind(reg: dst, from: regKinds).hlRegSize
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
                        M1Op.b_ne(try Immediate19(jumpOverDeath.value))
                    }
                )
                appendDebugPrintAligned4("Null access exception", builder: mem)
                appendSystemExit(1, builder: mem)
                jumpOverDeath.stop(at: mem.byteSize)
            case .OField(let dstReg, let objReg, let fieldRef):
                let objRegKind = requireTypeKind(reg: objReg, from: regKinds)
                let dstKind = requireTypeKind(reg: dstReg, from: regKinds)
                /* See comments on `OSetField` for notes on accessing field indexes */
                 
                switch(objRegKind) {
                case .obj: fallthrough
                case .struct:
                    // offset from obj address
                    let fieldOffset = requireFieldOffset(fieldRef: fieldRef, objIx: objReg, regs: regs)
                    
                    // offsets from .sp
                    let dstOffset = getRegStackOffset(regKinds, dstReg)
                    let objOffset = getRegStackOffset(regKinds, objReg)
                    
                    // TODO: OField and OSetField need a test based on inheritance
                    mem.append(
                        PseudoOp.debugPrint(
                            self,
                            "TODO: OField and OSetField need a test based on inheritance"))
                    mem.append(
                        PseudoOp.debugPrint(
                            self,
                            "TODO: OField and OSetField need to use appendLoad/appendStore"))
                    
                    mem.append(
                        M1Op.ldr(X.x0, .reg64offset(.sp, objOffset, nil)),
                        M1Op.ldr(X.x1, .reg64offset(.x0, fieldOffset, nil))
                    )
                    
                    if dstKind.hlRegSize == 8 {
                        mem.append(
                            M1Op.str(X.x1, .reg64offset(.sp, dstOffset, nil))
                        )
                    } else if dstKind.hlRegSize == 4 {
                        mem.append(
                            M1Op.str(W.w1, .reg64offset(.sp, dstOffset, nil))
                        )
                    } else {
                        fatalError("Dst size must be 4 or 8")
                    }
                default:
                    fatalError("OSetField not implemented for \(objRegKind)")
                }
            case .OAnd(let dst, let a, let b):
                let dstOffset = getRegStackOffset(regKinds, dst)
                let aOffset = getRegStackOffset(regKinds, a)
                let bOffset = getRegStackOffset(regKinds, b)
                
                mem.append(
                    M1Op.ldr(X.x0, .reg64offset(X.sp, aOffset, nil)),
                    M1Op.ldr(X.x1, .reg64offset(X.sp, bOffset, nil)),
                    M1Op.and(X.x2, X.x0, .r64shift(X.x1, .lsl(0))),
                    M1Op.str(X.x2, .reg64offset(X.sp, dstOffset, nil))
                )
            case .OThrow(let exc):
                mem.append(
                    M1Op.movz64(X.x0, UInt16(exc), nil),
                    PseudoOp.mov(X.x1, _throwAddress),
                    M1Op.blr(X.x1)
                )
            case .OTrap(let exc, let offset):
                mem.append(
                    M1Op.movz64(X.x0, UInt16(exc), nil),
                    M1Op.movz64(X.x1, UInt16(offset), nil),
                    PseudoOp.mov(X.x2, _trapAddress),
                    M1Op.blr(X.x2)
                )
            case .OEndTrap(let exc):
                mem.append(
                    M1Op.movz64(X.x0, UInt16(exc), nil),
                    PseudoOp.mov(X.x1, _endTrapAddress),
                    M1Op.blr(X.x1)
                )
            case .ONull(let dst):
                let dstOffset = getRegStackOffset(regKinds, dst)
                mem.append(
                    M1Op.movz64(X.x0, 0, nil),
                    M1Op.str(X.x0, .reg64offset(.sp, dstOffset, nil))
                )
            case .OBool(let dst, let value):
                let dstOffset = getRegStackOffset(regKinds, dst)
                mem.append(
                    M1Op.movz64(X.x0, UInt16(value), nil),
                    M1Op.strb(W.w0, .imm64(.sp, dstOffset, nil))
                )
            case .OMov(let dst, let src):
                fallthrough
            case .OSafeCast(let dst, let src):
                let dstOffset = getRegStackOffset(regKinds, dst)
                let srcOffset = getRegStackOffset(regKinds, src)
                mem.append(
                    M1Op.ldr(X.x0, .reg64offset(.sp, srcOffset, nil)),
                    M1Op.str(X.x0, .reg64offset(.sp, dstOffset, nil))
                )
            case .OLabel:
                mem.append(PseudoOp.debugPrint(self, "OLabel"))
            case .OSub(let dst, let a, let b):
                appendLoad(reg: .x0, from: a, kinds: regKinds, mem: mem)
                appendLoad(reg: .x1, from: b, kinds: regKinds, mem: mem)
                
                mem.append(
                    M1Op.sub(X.x2, X.x0, .r64shift(X.x1, .lsl(0)))
                )
                
                appendStore(reg: .x2, into: dst, kinds: regKinds, mem: mem)
            case .OAdd(let dst, let a, let b):
                appendLoad(reg: .x0, from: a, kinds: regKinds, mem: mem)
                appendLoad(reg: .x1, from: b, kinds: regKinds, mem: mem)
                mem.append(M1Op.add(X.x0, X.x0, .r64shift(X.x1, .lsl(0))))
                appendStore(reg: X.x0, into: dst, kinds: regKinds, mem: mem)
            case .OUShr(let dst, let a, let b):
                fallthrough
            case .OSShr(let dst, let a, let b):
                appendLoad(reg: .x0, from: a, kinds: regKinds, mem: mem)
                appendLoad(reg: .x1, from: b, kinds: regKinds, mem: mem)
                
                if case .OSShr = op {
                    appendSignMode(true, reg: .x0, from: a, kinds: regKinds, mem: mem)
                    mem.append(M1Op.asrv(X.x2, X.x0, X.x1))
                } else if case .OUShr = op {
                    mem.append(M1Op.lsrv(X.x2, X.x0, X.x1))
                } else {
                    fatalError("Unknown shift op")
                }
                
                appendStore(reg: X.x2, into: dst, kinds: regKinds, mem: mem)
            case .OArraySize(let dst, let src):
                _ = requireTypeKind(reg: src, from: regKinds, shouldMatch: .array)
                
                appendLoad(reg: .x0, from: src, kinds: regKinds, mem: mem)
                mem.append(
                    // varray: skip 2 pointers (16 bytes) and load 4 bytes
                    M1Op.ldr(W.w0, .reg(X.x0, .imm(16, nil)))
                )
                appendStore(reg: .x0, into: dst, kinds: regKinds, mem: mem)
            case .OType(let dst, let ty):
                guard let typeBase = ctx.hlcode?.pointee.types else {
                    fatalError("Fetching type failed")
                }
                let typeMemory = typeBase.advanced(by: ty)
                let typeMemoryVal = Int(bitPattern: typeMemory)
                print("Storing type mem \(typeMemoryVal)")
                mem.append(PseudoOp.mov(.x0, typeMemoryVal))
                appendStore(reg: .x0, into: dst, kinds: regKinds, mem: mem)
                
                let _test: (@convention(c) (UnsafeRawPointer) -> ()) = { (_ ptr: UnsafeRawPointer) in
                    let p = UnsafePointer<HLType_CCompat>(OpaquePointer(ptr))
                    print("Addr for type: \(p) \(Int(bitPattern: p))")
                    print("Got kind: \(p.pointee.kind)")
                }
                let _testAddress = unsafeBitCast(_test, to: UnsafeMutableRawPointer.self)
                mem.append(
                    PseudoOp.mov(X.x1, _testAddress),
                    M1Op.blr(X.x1))
            case .OIncr(let dst):
                appendLoad(reg: .x0, from: dst, kinds: regKinds, mem: mem)
                mem.append(M1Op.add(X.x0, X.x0, .imm(1, nil)))
                appendStore(reg: .x0, into: dst, kinds: regKinds, mem: mem)
            case .OSetArray(let array, let index, let src):
                let lsl = requireTypeSizeLsl(reg: src, from: regKinds)
                appendLoad(reg: .x0, from: src, kinds: regKinds, mem: mem)
                appendLoad(reg: .x1, from: index, kinds: regKinds, mem: mem)
                appendLoad(reg: .x2, from: array, kinds: regKinds, mem: mem)
                mem.append(
                    M1Op.lsl_i(X.x1, X.x1, try Immediate6(lsl)),
                    M1Op.str(X.x0, .reg(X.x2, .r64ext(X.x1, .sxtx(0))))
                )
            case .OGetArray(let dst, let array, let index):
                let lsl = requireTypeSizeLsl(reg: dst, from: regKinds)
                appendLoad(reg: .x1, from: index, kinds: regKinds, mem: mem)
                appendLoad(reg: .x2, from: array, kinds: regKinds, mem: mem)
                mem.append(
                    M1Op.lsl_i(X.x1, X.x1, try Immediate6(lsl)),
                    M1Op.ldr(X.x0, .reg(X.x2, .r64ext(X.x1, .sxtx(0))))
                )
                appendStore(reg: X.x0, into: dst, kinds: regKinds, mem: mem)
            case .ONop:
                mem.append(.nop)
            case .OXor(let dst, let a, let b):
                appendLoad(reg: .x0, from: a, kinds: regKinds, mem: mem)
                appendLoad(reg: .x1, from: b, kinds: regKinds, mem: mem)
                mem.append(M1Op.eor_r(X.x2, X.x0, X.x1, nil))
                appendStore(reg: X.x2, into: dst, kinds: regKinds, mem: mem)
            default:
                fatalError("Can't compile \(op.debugDescription)")
            }
        }
        
        // initialize targets for 'ret' jumps
        for var retTarget in retTargets {
            print("Stopping retTarget at \(mem.byteSize)")
            retTarget.stop(at: mem.byteSize)
        }
        
        if reservedStackBytes > 0 {
            mem.append(
                PseudoOp.debugPrint(self, "Free \(reservedStackBytes) bytes (pre-epilogue)"),
                (try M1Op._add(X.sp, X.sp, ByteCount(reservedStackBytes)))
            )
        }
        else {
            mem.append(
                PseudoOp.debugPrint(self,
                    "Skipping freeing stack because \(reservedStackBytes) bytes were needed"
                )
            )
        }
        
        appendEpilogue(builder: mem)
        mem.append(
            PseudoOp.debugPrint(self,
                "Returning"
            )
        )
        mem.append(.ret)
    }
}
