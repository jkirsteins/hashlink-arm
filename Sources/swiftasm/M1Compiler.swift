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
        printerr("args needing space: \(stackArgs.map { $0.debugDescription })")
        let result = stackArgs.reduce(0) { $0 + Int16($1.hlRegSize) }
        return (roundUpStackReservation(result), stackArgs)
    }
    
    /*
     First 7 args are in registers [x0;x7]. Others are on the stack.
     Stack should be extended to account for data which is in registers.
     Subsequently data should be moved from regs to stack, to enable use of all registers.
     */
    @discardableResult func appendStackInit(
        _ unfilteredRegs: HLTypeKinds,
        args unfilteredArgs: HLTypeKinds,
        builder: OpBuilder
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
        
        // filter after testing for mismatched
        let regs = unfilteredRegs.filter { $0 != .void }
        let args = unfilteredArgs.filter { $0 != .void }
        // only the first args need stack space, because we want to move them from the registers
        // into stack
        let (neededExtraStackSize, stackArgs) = calcStackArgReq(regs: regs, args: args)
        
        guard neededExtraStackSize > 0 else {
            builder.append(PseudoOp.debugMarker("No extra stack space needed"))
            return neededExtraStackSize
        }
        
        builder.append(
            PseudoOp.debugMarker("Reserving \(neededExtraStackSize) bytes for stack"),
            M1Op.sub(X.sp, X.sp, try .i(neededExtraStackSize))
        )
        
        var offset: ByteCount = 0
        for (ix, reg) in stackArgs.enumerated() {
            builder.append(
                PseudoOp.debugMarker(
                    "Moving \(Register64(rawValue: UInt8(ix))!) to \(offset)"
                )
            )
            switch(reg.hlRegSize) {
            case 4:
                builder.append(
                    M1Op.str(
                        Register32(rawValue: UInt8(ix))!,
                        .reg64offset(Register64.sp, offset, nil)
                    ))
            case 8:
                builder.append(M1Op.str(
                    Register64(rawValue: UInt8(ix))!,
                    .reg64offset(Register64.sp, offset, nil)
                ))
            default:
                fatalError("wip")
            }
            
            print("Inc offset by \(reg.hlRegSize) from \(reg)")
            offset += reg.hlRegSize
        }
        
        return neededExtraStackSize
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
        
        builder.append(
            // Stash registers we'll use (so we can reset)
            .str(reg, .reg64offset(.sp, -32, .pre)),
            .str(Register64.x1, .reg64offset(.sp, 8, nil)),
            .str(Register64.x0, .reg64offset(.sp, 16, nil)),
            .str(Register64.x16, .reg64offset(.sp, 24, nil))
        )
        adr.start(at: builder.byteSize)
        builder.append(.adr64(.x0, adr))
        
        builder.append(
            PseudoOp.mov(.x16, printfAddr),
            M1Op.blr(.x16),
            // restore
            M1Op.ldr(Register64.x16, .reg64offset(.sp, 24, nil)),
            M1Op.ldr(Register64.x0, .reg64offset(.sp, 16, nil)),
            M1Op.ldr(Register64.x1, .reg64offset(.sp, 8, nil)),
            M1Op.ldr(reg, .reg64offset(.sp, 32, .post))
        )
        
        jmpTarget.start(at: builder.byteSize)
        builder.append(.b(jmpTarget))
        adr.stop(at: builder.byteSize)
        builder.append(ascii: str).align(4)
        
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
    
    func appendPrologue(builder: OpBuilder) {
        builder.append(
            PseudoOp.debugMarker("Starting prologue"),
            M1Op.stp((.x29_fp, .x30_lr), .reg64offset(.sp, -16, .pre)),
            M1Op.movr64(.x29_fp, .sp)
        )
    }
    
    func appendEpilogue(builder: OpBuilder) {
        builder.append(
            PseudoOp.debugMarker("Starting epilogue"),
            M1Op.ldp((.x29_fp, .x30_lr), .reg64offset(.sp, 16, .post))
        )
    }
}

typealias Registers = [Resolvable<HLType>]

let _throw: (@convention(c) (UnsafeRawPointer) -> ()) = { (_ dyn: UnsafeRawPointer) in
    fatalError("not implemented")
}
let _throwAddress = unsafeBitCast(_throw, to: UnsafeMutableRawPointer.self)

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
        for i in 0..<ix { result += regs[Int(i)].hlRegSize }
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
        
        // grab it before it changes from prologue
        // let memory = mem.getDeferredPosition()
        let regs = compilable.regs
        let regKinds = regs.map { $0.value.kind }
        
        let findex = compilable.getFindex()
        
        let relativeBaseAddr = mem.getDeferredPosition()
        compilable.entrypoint.update(from: relativeBaseAddr)
        
        // if we need to return early, we jump to these
        var retTargets: [RelativeDeferredOffset] = []
        
        print("Compiling function \(findex) at deferred address \(compilable.entrypoint)")
        print("REGS: \n--" + regs.map { String(reflecting: $0) }.joined(separator: "\n--"))
        //        print("OPS: \n--" + funPtr.pointee.ops.map { String(reflecting: $0) }.joined(separator: "\n--"))
        
        let argKinds = compilable.args.map { $0.value.kind }
        
        mem.append(PseudoOp.debugMarker("==> STARTING FUNCTION \(findex)"))
        appendPrologue(builder: mem)
        let reservedStackBytes = try appendStackInit(
            regKinds,
            args: argKinds,
            builder: mem
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
                let regStackOffset = getRegStackOffset(regKinds, dst)
                mem.append(
                    PseudoOp.debugPrint(self, "Returning stack offset \(regStackOffset)"),
                    M1Op.ldr(X.x0, .reg64offset(.sp, regStackOffset, nil))
                )
                
                mem.append(PseudoOp.debugPrint(self, "Jumping to epilogue"))
                
                // jmp to end (NOTE: DO NOT ADD ANYTHING BETWEEN .start() and mem.append()
                var retTarget = RelativeDeferredOffset()
                print("Starting retTarget at \(mem.byteSize)")
                retTarget.start(at: mem.byteSize)
                retTargets.append(retTarget)
                mem.append(M1Op.b(retTarget)
                )
                
            case .OCall0(let dst, let funRef):
                let fn = ctx.callTargets.get(funRef)
                
                assert(
                    reg: dst,
                    from: regKinds,
                    matches: fn.ret.value
                )
                
                let regStackOffset = getRegStackOffset(regKinds, dst)
                
                mem.append(
                    PseudoOp.debugMarker("Call0 fn@\(funRef) -> \(dst)"),
                    PseudoOp.mov(.x10, fn.entrypoint),
                    M1Op.blr(.x10),
                    M1Op.str(X.x0, .reg64offset(X.sp, regStackOffset, nil))
                )
            case .OCall1(let dst, let fun, let arg0):
                let callTarget = ctx.callTargets.get(fun)
                
                assert(reg: dst, from: regKinds, matches: callTarget.ret.value)
                assert(reg: arg0, from: regKinds, matchesCallArg: 0, inFun: callTarget)
                
                let fnAddr = callTarget.entrypoint
                let regStackOffset = getRegStackOffset(regKinds, dst)
                let arg0StackOffset = getRegStackOffset(regKinds, arg0)
                
                mem.append(
                    PseudoOp.debugMarker("Call1 fn@\(fun)(\(arg0)) -> \(dst)"),
                    M1Op.ldr(X.x0, .reg64offset(X.sp, arg0StackOffset, nil)),
                    PseudoOp.mov(.x10, fnAddr),
                    M1Op.blr(.x10),
                    M1Op.str(X.x0, .reg64offset(X.sp, regStackOffset, nil))
                )
            case .OCall3(let dst, let fun, let arg0, let arg1, let arg2):
                let callTarget = ctx.callTargets.get(fun)
                
                assert(reg: dst, from: regKinds, matches: callTarget.ret.value)
                assert(reg: arg0, from: regKinds, matchesCallArg: 0, inFun: callTarget)
                assert(reg: arg1, from: regKinds, matchesCallArg: 1, inFun: callTarget)
                assert(reg: arg2, from: regKinds, matchesCallArg: 2, inFun: callTarget)
                
                let fnAddr = callTarget.entrypoint
                let regStackOffset = getRegStackOffset(regKinds, dst)
                let arg0StackOffset = getRegStackOffset(regKinds, arg0)
                let arg1StackOffset = getRegStackOffset(regKinds, arg1)
                let arg2StackOffset = getRegStackOffset(regKinds, arg2)
                
                mem.append(
                    PseudoOp.debugMarker("Call3 fn@\(fun)(\(arg0), \(arg1), \(arg2)) -> \(dst)"),
                    M1Op.ldr(X.x0, .reg64offset(X.sp, arg0StackOffset, nil)),
                    M1Op.ldr(X.x1, .reg64offset(X.sp, arg1StackOffset, nil)),
                    M1Op.ldr(X.x2, .reg64offset(X.sp, arg2StackOffset, nil)),
                    PseudoOp.mov(.x10, fnAddr),
                    M1Op.blr(.x10),
                    M1Op.str(X.x0, .reg64offset(X.sp, regStackOffset, nil))
                )
            case .OCall2(let dst, let fun, let arg0, let arg1):
                let callTarget = ctx.callTargets.get(fun)
                
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
                    M1Op.str(X.x0, .reg64offset(X.sp, regStackOffset, nil))
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
            case .OJULt(let a, let b, let offset):
                
                let wordsToSkip = Int(offset) + 1
                let targetInstructionIx = currentInstruction + wordsToSkip
                guard targetInstructionIx < addrBetweenOps.count else {
                    fatalError("Jump going to an invalid op (\(targetInstructionIx))")
                }
                
                let regOffsetA = getRegStackOffset(regKinds, a)
                let regOffsetB = getRegStackOffset(regKinds, b)
                
                let sizeA = requireTypeKind(reg: a, from: regKinds).hlRegSize
                let sizeB = requireTypeKind(reg: b, from: regKinds).hlRegSize
                
                mem.append(
                    PseudoOp.debugMarker("\(op.id) <\(a)@\(regOffsetA), \(b)@\(regOffsetB)> --> \(offset) (target instruction: \(targetInstructionIx))"),
                    M1Op.ldr(sizeA == 4 ? W.w0 : X.x0, .reg64offset(.sp, regOffsetA, nil)),
                    M1Op.ldr(sizeB == 4 ? W.w1 : X.x1, .reg64offset(.sp, regOffsetB, nil))
                )
                
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
                        case .OJULt:
                            return M1Op.b_lt(try Immediate19(jumpOffset.immediate))
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
                } else {
                    fatalError("Reg size must be 8")
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
                let dstOffset = getRegStackOffset(regKinds, dst)
                let aOffset = getRegStackOffset(regKinds, a)
                let bOffset = getRegStackOffset(regKinds, b)
                mem.append(
                    M1Op.ldr(X.x0, .reg64offset(.sp, aOffset, nil)),
                    M1Op.ldr(X.x1, .reg64offset(.sp, bOffset, nil)),
                    M1Op.lsl_r(X.x2, X.x0, X.x1),
                    M1Op.str(X.x2, .reg64offset(.sp, dstOffset, nil))
                )
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
                        M1Op.ldrb(W.w0, .reg64(X.x0, .r64shift(X.x1, .lsl(0))))
                    )
                } else if op.id == .OGetI16 {
                    mem.append(
                        // lsl1 the index register b/c index is
                        //    HL-side: ??? TODO: verify in a test that we need the .lsl(1) here
                        //    M1-side: expected in bytes not half-words
                        M1Op.ldrh(W.w0, .reg64(X.x0, .r64shift(X.x1, .lsl(1))))
                    )
                }
                
                let size = requireTypeKind(reg: dst, from: regKinds).hlRegSize
                if size == 4 {
                    mem.append(M1Op.str(W.w0, .reg64offset(.sp, dstOffset, nil)))
                } else if size == 8 {
                    mem.append(M1Op.str(X.x0, .reg64offset(.sp, dstOffset, nil)))
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
                appendDebugPrintAligned4("Entering OField", builder: mem)
                let objRegKind = requireTypeKind(reg: objReg, from: regKinds)
                appendDebugPrintAligned4("Required obj kind \(objRegKind)", builder: mem)
                let dstKind = requireTypeKind(reg: dstReg, from: regKinds)
                appendDebugPrintAligned4("Dst kind \(dstKind)", builder: mem)
                /* See comments on `OSetField` for notes on accessing field indexes */
                 
                switch(objRegKind) {
                case .obj: fallthrough
                case .struct:
                    // offset from obj address
                    let fieldOffset = requireFieldOffset(fieldRef: fieldRef, objIx: objReg, regs: regs)
                    appendDebugPrintAligned4("Required field offset \(fieldOffset)", builder: mem)
                    
                    // offsets from .sp
                    let dstOffset = getRegStackOffset(regKinds, dstReg)
                    appendDebugPrintAligned4("Required dstOffset \(dstOffset)", builder: mem)
                    let objOffset = getRegStackOffset(regKinds, objReg)
                    appendDebugPrintAligned4("Required objOffset \(objOffset)", builder: mem)
                    
                    // TODO: OField and OSetField need a test based on inheritance
                    
                    mem.append(
                        PseudoOp.debugPrint(self, "Loading x0."),
                        M1Op.ldr(X.x0, .reg64offset(.sp, objOffset, nil)),
                        PseudoOp.debugPrint(self, "Done. Loading x1."),
                        M1Op.ldr(X.x1, .reg64offset(.x0, fieldOffset, nil))
                    )
                    
                    if dstKind.hlRegSize == 8 {
                        mem.append(
                            PseudoOp.debugPrint(self, "Done. Storing x1 in 8 bytes"),
                            M1Op.str(X.x1, .reg64offset(.sp, dstOffset, nil)),
                            PseudoOp.debugPrint(self, "Stored 8 bytes")
                        )
                    } else if dstKind.hlRegSize == 4 {
                        mem.append(
                            PseudoOp.debugPrint(self, "Done. Storing x1 in 4 bytes"),
                            M1Op.str(W.w1, .reg64offset(.sp, dstOffset, nil)),
                            PseudoOp.debugPrint(self, "Stored 4 bytes")
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
                fatalError("not implemented")
            case .OTrap(let exc, let offset):
                fatalError("not implemented")
            case .OEndTrap(let exc):
                fatalError("not implemented")
            case .ONull(let dst):
                let dstOffset = getRegStackOffset(regKinds, dst)
                mem.append(
                    M1Op.movz64(X.x0, 0, nil),
                    M1Op.str(X.x0, .reg64offset(.sp, dstOffset, nil))
                )
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
                PseudoOp.debugMarker("Free \(reservedStackBytes) bytes"),
                (try M1Op._add(X.sp, X.sp, ByteCount(reservedStackBytes)))
            )
        }
        else {
            mem.append(
                PseudoOp.debugMarker(
                    "Skipping freeing stack because \(reservedStackBytes) bytes were needed"
                )
            )
        }
        
        appendEpilogue(builder: mem)
        mem.append(.ret)
    }
}
