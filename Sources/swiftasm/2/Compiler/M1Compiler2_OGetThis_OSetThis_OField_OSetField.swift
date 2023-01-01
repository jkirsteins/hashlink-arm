extension M1Compiler2 {
    /// Reusable implem for OGetThis and OField
    func __ogetthis_ofield(
        dstReg: Reg,
        objReg: Reg,
        fieldRef: RefField,
        regs: [any HLTypeProvider],
        mem: CpuOpBuffer,
        _callsite: (findex: RefFun, opnum: Int)? = nil
    ) throws {
        let objRegKind = requireTypeKind(reg: objReg, from: regs)
        let dstType = requireType(reg: dstReg, regs: regs)
        
        /* See comments on `OSetField` for notes on accessing field indexes */
        
        switch(objRegKind) {
        case .obj: fallthrough
        case .struct:
            // offset from obj address
            let fieldOffset = requireFieldOffset(fieldRef: fieldRef, objIx: objReg, regs: regs)
            appendLoad(reg: X.x0, from: objReg, kinds: regs, mem: mem)
            appendLoad(0, as: dstReg, addressRegister: .x0, offset: fieldOffset, kinds: regs, mem: mem)
            appendStore(0, into: dstReg, kinds: regs, mem: mem)
        case .virtual:
            let prependHeader: String
            if let _callsite = _callsite, _callsite.findex == 36 {
                prependHeader = "(fun@\(_callsite.findex); #\(_callsite.opnum)) OField/virtual"
            } else {
                prependHeader = "OField/virtual"
            }
            
            let getFunc = get_dynget(to: dstType.kind)
                
            // x0 -> points to ((*_vvirtual)(obj))+1
            appendLoad(reg: X.x0, from: objReg, kinds: regs, mem: mem)
            appendDebugPrintRegisterAligned4(X.x0, prepend: "\(prependHeader) object", builder: mem)
            
            // x1 point to field base (right after the (vvirtual) content at x0
            mem.append(M1Op.add(X.x1, X.x0, .imm(Int64(MemoryLayout<vvirtual>.stride), nil)))
            appendDebugPrintRegisterAligned4(X.x1, prepend: "\(prependHeader) field base", builder: mem)
            
            // x2 load field index multiplied by size of (void*)
            let fieldOffsetInBytes = fieldRef * MemoryLayout<OpaquePointer>.stride
            mem.append(M1Op.movz64(X.x2, UInt16(fieldOffsetInBytes), nil))
            appendDebugPrintAligned4("ofield/virtual field index: \(fieldRef)", builder: mem)
            appendDebugPrintRegisterAligned4(X.x2, prepend: "\(prependHeader) field offset: \(fieldOffsetInBytes) bytes", builder: mem)
            
            // add field offset to base
            mem.append(M1Op.add(X.x1, X.x1, .r64shift(X.x2, .lsl(0))))
            appendDebugPrintRegisterAligned4(X.x1, prepend: "\(prependHeader) field \(fieldRef)", builder: mem)
            
            // field source is pointer to a pointer, so we need to dereference it once before
            // we check if it is null or not (and if null - don't use)
            mem.append(M1Op.ldr(X.x1, .reg(X.x1, .imm(0, nil))))
            appendDebugPrintRegisterAligned4(X.x1, prepend: "\(prependHeader) dereferenced address", builder: mem)
                        
            // compare x1 to 0
            var jmpTarget_hlvfieldNoAddress = RelativeDeferredOffset()
            var jmpTarget_postCheck = RelativeDeferredOffset()
            mem.append(M1Op.movz64(X.x2, 0, nil))
            mem.append(M1Op.cmp(X.x1, X.x2))
            
            mem.appendWithOffset(offset: &jmpTarget_hlvfieldNoAddress, PseudoOp.b_eq_deferred(jmpTarget_hlvfieldNoAddress))
            appendDebugPrintAligned4("\(prependHeader) HAS ADDRESS", builder: mem)
            
            // load field value into x2
            appendLoad(2, as: dstReg, addressRegister: X.x1, offset: 0, kinds: regs, mem: mem)
            appendStore(2, into: dstReg, kinds: regs, mem: mem)
            appendDebugPrintRegisterAligned4(2, kind: dstType.kind, prepend: "\(prependHeader) result", builder: mem)
            
            // finish this branch
            mem.appendWithOffset(offset: &jmpTarget_postCheck, M1Op.b(jmpTarget_postCheck))
            
            // marker for other branch
            jmpTarget_hlvfieldNoAddress.stop(at: mem.byteSize)
            
            appendDebugPrintAligned4("\(prependHeader) HAS NO ADDRESS", builder: mem)
            
            var _fieldHashGetter: (@convention(c)(OpaquePointer, Int32)->(Int64)) = {
                opPtr, field in
                
                let p: UnsafePointer<vvirtual> = .init(opPtr)
                
                return p.pointee.t.pointee.virt.pointee.fields.advanced(by: Int(field)).pointee.hashedName
            }
            
            // fetch the field hash name
            appendLoad(reg: X.x0, from: objReg, kinds: regs, mem: mem)
            mem.append(M1Op.movz64(X.x1, UInt16(fieldRef), nil))
            mem.append(
                PseudoOp.mov(X.x2, unsafeBitCast(_fieldHashGetter, to: OpaquePointer.self)),
                M1Op.blr(X.x2)
            )
            
            // x1 = field hash name
            mem.append(M1Op.movr64(X.x0, X.x1))
            // x0 = obj
            appendLoad(reg: X.x0, from: objReg, kinds: regs, mem: mem)
            // x2 = dst type (only needed for f32/f64)
            if FP_TYPE_KINDS.contains(dstType.kind) {
                // TODO: missing test coverage here (if you switch X.x2 to anything else, it should fail tests)
                try Self.appendLoadTypeMemory(X.x2, reg: dstReg, regs: regs, mem: mem, ctx: ctx)
            }
            
            mem.append(
                PseudoOp.mov(X.x10, getFunc),
                M1Op.blr(X.x10))
            
            appendStore(0, into: dstReg, kinds: regs, mem: mem)
            appendDebugPrintRegisterAligned4(X.x0, prepend: "\(prependHeader) result", builder: mem)
            
            jmpTarget_postCheck.stop(at: mem.byteSize)
            appendDebugPrintAligned4("\(prependHeader) EXITING", builder: mem)
        default:
            fatalError("OField not implemented for \(objRegKind)")
        }
        
        let dstKind = requireTypeKind(reg: dstReg, from: regs)
        let finalPrepend: String
        if let _callsite = _callsite {
            finalPrepend = "(fun@\(_callsite.findex); op: #\(_callsite.opnum)) OGetThis/OField result"
        } else {
            finalPrepend = "OGetThis/OField result"
            
        }
        appendDebugPrintRegisterAligned4(0, kind: dstKind, prepend: finalPrepend, builder: mem)
    }
    
    /// Reusable implem for OSetThis and OSetField
    func __osetthis_osetfield(
        objReg: Reg,
        fieldRef: RefField,
        srcReg: Reg,
        regs: [any HLTypeProvider],
        mem: CpuOpBuffer
    ) throws {
        let objRegKind = requireTypeKind(reg: objReg, from: regs)

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

        switch(objRegKind.kind) {
        case .obj: fallthrough
        case .struct:
            // offset from obj address
            let fieldOffset = requireFieldOffset(fieldRef: fieldRef, objIx: objReg, regs: regs)

            appendLoad(reg: X.x0, from: srcReg, kinds: regs, mem: mem)
            appendLoad(reg: X.x1, from: objReg, kinds: regs, mem: mem)
            
            // force HLTypeKind.dyn to always store the full 8 bytes
            // (technically not needed, but appendStore will complain if we
            // specify X.x0 when register expects W.w0)
            appendStore(reg: X.x0, as: 0, intoAddressFrom: X.x1, offsetFromAddress: fieldOffset, kinds: [HLTypeKind.dyn], mem: mem)
        case .virtual:
            // TODO: deduplicate across ocallmethod
            /*
             typedef struct _vvirtual vvirtual;
             struct _vvirtual {
                 hl_type *t;
                 vdynamic *value;
                 vvirtual *next;
             };

             #define hl_vfields(v) ((void**)(((vvirtual*)(v))+1))
             */
            /* ASM for -->
             f( hl_vfields(o)[f] )
                *hl_vfields(o)[f] = v;
             else
                hl_dyn_set(o,hash(field),vt,v);
             */
            
            let srcType = requireTypeKind(reg: srcReg, from: regs)
            let setFunc = get_dynset(from: srcType.kind)
                
            // x0 -> points to ((*_vvirtual)(obj))+1
            appendLoad(reg: X.x0, from: objReg, kinds: regs, mem: mem)
            appendDebugPrintRegisterAligned4(X.x0, prepend: "osetfield/virtual", builder: mem)
            
            // x1 point to field base (right after the (vvirtual) content at x0
            mem.append(M1Op.add(X.x1, X.x0, .imm(Int64(MemoryLayout<vvirtual>.stride), nil)))
            appendDebugPrintRegisterAligned4(X.x1, prepend: "osetfield/virtual field base", builder: mem)
            
            // x2 load field index multiplied by size of (void*)
            let fieldOffsetInBytes = fieldRef * MemoryLayout<OpaquePointer>.stride
            mem.append(M1Op.movz64(X.x2, UInt16(fieldOffsetInBytes), nil))
            appendDebugPrintAligned4("ofield/virtual field index: \(fieldRef)", builder: mem)
            appendDebugPrintRegisterAligned4(X.x2, prepend: "osetfield/virtual field offset: \(fieldOffsetInBytes) bytes", builder: mem)
            
            // add field offset to base
            mem.append(M1Op.add(X.x1, X.x1, .r64shift(X.x2, .lsl(0))))
            appendDebugPrintRegisterAligned4(X.x1, prepend: "osetfield/virtual field \(fieldRef)", builder: mem)
            
            // field source is pointer to a pointer, so we need to dereference it once before
            // we check if it is null or not (and if null - don't use)
            mem.append(M1Op.ldr(X.x1, .reg(X.x1, .imm(0, nil))))
            appendDebugPrintRegisterAligned4(X.x1, prepend: "osetfield/virtual dereferenced address", builder: mem)
            
            // compare x1 to 0
            var jmpTarget_hlvfieldNoAddress = RelativeDeferredOffset()
            var jmpTarget_postCheck = RelativeDeferredOffset()
            mem.append(M1Op.movz64(X.x2, 0, nil))
            mem.append(M1Op.cmp(X.x1, X.x2))
            
            mem.appendWithOffset(offset: &jmpTarget_hlvfieldNoAddress, PseudoOp.b_eq_deferred(jmpTarget_hlvfieldNoAddress))
            appendDebugPrintAligned4("osetfield/virtual HAS ADDRESS", builder: mem)
            
            // load field value into x2 and store at x1
            appendLoad(2, from: srcReg, kinds: regs, mem: mem)
            appendStore(2, as: srcReg, intoAddressFrom: X.x1, offsetFromAddress: 0, kinds: regs, mem: mem)
            appendDebugPrintRegisterAligned4(2, kind: srcType, prepend: "osetfield/virtual src", builder: mem)
            appendDebugPrintRegisterAligned4(1, kind: srcType, prepend: "osetfield/virtual target", builder: mem)
            
            // finish this branch
            mem.appendWithOffset(offset: &jmpTarget_postCheck, M1Op.b(jmpTarget_postCheck))
            
            // marker for other branch
            jmpTarget_hlvfieldNoAddress.stop(at: mem.byteSize)
            
            appendDebugPrintAligned4("osetfield/virtual HAS NO ADDRESS - not implemented", builder: mem)
            appendSystemExit(1, builder: mem)
            
            jmpTarget_postCheck.stop(at: mem.byteSize)
            appendDebugPrintAligned4("osetfield/virtual EXITING", builder: mem)
        default:
            fatalError("OSetField not implemented for \(objRegKind)")
        }
    }
}
