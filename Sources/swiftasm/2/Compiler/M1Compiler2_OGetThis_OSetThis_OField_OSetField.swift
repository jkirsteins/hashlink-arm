extension M1Compiler2 {
    /// Reusable implem for OGetThis and OField
    func __ogetthis_ofield(
        dstReg: Reg,
        objReg: Reg,
        fieldRef: RefField,
        regs: [any HLTypeProvider],
        mem: CpuOpBuffer
    ) throws {
        let objRegKind = requireTypeKind(reg: objReg, from: regs)
        
        /* See comments on `OSetField` for notes on accessing field indexes */

        switch(objRegKind) {
        case .obj: fallthrough
        case .struct:
            appendDebugPrintAligned4("OField for obj/struct", builder: mem)
            // offset from obj address
            let fieldOffset = requireFieldOffset(fieldRef: fieldRef, objIx: objReg, regs: regs)
            appendDebugPrintAligned4("field offset \(fieldOffset) for \(objReg)", builder: mem)
            appendLoad(reg: X.x0, from: objReg, kinds: regs, mem: mem)
            appendDebugPrintAligned4("loading...", builder: mem)
            mem.append(M1Op.ldr(X.x1, .reg64offset(.x0, fieldOffset, nil)))
            appendDebugPrintAligned4("storing...", builder: mem)
            appendStore(reg: X.x1, into: dstReg, kinds: regs, mem: mem)
//
            // --- tmp
            let _c: (@convention(c)(OpaquePointer)->()) = {
                oPtr in
                
                let p: UnsafePointer<vdynamic> = .init(oPtr)
                print(p)
            }
            mem.append(PseudoOp.mov(X.x9, unsafeBitCast(_c, to: OpaquePointer.self)))
            mem.append(M1Op.blr(X.x9))
            // --- tmp
            
        case .virtual:
            let dstType = requireTypeKind(reg: dstReg, from: regs)
            let castFunc = get_dynget(to: dstType.kind)
                
            // x0 -> points to ((*_vvirtual)(obj))+1
            appendLoad(reg: X.x0, from: objReg, kinds: regs, mem: mem)
            mem.append(M1Op.add(X.x0, X.x0, .imm(Int64(MemoryLayout<vvirtual>.stride), nil)))
            
            // x0 -> [x0 + field offset into values]
//            mem.append(PseudoOp.mov(X.x1, fieldRef * MemoryLayout<UnsafePointer<vdynamic>>.stride))
//            mem.append(M1Op.add(X.x0, X.x0, .r64shift(X.x1, .lsl(0))))
            
            appendDebugPrintRegisterAligned4(X.x0, prepend: "ofield virtual", builder: mem)
            
            // compare x0 to 0
            var jmpTarget_hlvfieldNoAddress = RelativeDeferredOffset()
            var jmpTarget_postCheck = RelativeDeferredOffset()
            mem.append(M1Op.movz64(X.x1, 0, nil))
            mem.append(M1Op.cmp(X.x0, X.x1))
            
            mem.append(
                PseudoOp.withOffset(
                    offset: &jmpTarget_hlvfieldNoAddress,
                    mem: mem,
                    M1Op.b_eq(try! Immediate19(jmpTarget_hlvfieldNoAddress.value))
                )
            )
            appendDebugPrintAligned4("ofield virtual HAS ADDRESS", builder: mem)
            
            // field source is pointer to a pointer, so we need to dereference it once before
            mem.append(M1Op.ldr(X.x0, .reg(X.x0, .imm(0, nil))))
            
            // test
            let _c: (@convention(c) (OpaquePointer)->()) = {
                opPtr in
                
                let p: UnsafePointer<vvirtual>? = .init(opPtr)
                
//                Swift.assert(Int(bitPattern: p) - Int(bitPattern: p1) == 24)
                print("r1", p)
//                print("r2", p.pointee)
                
//                let p: UnsafePointer<vdynamic> = .init(opPtr)
//                print("p", p.pointee.t.pointee.kind._overrideDebugDescription)
//                print("p", p.pointee.next)
//                print("p", p.pointee.t.pointee.kind._overrideDebugDescription)
            }

//            mem.append(PseudoOp.mov(X.x10, unsafeBitCast(_c, to: OpaquePointer.self)), M1Op.blr(X.x10))
            //
            
            
            //
            switch(dstType.hlRegSize) {
            case 8:
                mem.append(M1Op.ldr(X.x1, .reg(X.x0, .imm(0, nil))))
            case 4:
                mem.append(M1Op.ldr(W.w1, .reg(X.x0, .imm(0, nil))))
            case 2:
                mem.append(M1Op.ldrh(W.w1, .reg(X.x0, .imm(0, nil))))
            case 1:
                mem.append(M1Op.ldrb(W.w1, .reg(X.x0, .imm(0, nil))))
            default:
                fatalError("ofield virtual Not implemented")
            }
            appendStore(reg: X.x1, into: dstReg, kinds: regs, mem: mem)
            appendDebugPrintRegisterAligned4(X.x1, prepend: "got it", builder: mem)
//            appendSystemExit(123, builder: mem)
            
            // finish this branch
            mem.append(
                PseudoOp.withOffset(
                    offset: &jmpTarget_postCheck,
                    mem: mem,
                    M1Op.b(jmpTarget_postCheck)
                )
            )
            
            // marker for other branch
            jmpTarget_hlvfieldNoAddress.stop(at: mem.byteSize)
            
            appendDebugPrintAligned4("ofield virtual HAS NO ADDRESS", builder: mem)
            appendSystemExit(11, builder: mem)
            
            jmpTarget_postCheck.stop(at: mem.byteSize)
            appendDebugPrintAligned4("ofield virtual EXITING", builder: mem)
        default:
            fatalError("OField not implemented for \(objRegKind)")
        }
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
            mem.append(M1Op.str(X.x0, .reg64offset(.x1, fieldOffset, nil)))
        case .virtual:
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
            let castFunc = get_dynset(from: srcType.kind)
                
            // x0 -> points to ((*_vvirtual)(obj))+1
            appendLoad(reg: X.x0, from: objReg, kinds: regs, mem: mem)
            mem.append(M1Op.add(X.x0, X.x0, .imm(Int64(MemoryLayout<vvirtual>.stride), nil)))
            
//            // x0 -> [x0 + field offset into values]
//            mem.append(PseudoOp.mov(X.x1, fieldRef * MemoryLayout<UnsafePointer<vdynamic>>.stride))
//            mem.append(M1Op.add(X.x0, X.x0, .r64shift(X.x1, .lsl(0))))
            
            appendDebugPrintRegisterAligned4(X.x0, prepend: "osetfield virtual", builder: mem)
            
            // compare x0 to 0
            var jmpTarget_hlvfieldNoAddress = RelativeDeferredOffset()
            var jmpTarget_postCheck = RelativeDeferredOffset()
            mem.append(M1Op.movz64(X.x1, 0, nil))
            mem.append(M1Op.cmp(X.x0, X.x1))
            
            mem.append(
                PseudoOp.withOffset(
                    offset: &jmpTarget_hlvfieldNoAddress,
                    mem: mem,
                    M1Op.b_eq(try! Immediate19(jmpTarget_hlvfieldNoAddress.value))
                )
            )
            appendDebugPrintAligned4("osetfield virtual HAS ADDRESS", builder: mem)
            
            // field source is pointer to a pointer, so we need to dereference it once before
            mem.append(M1Op.ldr(X.x0, .reg(X.x0, .imm(0, nil))))
            
            // test
            let _c: (@convention(c) (OpaquePointer)->()) = {
                opPtr in
                
                let p1: UnsafePointer<vvirtual> = .init(opPtr)
                let p = p1.advanced(by: 0)
                
//                Swift.assert(Int(bitPattern: p) - Int(bitPattern: p1) == 24)
                
//                let p: UnsafePointer<vdynamic> = .init(opPtr)
//                print("p", p.pointee.t.pointee.kind._overrideDebugDescription)
//                print("p", p.pointee.next)
//                print("p", p.pointee.t.pointee.kind._overrideDebugDescription)
            }
//            /*TODO*/appendLoad(reg: X.x0, from: objReg, kinds: regs, mem: mem)
//            /*TODO*/appendLoad(reg: X.x1, from: srcReg, kinds: regs, mem: mem)
//            appendDebugPrintRegisterAligned4(X.x0, prepend: "osetfield 2", builder: mem)
//            mem.append(PseudoOp.mov(X.x10, unsafeBitCast(_c, to: OpaquePointer.self)), M1Op.blr(X.x10))
            //
            
            //
            appendLoad(reg: X.x1, from: srcReg, kinds: regs, mem: mem)
            switch(srcType.hlRegSize) {
            case 8:
                mem.append(M1Op.str(X.x1, .reg(X.x0, .imm(0, nil))))
            case 4:
                mem.append(M1Op.str(W.w1, .reg(X.x0, .imm(0, nil))))
            case 2:
                mem.append(M1Op.strh(W.w1, .reg(X.x0, .imm(0, nil))))
            case 1:
                mem.append(M1Op.strb(W.w1, .reg(X.x0, .imm(0, nil))))
            default:
                fatalError("osetfield virtual Not implemented")
            }
            
            // finish this branch
            mem.append(
                PseudoOp.withOffset(
                    offset: &jmpTarget_postCheck,
                    mem: mem,
                    M1Op.b(jmpTarget_postCheck)
                )
            )
            
            // marker for other branch
            jmpTarget_hlvfieldNoAddress.stop(at: mem.byteSize)
            
            appendDebugPrintAligned4("osetfield virtual HAS NO ADDRESS", builder: mem)
            appendSystemExit(11, builder: mem)
            
            jmpTarget_postCheck.stop(at: mem.byteSize)
            appendDebugPrintAligned4("osetfield virtual EXITING", builder: mem)
        default:
            fatalError("OSetField not implemented for \(objRegKind)")
        }
    }
}
