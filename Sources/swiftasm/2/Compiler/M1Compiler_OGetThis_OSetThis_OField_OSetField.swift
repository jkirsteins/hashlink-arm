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
            // offset from obj address
            let fieldOffset = requireFieldOffset(fieldRef: fieldRef, objIx: objReg, regs: regs)
            appendLoad(reg: X.x0, from: objReg, kinds: regs, mem: mem)
            mem.append(M1Op.ldr(X.x1, .reg64offset(.x0, fieldOffset, nil)))
            appendStore(reg: X.x1, into: dstReg, kinds: regs, mem: mem)
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
        default:
            fatalError("OSetField not implemented for \(objRegKind)")
        }
    }
}
