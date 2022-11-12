fileprivate var cache: [Int:HLTypeObj] = [:]

extension HLTypeObj {
    static func fromPointer(_ ccompat: UnsafePointer<HLTypeObj_CCompat>) -> HLTypeObj {
        let addr = Int(bitPattern: ccompat)
        guard let result = cache[addr] else {
            let inst = HLTypeObj(unsafe: ccompat.pointee)
            cache[addr] = inst
            return inst
        }
        
        return result
    }
    
    fileprivate convenience init(unsafe ccompat: HLTypeObj_CCompat) {
        let name = Resolvable(ccompat.name, memory: ccompat.namePtr)
        let superType: Resolvable<HLType>?
        let global: Int32?
        
        if let ptr = ccompat.superPtr {
            superType = .type(fromUnsafe: ptr)
            
        } else {
            superType = nil
        }
        
        if ccompat.globalValue != 0 {
            global = ccompat.globalValue - 1
        } else {
            global = nil
        }
        
        let fields = ccompat.fields.enumerated().map { ix, item in
            let ptr = ccompat.fieldsPtr!.advanced(by: ix)
            return Resolvable.objField(fromUnsafe: ptr)
        }
        let proto = ccompat.proto.enumerated().map { ix, item in
            let ptr = ccompat.protoPtr!.advanced(by: ix)
            return Resolvable.objProto(fromUnsafe: ptr)
        }
        
        let bindings = ccompat.bindings.chunked(into: 2).map {
            guard $0.count == 2 else {
                fatalError("Odd number of binding values")
            }
            return HLTypeBinding(fieldRefIx: $0[0], functionIx: $0[1])
        }
        self.init(name: name, superType: superType, global: global, fields: fields, proto: proto, bindings: bindings)
    }
}

struct HLTypeObj_CCompat : Equatable, Hashable, CustomDebugStringConvertible {
    let nfields: Int32
    let nproto: Int32
    let nbindings: Int32
    
    let namePtr: UnsafePointer<CChar16> // uchar*
    let superPtr: UnsafePointer<HLType_CCompat>?
    
    // hl_obj_field *fields;
    let fieldsPtr: UnsafePointer<HLObjField_CCompat>?
    // hl_obj_proto *proto;
    let protoPtr: UnsafePointer<HLObjProto_CCompat>?
    
    // int *bindings;
    let bindingsPtr: UnsafePointer<Int32>?
    
    // void **global_value;
    let globalValue: Int32
    
    // hl_module_context *m;
    let moduleContext: UnsafeMutableRawPointer?
    
    // hl_runtime_obj *rt;
    // NOTE: you should never access this directly, as it might not be initialized
    let _rtDontAccess: UnsafePointer<HLRuntimeObj_CCompat>?
    
    func getRt(_ type: UnsafePointer<HLType_CCompat>) -> UnsafePointer<HLRuntimeObj_CCompat> {
        LibHl.hl_get_obj_rt(type)
    }
    
    var `super`: HLType_CCompat? {
        guard let rawPtr = self.superPtr else {
            return nil
        }
        return rawPtr.pointee
        
        //        guard let rawPtr = self.superPtr else {
        //            return nil
        //        }
        //        return rawPtr.bindMemory(to: HLType_CCompat.self, capacity: 1).pointee
        //
        //        // if let fails doesn't work, returns nil (even if value present)
        //        // TODO: figure out
        //        guard let superPtr = self.superPtr else {
        //            return nil
        //        }
        //
        //        return self.superPtr!.pointee
    }
    
    var bindings: [Int32] {
        // bindings consist of 2 int32 per 1 binding,
        // so number of values is nbindings*2
        // https://github.com/jkirsteins/hashlink/blob/metal/src/code.c
        let buf = UnsafeBufferPointer(
            start: bindingsPtr,
            count: Int(nbindings) * 2)
        return Array(buf)
    }
    
    var fields: [HLObjField_CCompat] {
        let buf = UnsafeBufferPointer(start: fieldsPtr, count: Int(nfields))
        return Array(buf)
    }
    
    var proto: [HLObjProto_CCompat] {
        let buf = UnsafeBufferPointer(start: protoPtr, count: Int(nproto))
        return Array(buf)
    }
    
    var name: String { .wrapUtf16(from: namePtr) }
    
    var debugDescription: String {
        ".obj(\(name))"
    }
}
