struct HLType_CCompat_Obj : Equatable, Hashable, CustomDebugStringConvertible {
    let nfields: Int32
    let nproto: Int32
    let nbindings: Int32

    let namePtr: UnsafeMutableRawPointer // uchar*
    let superTypePtr: UnsafeMutableRawPointer?

    // hl_obj_field *fields;
    let fieldsPtr: UnsafePointer<HLObjField_CCompat>
    // hl_obj_proto *proto;
    let proto: UnsafeMutableRawPointer

    // int *bindings;
    let bindings: UnsafeMutableRawPointer

    // void **global_value;
    let globalValue: UnsafeMutableRawPointer

    // hl_module_context *m;
    let moduleContext: UnsafeMutableRawPointer

    // hl_runtime_obj *rt;
    let rt: UnsafeMutableRawPointer

    var superType: HLType_CCompat? {
        printerr("Testing a")
        guard let superTypePtr = self.superTypePtr else { return nil }
        printerr("Testing b")
        return superTypePtr.bindMemory(to: HLType_CCompat.self, capacity: 1).pointee
    }

    var fields: [HLObjField_CCompat] {
        let buf = UnsafeBufferPointer(start: fieldsPtr, count: Int(nfields))
        return Array(buf)
    }

    var name: String { .wrapUtf16(from: namePtr) }   

    var debugDescription: String { 
        ".obj(\(name))"
    }
}