import Foundation

class CCompatWriter_HLTypeObj {
    let ctx: any JitContext2
    let objDataIn: any HLTypeObjProvider
    
    let objNameData: Data
    let fieldsPtr: UnsafeMutableBufferPointer<HLObjField_CCompat>
    
    let fieldWriters: CCompatWriter_HLObjFields
    let objNamePtr: UnsafeMutableBufferPointer<UInt8>   // actually CChar16
    
    init(_ ctx: any JitContext2, obj: any HLTypeObjProvider, typeLookup: TypeLookupHelper) throws {
        self.ctx = ctx
        self.objDataIn = obj
        self.fieldsPtr = .allocate(capacity: objDataIn.fieldsProvider.count)
        
        self.fieldWriters = try CCompatWriter_HLObjFields(ctx, fields: objDataIn.fieldsProvider, typeLookup: typeLookup)
        
        self.objNameData = (self.objDataIn.nameProvider.stringValue + "\0").data(using: .utf16LittleEndian)!
        self.objNamePtr = .allocate(capacity: self.objNameData.count)
    }
    
    deinit {
        self.fieldsPtr.deallocate()
        self.objNamePtr.deallocate()
    }
    
    func initialize(target: UnsafeMutablePointer<HLTypeObj_CCompat>) throws {
        // We might have a missing dependency if we're serializing a type that depends
        // on a yet-unserialized type. Throw, skip, and do a second pass later.
        
        try fieldWriters.initialize(target: fieldsPtr.baseAddress!)
        
        _ = objNamePtr.initialize(from: self.objNameData)
        
        target.initialize(to: HLTypeObj_CCompat(
            nfields: Int32(objDataIn.fieldsProvider.count),
            nproto: 0,
            nbindings: 0,
            namePtr: .init(OpaquePointer(objNamePtr.baseAddress!)),
            superPtr: nil,
            fieldsPtr: .init(fieldsPtr.baseAddress!),
            protoPtr: nil,
            bindingsPtr: nil,
            globalValue: -1,
            moduleContext: nil,
            _rtDontAccess: nil))
    }
}
