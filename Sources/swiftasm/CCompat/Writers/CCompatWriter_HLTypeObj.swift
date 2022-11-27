import Foundation

class CCompatWriter_HLTypeObj {
    let ctx: any JitContext2
    let objDataIn: any HLTypeObjProvider
    
    let objNameData: Data
    let fieldsPtr: UnsafeMutableBufferPointer<HLObjField_CCompat>
    
    let fieldWriters: CCompatWriter_HLObjFields
    let objNamePtr: UnsafeMutableBufferPointer<UInt8>   // actually CChar16
    
    let typeLookup: TypeLookupHelper
    
    init(_ ctx: any JitContext2, obj: any HLTypeObjProvider, typeLookup: TypeLookupHelper) throws {
        self.ctx = ctx
        self.objDataIn = obj
        self.fieldsPtr = .allocate(capacity: objDataIn.fieldsProvider.count)
        
        self.fieldWriters = try CCompatWriter_HLObjFields(ctx, fields: objDataIn.fieldsProvider, typeLookup: typeLookup)
        
        self.objNameData = (self.objDataIn.nameProvider.stringValue + "\0").data(using: .utf16LittleEndian)!
        self.objNamePtr = .allocate(capacity: self.objNameData.count)
        self.typeLookup = typeLookup
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
        
        let superTypePtr: UnsafePointer<HLType_CCompat>?
        if let superTypeProvider = objDataIn.superTypeProvider {
            guard let _superTypePtr = try typeLookup.getCCompatType(type: superTypeProvider) else {
                throw CCompatWriterError.missingDependency("Super type not serialized yet: \(superTypeProvider)")
            }
            superTypePtr = _superTypePtr
            print(objDataIn.nameProvider.stringValue, "has super")
        } else {
            superTypePtr = nil
            print(objDataIn.nameProvider.stringValue, "doesn't have super")
        }
        
        target.initialize(to: HLTypeObj_CCompat(
            nfields: Int32(objDataIn.fieldsProvider.count),
            nproto: 0,
            nbindings: 0,
            namePtr: .init(OpaquePointer(objNamePtr.baseAddress!)),
            superPtr: superTypePtr,
            fieldsPtr: .init(fieldsPtr.baseAddress!),
            protoPtr: nil,
            bindingsPtr: nil,
            globalValue: .init(bitPattern: -1)!,
            moduleContext: nil,
            _rtDontAccess: nil))
    }
}
