import Foundation

class CCompatWriter_HLObjField {
    let ctx: any JitContext2
    let fieldType: any HLTypeProvider
    let fieldNameData: Data
    
    let typeLookup: TypeLookupHelper
    let fieldNamePtr: UnsafeMutableBufferPointer<UInt8> // real target is CChar16 but we'll get the bytes from Data as CChar
    
    init(_ ctx: any JitContext2, field: any HLObjFieldProvider, typeLookup: TypeLookupHelper) throws {
        self.ctx = ctx
        self.fieldNameData = (field.nameProvider.stringValue + "\0").data(using: .utf16LittleEndian)!
        self.fieldType = field.typeProvider
        self.typeLookup = typeLookup
        self.fieldNamePtr = .allocate(capacity: self.fieldNameData.count)
    }
    
    deinit {
        self.fieldNamePtr.deallocate()
    }
    
    func initialize(target: UnsafeMutablePointer<HLObjField_CCompat>) throws {
        // We might have a missing dependency if we're serializing a type that depends
        // on a yet-unserialized type. Throw, skip, and do a second pass later.
        
        _ = self.fieldNamePtr.initialize(from: self.fieldNameData)
        
        guard let fieldTypeResolved = try self.typeLookup.getCCompatType(type: fieldType) else {
            print("HLTypeObj has unserialized field type \(fieldType)")
            throw CCompatWriterError.missingDependency("HLTypeObj has unserialized field type \(fieldType)")
        }
        
        let nonMutableName: UnsafePointer<CChar16> = .init(OpaquePointer(fieldNamePtr.baseAddress!))
        target.initialize(to: HLObjField_CCompat(
            namePtr: nonMutableName,
            tPtr: fieldTypeResolved,
            hashedName: Int64(LibHl.hl_hash_gen(nonMutableName, true))
        ))

        print("Afterwards name value is: \(target.pointee.nameProvider.stringValue) for \(target)")
    }
}
