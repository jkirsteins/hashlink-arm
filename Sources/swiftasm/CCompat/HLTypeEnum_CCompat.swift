/**
 Enum
 
        typedef struct {
            const uchar *name;
            int nconstructs;
            hl_enum_construct *constructs;
            void **global_value;
        } hl_type_enum;
 */
import Foundation

struct HLTypeEnum_CCompat : Equatable, Hashable, CustomStringConvertible {
    
    var description: String {
        "HLTypeEnum_CCompat"
    }
    
    let name: UnsafePointer<CChar16>
    let nconstructs: Int32
    let constructs: UnsafePointer<HLEnumConstruct_CCompat>
    let global_value: UnsafeRawPointer
}

extension HLTypeEnum_CCompat : HLTypeEnumProvider {
    var nameProvider: StringProvider {
        self.name
    }
    
    var constructsProvider: [any HLEnumConstructProvider] {
        Array(UnsafeBufferPointer(start: self.constructs, count: Int(nconstructs)))
    }
}
