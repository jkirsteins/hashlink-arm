/**
 Enum constract
 
    
        typedef struct {
            const uchar *name;
            int nparams;
            hl_type **params;
            int size;
            bool hasptr;
            int *offsets;
        } hl_enum_construct;
 */
import Foundation

struct HLEnumConstruct_CCompat : Equatable, Hashable, CustomStringConvertible {
    
    var description: String {
        "HLEnumConstruct_CCompat"
    }
    
    let name: UnsafePointer<CChar16>
    let nparams: Int32
    let params: UnsafePointer<UnsafePointer<HLType_CCompat>>
    let size: Int32
    let hasptr: Bool
    let offsets: UnsafePointer<Int32>
}

extension HLEnumConstruct_CCompat : HLEnumConstructProvider {
    var nameProvider: StringProvider {
        self.name
    }
}
