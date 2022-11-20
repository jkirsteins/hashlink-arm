/**
 Enum
 
        typedef struct _venum {
            hl_type *t;
            int index;
        } venum;
 */

struct venum: Hashable, Equatable, OverrideCustomDebugStringConvertible {
    
    
    let t: UnsafePointer<HLType_CCompat>
    let index: Int32
    // we need the struct size to be aligned to 8 to
    // match C
    let __pad: Int32
    
    var _overrideDebugDescription: String {
        "venum<\(t._overrideDebugDescription); \(index)>"
    }
    
    var debugDescription: String {
        _overrideDebugDescription
    }
}
