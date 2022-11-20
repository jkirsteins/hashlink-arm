/**
 Closure
 
        typedef struct _vclosure {
            hl_type *t;
            void *fun;
            int hasValue;
        #ifdef HL_64
            int stackCount;
        #endif
            void *value;
        } vclosure;
 */

struct vclosure: Hashable, Equatable, OverrideCustomDebugStringConvertible {
    
    var t: UnsafePointer<HLType_CCompat>
    var fun: OpaquePointer?
    var hasValue: Int32
    let stackCount: Int32
    var value: OpaquePointer?
    
    var _overrideDebugDescription: String {
        "vclosure<\(t._overrideDebugDescription); \(fun)>"
    }
    
    var debugDescription: String {
        _overrideDebugDescription
    }
}
