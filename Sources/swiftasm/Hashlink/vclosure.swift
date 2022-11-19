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
    
    
    let t: UnsafePointer<HLType_CCompat>
    let fun: OpaquePointer
    let hasValue: Int32
    let stackCount: Int32
    let value: OpaquePointer
    
    var _overrideDebugDescription: String {
        "vclosure<\(t._overrideDebugDescription); \(fun)>"
    }
    
    var debugDescription: String {
        _overrideDebugDescription
    }
}
