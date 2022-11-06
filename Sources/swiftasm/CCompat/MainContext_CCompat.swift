/**
 typedef struct {
     hl_code *code;
     hl_module *m;
     vdynamic *ret;
     pchar *file;
     int file_time;
 } main_context;
 */
struct MainContext_CCompat : Equatable, Hashable {
    var code: UnsafePointer<HLCode_CCompat>? = nil
    var m: UnsafePointer<HLModule_CCompat>? = nil
    var ret: UnsafeMutableRawPointer? = nil
    var file: UnsafePointer<CChar>? = nil
    var file_time: Int32 = 0
}
