extension String {
    static func wrapUtf16(from ptr: UnsafeMutableRawPointer) -> String {
        _wrapUtf16(from: UnsafePointer(OpaquePointer(ptr)))
    }

    static func wrapUtf16(from ptr: UnsafeRawPointer) -> String {
        _wrapUtf16(from: UnsafePointer(OpaquePointer(ptr)))
    } 

    static func _wrapUtf16(from ptr: UnsafePointer<CChar16>) -> String {
        let chars = LibHl.hl_ucs2length(UnsafePointer(OpaquePointer(ptr)), 0)
        let bytes = chars * 2
        
        guard let result = String(bytesNoCopy: UnsafeMutablePointer(OpaquePointer(ptr)), length: bytes, encoding: .utf16LittleEndian, freeWhenDone: false) else {
            fatalError("Could not initialize String from memory")
        }
        return result
    }

    static func wrapUtf8(from ptr: UnsafePointer<CChar>) -> String {
        let chars = LibHl.hl_utf8_length(UnsafePointer(OpaquePointer(ptr)), 0)
        let bytes = chars
        
        guard let result = String(bytesNoCopy: UnsafeMutablePointer(OpaquePointer(ptr)), length: bytes, encoding: .utf8, freeWhenDone: false) else {
            fatalError("Could not initialize String from memory")
        }
        return result
    }
}