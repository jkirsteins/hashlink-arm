import Darwin

struct LibHl {
    static let handle = dlopen("libhl.dylib", RTLD_NOW)
    
    static func load<T>(_ name: String) -> T {
        guard 
            let handle = self.handle, 
            let nat: UnsafeMutableRawPointer = dlsym(handle, name) else {
            fatalError("Could not load \(name)")
        }

        return unsafeBitCast(nat, to: T.self)
    }

    static let hl_global_free: (@convention(c) () -> ()) = { load("hl_global_free") }()
    static let hl_global_init: (@convention(c) () -> ()) = { load("hl_global_init") }()

    static let hl_utf8_length: (@convention(c) (UnsafePointer<Int8>?, Int) -> (Int)) = { load("hl_utf8_length") }()
    static func hl_utf8_length(_ val: String, pos: Int = 0) -> Int {
        return val.withCString {
            return hl_utf8_length($0, pos)
        }
    }

    static let hl_ucs2length: (@convention(c) (UnsafePointer<UniChar>?, Int) -> Int) = { load("hl_ucs2length") }()
    
    static let hl_to_utf16: (@convention(c) (UnsafePointer<Int8>?) -> UnsafeMutablePointer<UniChar>) = { load("hl_to_utf16") }()
    static func hl_to_utf16(_ val: String) -> UnsafeMutablePointer<UniChar> {
        return val.withCString {
            charPtr in 

            return hl_to_utf16(charPtr)
        }
    }
}
