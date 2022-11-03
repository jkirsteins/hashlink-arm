import Darwin

struct LibraryName : CustomDebugStringConvertible {
    let name: String 
    let resolvedName: String

    var debugDescription: String {
        "\(name)[->\(resolvedName)]"
    }
}

struct ResolvedLibrary {
    let handle: UnsafeMutableRawPointer
    let name: LibraryName

    static func resolved(name: String) -> String {
        let result: String
        switch(name) {
            case "std": result = "libhl"
            default: result = name
        }
        
        return "\(result).dylib"
    }

    init(name unresolvedName: String) {
        let rname = Self.resolved(name: unresolvedName)
        self.name = LibraryName(name: unresolvedName, resolvedName: rname)

        guard let h = dlopen(rname, RTLD_NOW/*RTLD_LAZY*/) else {
            fatalError("Could not load library \(rname)")
        }
        self.handle = h
    }

    func get(_ native: HLNative) -> UnsafeMutableRawPointer {        
        self.get(native.name.value)
    }

    func get(_ name: String) -> UnsafeMutableRawPointer {
        // Do NOT use hlp_. TODO: Understand what that is
        let realName = "hl_\(name)"
        let result = dlsym(self.handle, realName)

        guard let result = result else {
            fatalError("Could not find symbol \(realName) in \(self.name)")
        }
        print("Got function \(result) from \(realName) in \(self.name.resolvedName)")
        return result
    }
}
