
protocol OverrideCustomDebugStringConvertible: CustomDebugStringConvertible {
    var _overrideDebugDescription: String { get }
}

extension OverrideCustomDebugStringConvertible {
    var _overrideDebugDescription: String { self.debugDescription }
}

extension UnsafePointer<HLType_CCompat>: OverrideCustomDebugStringConvertible {
    var _overrideDebugDescription: String {
        if let funProvider = self.funProvider {
            let argsAsDebugArr: [any OverrideCustomDebugStringConvertible] = funProvider.argsProvider
            return "UnsafePointer<HLType_CCompat>(.fun; args: \(argsAsDebugArr._overrideDebugDescription); ret: \(funProvider.retProvider._overrideDebugDescription))"
        }
        if let objProvider = self.objProvider {
            return "UnsafePointer<HLType_CCompat>(.obj; \(objProvider.nameProvider.stringValue))"
        }
        return "UnsafePointer<HLType_CCompat>(\(self.kind.debugDescription))"
    }
}

extension Array: OverrideCustomDebugStringConvertible where Array.Element == any OverrideCustomDebugStringConvertible {
    var _overrideDebugDescription: String {
        return "arr[\(self.map { $0._overrideDebugDescription }.joined(separator: ", "))]"
    }
}
