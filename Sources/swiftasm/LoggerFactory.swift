import os.log

func fatal(_ message: String, _ logger: Logger, file: StaticString = #filePath,
           line: UInt = #line) -> Never {
    logger.critical("\(message)")
    fatalError(message, file: file, line: line)
}

class LoggerFactory {
    static func create(_ className: String) -> Logger {
        Logger.init(subsystem: "swiftasm", category: className)
    }
    
    static func create<T>(_ sender: T) -> Logger {
        Logger.init(subsystem: "swiftasm", category: String(reflecting: sender))
    }
}
