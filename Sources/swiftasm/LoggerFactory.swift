import os.log

func fatal(_ message: String, _ logger: Logger) -> Never {
    logger.critical("\(message)")
    fatalError(message)
}

class LoggerFactory {
    static func create(_ className: String) -> Logger {
        Logger.init(subsystem: "swiftasm", category: className)
    }
    
    static func create<T>(_ sender: T) -> Logger {
        Logger.init(subsystem: "swiftasm", category: String(reflecting: sender))
    }
}
