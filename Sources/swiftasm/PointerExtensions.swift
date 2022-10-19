extension UnsafeRawPointer {
    func boundPointee<T>() -> T {
        self.bindMemory(to: T.self, capacity: 1).pointee
    }
}