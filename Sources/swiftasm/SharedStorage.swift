@propertyWrapper public struct SharedStorage<T> {
    public init(wrappedValue: T) {
        storage = .init(value: wrappedValue)
    }
    
    private let storage: Storage
    
    public var wrappedValue: T {
        nonmutating get { storage.value }
        nonmutating set { storage.value = newValue }
    }
    
    private class Storage {
        init(value: T) {
            self.value = value
        }
        
        var value: T
    }
}