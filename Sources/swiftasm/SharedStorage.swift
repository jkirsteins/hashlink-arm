@propertyWrapper public struct SharedStorage<T: Equatable> : Equatable {
    public init(wrappedValue: T) {
        storage = .init(value: wrappedValue)
    }
    
    private let storage: Storage
    
    public var wrappedValue: T {
        nonmutating get { storage.value }
        nonmutating set { storage.value = newValue }
    }
    
    private class Storage : Equatable {
        init(value: T) {
            self.value = value
        }
        
        var value: T

        public static func == (lhs: SharedStorage<T>.Storage, rhs: SharedStorage<T>.Storage) -> Bool {
            return lhs.value == rhs.value
        }
    }

    public static func == (lhs: Self, rhs: Self) -> Bool {
        return lhs.storage == rhs.storage
    }
}