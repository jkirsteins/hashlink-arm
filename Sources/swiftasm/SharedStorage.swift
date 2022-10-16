@propertyWrapper public struct SharedStorage<T: Equatable> : Equatable, Hashable where T: Hashable {
    public init(wrappedValue: T) {
        storage = .init(value: wrappedValue)
    }
    
    private let storage: Storage
    
    public var wrappedValue: T {
        nonmutating get { storage.value }
        nonmutating set { storage.value = newValue }
    }

    public func isSameStorage(_ other: Self) -> Bool {
        self.storage == other.storage
    }
    
    private class Storage : Equatable, Hashable {
        init(value: T) {
            self.value = value
        }
        
        var value: T

        func hash(into hasher: inout Hasher) {
            value.hash(into: &hasher)
        }

        public static func == (lhs: SharedStorage<T>.Storage, rhs: SharedStorage<T>.Storage) -> Bool {
            return lhs.value == rhs.value
        }
    }

    public static func == (lhs: Self, rhs: Self) -> Bool {
        return lhs.storage == rhs.storage
    }
}