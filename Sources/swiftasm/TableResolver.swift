

class TableResolver<T: CustomDebugStringConvertible>: Hashable, Equatable where T: Equatable, T: Hashable {
    @SharedStorage var table: [T]
    let count: Int32

    init(table: SharedStorage<[T]>, count: Int32) {
        self._table = table
        self.count = count
    }

    func get(_ index: TableIndex) -> T {
        guard index < self.count else {
            fatalError("Fetching index \(index) but count is \(count) for \(T.self)")
        }
        guard self.table.count > index else {
            fatalError("<value not available yet - fetching \(index) from \(T.self)>")
        }
        return self.table[index]
    }

    var storage: SharedStorage<[T]> { self._table }

    func getResolvable(_ index: TableIndex) -> Resolvable<T> {
        Resolvable(ix: index, table: self)
    }

    func hash(into hasher: inout Hasher) {
        table.hash(into: &hasher)
        count.hash(into: &hasher)
    }

    static func == (lhs: TableResolver, rhs: TableResolver) -> Bool {
        return lhs.count == rhs.count  &&
            lhs.table == rhs.table
    }
}

typealias TableIndex = Int

struct Resolvable<T: CustomDebugStringConvertible> : Equatable, CustomDebugStringConvertible, Hashable where T: Equatable, T: Hashable {
    let ix: TableIndex
    let table: TableResolver<T>

    static func array(_ val: [T]) -> [Resolvable<T>] {
        let table = TableResolver(table: SharedStorage(wrappedValue: val), count: Int32(val.count))
        return val.enumerated().map { (ix, _) in
            table.getResolvable(ix)
        }
    }
    
    init(_ val: T) {
        self.ix = 0
        self.table = TableResolver(table: SharedStorage(wrappedValue: [val]), count: 1)
    }

    init(ix: TableIndex, table: TableResolver<T>) {
        self.ix = ix
        self.table = table
    }

    var value: T {
        table.get(ix)
    }

    var debugDescription: String {
        return "\(ix) : \(value.debugDescription)"
    }

    static func == (lhs: Resolvable, rhs: Resolvable) -> Bool {
        return lhs.ix == rhs.ix && lhs.table == rhs.table
    }
}

