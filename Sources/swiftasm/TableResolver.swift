

class TableResolver<T: CustomDebugStringConvertible> {
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
            fatalError("<value not available yet>")
        }
        return self.table[index]
    }

    func getResolvable(_ index: TableIndex) -> Resolvable<T> {
        Resolvable(ix: index, table: self)
    }
}

typealias TableIndex = Int

struct Resolvable<T: CustomDebugStringConvertible> : CustomDebugStringConvertible {
    let ix: TableIndex
    let table: TableResolver<T>

    var value: T {
        table.get(ix)
    }

    var debugDescription: String {
        return "\(ix) : \(value.debugDescription)"
    }
}

