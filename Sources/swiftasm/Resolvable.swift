import Foundation

typealias TableIndex = Int

struct Resolvable<T: CustomDebugStringConvertible> : Equatable, CustomDebugStringConvertible, Hashable where T: Equatable, T: Hashable {
    let ix: TableIndex
    let table: TableResolver<T>
    
    // Where is the source of truth for this resolvable
    let memory: UnsafeRawPointer?

    static func array(_ val: [T]) -> [Resolvable<T>] {
        let table = TableResolver(table: SharedStorage(wrappedValue: val), count: Int32(val.count))
        return val.enumerated().map { (ix, _) in
            table.getResolvable(ix)
        }
    }
    
    init(_ val: T, memory: UnsafeRawPointer?) {
        self.ix = 0
        self.table = TableResolver(table: SharedStorage(wrappedValue: [val]), count: 1)
        self.memory = memory
    }
    
    init(_ val: T) {
        self.init(val, memory: nil)
    }
    
    init(ix: TableIndex, table: TableResolver<T>) {
        self.ix = ix
        self.table = table
        self.memory = nil
    }

    var value: T {
        table.get(ix)
    }

    var debugDescription: String {
        return "Resolvable[\(ix) : \(value.debugDescription)]"
    }

    static func == (lhs: Resolvable, rhs: Resolvable) -> Bool {
        //        return lhs.ix == rhs.ix && lhs.table == rhs.table
        return lhs.value == rhs.value
    }
}



extension Resolvable<String> {
    var debugDescription: String { "\(self.value)@\(self.ix)" }
}

extension Resolvable<HLType> {
    var debugDescription: String {
        let t = self.value
        switch t {
        case .obj(let data): return "\(data.name.value)@\(self.ix)"
        default: return "\(t.debugDescription)@\(self.ix)"
        }
    }
    
    init(_ t: UnsafePointer<HLType_CCompat>) {
        self.ix = 0
        self.table = TableResolver(table: SharedStorage(wrappedValue: [HLType(t.pointee)]), count: 1)
        self.memory = UnsafeRawPointer(t)
    }
}

