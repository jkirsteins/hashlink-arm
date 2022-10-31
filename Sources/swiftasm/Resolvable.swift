import Foundation

typealias TableIndex = Int

fileprivate var __typeCache: [UnsafeRawPointer:Resolvable<HLType>] = [:]
fileprivate var __objFieldCache: [UnsafeRawPointer:Resolvable<HLObjField>] = [:]
fileprivate var __objProtoCache: [UnsafeRawPointer:Resolvable<HLObjProto>] = [:]

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
    
    static func type(fromUnsafe t: UnsafePointer<HLType_CCompat>) -> Resolvable<HLType> {
        if let exists = __typeCache[t] {
            return exists
        }
        
        let table: TableResolver<HLType> = TableResolver(table: SharedStorage(wrappedValue: []), count: 1)
        let res = Resolvable(ix: 0, table: table)
        __typeCache[t] = res
        
        table.storage.wrappedValue = [HLType(t)]
        
        return res
    }
}
 
extension Resolvable<HLObjField> {
    static func objField(fromUnsafe t: UnsafePointer<HLObjField_CCompat>) -> Resolvable<HLObjField> {
        if let exists = __objFieldCache[t] {
            return exists
        }
        
        let table: TableResolver<HLObjField> = TableResolver(table: SharedStorage(wrappedValue: []), count: 1)
        let res = Resolvable(ix: 0, table: table)
        __objFieldCache[t] = res
        
        table.storage.wrappedValue = [HLObjField(t)]
        
        return res
    }
}

extension Resolvable<HLObjProto> {
    static func objProto(fromUnsafe t: UnsafePointer<HLObjProto_CCompat>) -> Resolvable<HLObjProto> {
        if let exists = __objProtoCache[t] {
            return exists
        }
        
        let table: TableResolver<HLObjProto> = TableResolver(table: SharedStorage(wrappedValue: []), count: 1)
        let res = Resolvable(ix: 0, table: table)
        __objProtoCache[t] = res
        
        table.storage.wrappedValue = [HLObjProto(t)]
        
        return res
    }
}

