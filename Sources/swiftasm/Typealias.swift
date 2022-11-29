typealias JitBase = SharedStorage<UnsafeMutableRawPointer?>
typealias Reg = Int32
// JumpOffset can be negative (backwards jump)
typealias JumpOffset = Int32
typealias RefInt = TableIndex
typealias Ref = TableIndex
typealias RefFloat = TableIndex
typealias ValBool = Int32
typealias RefBytes = TableIndex
typealias RefString = TableIndex
typealias RefFun = TableIndex
typealias RefClosurePointer = TableIndex
typealias RefField = TableIndex
typealias RefProto = TableIndex
typealias RefGlobal = TableIndex
typealias RefType = TableIndex
typealias RefEnumConstruct = TableIndex
