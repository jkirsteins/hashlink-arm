struct HLGlobal : Equatable, CustomDebugStringConvertible, Hashable {
    let type: Resolvable<HLType>
    let memory: DeferredAbsoluteAddress = DeferredAbsoluteAddress(wrappedValue: nil)

    var hasAddress: Bool { memory.hasUsableValue }

    func allocate(for type: HLType) {
        printerr("Allocating memory for \(type) (\(type.kind.hlRegSize)b)")
        self.memory.wrappedValue = UnsafeMutableRawPointer.allocate(
            byteCount: Int(type.kind.hlRegSize), 
            alignment: MemoryLayout<UInt8>.alignment)
        self.memory.wrappedValue!.initializeMemory(as: UInt8.self, repeating: 0, count: Int(type.kind.hlRegSize))
    }

    func deallocate() {
        guard let m = memory.wrappedValue else { fatalError("Can't deallocate HLGlobal before allocating.l") }
        m.deallocate()
    }

    var debugDescription: String {
        return "global<\(type.debugDescription)>"
    }
}