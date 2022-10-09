class ByteBuffer {
    let incrementSize: Int
    var position: Int = 0
    private(set) var buffer: [UInt8] = []

    init(incrementSize: Int) { self.incrementSize = incrementSize }

    func createChunk() -> [UInt8] { Array(repeating: UInt8(0), count: incrementSize) }

    func ensureSpace(_ size: Int) {
        while position + size > buffer.count { buffer = buffer + createChunk() }
    }

    func push(_ vals: [UInt8]...) {
        vals.forEach { val in val.forEach { self.push($0) } }
    }

    func push(_ val: UInt8) {
        ensureSpace(MemoryLayout.size(ofValue: val))

        buffer[position] = val
        position += 1
    }

    func push(_ val: UInt16) {
        ensureSpace(MemoryLayout.size(ofValue: val))

        buffer[position] = UInt8(val & 0xFF)
        position += 1
        buffer[position] = UInt8((val >> 8) & 0xFF)
        position += 1
    }

    func push(_ val: UInt32) {
        ensureSpace(MemoryLayout.size(ofValue: val))

        buffer[position] = UInt8(val & 0xFF)
        position += 1
        buffer[position] = UInt8((val >> 8) & 0xFF)
        position += 1
        buffer[position] = UInt8((val >> 16) & 0xFF)
        position += 1
        buffer[position] = UInt8((val >> 24) & 0xFF)
        position += 1
    }
}
