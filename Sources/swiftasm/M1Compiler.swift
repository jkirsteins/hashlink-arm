class M1Compiler {
    func compile(native: HLFunction) -> ByteBuffer {
        let result = ByteBuffer(incrementSize: 32)

        for op in native.ops {
            switch op {
            default: fatalError("Can't compile \(op.debugDescription)")
            }
        }

        guard result.buffer.count > 0 else { fatalError("Empty compilation result") }
        return result
    }
}
