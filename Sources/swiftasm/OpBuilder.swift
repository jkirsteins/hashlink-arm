import Darwin
import Foundation

/* NOTE: this file shouldn't contain
anything specific to M1. Keep it a generic
output builder based on CpuOp protocol. */

class OpBuilder
{
    var ops: [any CpuOp] = []
    var position: Int { ops.count }
    var byteSize: ByteCount = 0
    let jitBase: SharedStorage<UnsafeMutableRawPointer?> = SharedStorage(wrappedValue: nil)

    /* Deferred refers to the base memory address (for JIT) not being available.
    
    Before compiling the whole block, the address is simply offset from 0.
    
    Aftercompiling the whole block, the address is <jitmemstart + offset> */
    func getDeferredPosition() -> DeferredBaseRelativeAddress {
        DeferredBaseRelativeAddress(
            jitBase: jitBase, 
            offsetFromBase: byteSize)
    }

    @discardableResult
    func align(_ to: Int64) -> OpBuilder {
        let origSize = self.byteSize
        var targetSize = origSize
        
        while targetSize % to != 0 { targetSize += 1 }

        while self.byteSize < targetSize {
            if origSize + 4 > targetSize {
                self.append(PseudoOp.zero)
            } else {
                self.append(.nop)
            }
        }

        return self
    }

    @discardableResult
    func append(_ instructions: any CpuOp...) -> OpBuilder
    {
        _internalAppend(instructions)
    }

    @discardableResult
    func append(_ instructions: [any CpuOp]) -> OpBuilder
    {
        _internalAppend(instructions)
    }

    // This should be the only place that modifies size/ops
    @discardableResult
    func _internalAppend(_ instructions: [any CpuOp]) -> OpBuilder
    {
        let increase = instructions.reduce(0) { $0 + $1.size }
        byteSize += increase
        ops.append(contentsOf: instructions)
        return self
    }

    @discardableResult
    func append(ascii str: String) -> OpBuilder
    {
        append(PseudoOp.ascii(str))
    }

    func safeBuild() throws -> [UInt8] {
        return try self.ops.flatMap { try $0.emit() }
    }

    func build() -> [UInt8] {
        return try! safeBuild()
    }

    // print copy-pastable into a test
    func hexPrint() {
        print("---- START ----")
        var printedAlready: ByteCount = 0
        for op in self.ops {
            let bytes = try! op.emit()
            for (ix, row) in bytes.chunked(into: 4).enumerated() {
                let strs = row.map { "0x" + String($0, radix: 16).leftPadding(toLength: 2, withPad: "0") }
                
                let debugString: String
                if case PseudoOp.ascii = op {
                    debugString = String(bytes: row, encoding: .ascii)!.replacingOccurrences(of: " ", with: ".")
                } else if ix == 0 {
                    debugString = op.debugDescription
                } else {
                    debugString = ""
                }
                
                print(strs.joined(separator: ", ") + ", // \(debugString)")
            }

            printedAlready += op.size
        }
        
        print("---- END ----")
    }

    func debugPrint() {
        print("---- START ----")
        var printedAlready: ByteCount = 0
        for op in self.ops {
            let bytes = try! op.emit()
            for (ix, row) in bytes.chunked(into: 4).enumerated() {
                let strs = row.map { "0x" + String($0, radix: 16).leftPadding(toLength: 2, withPad: "0") }

                let debugString: String? 
                if case PseudoOp.ascii = op {
                    debugString = String(bytes: row, encoding: .ascii)?.replacingOccurrences(of: " ", with: ".")
                } else if ix == 0 {
                    debugString = op.debugDescription
                } else {
                    debugString = nil
                }

                print("\(printedAlready):\t" + strs.joined(separator: " ").rightPadding(toLength: 19), terminator: (debugString != nil) ? "; " : "\n")
                if let debugString = debugString { print(debugString) }
            }
            
            printedAlready += op.size
        }
        // let arr2 = build()
        // for row in arr2.chunked(into: 4) {
        //     let strs = row.map { "0x" + String($0, radix: 16).leftPadding(toLength: 2, withPad: "0") }
        //     print(strs.joined(separator: " ") + "; ")
        // }
        
        print("---- END ----")
    }

    typealias JitMainType = (@convention(c) () -> Int64)

    func buildEntrypoint(_ entrypoint: any WholeFunction) -> JitMainType {
        
        let map = mmap(
            nil, 
            Int(byteSize), 
            PROT_WRITE | PROT_EXEC, 
            MAP_ANONYMOUS | MAP_PRIVATE | MAP_JIT, 
            -1, 0)

        // This is needed so that DeferredAddresses
        // are available
        self.jitBase.wrappedValue = map
        
        let code = build()
        
        if map == MAP_FAILED {
            fatalError("MAP FAILED \(errno)")
        }

        var jitMain: JitMainType? = nil

        pthread_jit_write_protect_np(0);
        memcpy(map, code, code.count)
        pthread_jit_write_protect_np(1);

        var entrypointAddress: UnsafeMutableRawPointer = entrypoint.memory.value

        print("Casting from \(entrypointAddress)")
        
        jitMain = unsafeBitCast(
            // map /*entrypointAddress*/, 
            /*map*/ entrypointAddress, 
            to: JitMainType.self)

        // var codeCopy = code
        // withUnsafeMutablePointer(to: &codeCopy) {
        //     codePtr in 

        //     jitMain = unsafeBitCast(
        //         map, 
        //         to: JitMainType.self)
        // }

        guard let jitMain = jitMain else { fatalError("Failed to init jitMain") }

        return jitMain
    }
}