import Darwin
import Foundation

enum PseudoOp: CpuOp, CustomDebugStringConvertible {
    case zero
    case ascii(String)

    var size: ByteCount {
        switch(self) {
            case .zero: return 1
            case .ascii(let v): return ByteCount(v.utf8.count)
        }
    }

    var debugDescription: String {
        switch(self) {
            case .zero: return ".zero"
            case .ascii(let val):
                return ".ascii(\(val))"
        }
    }

    func emit() throws -> [UInt8] {
        switch(self) {
            case .zero: return [0]
            case .ascii(let val):
                return Array(val.utf8)
        }
    }
}

class OpBuilder
{
    var ops: [any CpuOp] = []
    var position: Int { ops.count }
    var byteSize: ByteCount = 0
    let jitBase: SharedStorage<UnsafeMutableRawPointer?> = SharedStorage(wrappedValue: nil)

    func getDeferredPosition() -> DeferredAddress {
        DeferredAddress(
            jitBase: jitBase, 
            offsetFromBase: byteSize)
    }

    @discardableResult
    func appendStackReservation(_ regs: [HLType]) -> OpBuilder {
        let size = regs.reduce(0) { $0 + $1.neededBytes }
        self.append(
            //.sub sp, sp, size
        )
        fatalError("Not implemented")
    }

    @discardableResult
    func appendDebugPrintAligned4(_ val: String) -> OpBuilder {
        var adr = RelativeDeferredOffset()
        var jmpTarget = RelativeDeferredOffset()
        let str = "[jitdebug] \(val)"
        
        self.append(
            // Stash registers we'll use (so we can reset)
            .str(Register64.x0, .reg64offset(.sp, -32, .pre)),
            .str(Register64.x1, .reg64offset(.sp, 8, nil)),
            .str(Register64.x2, .reg64offset(.sp, 16, nil)),
            .str(Register64.x16, .reg64offset(.sp, 24, nil)),

            // unix write system call
            .movz64(.x0, 1, nil)
        )
        adr.start(at: self.byteSize)
        self.append(
            .adr64(.x1, adr),
            .movz64(.x2, UInt16(str.count), nil),
            .movz64(.x16, 4, nil), 
            .svc(0x80),
            
            // restore
            .ldr(Register64.x16, .reg64offset(.sp, 24, nil)),
            .ldr(Register64.x2, .reg64offset(.sp, 16, nil)),
            .ldr(Register64.x1, .reg64offset(.sp, 8, nil)),
            .ldr(Register64.x0, .reg64offset(.sp, 32, .post))
        )
        
        jmpTarget.start(at: self.byteSize)
        self.append(.b(jmpTarget))
        
        adr.stop(at: self.byteSize)
        self.append(ascii: str)
            .align(4)

        jmpTarget.stop(at: self.byteSize)
        
        // appendSystemExit(132)
        return self
    }

    @discardableResult
    func appendSystemExit(_ code: UInt8) -> OpBuilder {
        self.append(
            .movz64(.x0, UInt16(code), nil),
            .movz64(.x16, 1, nil),
            .svc(0x80)
        )
    }

    @discardableResult
    func appendPrologue() throws -> OpBuilder {
        self.append(
            .stp((.x29_fp, .x30_lr), .reg64offset(.sp, -16, .pre)),
            .movr64(.x29_fp, .sp)
        )
        return self
    }
    
    @discardableResult
    func appendEpilogue() throws -> OpBuilder {
        self.append(
            .ldp((.x29_fp, .x30_lr), .reg64offset(.sp, 16, .post))
        )
        return self
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