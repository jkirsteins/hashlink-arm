import Darwin
import Foundation

/* NOTE: this file shouldn't contain
anything specific to M1. Keep it a generic
output builder based on CpuOp protocol. */

typealias JitInt64 = (@convention(c) () -> Int64)
typealias JitVoid = (@convention(c) () -> Void)

class OpBuilder {
    var ops: [any CpuOp] = []
    var position: Int { ops.count }
    var byteSize: ByteCount = 0
    var jitBase: SharedStorage<UnsafeMutableRawPointer?> { ctx.jitBase }
    let ctx: JitContext

    init(ctx: JitContext) { self.ctx = ctx }

    /* Deferred refers to the base memory address (for JIT) not being available.

    Before compiling the whole block, the address is simply offset from 0.

    Aftercompiling the whole block, the address is <jitmemstart + offset> */
    func getDeferredPosition() -> DeferredBaseRelativeAddress {
        DeferredBaseRelativeAddress(jitBase: jitBase, offsetFromBase: byteSize)
    }

    @discardableResult func align(_ to: Int64) -> OpBuilder {
        let origSize = self.byteSize
        var targetSize = origSize
        while targetSize % to != 0 { targetSize += 1 }

        while self.byteSize < targetSize {
            if origSize + 4 > targetSize {
                self.append(PseudoOp.zero)
            }
            else {
                self.append(.nop)
            }
        }

        return self
    }

    @discardableResult func append(_ instructions: any CpuOp...) -> OpBuilder {
        try! _internalAppend(instructions)
    }

    @discardableResult func append(_ instructions: [any CpuOp]) -> OpBuilder {
        try! _internalAppend(instructions)
    }

    // This should be the only place that modifies size/ops
    @discardableResult func _internalAppend(_ instructions: [any CpuOp]) throws
        -> OpBuilder
    {
        guard map == nil else {
            throw GlobalError.invalidOperation(
                "Can not append instructions once addresses have been locked."
            )
        }

        let increase: Int64 = instructions.reduce(0) {
            return $0 + $1.size
        }
        byteSize += increase
        ops.append(contentsOf: instructions)
        return self
    }

    @discardableResult func append(ascii str: String) -> OpBuilder {
        append(PseudoOp.ascii(str))
    }

    func safeBuild() throws -> [UInt8] {
        if map == nil { try lockAddresses() }
        guard map != nil else {
            throw GlobalError.unexpected(
                "Memory should not be nil after locking addresses."
            )
        }
        return try self.ops.flatMap { op in
            print("Emitting \(op.debugDescription)")
            defer {
                print("  finished emitting \(op.debugDescription)")
            }
            return try op.emit()
        }
    }

    @discardableResult
    func lockAddressesAndBuild() -> [UInt8] {
        try! lockAddresses()
        return try! safeBuild()
    }

    // print copy-pastable into a test
    func hexPrint() {
        do {
        try lockAddresses()
        print("---- START ----")
        var printedAlready: ByteCount = 0
        for op in self.ops {
            let bytes = try op.emit()
            guard bytes.count > 0 else {
                if case PseudoOp.debugMarker(let message) = op {
                    print("// \(message)")
                }
                continue
            }
            for (ix, row) in bytes.chunked(into: 4).enumerated() {
                let strs = row.map {
                    "0x" + String($0, radix: 16).leftPadding(toLength: 2, withPad: "0")
                }
                let debugString: String
                if case PseudoOp.ascii = op {
                    debugString = String(bytes: row, encoding: .ascii)!
                        .replacingOccurrences(of: " ", with: ".")
                }
                else if ix == 0 {
                    debugString = op.debugDescription
                }
                else {
                    debugString = "... \(op.debugDescription)"
                }
                print(
                    strs.joined(separator: ", ")
                        + ", // \(debugString.replacingOccurrences(of: "\n", with: "\\n"))"
                )
            }

            printedAlready += op.size
        }
        print("---- END ----")
        } catch {
            fatalError("\(error): \(Thread.callStackSymbols.joined(separator: "\n"))")
        }
    }

    // print copy-pastable into a test
    func asmPrint() {
        try! lockAddresses()

        print("---- START ----")
        var printedAlready: ByteCount = 0
        for op in self.ops {
            let bytes = try! op.emit()
            guard bytes.count > 0 else {
                if case PseudoOp.debugMarker(let message) = op {
                    print("// \(message.replacingOccurrences(of: "\n", with: "\\n"))")
                }
                continue
            }
            for (ix, row) in bytes.chunked(into: 4).enumerated() {
                let debugString: String
                if case PseudoOp.ascii = op {
                    debugString = String(bytes: row, encoding: .ascii)!
                        .replacingOccurrences(of: " ", with: ".")
                }
                else if ix == 0 {
                    debugString = op.debugDescription
                }
                else {
                    debugString = ""
                }
                print(debugString)
            }

            printedAlready += op.size
        }
        print("---- END ----")
    }

    func debugPrint() {
        try! lockAddresses()

        print("---- START ----")
        var printedAlready: ByteCount = 0
        for op in self.ops {
            let bytes = try! op.emit()
            for (ix, row) in bytes.chunked(into: 4).enumerated() {
                let strs = row.map {
                    "0x" + String($0, radix: 16).leftPadding(toLength: 2, withPad: "0")
                }

                let debugString: String?
                if case PseudoOp.ascii = op {
                    debugString = String(bytes: row, encoding: .ascii)?
                        .replacingOccurrences(of: " ", with: ".")
                }
                else if ix == 0 {
                    debugString = op.debugDescription
                }
                else {
                    debugString = nil
                }

                print(
                    "\(printedAlready):\t"
                        + strs.joined(separator: " ").rightPadding(toLength: 19),
                    terminator: (debugString != nil) ? "; " : "\n"
                )
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

    var map: UnsafeMutableRawPointer? { self.jitBase.wrappedValue }

    /// Without locking, addresses might not be available. You have
    /// to call this before printing debug output, or before building entrypoints.
    func lockAddresses() throws {
        
        let missingCompiled = ctx.funcTracker.refs.subtracting(ctx.funcTracker.comps)
        guard missingCompiled.isEmpty else {
            fatalError("These functions are referenced but not compiled: \(missingCompiled)")
        }
        
        guard map == nil else {
            /* We might need to call this multiple times (e.g. if we add a hexPrint() debug statement ahead of proper emitting)

            This is an issue only if we try to append new instructions once memory is already locked in place.
            */
            return
        }

        self.jitBase.wrappedValue = mmap(
            nil,
            Int(byteSize),
            PROT_WRITE | PROT_EXEC,
            MAP_ANONYMOUS | MAP_PRIVATE | MAP_JIT,
            -1,
            0
        )
    }

    func _buildAddress(_ callable: any Callable) throws
        -> UnsafeMutableRawPointer
    {
        let code = lockAddressesAndBuild()
        if map == MAP_FAILED { fatalError("MAP FAILED \(errno)") }

        pthread_jit_write_protect_np(0)
        memcpy(map, code, code.count)
        pthread_jit_write_protect_np(1)

        return callable.entrypoint.value
    }

    func buildMain(_ entrypoint: FunctionAddresses.Entry) throws -> JitInt64 {
        try self.buildEntrypoint(entrypoint)
    }

    func buildEntrypoint<T>(_ ix: Int) throws -> T {
        try self.buildEntrypoint(ctx.callTargets.get(ix))
    }

    func buildEntrypoint<T>(_ entrypoint: FunctionAddresses.Entry) throws -> T {
        let entrypointAddress = try _buildAddress(entrypoint)
        print("Casting from \(entrypointAddress)")
        let jitMain: T? = unsafeBitCast(
            // map /*entrypointAddress*/,
            /*map*/ entrypointAddress,
            to: T.self
        )

        guard let jitMain = jitMain else { fatalError("Failed to init jitMain") }

        return jitMain
    }
}
