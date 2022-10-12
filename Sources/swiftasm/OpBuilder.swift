import Darwin
import Foundation

class OpBuilder
{
    var data = Data()

    @discardableResult
    func appendDebugPrintAligned4(_ val: String) -> OpBuilder {
        // self.append(
        //     .stp((.x0, .x1), .reg64offset(.sp, -16, .pre)),
        //     .movr64(.x29_fp, .sp)

        //     .movz64(.x0, 1, ._0),
        //     .adr64(.x1, 7 /*instructions including self*/ * 4),
        //     .movz64(.x2, UInt16(str.count), ._0),
        //     .movz64(.x16, 4, ._0),
        //     .svc(0x80),
            
        //     // return 3
        //     .movz64(.x0, 3, ._0),
        //     .ret,
        //     .nop   // to ensure aligning to 4
        // )
        fatalError("wip")
    }

    @discardableResult
    func append(_ instructions: [UInt8]) -> OpBuilder
    {
        data.append(contentsOf: instructions)
        return self
    }

    @discardableResult
    func append(utf8 str: String) -> OpBuilder
    {
        let bytes: [UInt8] = Array(str.utf8)

        data.append(contentsOf: bytes)
        return self
    }

    func build() -> [UInt8] {
        var arr2 = Array<UInt8>(repeating: 0, count: data.count/MemoryLayout<UInt8>.stride)
        _ = arr2.withUnsafeMutableBytes { data.copyBytes(to: $0) }
        return arr2
    }

    func debugPrint() {
        
        let arr2 = build()
        for row in arr2.chunked(into: 4) {
            let strs = row.map { "0x" + String($0, radix: 16).leftPadding(toLength: 2, withPad: "0") }
            print(strs.joined(separator: " "))
        }
        
        print("---- END ----")
    }

    typealias JitMainType = (@convention(c) () -> Int64)

    func buildEntrypoint() -> JitMainType {
        let code = build()

        let map = mmap(
            nil, 
            code.count, 
            PROT_WRITE | PROT_EXEC, 
            MAP_ANONYMOUS | MAP_PRIVATE | MAP_JIT, 
            -1, 0)
        
        if map == MAP_FAILED {
            fatalError("MAP FAILED \(errno)")
        }

        var jitMain: JitMainType? = nil

        pthread_jit_write_protect_np(0);
        memcpy(map, code, code.count)
        pthread_jit_write_protect_np(1);
            
        var codeCopy = code
        withUnsafeMutablePointer(to: &codeCopy) {
            codePtr in 

            jitMain = unsafeBitCast(
                map, 
                to: JitMainType.self)
        }

        guard let jitMain = jitMain else { fatalError("Failed to init jitMain") }

        return jitMain
    }
}