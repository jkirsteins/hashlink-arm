import ArgumentParser
import Darwin
import Foundation

extension Array {
    func chunked(into size: Int) -> [[Element]] {
        return stride(from: 0, to: count, by: size).map {
            Array(self[$0 ..< Swift.min($0 + size, count)])
        }
    }
}

class OpBuilder
{
    var data = Data()

    @discardableResult
    func append(_ instructions: [UInt8]) -> OpBuilder
    {
        data.append(contentsOf: instructions)
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

extension OpBuilder
{
    @discardableResult
    func append(_ op: Op) -> OpBuilder
    {
        let data = try! EmitterM1.emit(for: op)
        return self.append(data)
    }
}

@main
struct SwiftAsm: ParsableCommand {
    @Argument var hlFileIn: String
    
    mutating func run() throws {

        let file = try! Data(contentsOf: URL(fileURLWithPath: hlFileIn))
        let reader = ByteReader(file)

        let head = try! reader.readModule()
        let compiler = M1Compiler()
        print(String(reflecting: head))
        print("==> Compiling functions")

        let pathTest = head.functionResolver.get(29)
        let pathTest2 = head.functionResolver.get(30)

        let pathTestCompiled = compiler.compile(native: pathTest)
        let pathTest2Compiled = compiler.compile(native: pathTest2)

        fatalError("boop")
        for funIx in 0..<head.nfunctions {
            let fun: HLFunction = head.functionResolver.get(Int(funIx))
            print("Compiling \(fun.debugDescription)")
            print("    regs: \([fun.regs.map { $0.value.debugName }])")
            let bytes = compiler.compile(native: fun)
            print("    done \(fun.debugDescription)")
            print(bytes)
            fatalError()
        }

        return

        let builder = OpBuilder()
        builder.append(.nop)
        builder.append(.movz64(.x0, 23, ._0))
        builder.append(.ret)

        print("Building entrypoint")
        let entrypoint = builder.buildEntrypoint()
        print("Going for it")
        let result = entrypoint()
        print("Got", result)
    }
}
