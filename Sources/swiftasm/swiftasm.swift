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

@main
struct SwiftAsm: ParsableCommand {
    @Argument var hlFileIn: String

    func testrun() throws {

        let builder = OpBuilder()
        
        builder.appendDebugPrintAligned4("Hello 1\n")
        builder.appendDebugPrintAligned4("Hello 2\n")
        builder.appendSystemExit(255)
        // builder.appendDebugPrintAligned4("Hello 2")
        

        builder.debugPrint()
        print("Building entrypoint")
        let entrypoint2 = builder.buildEntrypoint()
        print("Going for it")
        let result = entrypoint2()
        print("Got", result)
    }
    
    mutating func run() throws {

        try testrun()
        return 

        let file = try! Data(contentsOf: URL(fileURLWithPath: hlFileIn))
        let reader = ByteReader(file)

        let head = try! reader.readModule()
        let compiler = M1Compiler()
        print(String(reflecting: head))
        print("==> Compiling functions")

        // TODO: unify functions and natives in one function table

        // entrypoint initializes types, memory, and all that good stuff
        let funcs = [
            // Type_init
            head.functionResolver.table.first { $0.findex == 295 }!,
            // entrypoint
            head.functionResolver.table.first { $0.findex == 404 }!,
            // pathTest
            head.functionResolver.table.first { $0.findex == 29 }!,
            // pathTest2
            head.functionResolver.table.first { $0.findex == 30 }!
        ]

        for f in funcs {
            let _ = try compiler.compile(native: f)
        }
        // let entrypointCompiled = compiler.compile(native: entrypoint)
        // let entrypointCompiled = compiler.compile(native: entrypoint)
        // let pathTestCompiled = compiler.compile(native: pathTest)
        // let pathTest2Compiled = compiler.compile(native: pathTest2)

        fatalError("boop")
        for funIx in 0..<head.nfunctions {
            let fun: HLFunction = head.functionResolver.table.first { $0.findex == funIx }!
            print("Compiling \(fun.debugDescription)")
            print("    regs: \([fun.regs.map { $0.value.debugName }])")
            let bytes = try compiler.compile(native: fun)
            print("    done \(fun.debugDescription)")
            print(bytes)
            fatalError()
        }

        return
    }
}
