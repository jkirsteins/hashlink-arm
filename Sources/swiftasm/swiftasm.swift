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

    // func testrun() throws {

    //     let builder = OpBuilder()
        
    //     builder.appendDebugPrintAligned4("Hello 1\n")
    //     builder.appendDebugPrintAligned4("Hello 2\n")
    //     builder.appendSystemExit(255)
    //     // builder.appendDebugPrintAligned4("Hello 2")
        

    //     builder.debugPrint()
    //     print("Building entrypoint")
    //     let entrypoint2 = builder.buildEntrypoint()
    //     print("Going for it")
    //     let result = entrypoint2()
    //     print("Got", result)
    // }

    // Not sure why this needed
    //     typedef struct {  
    //         hl_code *code;
    //         hl_module *m;
    //         vdynamic *ret;
    //         pchar *file;
    //         int file_time;
    // } main_context;
    struct MainContext {
        var code: UnsafePointer<HLCode_CCompat>? = nil
        var m: UnsafeMutableRawPointer? = nil
        var ret: UnsafeMutableRawPointer? = nil
        var file: UnsafePointer<CChar>? = nil
        var file_time: Int32 = 0
    }
    
    mutating func run() throws {

        var _mctx = MainContext()

        LibHl.hl_global_init()
        LibHl._hl_register_thread(&_mctx)

        let file = try! Data(contentsOf: URL(fileURLWithPath: hlFileIn))
        let reader = ByteReader(file)

        let hlcode = LibHl.load_code(hlFileIn)
        
        // TODO: remove when not needed. Useful for printing for now
        let mod = try! reader.readModule()        
        printerr(String(reflecting: mod))
        printerr("==> Compiling functions")

        let ctx = JitContext(module: mod, hlcode: hlcode)
        let jit = OpBuilder(ctx: ctx)
//        let compiler = M1Compiler2() 
//
//        for fn in mod.storage.functionResolver.table {
//            try compiler.compile(findex: fn.findex, into: jit)
//        }
 
    //     // entrypoint initializes types, memory, and all that good stuff
    //     let funcs = [
    //         head.storage.functionResolver.table.first { $0.findex == 295 }!,
    //         head.storage.functionResolver.table.first { $0.findex == 404 }!,
            
    //         // head.functionResolver.table.first { $0.findex == 1 }!,

    //         // // Type_init
    //         // head.functionResolver.table.first { $0.findex == 295 }!,
    //         // // entrypoint
    //         // head.functionResolver.table.first { $0.findex == 404 }!,
    //         // // pathTest
    //         // head.functionResolver.table.first { $0.findex == 29 }!,
    //         // // pathTest2
    //         // head.functionResolver.table.first { $0.findex == 30 }!
    //     ]

    //     let ctx = JitContext(storage: head.storage)
    //     let jit = OpBuilder()

    //    fatalError("wip")

    //     // compiledTable.wrappedValue = try /*head.functionResolver.table*/funcs.map {
    //     // // compiledTable.wrappedValue = try head.functionResolver.table.map {
    //     //     try compiler.compile(native: $0, into: jit, ctx: ctx)
    //     //     // break
    //     // }


    //     // let entrypointCompiled = compiler.compile(native: entrypoint)
    //     // let entrypointCompiled = compiler.compile(native: entrypoint)
    //     // let pathTestCompiled = compiler.compile(native: pathTest)
    //     // let pathTest2Compiled = compiler.compile(native: pathTest2)

    //     try wft.requireReady()

    //     let compiledEntrypoint = try wft.get(Int(head.entrypoint))
    //     let xxx = compiledEntrypoint as! HLCompiledFunction
        
    //     jit.debugPrint()

    //     print("Got \(xxx)@\((xxx.memory as! any DeferredMemoryAddress))")


    //     let finalEntrypoint = jit.buildMain(compiledEntrypoint)
    //     let result = finalEntrypoint()
    //     print("Entrypoint returned \(result)")
    //     // fatalError("Ready")
    //     // for funIx in 0..<head.nfunctions {
    //     //     let fun: HLFunction = head.functionResolver.table.first { $0.findex == funIx }!
    //     //     print("Compiling \(fun.debugDescription)")
    //     //     print("    regs: \([fun.regs.map { $0.value.debugName }])")
    //     //     let bytes = try compiler.compile(native: fun)
    //     //     print("    done \(fun.debugDescription)")
    //     //     print(bytes)
    //     //     fatalError()
    //     // }

    //     return
    }
}
