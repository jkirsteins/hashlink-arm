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
    
    @Flag(help: "Insert debugging messages in the compiled code.")
    var jitdebug: Bool = false


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
    
    static var logger = LoggerFactory.create(SwiftAsm.self)
    
    mutating func run() throws {
        
        let tmpDeps: [RefFun] = [
//            332, 42, 231, 230, 259, 16,
//            340,
//            245,
//            230,
//            335,
//            341,
//            20,
//            344,
//            237,
//            231,
//            348,
//            289,
//            5,
//            241,
//            269,
//            332,
//            305, 437, 350, 28, 14, 42, 240, 337, 303
]

        let mod = try! Bootstrap.start2(hlFileIn, args: [])
        let sut = M1Compiler2(ctx: mod, stripDebugMessages: !jitdebug, cache: nil)
        let buf = CpuOpBuffer()
        
        var offsetToCompilable: Dictionary<ByteCount, (ByteCount, any Compilable2)> = [:]
        
        Self.logger.debug("Compiling...")
        
        let progressChunkSize: Float = 0.05
        var progressLastReport: Float = 0
        
        for frix in (0..<mod.nfunctions) {
            let fix = mod.mainContext.pointee.code!.pointee.functions.advanced(by: Int(frix)).pointee.findex
            
            if !tmpDeps.isEmpty && !tmpDeps.contains(Int(fix)) { continue }
            
            let startByteSize = buf.byteSize
            let compilable = try sut.compile(findex: RefFun(fix), into: buf)
            offsetToCompilable[startByteSize] = (buf.byteSize, compilable)
            
            let progress = Float(frix+1)/Float(mod.nfunctions)
            if (progress-progressLastReport) > progressChunkSize {
                progressLastReport = progress
                print("Progress", String(format: "%.2f%%", progress * 100))
            }
        }
        
        let epIx = mod.mainContext.pointee.code!.pointee.entrypoint
        
        Self.logger.debug("Linking...")
        let mapper = BufferMapper(ctx: mod, buffer: buf)
        let mem = try mapper.getMemory()
        
        Self.logger.debug("Executing entrypoint _@\(epIx)...")
        let addr = try mod.getCallable(findex: RefFun(epIx))!.address.value
        
        let _cc = unsafeBitCast(addr, to: (@convention(c) ()->()).self)
        _cc()
    }
}
