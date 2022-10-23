import XCTest

@testable import swiftasm

final class HLFunction_CCompatTests: XCTestCase {
    override class func setUp() {
        LibHl.hl_global_init()
    }

    override class func tearDown() {
        LibHl.hl_global_free()
    }
    
    func testProtocol_compilable() throws {
        // TODO: adapt test with parsed data from bundled bytecode
        let regs = [HLType.i32, .i64, .bytes, .f64]
        let ptr = regs.createCCompatPointer()
        let ptrWrapped = ptr.createCCompatPointer()
        defer {
            ptrWrapped.deallocate()
            ptr.deallocate()
        }
        
        let type = HLType.fun(
            HLTypeFun(args: [Resolvable(.i32), Resolvable(.bytes)],
                          ret: Resolvable(.f64))
        )
        let typePtr = type.createCCompatPointer()
        defer { typePtr.deallocate() }
        
        let ops = [
            HLOpCode_CCompat(op: 0, p1: 1, p2: 2, p3: 3, extra: nil),
            HLOpCode_CCompat(op: 1, p1: 2, p2: 3, p3: 4, extra: nil)
        ]
        let opsPtr: UnsafeMutableBufferPointer<HLOpCode_CCompat> = .allocate(capacity: ops.count)
        _ = opsPtr.initialize(from: ops)
        defer { opsPtr.deallocate() }
        
        let sut = HLFunction_CCompat(
            findex: 0,
            nregs: Int32(regs.count),
            nops: Int32(ops.count),
            ref: 0,
            typePtr: typePtr,
            regsPtr: ptrWrapped,
            opsPtr: UnsafePointer(OpaquePointer(opsPtr.baseAddress)),
            debug: nil,
            objPtr: nil,
            unionPtr: nil)
        
        XCTAssertEqual(sut.regs.map { $0.value }, [.i32, .i64, .bytes, .f64])
        XCTAssertEqual(sut.args.map { $0.value }, [.i32, .bytes])
        XCTAssertEqual(sut.ops, [
            .OMov(dst: 1, src: 2),
            .OInt(dst: 2, ptr: 3)
        ])
        XCTAssertEqual(sut.ret.value, .f64)
    }
    
    func testParsingCallables() throws {
        let mod1 = Bundle.module.url(forResource: "mod1", withExtension: "hl")!.path
        
        let res = LibHl.load_code(mod1)
        let ms = ModuleStorage()
        let ctx = JitContext(storage: ms, hlcode: res)
        
        let fn = res.pointee.getFunction(18)
        let addr = FullyDeferredRelativeAddress(jitBase: ctx.jitBase)
        
        let sut = HLFunction_CCompat__WithMemory(ptr: fn, entrypoint: addr)
        let callable: Callable = sut
        let compilable: Compilable = sut
        
        // callable
        XCTAssertEqual(callable.ret.value.kind, .obj)
        XCTAssertEqual(callable.args.map { $0.value.kind }, [.obj])
        
        // compilable
        XCTAssertEqual(
            compilable.regs.map { $0.value.kind },
            [.obj, .bytes, .i32, .i32, .obj])
        
        XCTAssertEqual(
            compilable.ops,
            [
                .OGetThis(dst: 1, field: 0),
                .OInt(dst: 2, ptr: 0),
                .OGetThis(dst: 3, field: 1),
                .OCall3(dst: 1, fun: 236, arg0: 1, arg1: 2, arg2: 3),
                .OGetThis(dst: 2, field: 1),
                .ONew(dst: 4),
                .OSetField(obj: 4, field: 0, src: 1),
                .OSetField(obj: 4, field: 1, src: 2),
                .ORet(ret: 4)
            ])
        
    }
}
