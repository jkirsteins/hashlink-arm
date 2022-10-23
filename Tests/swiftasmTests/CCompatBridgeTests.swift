//import XCTest
//@testable import swiftasm
//
//final class CCompatBridgeTests: XCTestCase {
//
//    func testPointer_type_simple() throws {
//        let types = [HLType.i32, .void, .i64, .f32, .f64, .bool]
//        for type in types {
//            let ptr = type.createCCompatPointer()
//            defer { ptr.deallocate() }
//            XCTAssertEqual(ptr.pointee.kind, type.kind)
//        }
//    }
//    
//    func testArrayOfPointers() throws {
//        let regs: [HLType] = [.i32, .i64, .void]
//        let regPtrs = regs.map { $0.createCCompatPointer() }
//        let ptr = regPtrs.createCCompatPointer()
//        
//        
//        XCTAssertEqual(ptr.pointee.pointee.kind, .i32)
//        XCTAssertEqual(ptr.advanced(by: 1).pointee.pointee.kind, .i64)
//        XCTAssertEqual(ptr.advanced(by: 2).pointee.pointee.kind, .void)
//        
//    }
//    
//    func testArrayMap() throws {
//        let regs: [HLType] = [.i32, .i64, .void]
//        let ptr = regs.createCCompatPointer()
//        
//        XCTAssertEqual(ptr.pointee.kind, .i32)
//        XCTAssertEqual(ptr.advanced(by: 1).pointee.kind, .i64)
//        XCTAssertEqual(ptr.advanced(by: 2).pointee.kind, .void)
//    }
//    
//    func testHLFunction() throws {
//        let fun = HLFunction(
//            type: Resolvable(.i32),
//            findex: 132,
//            regs: Resolvable.array([.i32, .i64, .void]),
//            ops: [.ORet(ret: 0)],
//            assigns: []
//        )
//        let ptr = fun.createCCompatPointer()
//        defer { ptr.deallocate() }
//        
//        XCTAssertEqual(ptr.pointee.cType.kind, .i32)
//        XCTAssertEqual(ptr.pointee.findex, 132)
//        XCTAssertEqual(ptr.pointee.cRegs.map { $0.kind }, [.i32, .i64, .void])
//    }
//    
//    func testHLNative() throws {
//        let nat = HLNative(
//            lib: Resolvable("std"),
//            name: Resolvable("fun_name"),
//            type: Resolvable(
//                HLType.fun(
//                    HLTypeFun(
//                        args: [Resolvable(.i32)],
//                        ret: Resolvable(.i32)))),
//            findex: 1,
//            memory: UnsafeMutableRawPointer(bitPattern: 123)!
//        )
//        let ptr = nat.createCCompatPointer()
//        defer { ptr.deallocate() }
//        
//        XCTAssertEqual(ptr.pointee.findex, 1)
//        XCTAssertEqual(ptr.pointee.lib, "std")
//        XCTAssertEqual(ptr.pointee.name, "fun_name")
//    }
//    
//}
//
