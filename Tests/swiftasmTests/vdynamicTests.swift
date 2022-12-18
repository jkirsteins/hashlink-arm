import XCTest
@testable import swiftasm

final class vdynamicTests: XCTestCase {

    func testSet() throws {
        withUnsafePointer(to: vdynamic(t: .init(bitPattern: 123)!, union: nil)) {
            xPtr in
            
            vdynamic.set(d: 123.0, in: xPtr)
            XCTAssertEqual(xPtr.pointee.d, 123.0)
            XCTAssertNotEqual(xPtr.pointee.f, 123.0)
            
            vdynamic.set(f: 0.123, in: xPtr)
            XCTAssertNotEqual(xPtr.pointee.d, 123.0)
            XCTAssertEqual(xPtr.pointee.f, 0.123)
            
            vdynamic.set(i: 543, in: xPtr)
            XCTAssertEqual(xPtr.pointee.i, 543)
            
            vdynamic.set(i64: Int64.max, in: xPtr)
            XCTAssertEqual(xPtr.pointee.i64, Int64.max)
            
            vdynamic.set(ui8: UInt8.max, in: xPtr)
            XCTAssertEqual(xPtr.pointee.ui8, UInt8.max)
            
            vdynamic.set(ui16: UInt16.max, in: xPtr)
            XCTAssertEqual(xPtr.pointee.ui16, UInt16.max)
        }
    }
}

