import XCTest
@testable import swiftasm

final class ByteBufferTests: XCTestCase {
    

    func testPush_UInt8() throws {
        let sut = ByteBuffer(incrementSize: 2)
            
        XCTAssertEqual(sut.buffer, [])
        sut.push(UInt8(1))
        XCTAssertEqual(sut.buffer, [1, 0])
        sut.push(UInt8(1))
        XCTAssertEqual(sut.buffer, [1, 1])
        sut.push(UInt8(2))
        XCTAssertEqual(sut.buffer, [1, 1, 2, 0])
        XCTAssertEqual(sut.position, 3)
    }

    func testPush_incrementSizeTooSmall() throws {
        let sut = ByteBuffer(incrementSize: 2)
            
        XCTAssertEqual(sut.buffer, [])
        sut.push(UInt32(1))
        XCTAssertEqual(sut.buffer, [1, 0, 0, 0])
        XCTAssertEqual(sut.position, 4)
    }

    func testWriteRead() throws {
        let sut = ByteBuffer(incrementSize: 2)
            
        sut.push(UInt32(472))
        sut.push(UInt32(67823))
        sut.push(UInt32(102367823))
        let reader = ByteReader(Data(sut.buffer))
        
        XCTAssertEqual(try reader.readUInt32(), 472)
        XCTAssertEqual(try reader.readUInt32(), 67823)
        XCTAssertEqual(try reader.readUInt32(), 102367823)
    }
}

