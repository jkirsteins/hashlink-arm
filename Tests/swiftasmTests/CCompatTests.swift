import XCTest
@testable import swiftasm

final class CCompatTests: XCTestCase {

    func testSizes() throws {
        XCTAssertEqual(MemoryLayout<HLTypeKind>.size, 4)
        XCTAssertEqual(MemoryLayout<HLType_CCompat>.size, 32)
        XCTAssertEqual(MemoryLayout<HLTypeObj_CCompat>.size, 80)
        XCTAssertEqual(MemoryLayout<HLObjField_CCompat>.size, 24)
        XCTAssertEqual(MemoryLayout<HLTypeFun_CCompat>.size, 80)
        XCTAssertEqual(MemoryLayout<HLType_CCompat_Fun_Closure>.size, 32)
        XCTAssertEqual(MemoryLayout<HLType_CCompat_Fun_ClosureType>.size, 16)
        XCTAssertEqual(MemoryLayout<HLObjProto_CCompat>.size, 24)
        XCTAssertEqual(MemoryLayout<HLRuntimeObj_CCompat>.size, 120)
        XCTAssertEqual(MemoryLayout<HLRuntimeObj_CCompat>.size, 120)
        XCTAssertEqual(MemoryLayout<HLCode_CCompat>.size, 184)
        XCTAssertEqual(MemoryLayout<HLFunction_CCompat>.size, 64)
        XCTAssertEqual(MemoryLayout<HLNative_CCompat>.size, 32)
        XCTAssertEqual(MemoryLayout<vdynamic>.size, 16)
        XCTAssertEqual(MemoryLayout<varray>.size, 24)
        XCTAssertEqual(MemoryLayout<HLConstant_CCompat>.size, 16)
        XCTAssertEqual(MemoryLayout<MainContext_CCompat>.size, 40)
        
        XCTAssertEqual(MemoryLayout<HLEnumConstruct_CCompat>.size, 40)
        XCTAssertEqual(MemoryLayout<HLTypeEnum_CCompat>.size, 32)
        XCTAssertEqual(MemoryLayout<venum>.size, 16)
        XCTAssertEqual(MemoryLayout<vclosure>.size, 32)
        XCTAssertEqual(MemoryLayout<HLAlloc_CCompat>.size, 8)
        
        XCTAssertEqual(MemoryLayout<HLThreadInfo_CCompat>.size, 2824)
        XCTAssertEqual(MemoryLayout<HLTrapCtx_CCompat>.size, 208)
        XCTAssertEqual(MemoryLayout<vvirtual>.size, 24)
        XCTAssertEqual(MemoryLayout<HLTypeVirtual_CCompat>.size, 32)
        XCTAssertEqual(MemoryLayout<HLFieldLookup_CCompat>.size, 16)
    }
}

