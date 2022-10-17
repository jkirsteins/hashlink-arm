import XCTest

@testable import swiftasm

final class WholeFunctionsTableTests: XCTestCase {
    func testResolution() throws {
        let nativeTable = SharedStorage(wrappedValue: [HLNative]())
        let functionTable = SharedStorage(wrappedValue: [HLCompiledFunction]())
        let typeTable = SharedStorage(wrappedValue: [
            HLType.void
        ])
        let stringTable = SharedStorage(wrappedValue: [
            "nat1", // 0
            "nat2", // 1
            "fun1", // 2
            "fun2", // 3
            "lib1", // 4
            "lib2", // 5
        ])
        
        let natives = TableResolver(table: nativeTable, count: 2)
        let functions = TableResolver(table: functionTable, count: 2)
        let strings = TableResolver(table: stringTable, count: Int32(stringTable.wrappedValue.count))
        let types = TableResolver(table: typeTable, count: 2)

        typeTable.wrappedValue.append(HLType.fun(HLTypeFunData(args: [], ret: types.getResolvable(0 /*void*/))))
        
        var dummy = [5]
        let ptr: UnsafeMutableRawPointer = UnsafeMutableRawPointer(mutating: dummy)
        
        // populate native
        nativeTable.wrappedValue.append(HLNative(
            lib: strings.getResolvable(4),
            name: strings.getResolvable(0),
            type: types.getResolvable(1),
            findex: 0,
            memory: ptr))
        nativeTable.wrappedValue.append(HLNative(
            lib: strings.getResolvable(4),
            name: strings.getResolvable(1),
            type: types.getResolvable(1),
            findex: 2,
            memory: ptr))

        // populate fun
        functionTable.wrappedValue.append(HLCompiledFunction(function: HLFunction(
            type: types.getResolvable(1),
            findex: 1, 
            regs: [types.getResolvable(0)], // void
            ops: [HLOpCode.OLabel, HLOpCode.ORet(ret: 0)],
            assigns: []
        ), memory: ptr))
        functionTable.wrappedValue.append(HLCompiledFunction(function: HLFunction(
            type: types.getResolvable(1),
            findex: 3, 
            regs: [types.getResolvable(0), types.getResolvable(0)], // void, void
            ops: [HLOpCode.ORet(ret: 0)],
            assigns: []
        ), memory: ptr))

        let sut = WholeFunctionsTable(natives: natives, compiledFunctions: functions, jitBase: SharedStorage(wrappedValue: nil))
        try sut.requireReady()
        
        XCTAssertEqual("native/lib1/nat1/0", (try sut.get(0)).wholeFunctionDebugDescription)
        XCTAssertEqual("compiled/ops:2/regs:1/1", (try sut.get(1)).wholeFunctionDebugDescription)
        XCTAssertEqual("native/lib1/nat2/2", (try sut.get(2)).wholeFunctionDebugDescription)
        XCTAssertEqual("compiled/ops:1/regs:2/3", (try sut.get(3)).wholeFunctionDebugDescription)
    }

    func testRequireReady_duplicateFindex() throws {
        let nativeTable = SharedStorage(wrappedValue: [HLNative]())
        let functionTable = SharedStorage(wrappedValue: [HLCompiledFunction]())
        let typeTable = SharedStorage(wrappedValue: [
            HLType.void
        ])
        let stringTable = SharedStorage(wrappedValue: [
            "nat1", // 0
            "nat2", // 1
            "lib1", // 2
            "lib2", // 3
        ])
        
        let natives = TableResolver(table: nativeTable, count: 2)
        let functions = TableResolver(table: functionTable, count: 0)
        let strings = TableResolver(table: stringTable, count: Int32(stringTable.wrappedValue.count))
        let types = TableResolver(table: typeTable, count: 2)

        typeTable.wrappedValue.append(HLType.fun(HLTypeFunData(args: [], ret: types.getResolvable(0 /*void*/))))
        
        var dummy = [5]
        let ptr: UnsafeMutableRawPointer = UnsafeMutableRawPointer(mutating: dummy)
        
        // populate native
        nativeTable.wrappedValue.append(HLNative(
            lib: strings.getResolvable(4),
            name: strings.getResolvable(0),
            type: types.getResolvable(1),
            findex: 1,
            memory: ptr))
        nativeTable.wrappedValue.append(HLNative(
            lib: strings.getResolvable(4),
            name: strings.getResolvable(1),
            type: types.getResolvable(1),
            findex: 1,
            memory: ptr))

        let sut = WholeFunctionsTable(natives: natives, compiledFunctions: functions, jitBase: SharedStorage(wrappedValue: nil))

        XCTAssertThrowsError(try sut.requireReady())
        { error in
            XCTAssertEqual(
                error as? swiftasm.WholeFunctionsTableError,
                WholeFunctionsTableError.invalidUnderlyingData("Some function indexes are duplicated")
            )
        }
    }

    // TODO: add helper for adding debug printing

    func testRequireReady_findexGap() throws {
        let nativeTable = SharedStorage(wrappedValue: [HLNative]())
        let functionTable = SharedStorage(wrappedValue: [HLCompiledFunction]())
        let typeTable = SharedStorage(wrappedValue: [
            HLType.void
        ])
        let stringTable = SharedStorage(wrappedValue: [
            "nat1", // 0
            "nat2", // 1
            "lib1", // 2
            "lib2", // 3
        ])
        
        let natives = TableResolver(table: nativeTable, count: 2)
        let functions = TableResolver(table: functionTable, count: 0)
        let strings = TableResolver(table: stringTable, count: Int32(stringTable.wrappedValue.count))
        let types = TableResolver(table: typeTable, count: 2)

        typeTable.wrappedValue.append(HLType.fun(HLTypeFunData(args: [], ret: types.getResolvable(0 /*void*/))))
        
        var dummy = [5]
        let ptr: UnsafeMutableRawPointer = UnsafeMutableRawPointer(mutating: dummy)

        // populate native
        nativeTable.wrappedValue.append(HLNative(
            lib: strings.getResolvable(4),
            name: strings.getResolvable(0),
            type: types.getResolvable(1),
            findex: 0,
            memory: ptr))
        nativeTable.wrappedValue.append(HLNative(
            lib: strings.getResolvable(4),
            name: strings.getResolvable(1),
            type: types.getResolvable(1),
            findex: 2,
            memory: ptr))

        let sut = WholeFunctionsTable(natives: natives, compiledFunctions: functions, jitBase: SharedStorage(wrappedValue: nil))

        XCTAssertThrowsError(try sut.requireReady())
        { error in
            XCTAssertEqual(
                error as? swiftasm.WholeFunctionsTableError,
                WholeFunctionsTableError.invalidUnderlyingData("Function indexes have a gap")
            )
        }
    }
}
