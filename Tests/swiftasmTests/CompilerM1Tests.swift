import XCTest

@testable import swiftasm

final class CompilerM1Tests: XCTestCase {
    
    func testCompile_addressesAreAvailable() throws {
        let typeTable = SharedStorage(wrappedValue: [
            HLType.void
        ])

        let compiledTable: SharedStorage<[HLCompiledFunction]> = SharedStorage(wrappedValue: [])
        let nativeTable: SharedStorage<[HLNative]> = SharedStorage(wrappedValue: [])
        
        
        let types = TableResolver(table: typeTable, count: 2)
        let compiledFunctions = TableResolver(table: compiledTable, count: 2)
        let natives = TableResolver(table: nativeTable, count: 0)

        typeTable.wrappedValue.append(HLType.fun(HLTypeFunData(args: [], ret: types.getResolvable(0 /*void*/))))
        

        let sut = M1Compiler()

        let f1 = HLFunction(
            type: types.getResolvable(1),   // ()->void,
            findex: 0,
            regs: [types.getResolvable(0) /*void is always present*/],
            ops: [
                .OCall0(dst: 0 /*reg*/, fun: 1 /*findex*/),
                // .ORet(ret: 0)
            ],
            assigns: []
        )
        let f2 = HLFunction(
            type: types.getResolvable(1),   // ()->void,
            findex: 1,
            regs: [types.getResolvable(0) /*void is always present*/],
            ops: [
                .OCall0(dst: 0 /*reg*/, fun: 1 /*findex*/),
                // .ORet(ret: 0)
            ],
            assigns: []
        )

        let jitBase: SharedStorage<UnsafeMutableRawPointer?> = SharedStorage(wrappedValue: nil)
        let wft = WholeFunctionsTable(natives: natives, functions: compiledFunctions, jitBase: jitBase)
        let opb = OpBuilder()
        let ctx = JitContext(wft: wft)

        let compiled1 = try sut.compile(native: f1, into: opb, ctx: ctx)
        let compiled2 = try sut.compile(native: f2, into: opb, ctx: ctx)

        // finalize
        // compiledTable.wrappedValue += [compiled2, compiled1]
        jitBase.wrappedValue = UnsafeMutableRawPointer(bitPattern: 1337)

        try wft.requireReady()        
        let rawData = opb.build()

        let f1addr = wft.getAddr(0) // should map w jitBase
        let f2addr = wft.getAddr(1) // should be jitBase + len(f1)

        XCTAssertEqual(1337, (try wft.get(0)).memory.immediate)
        XCTAssertEqual(1573, (try wft.get(1)).memory.immediate)
        XCTAssertEqual(f1addr.immediate, (try wft.get(0)).memory.immediate)
        XCTAssertEqual(f2addr.immediate, (try wft.get(1)).memory.immediate)
        
        // TODO: nothing yet
        // XCTAssertEqual([], rawData)
    }
}
