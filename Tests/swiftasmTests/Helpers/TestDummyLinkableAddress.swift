@testable import swiftasm

struct TestDummyLinkableAddress : LinkableAddress {
    func setOffset(_ offset: swiftasm.ByteCount) { fatalError("Don't use the test dummy") }
    var hasOffset: Bool { fatalError("Don't use the test dummy") }
    var value: UnsafeMutableRawPointer { fatalError("Don't use the test dummy") }
    func isEqual(_ to: any swiftasm.MemoryAddress) -> Bool { fatalError("Don't use the test dummy") }
}
