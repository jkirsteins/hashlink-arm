import XCTest

@testable import swiftasm

class CCompatTestCase : XCTestCase {
    class override func setUp() {
        LibHl.hl_global_init()
    }
    
    class override func tearDown() {
        LibHl.hl_global_free()
    }
}
