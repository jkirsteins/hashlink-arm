@testable import swiftasm

struct TestNative2: NativeCallable2 {
    let findex: swiftasm.RefFun
    
    let libProvider: any StringProvider
    let nameProvider: any StringProvider
    let typeProvider: any HLTypeProvider
    
    let address: any MemoryAddress
}
