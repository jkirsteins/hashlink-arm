struct FunctionCallable2 : Callable2, CustomDebugStringConvertible {
    let function: UnsafePointer<HLFunction_CCompat>
    let address: any MemoryAddress
    
    var findex: RefFun { RefFun(function.pointee.findex) }
    
    var typeProvider: any HLTypeProvider { function.pointee.typePtr! }
    
    var debugDescription: String {
        "FunctionCallable2<\(function.debugDescription)>"
    }
}
