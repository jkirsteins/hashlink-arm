struct NativeCallable2Impl : NativeCallable2 {
    let native: UnsafePointer<HLNative_CCompat>
    let address: any MemoryAddress
    
    var findex: RefFun { RefFun(native.pointee.findex) }
    
    var typeProvider: any HLTypeProvider { native.pointee.typePtr }
    
    let name: String
    let lib: String
    
    var nameProvider: any StringProvider { name }
    var libProvider: any StringProvider { lib }
}
