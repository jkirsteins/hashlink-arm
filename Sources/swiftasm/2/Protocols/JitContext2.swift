protocol JitContext2 : HLTypeListProvider, HLIntListProvider, HLFloatListProvider, HLStringListProvider, HLFunctionListProvider, HLNativeListProvider, HLByteListProvider, HLGlobalListProvider {
    var funcTracker: FunctionTracker { get }
    
    /// Used in tests
    func getOrderedCompilablesByRealIx__slow() throws -> [any Compilable2]
    func getOrderedNativesByRealIx__slow() throws -> [any NativeCallable2]
    
    func getCompilable(findex fix: RefFun) throws -> (any Compilable2)?
    
    var versionHint: Int? { get }
}
