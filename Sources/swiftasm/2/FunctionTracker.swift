class FunctionTracker {
    var refs: Set<RefFun> = Set()
    var comps: Set<RefFun> = Set()
    
    func referenced(_ entry: FunctionAddresses.Entry) {
        guard case .compilable(let compilable) = entry else {
            return
        }
        
        refs.insert(compilable.getFindex())
    }
    
    func referenced2(_ call: Callable2) {
        guard type(of: call) == FunctionCallable2.self else { return }
        refs.insert(call.findex)
    }
    
    func compiled(_ ix: RefFun) {
        comps.insert(ix)
    }
}
