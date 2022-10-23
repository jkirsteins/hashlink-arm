struct FunctionAddresses {
    let entries: [FunctionAddresses.Entry] 

    enum Entry: Callable {
        case native(any NativeCallable)
        case compilable(any Compilable)
        
        var entrypoint: any MemoryAddress {
            switch(self) {
            case .compilable(let c): return c.entrypoint
            case .native(let c): return c.entrypoint
            }
        }
        
        var ret: Resolvable<HLType> {
            switch(self) {
            case .compilable(let c): return c.ret
            case .native(let c): return c.ret
            }
        }
        
        var args: [Resolvable<HLType>] {
            switch(self) {
            case .compilable(let c): return c.args
            case .native(let c): return c.args
            }
        }
    }

    func get(_ ix: Int) -> Entry {
        entries[ix]
    }
    
    /// NOTE: This should be only used in tests, will leak memory
    init(_ storage: ModuleStorage, jitBase: JitBase) {
        let functions: [(Int64, Entry)] = storage.functionResolver.table.map { fun in
            return (Int64(fun.findex), Entry.compilable(
                HLCompiledFunction(function: fun, memory: FullyDeferredRelativeAddress(jitBase: jitBase))
            ))
        }
        
        let natives: [(Int64, Entry)] = storage.nativeResolver.table.map { native in
            return (Int64(native.findex), Entry.native(native))
        }
        
        let addressesWIndexes = (functions + natives).sorted(by: { $0.0 < $1.0})
        let wholeTableIndexes = addressesWIndexes.map({ $0.0 })
        
        // function indexes can't have duplicates
        assert(Array(Set(wholeTableIndexes)).sorted() == wholeTableIndexes.sorted())

        // function indexes can't have a gap
        if wholeTableIndexes.count > 0 {
            assert(wholeTableIndexes[wholeTableIndexes.count - 1] == wholeTableIndexes.count - 1)
        }

        self.entries = addressesWIndexes.map { $0.1 }
        assert(self.entries.count == storage.nativeResolver.count + storage.functionResolver.count)
    }

    init(_ hlcode: HLCode_CCompat, jitBase: JitBase) {
        let faddr: [(Int64, Entry)] = (0..<hlcode.nfunctions).map { loopIx in
            let funPtr = hlcode.getFunction(Int(loopIx))
            let fix = Int64(funPtr.pointee.findex)
            return (
                fix,
                Entry.compilable(
                    HLFunction_CCompat__WithMemory(
                        ptr: funPtr,
                        entrypoint: FullyDeferredRelativeAddress(jitBase: jitBase)
                    )
                )
            )
        }
        
        let naddr: [(Int64, Entry)] = (0..<hlcode.nnatives).map { nix in
            let natPtr = hlcode.getNative(Int(nix))
            let rlib = ResolvedLibrary(name: natPtr.pointee.lib)
            let rfun = rlib.get(natPtr.pointee.name)
            return (natPtr.pointee.findex, .native(HLNative_CCompat__Resolved(ptr: natPtr, entrypoint: rfun)))
        }

        let addressesWIndexes = (faddr + naddr).sorted(by: { $0.0 < $1.0})
        let wholeTableIndexes = addressesWIndexes.map({ $0.0 })
        
        // function indexes can't have duplicates
        assert(Array(Set(wholeTableIndexes)).sorted() == wholeTableIndexes.sorted())

        // function indexes can't have a gap
        if wholeTableIndexes.count > 0 {
            assert(wholeTableIndexes[wholeTableIndexes.count - 1] == wholeTableIndexes.count - 1)
        }

        self.entries = addressesWIndexes.map { $0.1 }
        assert(self.entries.count == hlcode.nfunctions + hlcode.nnatives)
    }
}
