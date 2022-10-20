struct FunctionAddresses {
    let addresses: [any MemoryAddress] 

    init(_ hlcode: HLCode_CCompat, jitBase: JitBase) {
        let faddr = (0..<hlcode.nfunctions).map { loopIx in 
            let fun = hlcode.getFunction(Int(loopIx))
            let fix = Int64(fun.findex)
            return (fix, FullyDeferredRelativeAddress(jitBase: jitBase) as any MemoryAddress)
        }
        
        let naddr = (0..<hlcode.nnatives).map { nix in
            let nat = hlcode.getNative(Int(nix))
            let rlib = ResolvedLibrary(name: nat.lib)
            let rfun = rlib.get(nat.name)
            return (nat.findex, rfun as any MemoryAddress)
        }

        let addressesWIndexes = (faddr + naddr).sorted(by: { $0.0 < $1.0})
        let wholeTableIndexes = addressesWIndexes.map({ $0.0 })
        
        // function indexes can't have duplicates
        assert(Array(Set(wholeTableIndexes)).sorted() == wholeTableIndexes.sorted())

        // function indexes can't have a gap
        if wholeTableIndexes.count > 0 {
            assert(wholeTableIndexes[wholeTableIndexes.count - 1] == wholeTableIndexes.count - 1)
        }

        self.addresses = addressesWIndexes.map { $0.1 }
        assert(self.addresses.count == hlcode.nfunctions + hlcode.nnatives)
    }
}