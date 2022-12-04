
class CCompatJitContext : JitContext2 {
    
    let jitBase: JitBase
    let mainContext: UnsafeMutablePointer<MainContext_CCompat>
    let funcTracker = FunctionTracker()
    
    // function index (not findex) to linkable addr
    let linkableAddresses: Dictionary<Int, any LinkableAddress>
    
    /// The writer is only needed if we are initializing the memory to match the CCompat layout
    /// (this is meant for tests. The aim is not to have a fully fledged, and compatible serializer)
    let _writer: CCompatWriter_MainContext?
    
    let filePtr: UnsafeMutableBufferPointer<CChar>?
    let libhlAllocatedCode: UnsafePointer<HLCode_CCompat>?
    
    var versionHint: Int? {
        if let v = mainContext.pointee.code?.pointee.version {
            return Int(v)
        }
        return nil
    }
    
    init(_ file: String) throws {
        let jitBase = JitBase(wrappedValue: nil)
        
        self.jitBase = jitBase
        self.mainContext = .allocate(capacity: 1)
        self.mainContext.initialize(to: MainContext_CCompat())
        self._writer = nil
        
        // file
        let fileChars = file.utf8CString
        self.filePtr = .allocate(capacity: fileChars.count)
        _ = self.filePtr!.initialize(from: fileChars)
        
        assert(self.mainContext.pointee.file == nil)
        self.mainContext.pointee.file = .init(filePtr!.baseAddress)
        
        // load code
        let code = UnsafePointer(LibHl.load_code(file))
        assert(code.pointee.alloc != 0)
        assert(self.mainContext.pointee.code == nil)
        self.mainContext.pointee.code = code
        self.libhlAllocatedCode = code
        
        // set up module
        let module = LibHl.hl_module_alloc(code)
        self.mainContext.pointee.m = module
        
        // init module
        let res = LibHl.hl_module_init(module, false)
        guard res == 1 else {
            throw GlobalError.unexpected("Failed to init module (status \(res))")
        }
        
        self.linkableAddresses = .init((0..<code.pointee.nfunctions).map { realIx in
            (Int(realIx), FullyDeferredRelativeAddress(jitBase: jitBase))
        }, uniquingKeysWith: { _, _ in fatalError("No duplicate keys allowed") })
        
        // sanity checks
        assert(module.pointee.code == code)
        assert(self.mainContext.pointee.file != nil)
        assert(self.mainContext.pointee.m != nil)
        assert(self.mainContext.pointee.m?.pointee.code == self.mainContext.pointee.code)
    }
    
    /// This will not initialize the memory. Use this with `Bootstrap.start`.
    init(_ code: UnsafePointer<HLCode_CCompat>) {
        let jitBase = JitBase(wrappedValue: nil)
        
        self.jitBase = jitBase
        self.libhlAllocatedCode = nil
        self.mainContext = .allocate(capacity: 1)
        self.mainContext.initialize(to: MainContext_CCompat())
        self._writer = nil
        self.filePtr = nil
        
        self.linkableAddresses = .init((0..<code.pointee.nfunctions).map { realIx in
            (Int(realIx), FullyDeferredRelativeAddress(jitBase: jitBase))
        }, uniquingKeysWith: { _, _ in fatalError("No duplicate keys allowed") })
    }
    
    /// This will initialize the memory. Use this for tests.
    init(ctx: any JitContext2) throws {
        let writer = try CCompatWriter_MainContext(ctx, file: #file)
        
        self.filePtr = nil
        self.libhlAllocatedCode = nil
        let jitBase = JitBase(wrappedValue: nil)
        self.jitBase = jitBase
        let mainContext: UnsafeMutablePointer<MainContext_CCompat> = .allocate(capacity: 1)
        self.mainContext = mainContext
        
        // zero the memory so we don't complain that it's already written
        mainContext.withMemoryRebound(to: UInt8.self, capacity: MemoryLayout<MainContext_CCompat>.size) {
            ptr in
            ptr.initialize(repeating: 0, count: MemoryLayout<MainContext_CCompat>.size)
        }
        
        
        self._writer = writer
        
        self.linkableAddresses = .init((0..<ctx.nfunctions).map { realIx in
            (Int(realIx), FullyDeferredRelativeAddress(jitBase: jitBase))
        }, uniquingKeysWith: { _, _ in fatalError("No duplicate keys allowed") })
        
        try writer.initialize(target: self.mainContext)
    }
    
    deinit {
        mainContext.deinitialize(count: 1)
        mainContext.deallocate()
        filePtr?.deallocate()
        
        if let libhlAllocatedCode = self.libhlAllocatedCode {
            LibHl.hl_code_free(libhlAllocatedCode)
        }
    }
    
    private func withModule<T>(_ callback: (UnsafePointer<HLModule_CCompat>)throws->T) throws -> T {
        guard let m = mainContext.pointee.m else {
            throw GlobalError.unexpected("Module data not available")
        }
        return try callback(m)
    }
    
    var ntypes: UInt32  {
        try! withModule {
            $0.pointee.code.pointee.ntypes
        }
    }
    
    var nstrings: UInt32  {
        try! withModule {
            $0.pointee.code.pointee.nstrings
        }
    }
    
    var nbytes: UInt32  {
        try! withModule {
            $0.pointee.code.pointee.nbytes
        }
    }
    
    var nglobals: UInt32  {
        try! withModule {
            $0.pointee.code.pointee.nglobals
        }
    }
    
    var nfunctions: UInt32  {
        try! withModule {
            $0.pointee.code.pointee.nfunctions
        }
    }
    
    var nnatives: UInt32  {
        try! withModule {
            $0.pointee.code.pointee.nnatives
        }
    }
    
    var nints: UInt32  {
        try! withModule {
            $0.pointee.code.pointee.nints
        }
    }
    
    var nfloats: UInt32  {
        try! withModule {
            $0.pointee.code.pointee.nfloats
        }
    }
    
    func getType(_ ix: Int) throws -> any HLTypeProvider {
        try withModule {
            return $0.pointee.code.pointee.getType(ix)
        }
    }
    
    func getInt(_ ix: Int) throws -> Int32 {
        try withModule {
            return $0.pointee.code.pointee.getInt(ix)
        }
    }
    
    func getFloat(_ ix: Int) throws -> Float64 {
        try withModule {
            return $0.pointee.code.pointee.getFloat(ix)
        }
    }
    
    func getString(_ ix: Int) throws -> any StringProvider {
        fatalError("Not implemented")
    }
    
    func getBytes(_ ix: Int) throws -> any BytesProvider {
        try withModule {
            guard let version = self.versionHint else {
                throw GlobalError.invalidOperation("Fetching bytes is version-dependent, but version is not known.")
            }
            guard version >= 5 else {
                let result = $0.pointee.code.pointee.strings.advanced(by: ix)
                return result.pointee
            }
            let offset = $0.pointee.code.pointee.bytes_pos.advanced(by: ix).pointee
            let bytes = $0.pointee.code.pointee.bytes.advanced(by: Int(offset))
            
            return bytes
        }
    }
    
    func getAllBytes_forWriters() throws -> ([UInt8], [Int32]) {
        fatalError("This method is only useful for `TestJitModule`")
    }
    
    /// Get index into the function pointers/addresses from a findex value. This works for both native/compilable functions.
    ///
    /// NOTE: you can NOT get an index into the functions/natives array from findex using this.
    func getFunctionTableIndex(findex fix: RefFun) throws -> Int32 {
        try withModule { m in
            let realIx = m.pointee.functions_indexes.advanced(by: fix).pointee
            guard realIx >= 0 && realIx < m.pointee.code.pointee.nfunctions + m.pointee.code.pointee.nnatives else {
                throw GlobalError.unexpected("Real index \(realIx) is not valid for findex \(fix)")
            }
            return realIx
        }
    }
    
    func getNativeIndex(findex fix: RefFun) throws -> Int32 {
        try withModule { m in
            let realIx = m.pointee.functions_indexes.advanced(by: fix).pointee - Int32(m.pointee.code.pointee.nfunctions)
            guard realIx >= 0 && realIx < m.pointee.code.pointee.nnatives else {
                throw GlobalError.unexpected("Native index \(realIx) is not valid for findex \(fix)")
            }
            return realIx
        }
    }
    
    func getOrderedCompilablesByRealIx__slow() throws -> [any Compilable2] {
        try withModule { m in
            let bufPointer: UnsafeBufferPointer<HLFunction_CCompat> = .init(
                start: m.pointee.code.pointee.functions,
                count: Int(m.pointee.code.pointee.nfunctions))
            return try bufPointer.map {
                guard let c = try getCompilable(findex: RefFun($0.findex)) else {
                    fatalError("Could not fetch compilable (findex=\($0.findex)) which should exist.")
                }
                return c
            }
        }
    }
    
    func getOrderedNativesByRealIx__slow() throws -> [any NativeCallable2] {
        try withModule { m in
            let bufPointer: UnsafeBufferPointer<HLNative_CCompat> = .init(
                start: m.pointee.code.pointee.natives,
                count: Int(m.pointee.code.pointee.nnatives))
            return try bufPointer.map {
                guard let c = try getNative(findex: RefFun($0.findex)) else {
                    fatalError("Could not fetch compilable (findex=\($0.findex)) which should exist.")
                }
                return c
            }
        }
    }
    
    func requireInt(_ ix: Ref) throws -> Int32 {
        guard let result = try getInt(ix) else {
            throw GlobalError.invalidOperation("Required int (ix==\(ix)) not found.")
        }
        return result
    }
    
    func requireFloat(_ ix: Ref) throws -> Float64 {
        guard let result = try getFloat(ix) else {
            throw GlobalError.invalidOperation("Required float (ix==\(ix)) not found.")
        }
        return result
    }
    
    func getInt(_ ix: Ref) throws -> Int32? {
        try withModule { m in
            guard ix < m.pointee.code.pointee.nints else {
                return nil
            }
            return m.pointee.code.pointee.getInt(ix)
        }
    }
    
    func getFloat(_ ix: Ref) throws -> Float64? {
        try withModule { m in
            guard ix < m.pointee.code.pointee.nfloats else {
                return nil
            }
            return m.pointee.code.pointee.getFloat(ix)
        }
    }
    
    func requireGlobal(_ globalRef: Ref) throws -> UnsafePointer<HLType_CCompat> {
        guard let result: UnsafePointer<HLType_CCompat> = try getGlobal(globalRef) else {
            throw GlobalError.invalidOperation("Required global (ix==\(globalRef)) not found.")
        }
        return result
    }
    
    func getGlobal(_ globalRef: Ref) throws -> UnsafePointer<HLType_CCompat>? {
        try withModule { m in
            m.pointee.code.pointee.getGlobal(globalRef)
        }
    }
    
    func getGlobal(_ globalRef: Ref) throws -> (any HLTypeProvider)? {
        let x: UnsafePointer<HLType_CCompat>? = try getGlobal(globalRef)
        return x
    }
    
    func requireGlobalData(_ globalRef: Ref) throws -> UnsafePointer<UnsafePointer<vdynamic>?> {
        guard let result = try getGlobalData(globalRef) else {
            throw GlobalError.invalidOperation("Required global data (ix==\(globalRef)) not found.")
        }
        return result
    }
    
    func getGlobalData(_ globalRef: Ref) throws -> UnsafePointer<UnsafePointer<vdynamic>?>? {
        // globals will only be obj and struct
//        void *addr = m->globals_data + m->globals_indexes[o->p2];
        
        try withModule { m in
            guard let globals_indexes = m.pointee.globals_indexes else { return .none }
            guard let globals_data = m.pointee.globals_data else { return .none }
            
            let globalIndex = globals_indexes.advanced(by: globalRef).pointee
            let globalDataPtr = globals_data.advanced(by: Int(globalIndex)).bindMemory(to: Optional<UnsafePointer<vdynamic>>.self, capacity: 1)
            
            return globalDataPtr
        }
    }
    
    func requireCallable(findex fix: RefFun) throws -> (any Callable2) {
        guard let result = try getCallable(findex: fix) else {
            throw GlobalError.invalidOperation("Required callable (fix==\(fix)) not found.")
        }
        return result
    }
    
    func getCallable(findex fix: RefFun) throws -> (any Callable2)? {
        try withModule {
            (m)->(any Callable2)? in
            
            let realIx = try getFunctionTableIndex(findex: fix)
            
            let isNative = realIx >= m.pointee.code.pointee.nfunctions
            
            if isNative {
                let nativeIndex = try self.getNativeIndex(findex: fix)
                guard let nat = self.mainContext.pointee.code?.pointee.getNative(Int(nativeIndex)) else {
                    print("Native (findex=\(fix); real=\(nativeIndex)) not found.")
                    return nil
                }
                guard let addr = self.mainContext.pointee.m?.pointee.functions_ptrs.advanced(by: fix).pointee else {
                    fatalError("Native (fix==\(fix)) not resolved.")
                }
                
                return NativeCallable2Impl(native: nat, address: addr, name: nat.pointee.name, lib: nat.pointee.lib)
            } else {
                // NOTE: realIx is the same as function index due to implementation detail that natives are sorted
                // after the HL functions.
                guard let fun = self.mainContext.pointee.code?.pointee.getFunction(Int(realIx)) else {
                    return nil
                }
                
                guard let addr = self.linkableAddresses[Int(realIx)] else {
                    fatalError("Function (fix==\(fix)) has no address.")
                }
                
                return FunctionCallable2(function: fun, address: addr)
            }
        }
        
    }
    
    var patchedOps = [RefFun:[HLOpCode]]()
    func patch(findex: RefFun, ops: [HLOpCode]) {
        self.patchedOps[findex] = ops
    }
    
    func getNative(findex fix: RefFun) throws -> (any NativeCallable2)? {
        try withModule { (m)->(NativeCallable2Impl?) in
            let addrPtr = m.pointee.functions_ptrs.advanced(by: fix)
            
            guard let addr = addrPtr.pointee else {
                return nil
            }
            
            let realIx = try getFunctionTableIndex(findex: fix)
            
            guard let nat = self.mainContext.pointee.code?.pointee.getNative(Int(realIx)) else {
                return nil
            }
            
            return NativeCallable2Impl(native: nat, address: addr, name: nat.pointee.name, lib: nat.pointee.lib)
        }
    }
    
    func getCompilable(findex fix: RefFun) throws -> (any Compilable2)? {
        try withModule { (m)->(PointerCompilable?) in
            let addr = m.pointee.functions_ptrs.advanced(by: fix)
            
            if addr.pointee != nil {
                // already compiled
                return nil
            }
            
            let realIx = try getFunctionTableIndex(findex: fix)
            
            guard let fun = self.mainContext.pointee.code?.pointee.getFunction(Int(realIx)) else {
                return nil
            }
            
            guard let funTypePtr = fun.pointee.typePtr else {
                throw GlobalError.unexpected("Function type found but it lacks function data (findex=\(fix))")
            }
            
            let regs = UnsafeBufferPointer(start: fun.pointee.regsPtr, count: Int(fun.pointee.nregs))
            
            guard let laddr = linkableAddresses[Int(realIx)] else {
                fatalError("Linkable addresses not available (real index \(realIx))")
            }
            
            let ops_final: [HLOpCode]
            if let opsPatched = patchedOps[fix] {
                ops_final = opsPatched
            } else {
                ops_final = fun.pointee.ops
            }
            
            return PointerCompilable(
                findex: fix,
                ops: ops_final,
                linkableAddress: laddr,
                regsProvider: Array(regs),
                typeProvider: funTypePtr)
        }
    }
}
