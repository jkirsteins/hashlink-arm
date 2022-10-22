import Foundation

// protocol ModuleHeader : CustomDebugStringConvertible {
//     var version: UInt8 { get }
//     var	flags: Int32 { get }
//     var	nints: Int32 { get }
//     var	nfloats: Int32 { get }
//     var	nstrings: Int32 { get }
//     var	nbytes: Int32 { get }
//     var	ntypes: Int32 { get }
//     var	nglobals: Int32 { get }
//     var	nnatives: Int32 { get }
//     var	nfunctions: Int32 { get }
//     var	nconstants: Int32 { get }
//     var	entrypoint: Int32 { get }
// }

struct Constants {

}

struct ModuleSignature {
    let h: UInt8
    let l: UInt8
    let b: UInt8
    let v: UInt8

    init(h: UInt8, l: UInt8, b: UInt8, v: UInt8) {
        self.h = h 
        self.l = l 
        self.b = b 
        self.v = v 
    }

    static func valid() -> ModuleSignature {
        ModuleSignature(
            h: UInt8(ascii: "H"),
            l: UInt8(ascii: "L"),
            b: UInt8(ascii: "B"),
            v: 4 
        )
    }

    func validate() {
        let firstB = String(bytes: [h, l, b], encoding: .ascii)!
        guard firstB == "HLB" else {
            fatalError(
                "Invalid header (first three bytes must be HLB but got \(firstB))"
            )
        }
    }
}

class ByteReader {
    let data: Data
    var pointer = 0

    init(_ data: Data) { self.data = data }

    func readIndex() throws -> TableIndex { return TableIndex(try readVarInt()) }

    func readVarInt() throws -> Int32 {
        let b = try readOctetAsInt32()
        if b & 0x80 == 0 {
            return b & 0x7F
        }
        else if b & 0x40 == 0 {
            let v = (try readOctetAsInt32()) | ((b & 31) << 8)

            if b & 0x20 == 0 {
                return v
            }
            else {
                return -v
            }
        }
        else {
            let c = try readOctetAsInt32()
            let d = try readOctetAsInt32()
            let e = try readOctetAsInt32()
            let v = ((b & 31) << 24) | (c << 16) | (d << 8) | e

            if b & 0x20 == 0 {
                return v
            }
            else {
                return -v
            }
        }
    }

    func readReg() throws -> Reg { try readVarInt() }

    func readJumpOffset() throws -> JumpOffset { try readVarInt() }

    func readBool() throws -> Int32 { try readVarInt() }

    func readRef() throws -> Ref { try readIndex() }

    func readModule() throws -> Module {
        guard self.pointer == 0 else {
            fatalError("Don't read the header if pointer not at start")
        }

        let sig = ModuleSignature(
            h: try self.readUInt8(),
            l: try self.readUInt8(),
            b: try self.readUInt8(),
            v: try self.readUInt8()
        )

        let firstB = String(bytes: [sig.h, sig.l, sig.b], encoding: .ascii)!
        guard firstB == "HLB" else {
            fatalError(
                "Invalid header (first three bytes must be HLB but got \(firstB))"
            )
        }

        guard sig.v == 4 else { fatalError("Supported version is 4") }

        //
        let flags = try self.readVarInt()
        let nints = try self.readVarInt()
        let nfloats = try self.readVarInt()
        let nstrings = try self.readVarInt()
        let nbytes = sig.v >= 5 ? try self.readVarInt() : 0
        let ntypes = try self.readVarInt()
        let nglobals = try self.readVarInt()
        let nnatives = try self.readVarInt()
        let nfunctions = try self.readVarInt()
        let nconstants = sig.v >= 4 ? try self.readVarInt() : 0
        let entrypoint = try self.readVarInt()
        let constInts = try Array(repeating: 0, count: Int(nints)).map { _ in
            try self.readInt32()
        }
        let constFloats = try Array(repeating: 0, count: Int(nfloats)).map { _ in
            try self.readDouble()
        }

        let storage = ModuleStorage(
            nstrings: nstrings, 
            nints: nints,
            nfloats: nfloats,
            ntypes: ntypes, 
            nglobals: nglobals, 
            nnatives: nnatives, 
            nfunctions: nfunctions, 
            nconstants: nconstants)
        storage.stringTable.wrappedValue = try self.readStrings(nstrings)
        storage.int32Table.wrappedValue = constInts
        storage.float64Table.wrappedValue = constFloats

        if sig.v >= 5 { fatalError("byte reading not implemented") }

        let hasdebug = (flags & 1 != 0)
        let debugEntries: [String]
        if hasdebug {
            let debugEntryCount = try self.readVarInt()
            debugEntries = try self.readStrings(debugEntryCount)
        }
        else {
            debugEntries = []
        }
        _ = debugEntries // suppress warning about usage

        // only need _resolvableTypes for debug printing
        let _resolvableTypes = try Array(repeating: 0, count: Int(ntypes)).enumerated()
            .map { ix, _ in

                let type = try HLType.read(
                    from: self,
                    strings: storage.stringResolver,
                    types: storage.typeResolver
                )
                storage.typeTable.wrappedValue += [type]

                return storage.typeResolver.getResolvable(ix)
            }

        print("==> Types")
        for rt in _resolvableTypes { print("\(rt.ix): \(rt.value.debugDescription)") }

        // globals
        let _resolvableGlobals = try Array(repeating: 0, count: Int(nglobals))
            .enumerated().map { ix, _ in

                let globalTypeIx = try readIndex()
                let type = storage.typeResolver.getResolvable(globalTypeIx)
                let global = HLGlobal(type: type)
                storage.globalTable.wrappedValue += [global]

                return global
            }

        print("==> Globals")
        for (ix, rt) in _resolvableGlobals.enumerated() { print("\(ix): \(rt.debugDescription)") }
        
        print("==> Allocate global memory")
        for g in _resolvableGlobals {
            g.allocate(for: g.type.value)
        }
        print("    done")

        // natives
        let _natives = try Array(repeating: 0, count: Int(nnatives)).enumerated().map {
            ix,
            _ in

            let lib = storage.stringResolver.getResolvable(try readIndex())
            let name = storage.stringResolver.getResolvable(try readIndex())
            let type = storage.typeResolver.getResolvable(try readIndex())
            let findex = try readVarInt()

            //
            let rlib = ResolvedLibrary(name: lib.value)
            let rfun = rlib.get(name.value)

            let native = HLNative(
                lib: lib,
                name: name,
                type: type,
                findex: findex,
                memory: rfun
            )
            storage.nativeTable.wrappedValue += [native]

            return native
        }

        print("==> Natives")
        for (ix, rt) in _natives.enumerated() {
            print("\(ix) : \(rt.debugDescription)")
        }
        
        // functions
        let loadedFunctions = try Array(repeating: 0, count: Int(nfunctions))
            .enumerated().map { ix, _ in

                let type = storage.typeResolver.getResolvable(try self.readIndex())
                let findex = try self.readVarInt()
                let nregs = try self.readVarInt()
                let nops = try self.readVarInt()

                print(
                    "fun \(findex) \(type.debugDescription) \(nregs) regs \(nops) ops"
                )

                let regs = try Array(repeating: 0, count: Int(nregs)).map { _ in
                    storage.typeResolver.getResolvable(try self.readIndex())
                }

                let ops = try Array(repeating: 0, count: Int(nops)).enumerated().map {
                    pos,
                    _ in try HLOpCode.read(for: UInt32(pos), from: self)
                }

                print("ops", ops)
                print("\n==>\n")

                // debug info
                // see: https://github.com/Gui-Yom/hlbc/blob/967c0f186e4c21861ad7010be1114a031d67dd7d/src/deser.rs#L229
                if hasdebug {
                    var tmp: [(Int32, Int32)] = []
                    var currfile: Int32 = -1
                    var currline: Int32 = 0
                    var i = 0
                    while i < nops {
                        var c = Int32(try self.readUInt8())
                        if c & 1 != 0 {
                            c >>= 1
                            currfile = (c << 8) | Int32(try self.readUInt8())
                        }
                        else if c & 2 != 0 {
                            let delta = c >> 6
                            var count = (c >> 2) & 15
                            while count > 0 {
                                count -= 1
                                tmp.append((currfile, currline))
                                i += 1
                            }
                            currline += delta
                        }
                        else if c & 4 != 0 {
                            currline += c >> 3
                            tmp.append((currfile, currline))
                            i += 1
                        }
                        else {
                            let b2 = Int32(try self.readUInt8())
                            let b3 = Int32(try self.readUInt8())
                            currline = (c >> 3) | (b2 << 5) | (b3 << 13)
                            tmp.append((currfile, currline))
                            i += 1
                        }
                    }
                }

                let nassigns: Int32
                if hasdebug && sig.v >= 3 {
                    nassigns = try self.readVarInt()
                }
                else {
                    nassigns = 0
                }

                let assigns = try Array(repeating: 0, count: Int(nassigns)).map { _ in
                    HLFunctionAssign(
                        variableName: storage.stringResolver.getResolvable(
                            try self.readIndex()
                        ),
                        opcodeId: try self.readVarInt()
                    )
                }

                return HLFunction(
                    type: type,
                    findex: findex,
                    regs: regs,
                    ops: ops,
                    assigns: assigns
                )
            }.sorted(by: { $0.findex < $1.findex })

        storage.functionTable.wrappedValue = loadedFunctions

        print("==> Functions")
        for (ix, rt) in storage.functionTable.wrappedValue.enumerated() {
            print("\(ix) : \(rt.debugDescription)")
            for op in rt.ops { print("    \(op.debugDescription)") }
        }
        // constants
        storage.constantTable.wrappedValue = try Array(repeating: 0, count: Int(nconstants))
            .enumerated().map { ix, _ in let global = try self.readIndex()
                let nfields = try self.readVarInt()
                let fields = try Array(repeating: 0, count: Int(nfields)).map { _ in
                    return try self.readIndex()
                }
                return HLConstant(
                    global: storage.globalResolver.getResolvable(global),
                    fields: fields
                )
            }

        let result = Module(
            signature: sig,
            flags: flags,
            nints: nints,
            nfloats: nfloats,
            nstrings: nstrings,
            nbytes: nbytes,
            ntypes: ntypes,
            nglobals: nglobals,
            nnatives: nnatives,
            nfunctions: nfunctions,
            nconstants: nconstants,
            entrypoint: entrypoint,
            storage: storage
        )

        print("==> Asserts")
        for g in storage.globalResolver.table {
            guard g.hasAddress else {
                fatalError("Global doesn't have an address.")
            }
        }

        return result
    }

    private func skip(_ amount: Int) { self.pointer += amount }

    private func readStrings(_ count: Int32) throws -> [String] {
        try readStrings(Int(count))
    }

    private func readStrings(_ count: Int) throws -> [String] {
        let stringDataSize = try self.readUInt32()
        let expectedPostDataPointer = UInt32(pointer) + UInt32(stringDataSize)
        let strings = try Array(repeating: 0, count: count).map { _ in
            try self.readString()
        }
        guard expectedPostDataPointer == pointer else {
            fatalError("Invalid string read")
        }
        for i in 0..<count {
            let siz = try self.readVarInt()
            guard siz == strings[strings.startIndex.advanced(by: Int(i))].count else {
                fatalError(
                    "Invalid file. String length encoding doesn't match at index \(i)"
                )
            }
        }
        return strings
    }

    public func readString(length: Int) throws -> String {
        let bytes = try Array(repeating: 0, count: length).map { _ in
            try self.readUInt8()
        }
        guard let str = String(bytes: bytes, encoding: .ascii) else {
            fatalError("Could not read string of length \(length)")
        }
        guard try self.peekUInt8() != 0 else {
            fatalError("Expected no zero terminator")
        }
        return str
    }

    public func readString() throws -> String {
        var bytes = [UInt8]()
        while try peekUInt8() != 0 { bytes += [try readUInt8()] }

        _ = try readUInt8() // skip the 0 terminator

        guard let result = String(bytes: bytes, encoding: .ascii) else {
            fatalError("Failed to decode string")
        }

        return result
    }

    private func parseLEUIntX<Result>(_: Result.Type, advance: Bool = true) throws
        -> Result where Result: UnsignedInteger
    {
        let expected = MemoryLayout<Result>.size

        guard data.count >= pointer + expected else {
            fatalError("Not enough data before seeking")
        }

        let result = self.data[
            self.data.startIndex.advanced(
                by: pointer
            )..<self.data.startIndex.advanced(by: pointer + expected)
        ]

        defer { if advance { pointer += expected } }
        guard result.count >= expected else { fatalError("Not enough data") }

        return result.prefix(expected).reversed().reduce(
            0,
            { soFar, new in (soFar << 8) | Result(new) }
        )
    }

    func readOctetAsInt32() throws -> Int32 { return Int32(try readUInt8()) }

    func readUInt32() throws -> UInt32 { try parseLEUIntX(UInt32.self) }

    func readUInt64() throws -> UInt64 { try parseLEUIntX(UInt64.self) }

    func readInt32() throws -> Int32 { Int32(bitPattern: try readUInt32()) }

    func readDouble() throws -> Double { Double(bitPattern: try readUInt64()) }

    func readUInt8() throws -> UInt8 { try parseLEUIntX(UInt8.self) }

    func readUInt16() throws -> UInt16 { try parseLEUIntX(UInt16.self) }

    func readInt16() throws -> Int16 { Int16(bitPattern: try readUInt16()) }

    func peekUInt8() throws -> UInt8 { try parseLEUIntX(UInt8.self, advance: false) }
}
