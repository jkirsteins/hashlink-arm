import Foundation

protocol ModuleHeader : CustomDebugStringConvertible {
    var version: UInt8 { get }
    var	flags: Int32 { get }
    var	nints: Int32 { get }
    var	nfloats: Int32 { get }
    var	nstrings: Int32 { get }
    var	nbytes: Int32 { get }
    var	ntypes: Int32 { get }
    var	nglobals: Int32 { get }
    var	nnatives: Int32 { get }
    var	nfunctions: Int32 { get }
    var	nconstants: Int32 { get }
    var	entrypoint: Int32 { get }
}

struct ModuleHeader_v4 : ModuleHeader {
    let signature: ModuleSignature
    var version: UInt8 { signature.v }

    let	flags: Int32
    let	nints: Int32
    let	nfloats: Int32
    let	nstrings: Int32
    var	nbytes: Int32 { 0 }
    let	ntypes: Int32
    let	nglobals: Int32
    let	nnatives: Int32
    let	nfunctions: Int32
    let	nconstants: Int32
    let	entrypoint: Int32

    var debugDescription: String {
return """
hl v\(version)
entry @\(entrypoint)
\(nstrings) strings
\(0) bytes
\(nints) ints
\(nfloats) floats
\(nglobals) globals
\(nnatives) natives
\(nfunctions) functions
??? objects protos (not types)
\(nconstants) constant values
"""
    }
}

struct ModuleSignature {
    let h: UInt8
    let l: UInt8
    let b: UInt8
    let v: UInt8
}

/* Contains nbytes over v4 */
struct ModuleHeader_v5 : ModuleHeader {
    let signature: ModuleSignature
    var version: UInt8 { signature.v }

    let	flags: Int32
    let	nints: Int32
    let	nfloats: Int32
    let	nstrings: Int32
    let	nbytes: Int32
    let	ntypes: Int32
    let	nglobals: Int32
    let	nnatives: Int32
    let	nfunctions: Int32
    let	nconstants: Int32
    let	entrypoint: Int32

    var debugDescription: String {
return """
hl v\(version)
entry \(entrypoint)
\(nstrings) strings
\(nbytes) bytes
\(nints) ints
\(nfloats) floats
\(nglobals) globals
\(nnatives) natives
\(nfunctions) functions
??? objects protos (not types)
\(nconstants) constant values
"""
    }
}

class ByteReader
{
    let data: Data
    var pointer = 0
    
    init(_ data: Data) {
        self.data = data
    }

    func readVarInt() throws -> Int32 {
        let b = try readOctetAsInt32()
        if b & 0x80 == 0 {
            return (Int32)(b & 0x7F)
        } else if b & 0x40 == 0 {
            let v = (try readOctetAsInt32()) | ((b & 31) << 8);
            
            if b & 0x20 == 0 { return v } else { return -v }
        } else {
            let c = try readOctetAsInt32()
            let d = try readOctetAsInt32()
            let e = try readOctetAsInt32()
            let v = ((b & 31) << 24) | (c << 16) | (d << 8) | e;
            
            if b & 0x20 == 0 { return v } else { return -v }
        }
    }

    func readHeader() throws -> ModuleHeader {
        guard self.pointer == 0 else {
            fatalError("Don't read the header if pointer not at start")
        }
        
        let sig = ModuleSignature(
            h: try self.readUInt8(), 
            l: try self.readUInt8(), 
            b: try self.readUInt8(), 
            v: try self.readUInt8())

        guard String(bytes: [sig.h, sig.l, sig.b], encoding: .ascii)! == "HLB" else {
            fatalError("Invalid header (first three bytes must be HLB)")
        }

        guard sig.v == 4 else {
            fatalError("Supported version is 4")
        }

        let result = ModuleHeader_v4(
            signature: sig, 
            flags: try self.readVarInt(), 
            nints: try self.readVarInt(),
            nfloats: try self.readVarInt(),
            nstrings: try self.readVarInt(),
            ntypes: try self.readVarInt(),
            nglobals: try self.readVarInt(),
            nnatives: try self.readVarInt(),
            nfunctions: try self.readVarInt(),
            nconstants: try self.readVarInt(),
            entrypoint: try self.readVarInt())

        return result
    }

    func readOctetAsInt32() throws -> Int32 {
        return Int32(try readUInt8())
    }

    func readUInt8() throws -> UInt8 {
        let result = self.data[
            self.data.startIndex.advanced(by: pointer)..<self.data.startIndex.advanced(by: pointer+1)]

        defer { pointer += 1 }

        return result.reduce(0) {
            _, next in 

            next
        }
    }
}