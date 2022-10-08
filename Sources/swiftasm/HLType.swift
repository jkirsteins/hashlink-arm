import Foundation

struct HLTypeField : CustomDebugStringConvertible {
    let name: Resolvable<String>
    let type: Resolvable<HLType>

    var debugDescription: String {
        "\(name.value): <\(type.value.debugName)>@\(type.ix)"
    }
}

struct HLTypeBinding : CustomDebugStringConvertible {
    let fieldRefIx: Int32
    let functionIx: Int32

    var debugDescription: String {
        "HLTypeBinding<\(fieldRefIx), \(functionIx)>"
    }
}

struct HLTypeProto : CustomDebugStringConvertible{
    let name: Resolvable<String>
    let functionIx: Int32
    let pIx: Int32

    var debugDescription: String {
        "\(name.value): <fun>@\(functionIx) (\(pIx))"
    }
}

struct HLTypeEnumConstruct : CustomDebugStringConvertible 
{
    let name: Resolvable<String>
    let params: [Resolvable<HLType>] 

    var debugDescription: String {
        "enumConstruct(\(name.value))"
    }
}

struct HLTypeEnumData : CustomDebugStringConvertible {
    let name: Resolvable<String>
    let	global: Int32
    let constructs: [HLTypeEnumConstruct]
    
    var debugDescription: String {
        return "\(name.value)"
    }
}

struct HLTypeRefData : CustomDebugStringConvertible {
    let type: Resolvable<HLType>

    var debugDescription: String {
        type.value.debugName
    }
}

struct HLTypeNullData : CustomDebugStringConvertible {
    let type: Resolvable<HLType>

    var debugDescription: String {
        "null<\(type.value.debugName)>"
    }
}

struct HLTypeAbstractData : CustomDebugStringConvertible {
    let name: Resolvable<String>
    
    var debugDescription: String {
        return "\(name.value)"
    }
}

struct HLTypeVirtualData : CustomDebugStringConvertible {
    let fields: [HLTypeField]
    
    var debugDescription: String {
        return "virtual<\(fields.map { "\($0.name.value): \($0.type.debugDescription)" }.joined(separator: ", "))>"
    }
}

struct HLTypeFunData : CustomDebugStringConvertible {
    let args: [Resolvable<HLType>]
    let ret: Resolvable<HLType>

    var debugDescription: String {
        return "(\(args.map { $0.debugDescription }.joined(separator: ", "))) -> (\(ret.debugDescription))"
    }
}

extension Resolvable<String> {
    var debugDescription: String {
        "\(self.value)@\(self.ix)"
    }
}

extension Resolvable<HLType> {
    var debugDescription: String {
        let t = self.value
        switch(t) {
            case .obj(let data): return "\(data.name.value)@\(self.ix)"
            default:
            return "\(t.debugDescription)@\(self.ix)"
        }
    }
}

struct HLTypeObjData : CustomDebugStringConvertible{
    let	name: Resolvable<String>
    let	superName: Resolvable<String>?
    let	global: Int32

    let fields: [HLTypeField]
    let protos: [HLTypeProto]
    let bindings: [HLTypeBinding]

    var debugDescription: String {
"""
\(name.debugDescription) \(superName == nil ? "" : "extends \(superName!.debugDescription)")
global: \(global)
fields: \(fields.count > 0 ? "\n" : "")\((fields.map { "  \($0.debugDescription)" }).joined(separator: "\n"))
protos: \(protos.count > 0 ? "\n" : "")\(protos.map { "  \($0.debugDescription)" }.joined(separator: "\n"))
bindings: \(bindings.count > 0 ? "\n" : "")\(bindings.map { "  \($0.debugDescription)" }.joined(separator: "\n"))
"""
    }
}



enum HLType : CustomDebugStringConvertible {

    case void                           // 0
    case u8                             // 1
    case u16                            // 2
    case i32                            // 3
    case i64                            // 4
    case f32                            // 5
    case f64                            // 6
    case bool                           // 7
    case bytes                          // 8
    case dyn                            // 9
    case fun(HLTypeFunData)             // 10
    case obj(HLTypeObjData)             // 11
    case array                          // 12
    case type                           // 13
    case ref(HLTypeRefData)             // 14
    case virtual(HLTypeVirtualData)     // 15
    case dynobj                         // 16
    case abstract(HLTypeAbstractData)   // 17
    case `enum`(HLTypeEnumData)         // 18
    case null(HLTypeNullData)           // 19
    case method                         // 20
    case `struct`                       // 21

    var debugName: String {
        switch(self) {
            case .void: return "void"
            case .u8: return "u8"
            case .u16: return "u16"
            case .i32: return "i32"
            case .i64: return "i64"
            case .f32: return "f32"
            case .f64: return "f64"
            case .bool: return "bool"
            case .bytes: return "bytes" 
            case .dyn: return "dynamic"
            case .fun: return "fun"
            case .obj(let data): return data.name.value
            case .array: return "array"
            case .type: return "type"
            case .ref: return "ref"
            case .virtual: return "virtual"
            case .dynobj: return "dynobj"
            case .abstract(let data): return data.name.value
            case .`enum`(let data): return "enum(\(data.name.value))"
            case .null(let data): return "null(\(data.type.value.debugName))"
            case .method: return "method"
            case .`struct`: return "struct"
        }
    }

    var debugDescription: String {
        switch(self) {
            case .obj(let data): return data.debugDescription
            case .virtual(let data): return data.debugDescription
            case .fun(let data): return data.debugDescription
            default:
            return self.debugName
        }
    }

    static func read(
        from reader: ByteReader, 
        strings: TableResolver<String>,
        types: TableResolver<HLType>) throws -> HLType {
        let typeKind = try reader.readUInt8()
        switch typeKind {
            case 0: return .void
            case 1: return .u8
            case 2: return .u16
            case 3: return .i32
            case 4: return .i64
            case 5: return .f32
            case 6: return .f64
            case 7: return .bool
            case 8: return .bytes
            case 9: return .dyn
            case 10: return .fun(try readFunData(from: reader, types: types))
            case 11: return .obj(try readObjData(from: reader, strings: strings, types: types))
            case 12: return .array
            case 13: return .type 
            case 14: return .ref(try readRefData(from: reader, types: types))
            case 15: return .virtual(try readVirtualData(from: reader, strings: strings, types: types))
            case 17: return .abstract(try readAbstractData(from: reader, strings: strings))
            case 18: return .enum(try readEnumData(from: reader, strings: strings, types: types))
            case 19: return .null(try readNullData(from: reader, types: types))
            default:
            fatalError("Type kind \(typeKind) not supported")
        }
    }

    static func readRefData(
        from reader: ByteReader, 
        types: TableResolver<HLType>) throws -> HLTypeRefData
    {
        let ix = try reader.readIndex()
        return HLTypeRefData(type: types.getResolvable(ix))
    }

    static func readNullData(
        from reader: ByteReader, 
        types: TableResolver<HLType>) throws -> HLTypeNullData
    {
        let ix = try reader.readIndex()
        return HLTypeNullData(type: types.getResolvable(ix))
    }
    
    static func readEnumData(
        from reader: ByteReader, 
        strings: TableResolver<String>,
        types: TableResolver<HLType>) throws -> HLTypeEnumData
    {
        let ix = try reader.readIndex()
        let name = strings.getResolvable(ix)
        let global = try reader.readVarInt()
        let nconstructs = try reader.readVarInt()
        
        let constructs = try Array(repeating: 0, count: Int(nconstructs)).map {
            _ in 
            
            let name = strings.getResolvable(try reader.readIndex())
            let nparams = try reader.readVarInt()
            let params = try Array(repeating: 0, count: Int(nparams)).map {
                _ in types.getResolvable(try reader.readIndex())
            }

            return HLTypeEnumConstruct(name: name, params: params)
        }

        return HLTypeEnumData(name: name, global: global, constructs: constructs)
    }

    static func readFunData(
        from reader: ByteReader, 
        types: TableResolver<HLType>) throws -> HLTypeFunData
    {
        let nargs = try reader.readVarInt()
        let args = try Array(repeating: 0, count: Int(nargs)).map {
            _ in types.getResolvable(try reader.readIndex())
        }
        let ret = types.getResolvable(try reader.readIndex())
        return HLTypeFunData(args: args, ret: ret)
    }

    static func readAbstractData(
        from reader: ByteReader, 
        strings: TableResolver<String>) throws -> HLTypeAbstractData
    {
        let ix = try reader.readIndex()
        let name = strings.getResolvable(ix)
        print("Loaded abstract \(name.value)")
        return HLTypeAbstractData(name: name)
    }

    static func readVirtualData(
        from reader: ByteReader, 
        strings: TableResolver<String>,
        types: TableResolver<HLType>) throws -> HLTypeVirtualData {

        let nfields = try reader.readVarInt()

        let fields = try Array(repeating: 0, count: Int(nfields)).map {
            _ in 

            let name = strings.getResolvable(try reader.readIndex())
            let type = types.getResolvable(try reader.readIndex())
            return HLTypeField(
                name: name,
                type: type)
        }

        return HLTypeVirtualData(fields: fields)
    }

    static func readObjData(
        from reader: ByteReader, 
        strings: TableResolver<String>,
        types: TableResolver<HLType>) throws -> HLTypeObjData {
        
        let name = strings.getResolvable(try reader.readIndex())
        let superNameIx = try reader.readIndex()
        let superName = superNameIx >= 0 ? strings.getResolvable(superNameIx) : nil
        let global = try reader.readVarInt()
        let nfields = try reader.readVarInt()
        let nprotos = try reader.readVarInt()
        let nbindings = try reader.readVarInt()

        // HLTypeObjData
        // let	name: TableResolver<String>.Index
        // let	superName: TableResolver<String>.Index?
        // let	global: Int32

        // let fields: [HLTypeField]
        // let protos: [HLTypeProto]
        // let bindings: [HLTypeBinding]

        let fields = try Array(repeating: 0, count: Int(nfields)).map {
            _ in 

            let name = strings.getResolvable(try reader.readIndex())
            let type = types.getResolvable(try reader.readIndex())
            return HLTypeField(
                name: name,
                type: type)
        }

        let protos = try Array(repeating: 0, count: Int(nprotos)).map {
            _ in 

            let name = strings.getResolvable(try reader.readIndex())
            // print("Proto \(name.value)")
            return HLTypeProto(
                name: name,
                functionIx: try reader.readVarInt(),
                pIx: try reader.readVarInt()
            )
        }

        let bindings = try Array(repeating: 0, count: Int(nbindings)).map {
            _ in 

            return HLTypeBinding(
                fieldRefIx: try reader.readVarInt(),
                functionIx: try reader.readVarInt()
            )
        }

        return HLTypeObjData(
            name: name,
            superName: superName, 
            global: global,
            fields: fields,
            protos: protos,
            bindings: bindings)
    }
}

