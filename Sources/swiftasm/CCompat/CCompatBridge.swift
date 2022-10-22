//
//  File.swift
//  
//
//  Created by Janis Kirsteins on 21/10/2022.
//

import Foundation

//let unsafeCompat: UnsafeMutablePointer<HLCode_CCompat> = UnsafeMutablePointer.allocate(capacity: 1)
//unsafeCompat.initialize(to: HLCode_CCompat.from(storage))

protocol CCompatProvider {
    associatedtype CCompatType
    
    func createCCompatPointer() -> UnsafePointer<CCompatType>
    func createCCompatInstance() -> CCompatType
}

extension CCompatProvider {
    func createCCompatPointer() -> UnsafePointer<CCompatType> {
        let res: UnsafeMutablePointer<CCompatType> = .allocate(capacity: 1)
        res.initialize(to: self.createCCompatInstance())
        return UnsafePointer(res)
    }
    func createCCompatInstance() -> CCompatType {
        fatalError()
    }
}

extension HLTypeFunData : CCompatProvider {
    func createCCompatInstance() -> HLType_CCompat_Fun {
        let argPtr = self.args.map { $0.value }.createCCompatPointer()
        let retPtr = self.ret.value.createCCompatPointer()
        return HLType_CCompat_Fun(
            argsPtr: argPtr.createCCompatPointer(),
            retPtr: retPtr,
            nargs: UInt32(args.count),
            
            // rest are not real!
            parent: UnsafePointer(bitPattern: 123)!,
            closure_type: HLType_CCompat_Fun_ClosureType(kind: .void, p: UnsafeRawPointer(bitPattern: 123)!),
            closure: HLType_CCompat_Fun_Closure(argsPtr: nil, ret: nil, nargs: 0, parent: nil))
    }
}


extension HLTypeObjData : CCompatProvider {
    func createCCompatInstance() -> HLType_CCompat_Obj {
        fatalError("not implem")
//        let res = HLType_CCompat_Obj(
//            nfields: Int32(self.fields.count),
//            nproto: Int32(self.protos.count),
//            nbindings: Int32(self.bindings.count),
//            namePtr: .init(mutating: self.name.value.createCCompatPointer()),
//            superTypePtr: .init(mutating: self.superType?.value.createCCompatPointer()),
//            fieldsPtr: .init(mutating: self.fields.createCCompatPointer()),
//            proto: "",//self.protos.createCCompatPointer(),
//            bindings: "",//self.bindings.createCCompatInstance(),
//            globalValue: "",//.allocate(byteCount: 8, alignment: 1),
//            moduleContext: "",//.allocate(byteCount: 24, alignment: 1),
//            rt: ""//.allocate(byteCount: 8, alignment: 1)
//        )
//        fatalError("wip")
    }
}

extension HLType : CCompatProvider {
    func createCCompatInstance() -> HLType_CCompat {
        let unionPtr: UnsafeMutableRawPointer?
        switch(self) {
        case .i32, .i64, .void, .f32, .f64, .u8, .u16, .bool: unionPtr = nil
        case .obj(let objData): unionPtr = UnsafeMutableRawPointer(mutating: objData.createCCompatPointer())
        case .fun(let funData): unionPtr = UnsafeMutableRawPointer(mutating: funData.createCCompatPointer())
        default: fatalError("HLType#createCCompatInstance: not implemented for \(self)")
        }
        return HLType_CCompat(
            kind: self.kind,
            union: unionPtr,
            vobjProto: .allocate(byteCount: 8, alignment: 1),
            markBits: .allocate(byteCount: 4, alignment: 1))
    }
}

extension HLFunction : CCompatProvider {
    func createCCompatInstance() -> HLFunction_CCompat {
        let regsConverted = self.regs.map({$0.value}).createCCompatPointer()
        
        let regsFirst: UnsafeMutablePointer<UnsafePointer<HLType_CCompat>> = .allocate(capacity: 1)
        regsFirst.initialize(to: regsConverted)
        
        return HLFunction_CCompat(
            findex: self.findex,
            nregs: Int32(self.regs.count),
            nops: Int32(self.ops.count),
            ref: 0,
            typePtr: self.type.value.createCCompatPointer(),
            regsPtr: regsFirst,
            opsPtr: UnsafePointer(bitPattern: 0),
            debug: UnsafePointer(bitPattern: 0),
            objPtr: UnsafePointer(bitPattern: 0),
            unionPtr: UnsafePointer(bitPattern: 0))
    }
}

extension UnsafePointer : CCompatProvider {
    func createCCompatInstance() -> UnsafePointer<Pointee> {
        self
    }
}

extension Array : CCompatProvider where Element: CCompatProvider {
    typealias CCompatType = Element.CCompatType
    
    func createCCompatPointer() -> UnsafePointer<Element.CCompatType> {
        let res: UnsafeMutableBufferPointer<Element.CCompatType> = .allocate(capacity: self.count)
        
        for (ix, item) in self.enumerated() {
            let val = item.createCCompatInstance()
            res.baseAddress!.advanced(by: ix).initialize(to: val)
        }
        
        return UnsafePointer(OpaquePointer(res.baseAddress!))
    }
}

extension HLNative : CCompatProvider {
    func createCCompatInstance() -> HLNative_CCompat {
        HLNative_CCompat(
            libPtr: self.lib.value.createCCompatPointer(),
            namePtr: self.name.value.createCCompatPointer(),
            typePtr: self.type.value.createCCompatPointer(),
            findex: Int64(self.findex))
    }
}

//extension HLGlobal : CCompatProvider {
//    func createCCompat() -> UnsafePointer<HLGlobal_CCompat> {
//        fatalError()
//    }
//}
//
//extension HLConstant : CCompatProvider {
//    func createCCompat() -> UnsafePointer<HLConstant_CCompat> {
//        fatalError()
//    }
//}

extension String : CCompatProvider {
    func createCCompatPointer() -> UnsafePointer<CChar> {
        let res: UnsafeMutableBufferPointer<CChar> = .allocate(capacity: self.count + 1 /*for \0*/)
        _ = res.initialize(from: self.utf8CString)
        return UnsafePointer(OpaquePointer(res.baseAddress!))
    }
    
    func createCCompatInstance() -> CChar {
        fatalError("No instance for strings")
    }
}

extension TableResolver : CCompatProvider where T: CCompatProvider {
    typealias CCompatType = UnsafePointer<T.CCompatType>
    
    func createCCompatPointer() -> UnsafePointer<UnsafePointer<T.CCompatType>> {
        let arrayPtr = self.table.createCCompatPointer()
        let ptr: UnsafeMutablePointer<UnsafePointer<T.CCompatType>> = .allocate(capacity: 1)
        ptr.initialize(to: arrayPtr)
        return UnsafePointer(OpaquePointer(ptr))
    }
    
    func createCCompatInstance() -> UnsafePointer<T.CCompatType> {
        fatalError("wip")
    }
}

extension ModuleStorage : CCompatProvider {
    func createCCompatInstance() -> HLCode_CCompat {
        HLCode_CCompat(
            version: 4,
            nints: 0,
            nfloats: 0,
            nstrings: 0,
            nbytes: 0,
            ntypes: 0,
            nglobals: 0,
            nnatives: UInt32(self.nativeResolver.table.count),
            nfunctions: UInt32(self.functionResolver.table.count),
            nconstants: 0,
            entrypoint: 0,
            ndebugfiles: 0,
            hasdebug: false,
            ints: UnsafeMutablePointer.allocate(capacity: 0),
            floats: UnsafeMutablePointer.allocate(capacity: 0),
            strings: self.stringResolver.createCCompatPointer(),
            string_lens: UnsafeMutablePointer.allocate(capacity: 0),
            bytes: UnsafeMutablePointer.allocate(capacity: 0),
            bytes_pos: UnsafeMutablePointer.allocate(capacity: 0),
            debugfiles: UnsafeMutablePointer.allocate(capacity: 0),
            debuffiles_lens: UnsafeMutablePointer.allocate(capacity: 0),
            ustrings: UnsafeMutablePointer.allocate(capacity: 0),
            types: UnsafeMutablePointer.allocate(capacity: 0),
            globals: UnsafeMutablePointer.allocate(capacity: 0),
            natives: self.nativeResolver.createCCompatPointer().pointee,
            functions: self.functionResolver.createCCompatPointer().pointee,
            constants: UnsafeMutablePointer.allocate(capacity: 0),
            alloc: 0,
            falloc: 0)
    }
}
