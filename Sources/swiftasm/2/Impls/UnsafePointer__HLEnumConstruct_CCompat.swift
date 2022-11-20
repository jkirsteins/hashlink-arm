extension UnsafePointer<HLEnumConstruct_CCompat> : HLEnumConstructProvider {
    var nameProvider: any StringProvider {
        self.pointee.name
    }
    
    var params: UnsafePointer<UnsafePointer<HLType_CCompat>> {
        self.pointee.params
    }
    
    var size: Int32 {
        self.pointee.size
    }
    
    var hasptr: Bool {
        self.pointee.hasptr
    }
    
    var offsets: UnsafePointer<Int32> {
        self.pointee.offsets
    }
    
    
}
