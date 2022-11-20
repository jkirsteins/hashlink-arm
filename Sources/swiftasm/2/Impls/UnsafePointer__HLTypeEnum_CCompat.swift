extension UnsafePointer<HLTypeEnum_CCompat> : HLTypeEnumProvider {
    var nameProvider: StringProvider {
        self.pointee.nameProvider
    }
    
    var constructsProvider: [any HLEnumConstructProvider] {
        self.pointee.constructsProvider
    }
    
    var global_value: UnsafeRawPointer {
        self.pointee.global_value
    }
}
