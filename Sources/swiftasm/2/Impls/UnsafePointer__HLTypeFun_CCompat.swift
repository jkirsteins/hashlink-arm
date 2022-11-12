
extension UnsafePointer<HLTypeFun_CCompat> : HLTypeFunProvider {
    var argsProvider: [any HLTypeProvider] { self.pointee.argsProvider }
    var retProvider: any HLTypeProvider { self.pointee.retProvider }
}
