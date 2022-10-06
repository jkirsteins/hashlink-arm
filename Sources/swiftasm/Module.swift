struct Module: CustomDebugStringConvertible {
    let signature: ModuleSignature
    var version: UInt8 { signature.v }

    let flags: Int32
    let nints: Int32
    let nfloats: Int32
    let nstrings: Int32

    // only v5 upwards
    let nbytes: Int32

    let ntypes: Int32
    let nglobals: Int32
    let nnatives: Int32
    let nfunctions: Int32
    let nconstants: Int32
    let entrypoint: Int32

    // [i32]
    let constInts: [Int32]
    // [f64]
    let constFloats: [Double]

    let stringResolver: TableResolver<String>

    var debugDescription: String {
        return """
            hl v\(version)
            entry @\(entrypoint)
            \(nstrings) strings
            \(0) bytes
            \(nints) ints
            \(constInts.enumerated().map { (ix, el) in "    @\(ix) : \(el)" }.joined(separator: "\n"))
            \(nfloats) floats
            \(constFloats.enumerated().map { (ix, el) in "    @\(ix) : \(el)" }.joined(separator: "\n"))
            \(nglobals) globals
            \(nnatives) natives
            \(nfunctions) functions
            ??? objects protos (not types)
            \(nconstants) constant values
            strings
            \(stringResolver.table.enumerated().map { (ix, el) in "    @\(ix) : \(el)" }.joined(separator: "\n"))
            """
    }
}