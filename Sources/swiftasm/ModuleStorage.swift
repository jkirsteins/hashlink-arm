extension Int32: CustomDebugStringConvertible {
    public var debugDescription: String { "i32(\(self))" }
}

extension Double {
    public var debugDescription: String { "f64(\(self))" }
}

struct ModuleStorage {
    let stringTable: SharedStorage<[String]>
    let int32Table: SharedStorage<[Int32]>
    let float64Table: SharedStorage<[Double]>
    let typeTable: SharedStorage<[HLType]>
    let globalTable: SharedStorage<[HLGlobal]>
    let nativeTable: SharedStorage<[HLNative]>
    let functionTable: SharedStorage<[HLFunction]>
    let constantTable: SharedStorage<[HLConstant]>

    let stringResolver: TableResolver<String>
    let int32Resolver: TableResolver<Int32>
    let float64Resolver: TableResolver<Double>
    let typeResolver: TableResolver<HLType>
    let globalResolver: TableResolver<HLGlobal>
    let nativeResolver: TableResolver<HLNative>
    let functionResolver: TableResolver<HLFunction>
    let constantResolver: TableResolver<HLConstant>

    init(nfunctions: Int32 = 0) {
        self.init(
            nstrings: 0,
            nints: 0,
            nfloats: 0,
            ntypes: 0,
            nglobals: 0,
            nnatives: 0,
            nfunctions: nfunctions,
            nconstants: 0
        )
    }

    init(types: [HLType], functions: [HLFunction]) {
        self.init(
            nstrings: 0,
            nints: 0,
            nfloats: 0,
            ntypes: Int32(types.count),
            nglobals: 0,
            nnatives: 0,
            nfunctions: Int32(functions.count),
            nconstants: 0
        )
        self.typeTable.wrappedValue = types
        self.functionTable.wrappedValue = functions
    }

    init(functions: [HLFunction]) {
        self.init(
            nstrings: 0,
            nints: 0,
            nfloats: 0,
            ntypes: 0,
            nglobals: 0,
            nnatives: 0,
            nfunctions: Int32(functions.count),
            nconstants: 0
        )
        self.functionTable.wrappedValue = functions
    }

    init(functions: [HLFunction], ints: [Int32]) {
        self.init(
            nstrings: 0,
            nints: Int32(ints.count),
            nfloats: 0,
            ntypes: 0,
            nglobals: 0,
            nnatives: 0,
            nfunctions: Int32(functions.count),
            nconstants: 0
        )
        self.int32Table.wrappedValue = ints
        self.functionTable.wrappedValue = functions
    }

    init(functions: [HLFunction], natives: [HLNative], ints: [Int32]) {
        self.init(
            nstrings: 0,
            nints: Int32(ints.count),
            nfloats: 0,
            ntypes: 0,
            nglobals: 0,
            nnatives: Int32(natives.count),
            nfunctions: Int32(functions.count),
            nconstants: 0
        )
        self.int32Table.wrappedValue = ints
        self.functionTable.wrappedValue = functions
        self.nativeTable.wrappedValue = natives
    }

    init(nfunctions: Int32 = 0, natives: [HLNative]) {
        self.init(
            nstrings: 0,
            nints: 0,
            nfloats: 0,
            ntypes: 0,
            nglobals: 0,
            nnatives: Int32(natives.count),
            nfunctions: nfunctions,
            nconstants: 0
        )
        self.nativeTable.wrappedValue = natives
    }

    init(functions: [HLFunction], natives: [HLNative]) {
        self.init(
            nstrings: 0,
            nints: 0,
            nfloats: 0,
            ntypes: 0,
            nglobals: 0,
            nnatives: Int32(natives.count),
            nfunctions: Int32(functions.count),
            nconstants: 0
        )
        self.nativeTable.wrappedValue = natives
        self.functionTable.wrappedValue = functions
    }

    init(
        nstrings: Int32 = 0,
        nints: Int32 = 0,
        nfloats: Int32 = 0,
        ntypes: Int32 = 0,
        nglobals: Int32 = 0,
        nnatives: Int32 = 0,
        nfunctions: Int32 = 0,
        nconstants: Int32 = 0
    ) {
        let stringTable = SharedStorage(wrappedValue: [String]())
        let int32Table = SharedStorage(wrappedValue: [Int32]())
        let float64Table = SharedStorage(wrappedValue: [Double]())
        let typeTable = SharedStorage(wrappedValue: [HLType]())
        let globalTable = SharedStorage(wrappedValue: [HLGlobal]())
        let nativeTable = SharedStorage(wrappedValue: [HLNative]())
        let functionTable = SharedStorage(wrappedValue: [HLFunction]())
        let constantTable = SharedStorage(wrappedValue: [HLConstant]())

        // Storage
        self.stringTable = stringTable
        self.int32Table = int32Table
        self.float64Table = float64Table
        self.typeTable = typeTable
        self.globalTable = globalTable
        self.nativeTable = nativeTable
        self.functionTable = functionTable
        self.constantTable = constantTable

        // Resolvers
        self.stringResolver = TableResolver(table: stringTable, count: nstrings)
        self.int32Resolver = TableResolver(table: int32Table, count: nints)
        self.float64Resolver = TableResolver(table: float64Table, count: nfloats)
        self.typeResolver = TableResolver(table: typeTable, count: ntypes)
        self.globalResolver = TableResolver(table: globalTable, count: nglobals)
        self.nativeResolver = TableResolver(table: nativeTable, count: nnatives)
        self.functionResolver = TableResolver(table: functionTable, count: nfunctions)
        self.constantResolver = TableResolver(table: constantTable, count: nconstants)
    }
}
