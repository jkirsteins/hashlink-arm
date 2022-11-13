protocol NativeCallable2: Callable2 {
    var libProvider: any StringProvider { get }
    var nameProvider: any StringProvider { get }
}

extension NativeCallable2 {
    var compilable: Bool { false }
}
