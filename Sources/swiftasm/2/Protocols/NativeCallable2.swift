protocol NativeCallable2: Callable2 {
    var libProvider: any StringProvider { get }
    var nameProvider: any StringProvider { get }
}
