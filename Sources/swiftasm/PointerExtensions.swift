extension UnsafePointer {
    func getArray<T>(count: Int32) -> [T] where Pointee == UnsafePointer<T> {
        let ptrArrPtr = UnsafeBufferPointer(start: self, count: Int(count))
        return ptrArrPtr.map { $0.pointee }
    }
}
