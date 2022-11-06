import Foundation

public struct CStringArray {
    public let pointer: UnsafeMutablePointer<UnsafeMutablePointer<CChar>?>
    public let count: Int
    private var data: Data
    
    public var constPointer: UnsafePointer<UnsafePointer<CChar>?> {
        UnsafePointer(OpaquePointer(pointer))
    }

    public init(_ array: [String]) {
        let count = array.count

        // Allocate memory to hold the CStrings and a terminating nil
        let pointer = UnsafeMutablePointer<UnsafeMutablePointer<CChar>?>.allocate(capacity: count + 1)
        pointer.initialize(repeating: nil, count: count + 1)  // Implicit terminating nil at the end of the array

        // Populate the allocated memory with pointers to CStrings
        var e = 0
        array.forEach {
            pointer[e] = strdup($0)
            e += 1
        }

        // This uses the deallocator available on the data structure as a solution to the fact that structs do not have `deinit`
        self.data = Data(bytesNoCopy: pointer, count: MemoryLayout<UnsafeMutablePointer<CChar>>.size * count, deallocator: .custom({_,_ in
            for i in 0...count - 1 {
                free(pointer[i])
            }
            pointer.deallocate()
        }))

        self.pointer = pointer
        self.count = array.count
    }

    public subscript(index: Data.Index) -> UnsafeMutablePointer<CChar>? {
        get {
            precondition(index >= 0 && index < count, "Index out of range")
            return pointer[index]
        }
    }

    public subscript(index: Data.Index) -> String? {
        get {
            precondition(index >= 0 && index < count, "Index out of range")
            if let pointee = pointer[index] {
                return String(cString: pointee)
            }

            return nil
        }
    }
}
