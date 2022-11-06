/**
 struct hl_alloc_block {
     int size;
     hl_alloc_block *next;
     unsigned char *p;
 };
 */
struct HLAllocBlock_CCompat: Hashable, Equatable {
    let size: Int32
    let next: UnsafePointer<HLAllocBlock_CCompat>
    let p: UnsafePointer<UInt8>
}

/**
 typedef struct { hl_alloc_block *cur; } hl_alloc;
 */
struct HLAlloc_CCompat: Hashable, Equatable {
    let cur: UnsafePointer<HLAllocBlock_CCompat>
}
