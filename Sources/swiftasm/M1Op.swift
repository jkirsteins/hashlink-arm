enum M1Op {
    case nop
    case ret

    /* Supervisor call
    
    https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/SVC--Supervisor-Call-?lang=en
    */
    case svc(UInt16)

    /*
    - 
    - STUR when not using pre/post indexing:
      https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/STUR--Store-Register--unscaled--?lang=en
    - When using pre/post indexing:
      https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/STR--immediate---Store-Register--immediate--?lang=en
    */
    case str(any Register, Offset)
    
    // https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/STUR--Store-Register--unscaled--?lang=en
    case stur(any Register, Register64, Int16)

    /* 
    Form PC-relative address adds an immediate value to the PC value to form a PC-relative address, and writes the result to the destination register.

    https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/ADR--Form-PC-relative-address-?lang=en 
    */
    case adr64(Register64, RelativeOffset)

    // case bl
    case blr(Register64)
    case bl(Int32)  // 26 bits max

    // https://developer.arm.com/documentation/dui0802/a/A64-General-Instructions/MOVZ
    case movz32(Register32, UInt16, Register32.Shift?)
    case movz64(Register64, UInt16, Register64.Shift?)

    // when SP not included:
    //  - https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/MOV--register---Move--register---an-alias-of-ORR--shifted-register--?lang=en
    //  - MOV <Wd, Wm> is an alias of ORR <Wd>, WZR, <Wm> when SP not present
    // when SP included:
    //  - https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/MOV--to-from-SP---Move-between-register-and-stack-pointer--an-alias-of-ADD--immediate--
    //  
    case movr64(Register64, Register64) 

    // https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/ORR--shifted-register---Bitwise-OR--shifted-register--?lang=en
    case orr64(Register64, Register64, Register64, Register64.Shift?)  

    // https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/MOVK--Move-wide-with-keep-
    case movk64(Register64, UInt16, Register64.Shift?)

    case stp((Register64, Register64), Offset)

    // https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/LDP--Load-Pair-of-Registers-?lang=en
    case ldp((Register64, Register64), Offset)

    /*
    # LDR (immediate)

    Loads a word or doubleword from memory and writes it to a register.

    The address that is used for the load is calculated from a base register and an immediate offset.

    Overview:
      - https://thinkingeek.com/2016/11/13/exploring-aarch64-assembler-chapter-5/

    Encoding:
        - https://developer.arm.com/documentation/ddi0596/2021-06/Index-by-Encoding/Loads-and-Stores?lang=en#ldst_pos
        - https://developer.arm.com/documentation/ddi0596/2021-06/Base-Instructions/LDR--immediate---Load-Register--immediate--?lang=en
    */
    case ldr(LdrMode)

    // should supersede .ldr(LdrMode)
    case ldr2(any Register, Offset)
}

