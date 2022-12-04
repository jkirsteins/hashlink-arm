def p(num, bits, offset)
    mask = bits.times.collect { |x| x }.reduce(0) { |x,y| x | (0b1 << y) }
    res = (num >> offset) & mask
    res = res.to_s(2)
    res.rjust(bits, "0")
end

def match(pattern, valIn)
    val = valIn.gsub(" ", "")
    pattern.gsub!(" ", "")
    pattern.gsub!("\t", "")
    if val.size != pattern.size 
        abort("#{val} not same len as #{pattern}")
    end
    pattern.each_char.with_index do |pchar,ix|
        if pchar == "x" 
            next
        end
        if pchar != val[ix]
            return false
        end
    end

    true
end

def lookup_loads_store_register__unsigned_immediate(val)
    size = p(val, 2, 30)
    v = p(val, 1, 26)
    opc = p(val, 2, 22)
    imm12 = p(val, 12, 10)
    rn = p(val, 5, 5)
    rt = p(val, 5, 0)
    
    puts("Loads and stores: https://developer.arm.com/documentation/ddi0596/2020-12/Index-by-Encoding/Loads-and-Stores?lang=en#ldst_pos")
    puts("    size = #{size}")
    puts("    v = #{v}")
    puts("    opc = #{opc}")
    puts("    imm12 = #{imm12}")
    puts("    rn = #{rn}")
    puts("    rt = #{rt}")
    
    valStr = "#{size}#{v}#{opc}"
    if match("x1 1 1x", valStr)
        abort("UNALLOCATED")
    elsif match("00 0 00", valStr) 
        abort("STRB (immediate)")
    elsif match("00 0 01", valStr) 
        abort("LDRB (immediate)")
    elsif match("00 0 10", valStr) 
        abort("LDRSB (immediate) — 64-bit")
    elsif match("00 0 11", valStr) 
        abort("LDRSB (immediate) — 32-bit")
    elsif match("00 1 00", valStr) 
        abort("STR (immediate, SIMD&FP) — 8-bit")
    elsif match("00 1 01", valStr)  
        abort("LDR (immediate, SIMD&FP) — 8-bit")
    elsif match("00 1 10", valStr)  
        abort("STR (immediate, SIMD&FP) — 128-bit")
    elsif match("00 1 11", valStr)  
        abort("LDR (immediate, SIMD&FP) — 128-bit")
    elsif match("01 0 00", valStr)  
        abort("STRH (immediate)")
    elsif match("01 0 01", valStr)  
        puts("LDRH (immediate)")
        puts("    https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/LDRH--immediate---Load-Register-Halfword--immediate--?lang=en")
    elsif match("01 0 10", valStr)  
        abort("LDRSH (immediate) — 64-bit")
    elsif match("01 0 11", valStr)  
        abort("LDRSH (immediate) — 32-bit")
    elsif match("01 1 00", valStr)  
        abort("STR (immediate, SIMD&FP) — 16-bit")
    elsif match("01 1 01", valStr)  
        abort("LDR (immediate, SIMD&FP) — 16-bit")
    elsif match("1x 0 11", valStr)  
        abort("UNALLOCATED")
    elsif match("1x 1 1x", valStr)  
        abort("UNALLOCATED")
    elsif match("10 0 00", valStr)  
        abort("STR (immediate) — 32-bit")
    elsif match("10 0 01", valStr)  
        abort("LDR (immediate) — 32-bit")
    elsif match("10 0 10", valStr)  
        abort("LDRSW (immediate)")
    elsif match("10 1 00", valStr)  
        abort("STR (immediate, SIMD&FP) — 32-bit")
    elsif match("10 1 01", valStr)  
        abort("LDR (immediate, SIMD&FP) — 32-bit")
    elsif match("11 0 00", valStr)  
        puts("STR (immediate) — 64-bit")
        puts("    https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/STR--immediate---Store-Register--immediate--?lang=en")
    elsif match("11 0 01", valStr)  
        abort("LDR (immediate) — 64-bit")
    elsif match("11 0 10", valStr)  
        abort("PRFM (immediate)")
    elsif match("11 1 00", valStr)  
        puts("STR (immediate, SIMD&FP) — 64-bit")
        puts("    https://developer.arm.com/documentation/ddi0596/2020-12/SIMD-FP-Instructions/STR--immediate--SIMD-FP---Store-SIMD-FP-register--immediate-offset--?lang=en")
    elsif match("11 1 01", valStr)  
        abort("LDR (immediate, SIMD&FP) — 64-bit")
    else
        abort("UNALLOCATED")
    end
    
end

def lookup_loads_store_register__register_offset(val)
    size = p(val, 2, 30)
    v = p(val, 1, 26)
    opc = p(val, 2, 22)
    rm = p(val, 5, 16)
    opt = p(val, 3, 13)
    s = p(val, 1, 12)
    rn = p(val, 5, 5)
    rt = p(val, 5, 0)

    puts("Loads and stores: https://developer.arm.com/documentation/ddi0596/2020-12/Index-by-Encoding/Loads-and-Stores?lang=en#ldst_regoff")
    puts("    size = #{size}")
    puts("    v = #{v}")
    puts("    opc = #{opc}")
    puts("    rm = #{rm}")
    puts("    opt = #{opt}")
    puts("    s = #{s}")
    puts("    rn = #{rn}")
    puts("    rt = #{rt}")
    
    valStr = "#{size}#{v}#{opc}#{opt}"
    if match("x1 1 1x xxx", valStr)
        abort("UNALLOCATED")
    elsif match("00 0 00 xxx", valStr) and !match("xx x xx 011", valStr)
        abort("STRB (register) — extended register")
    elsif match("00 0 00 011", valStr)
        puts("STRB (register) — shifted register")
        puts("    https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/STRB--register---Store-Register-Byte--register--?lang=en")
    elsif match("00 0 01 xxx", valStr) and !match("xx x xx 011", valStr)
        puts("LDRB (register) — extended register")
        puts("    https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/LDRB--register---Load-Register-Byte--register--")
    elsif match("00 0 01 011", valStr)
        puts("LDRB (register) — shifted register")
        puts("    https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/LDRB--register---Load-Register-Byte--register--")
    elsif match("00 0 10 xxx", valStr) and !match("xx x xx 011", valStr)
        abort("LDRSB (register) — 64-bit with extended register offset")
    elsif match("00 0 10 011", valStr)
        abort("LDRSB (register) — 64-bit with shifted register offset")
    elsif match("00 0 11 xxx", valStr) and !match("xx x xx 011", valStr)
        abort("LDRSB (register) — 32-bit with extended register offset")
    elsif match("00 0 11 011", valStr)
        abort("LDRSB (register) — 32-bit with shifted register offset")
    elsif match("00 1 00 xxx", valStr) and !match("xx x xx 011", valStr)
        abort("STR (register, SIMD&FP)")
    elsif match("00 1 00 011", valStr)
        abort("STR (register, SIMD&FP)")
    elsif match("00 1 01 xxx", valStr) and !match("xx x xx 011", valStr)
        abort("LDR (register, SIMD&FP)")
    elsif match("00 1 01 011", valStr)
        puts("LDR (register, SIMD&FP)")
        puts("    https://developer.arm.com/documentation/ddi0596/2020-12/SIMD-FP-Instructions/LDR--register--SIMD-FP---Load-SIMD-FP-Register--register-offset--?lang=en")
    elsif match("00 1 10 xxx", valStr)
        abort("STR (register, SIMD&FP)")
    elsif match("00 1 11 xxx", valStr)
        puts("LDR (register, SIMD&FP)")
        puts("    https://developer.arm.com/documentation/ddi0596/2020-12/SIMD-FP-Instructions/LDR--register--SIMD-FP---Load-SIMD-FP-Register--register-offset--?lang=en")
    elsif match("01 0 00 xxx", valStr)
        puts("STRH (register)")
        puts("    https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/STRH--register---Store-Register-Halfword--register--?lang=en")
    elsif match("01 0 01 xxx", valStr)
        puts("LDRH (register)")
        puts("    https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/LDRH--register---Load-Register-Halfword--register--?lang=en")
    elsif match("01 0 10 xxx", valStr)
        abort("LDRSH (register) — 64-bit")
    elsif match("01 0 11 xxx", valStr)
        abort("LDRSH (register) — 32-bit")
    elsif match("01 1 00 xxx", valStr)
        abort("STR (register, SIMD&FP)")
    elsif match("01 1 01 xxx", valStr)
        puts("LDR (register, SIMD&FP)")
        puts("    https://developer.arm.com/documentation/ddi0596/2020-12/SIMD-FP-Instructions/LDR--register--SIMD-FP---Load-SIMD-FP-Register--register-offset--?lang=en")
    elsif match("1x 0 11 xxx", valStr)
        abort("UNALLOCATED")
    elsif match("1x 1 1x xxx", valStr)
        abort("UNALLOCATED")
    elsif match("10 0 00 xxx", valStr)
        puts("STR (register) — 32-bit")
        puts("    https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/STR--register---Store-Register--register--?lang=en")
    elsif match("10 0 01 xxx", valStr)
        puts("LDR (register) — 32-bit")
        puts("    https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/LDR--register---Load-Register--register--?lang=en")
    elsif match("10 0 10 xxx", valStr)
        abort("LDRSW (register)")
    elsif match("10 1 00 xxx", valStr)
        abort("STR (register, SIMD&FP)")
    elsif match("10 1 01 xxx", valStr)
        abort("LDR (register, SIMD&FP)")
    elsif match("11 0 00 xxx", valStr)
        puts("STR (register) — 64-bit")
        puts("    https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/STR--register---Store-Register--register--?lang=en")
    elsif match("11 0 01 xxx", valStr)
        puts("LDR (register) — 64-bit")
        puts("    https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/LDR--register---Load-Register--register--?lang=en")
    elsif match("11 0 10 xxx", valStr)
        abort("PRFM (register)")
    elsif match("11 1 00 xxx", valStr)
        abort("STR (register, SIMD&FP)")
    elsif match("11 1 01 xxx", valStr)
        puts("LDR (register, SIMD&FP)")
        puts("    https://developer.arm.com/documentation/ddi0596/2020-12/SIMD-FP-Instructions/LDR--register--SIMD-FP---Load-SIMD-FP-Register--register-offset--?lang=en")
    else 
        abort("Unknown")
    end
end
    

# https://developer.arm.com/documentation/ddi0596/2020-12/Index-by-Encoding/Loads-and-Stores?lang=en
def lookup_loads_stores(val)
    op0 = p(val, 4, 28)
    op1 = p(val, 1, 26)
    op2 = p(val, 2, 23)
    op3 = p(val, 6, 16)
    op4 = p(val, 2, 10)

    puts("Loads and stores:")
    puts("    op0 = #{op0}")
    puts("    op1 = #{op1}")
    puts("    op2 = #{op2}")
    puts("    op3 = #{op3}")
    puts("    op4 = #{op4}")
    valStr = "#{op0}#{op1}#{op2}#{op3}#{op4}"
    if match("0x00 1 00 000000 xx", valStr)
        abort("Advanced SIMD load/store multiple structures")
    elsif match("0x00 1 01 0xxxxx xx", valStr)
        abort("Advanced SIMD load/store multiple structures (post-indexed)")
    elsif match("0x00 1 0x 1xxxxx xx", valStr)
        abort("UNALLOCATED")
    elsif match("0x00 1 10 x00000 xx", valStr)
        abort("Advanced SIMD load/store single structure")
    elsif match("0x00 1 x0 x1xxxx xx", valStr)
        abort("UNALLOCATED")
    elsif match("0x00 1 x0 xx1xxx xx", valStr)
        abort("UNALLOCATED")
    elsif match("0x00 1 x0 xxx1xx xx", valStr)
        abort("UNALLOCATED")
    elsif match("0x00 1 x0 xxxx1x xx", valStr)
        abort("UNALLOCATED")
    elsif match("0x00 1 x0 xxxxx1 xx", valStr)
        abort("UNALLOCATED")
    elsif match("1101 0 1x 1xxxxx xx", valStr)
        abort("Load/store memory tags")
    elsif match("1x00 1 xx xxxxxx xx", valStr)
        abort("UNALLOCATED")
    elsif match("xx00 0 0x xxxxxx xx", valStr)
        abort("Load/store exclusive")
    elsif match("xx01 0 1x 0xxxxx 00", valStr)
        abort("LDAPR/STLR (unscaled immediate)")
    elsif match("xx01 x 0x xxxxxx xx", valStr)
        abort("Load register (literal)")
    elsif match("xx10 x 00 xxxxxx xx", valStr)
        abort("Load/store no-allocate pair (offset)")
    elsif match("xx10 x 01 xxxxxx xx", valStr)
        abort("Load/store register pair (post-indexed)")
    elsif match("xx10 x 10 xxxxxx xx", valStr)
        abort("Load/store register pair (offset)")
    elsif match("xx10 x 11 xxxxxx xx", valStr)
        abort("Load/store register pair (pre-indexed)")
    elsif match("xx11 x 01 0xxxxx 00", valStr)
        abort("Load/store register (unscaled immediate)")
    elsif match("xx11 x 0x 0xxxxx 01", valStr)
        abort("Load/store register (immediate post-indexed)")
    elsif match("xx11 x 0x 0xxxxx 10", valStr)
        abort("Load/store register (unprivileged)")
    elsif match("xx11 x 0x 0xxxxx 11", valStr)
        abort("Load/store register (immediate pre-indexed)")
    elsif match("xx11 x 0x 1xxxxx 00", valStr)
        abort("Atomic memory operations")
    elsif match("xx11 x 0x 1xxxxx 10", valStr)
        lookup_loads_store_register__register_offset(val)
    elsif match("xx11 x 0x 1xxxxx x1", valStr)
        abort("Load/store register (pac)")
    elsif match("xx11 x 1x xxxxxx xx", valStr)
        lookup_loads_store_register__unsigned_immediate(val)
    else 
        abort("Unknown #{val}")
    end
end

# https://developer.arm.com/documentation/ddi0596/2020-12/Index-by-Encoding/Data-Processing----Immediate?lang=en#log_imm
def lookup_data_processing__immediate__logical(val)
    sf = p(val, 1, 31)
    opc = p(val, 2, 29)
    n = p(val, 1, 22)
    immr = p(val, 6, 16)
    imms = p(val, 6, 10)
    rn = p(val, 5, 5)
    rd = p(val, 5, 0)

    puts("Logical (immediate): https://developer.arm.com/documentation/ddi0596/2020-12/Index-by-Encoding/Data-Processing----Immediate?lang=en#log_imm")
    puts("    sf = #{sf}")
    puts("    opc = #{opc}")
    puts("    n = #{n}")
    puts("    immr = #{immr}")
    puts("    imms = #{imms}")
    puts("    rn = #{rn}")
    puts("    rd = #{rd}")
    valStr = "#{sf}#{opc}#{n}"

    if match("0 xx 1", valStr)
        abort("UNALLOCATED")
    elsif match("0 00 0", valStr)
        abort("AND (immediate) — 32-bit")
    elsif match("0 01 0", valStr)
        abort("ORR (immediate) — 32-bit")
    elsif match("0 10 0", valStr)
        abort("EOR (immediate) — 32-bit")
    elsif match("0 11 0", valStr)
        abort("ANDS (immediate) — 32-bit")
    elsif match("1 00 x", valStr) 
        puts("AND (immediate) — 64-bit")
        puts("    https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/AND--immediate---Bitwise-AND--immediate--?lang=en")
    elsif match("1 01 x", valStr)
        abort("ORR (immediate) — 64-bit")
    elsif match("1 10 x", valStr)
        abort("EOR (immediate) — 64-bit")
    elsif match("1 11 x", valStr)
        abort("ANDS (immediate) — 64-bit")
    else 
        abort("Unknown")
    end 
end

def lookup_data_processing__immediate__bitfield(val)
    sf = p(val, 1, 31)
    opc = p(val, 2, 29)
    n = p(val, 1, 22)
    immr = p(val, 6, 16)
    imms = p(val, 6, 10)
    rn = p(val, 5, 5)
    rd = p(val, 5, 0)

    puts("Bitfield: https://developer.arm.com/documentation/ddi0596/2020-12/Index-by-Encoding/Data-Processing----Immediate?lang=en#bitfield")
    puts("    sf = #{sf}")
    puts("    opc = #{opc}")
    puts("    n = #{n}")
    puts("    immr = #{immr}")
    puts("    imms = #{imms}")
    puts("    rn = #{rn}")
    puts("    rd = #{rd}")

    valStr = "#{sf}#{opc}#{n}" 
    if match("x 11 x", valStr)
        abort("UNALLOCATED")
    elsif match("0 xx 1", valStr)
        abort("UNALLOCATED")
    elsif match("0 00 0", valStr)
        abort("SBFM — 32-bit")
    elsif match("0 01 0", valStr)
        abort("BFM — 32-bit")
    elsif match("0 10 0", valStr)
        puts("UBFM — 32-bit")
        puts("    https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/UBFM--Unsigned-Bitfield-Move-?lang=en")
    elsif match("1 xx 0", valStr)
        abort("UNALLOCATED")
    elsif match("1 00 1", valStr)
        puts("SBFM — 64-bit")
        puts("    https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/SBFM--Signed-Bitfield-Move-?lang=en")
    elsif match("1 01 1", valStr)
        abort("BFM — 64-bit")
    elsif match("1 10 1", valStr)
        puts("UBFM — 64-bit")
        puts("    https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/UBFM--Unsigned-Bitfield-Move-?lang=en")
    else 
        abort("UNALLOCATED")
    end
end

def lookup_data_processing__register__addsub_shifted_register(val)
    puts("Add/subtract (shifted register): https://developer.arm.com/documentation/ddi0596/2020-12/Index-by-Encoding/Data-Processing----Register?lang=en#addsub_shift")
    sf = p(val, 1, 31)
    op = p(val, 1, 30)
    s = p(val, 1, 29)
    sh = p(val, 2, 22)
    rm = p(val, 5, 16)
    imm6 = p(val, 6, 10)
    rn = p(val, 5, 5)
    rd = p(val, 5, 0)
    puts("    sf: #{sf}")
    puts("    op: #{op}")
    puts("    S: #{s}")
    puts("    shift: #{sh}")
    puts("    Rm: #{rm}")
    puts("    imm6: #{imm6}")
    puts("    Rn: #{rn}")
    puts("    Rd: #{rd}")

    valStr = "#{sf} #{op} #{s} #{sh} #{imm6}"
    if match("x x x 11 xxxxxx", valStr)
        abort("UNALLOCATED")
    elsif match("0 x x xx 1xxxxx", valStr)
        abort("UNALLOCATED")
    elsif match("0 0 0 xx xxxxxx", valStr)
        puts("ADD (shifted register) — 32-bit")
        puts("    https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/ADD--shifted-register---Add--shifted-register--?lang=en")
    elsif match("0 0 1 xx xxxxxx", valStr)
        abort("ADDS (shifted register) — 32-bit")
    elsif match("0 1 0 xx xxxxxx", valStr)
        abort("SUB (shifted register) — 32-bit")
    elsif match("0 1 1 xx xxxxxx", valStr)
        abort("SUBS (shifted register) — 32-bit")
    elsif match("1 0 0 xx xxxxxx", valStr)
        puts("ADD (shifted register) — 64-bit")
        puts("    https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/ADD--shifted-register---Add--shifted-register--?lang=en")
    elsif match("1 0 1 xx xxxxxx", valStr)
        abort("ADDS (shifted register) — 64-bit")
    elsif match("1 1 0 xx xxxxxx", valStr)
        puts("SUB (shifted register) — 64-bit")
        puts("    https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/SUB--shifted-register---Subtract--shifted-register--?lang=en")
    elsif match("1 1 1 xx xxxxxx", valStr)
        abort("SUBS (shifted register) — 64-bit")
    else 
        abort("Unknown")
    end
end

def lookup_data_processing__register__logical_shifted_register(val)
    puts("Logical (shifted register): https://developer.arm.com/documentation/ddi0596/2020-12/Index-by-Encoding/Data-Processing----Register?lang=en#log_shift")
    sf = p(val, 1, 31)
    opc = p(val, 2, 29)
    sh = p(val, 2, 22)
    n = p(val, 1, 21)
    rm = p(val, 5, 16)
    imm6 = p(val, 6, 10)
    rn = p(val, 5, 5)
    rd = p(val, 5, 0)
    puts("    sf: #{sf}")
    puts("    opc: #{opc}")
    puts("    shift: #{sh}")
    puts("    n: #{n}")
    puts("    Rm: #{rm}")
    puts("    imm6: #{imm6}")
    puts("    Rn: #{rn}")
    puts("    Rd: #{rd}")

    valStr = "#{sf} #{opc} #{n} #{imm6}"
    matches = {
        ["0 xx x 1xxxxx"] => "UNALLOCATED",
        ["0 00 0 xxxxxx"] => "AND (shifted register) — 32-bit",
        ["0 00 1 xxxxxx"] => "BIC (shifted register) — 32-bit",
        ["0 01 0 xxxxxx"] => ["ORR (shifted register) — 32-bit", "https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/ORR--shifted-register---Bitwise-OR--shifted-register--?lang=en"],
        ["0 01 1 xxxxxx"] => "ORN (shifted register) — 32-bit",
        ["0 10 0 xxxxxx"] => ["EOR (shifted register) — 32-bit", "https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/EOR--shifted-register---Bitwise-Exclusive-OR--shifted-register--?lang=en"],
        ["0 10 1 xxxxxx"] => "EON (shifted register) — 32-bit",
        ["0 11 0 xxxxxx"] => "ANDS (shifted register) — 32-bit",
        ["0 11 1 xxxxxx"] => "BICS (shifted register) — 32-bit",
        ["1 00 0 xxxxxx"] => "AND (shifted register) — 64-bit",
        ["1 00 1 xxxxxx"] => "BIC (shifted register) — 64-bit",
        ["1 01 0 xxxxxx"] => ["ORR (shifted register) — 64-bit", "https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/ORR--shifted-register---Bitwise-OR--shifted-register--?lang=en"],
        ["1 01 1 xxxxxx"] => "ORN (shifted register) — 64-bit",
        ["1 10 0 xxxxxx"] => ["EOR (shifted register) — 64-bit", "https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/EOR--shifted-register---Bitwise-Exclusive-OR--shifted-register--?lang=en"],
        ["1 10 1 xxxxxx"] => "EON (shifted register) — 64-bit",
        ["1 11 0 xxxxxx"] => "ANDS (shifted register) — 64-bit",
        ["1 11 1 xxxxxx"] => "BICS (shifted register) — 64-bit",
    }
    _handle_matches(matches, valStr, val)
end

def lookup_data_processing__3source(val)
    puts("Data processing (3 source): https://developer.arm.com/documentation/ddi0596/2020-12/Index-by-Encoding/Data-Processing----Register?lang=en#dp_3src")
    sf = p(val, 1, 31)
    op54 = p(val, 2, 29)
    op31 = p(val, 3, 21)
    oo = p(val, 1, 15)
    rm = p(val, 5, 16)
    rd = p(val, 5, 0)
    rn = p(val, 5, 5)
    ra = p(val, 5, 10)
    valStr = "#{sf} #{op54} #{op31} #{oo}"

    puts("    sf = #{sf}")
    puts("    op54 = #{op54}")
    puts("    op31 = #{op31}")
    puts("    oo = #{oo}")
    puts("    rn = #{rn}")
    puts("    rm = #{rm}")
    puts("    rd = #{rd}")
    puts("    ra = #{ra}")
    
    matches = {
        [
      "x 00 010 1",
         "x 00 011 x",
         "x 00 100 x",
         "x 00 110 1",
         "x 00 111 x",
         "x 01    xxx x",
         "x 1x    xxx x",
         "0 00 001 0",
         "0 00 001 1",
         "0 00 010 0",
         "0 00 101 0",
         "0 00 101 1",
         "0 00 110 0"
     ] => "UNALLOCATED",
        ["0 00 000 0"] => ["MADD — 32-bit", "https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/MADD--Multiply-Add-?lang=en"],
        ["0 00 000 1"] => "MSUB — 32-bit",
        ["1 00 000 0"] => ["MADD — 64-bit", "https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/MADD--Multiply-Add-?lang=en"],
        ["1 00 000 1"] => "MSUB — 64-bit",
        ["1 00 001 0"] => "SMADDL",
        ["1 00 001 1"] => "SMSUBL",
        ["1 00 010 0"] => "SMULH",
        ["1 00 101 0"] => "UMADDL",
        ["1 00 101 1"] => "UMSUBL",
        ["1 00 110 0"] => "UMULH",
    }
    _handle_matches(matches, valStr, val)
end

def lookup_data_processing__2source(val)
    puts("Data processing (2 source): https://developer.arm.com/documentation/ddi0596/2020-12/Index-by-Encoding/Data-Processing----Register?lang=en#dp_2src")
    sf = p(val, 1, 31)
    s = p(val, 1, 29)
    opcode = p(val, 6, 10)
    rm = p(val, 5, 16)
    rd = p(val, 5, 0)
    rn = p(val, 5, 5)
    valStr = "#{sf} #{s} #{opcode}"

    puts("    sf = #{sf}")
    puts("    s = #{s}")
    puts("    opcode = #{opcode}")
    puts("    rd = #{rd}")
    puts("    rn = #{rn}")
    puts("    rm = #{rm}")
    
    matches = {
        [
            "x x 000001", "x x 011xxx", "x x 1xxxxx",
            "x 0 00011x", "x 0 001101", "x 0 00111x", "x 1 00001x", "x 1 0001xx", "x 1 001xxx", "x 1 01xxxx",
            "0 x 000000",
            "0 0 00010x",
            "0 0 001100",
            "0 0 010x11",
            "1 0 010xx0",
            "1 0 010x0x"
        ] => "UNALLOCATED",
        ["0 0 000010"] => ["UDIV — 32-bit", "https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/UDIV--Unsigned-Divide-?lang=en"],
        ["0 0 000011"] => ["SDIV — 32-bit", "https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/SDIV--Signed-Divide-?lang=en"],
        ["0 0 001000"] => "LSLV — 32-bit",
        ["0 0 001001"] => ["LSRV — 32-bit", "https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/LSRV--Logical-Shift-Right-Variable-?lang=en"],
        ["0 0 001010"] => ["ASRV — 32-bit", "https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/ASRV--Arithmetic-Shift-Right-Variable-?lang=en"],
        ["0 0 001011"] => "RORV — 32-bit",
        ["0 0 010000"] => "CRC32B, CRC32H, CRC32W, CRC32X — CRC32B",
        ["0 0 010001"] => "CRC32B, CRC32H, CRC32W, CRC32X — CRC32H",
        ["0 0 010010"] => "CRC32B, CRC32H, CRC32W, CRC32X — CRC32W",
        ["0 0 010100"] => "CRC32CB, CRC32CH, CRC32CW, CRC32CX — CRC32CB",
        ["0 0 010101"] => "CRC32CB, CRC32CH, CRC32CW, CRC32CX — CRC32CH",
        ["0 0 010110"] => "CRC32CB, CRC32CH, CRC32CW, CRC32CX — CRC32CW",
        ["1 0 000000"] => "SUBP FEAT_MTE",
        ["1 0 000010"] => ["UDIV — 64-bit", "https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/UDIV--Unsigned-Divide-?lang=en"],
        ["1 0 000011"] => "SDIV — 64-bit",
        ["1 0 000100"] => "IRG FEAT_MTE",
        ["1 0 000101"] => "GMI FEAT_MTE",
        ["1 0 001000"] => "LSLV — 64-bit",
        ["1 0 001001"] => ["LSRV — 64-bit", "https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/LSRV--Logical-Shift-Right-Variable-?lang=en"],
        ["1 0 001010"] => ["ASRV — 64-bit", "https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/ASRV--Arithmetic-Shift-Right-Variable-?lang=en"],
        ["1 0 001011"] => "RORV — 64-bit",
        ["1 0 001100"] => "PACGA FEAT_PAuth",
        ["1 0 010011"] => "CRC32B, CRC32H, CRC32W, CRC32X — CRC32X",
        ["1 0 010111"] => "CRC32CB, CRC32CH, CRC32CW, CRC32CX — CRC32CX",
        ["1 1 000000"] => "SUBPS"
    }
    _handle_matches(matches, valStr, val)
end

def _handle_matches(matches, valStr, val)
    matches.each do |patterns, target|
        patterns.each do |pattern|
            if match(pattern, valStr)
                if target.is_a? Array
                    puts("#{target[0]}")
                    puts("    #{target[1]}")
                elsif target.respond_to? :call
                    target.call(val)
                else 
                    abort(target)
                end
            end
        end
    end
    nil
end

def lookup_data_processing__register(val)
    op0 = p(val, 1, 29)
    op1 = p(val, 1, 28)
    op2 = p(val, 4, 21)
    op3 = p(val, 6, 10)

    valStr = "#{op0}#{op1}#{op2}#{op3}"

    puts("Data Processing -- Register: https://developer.arm.com/documentation/ddi0596/2020-12/Index-by-Encoding/Data-Processing----Register?lang=en")

    if match("0 1 0110 xxxxxx", valStr) 
        lookup_data_processing__2source(val)
    elsif match("1 1 0110 xxxxxx", valStr) 
        abort("Data-processing (1 source)")
    elsif match("x 0 0xxx xxxxxx", valStr) 
        lookup_data_processing__register__logical_shifted_register(val)
    elsif match("x 0 1xx0 xxxxxx", valStr)
        lookup_data_processing__register__addsub_shifted_register(val)
    elsif match("x 0 1xx1 xxxxxx", valStr)
        abort("Add/subtract (extended register)")
    elsif match("x 1 0000 000000", valStr)
        abort("Add/subtract (with carry)")
    elsif match("x 1 0000 x00001", valStr)
        abort("Rotate right into flags")
    elsif match("x 1 0000 xx0010", valStr)
        abort("Evaluate into flags")
    elsif match("x 1 0010 xxxx0x", valStr)
        abort("Conditional compare (register)")
    elsif match("x 1 0010 xxxx1x", valStr)
        abort("Conditional compare (immediate)")
    elsif match("x 1 0100 xxxxxx", valStr)
        abort("Conditional select")
    elsif match("x 1 1xxx xxxxxx", valStr)
        lookup_data_processing__3source(val)
    else
        abort("Unknown")
    end
end

# https://developer.arm.com/documentation/ddi0596/2020-12/Index-by-Encoding/Data-Processing----Immediate
def lookup_data_processing__immediate(val)
    op0 = p(val, 3, 23)

    puts("Data processing (immediate):")
    puts("    https://developer.arm.com/documentation/ddi0596/2020-12/Index-by-Encoding/Data-Processing----Immediate")

    valStr = "#{op0}" 
    if match("00x", valStr)
        abort("PC-rel. addressing")
    elsif match("010", valStr)
        abort("Add/subtract (immediate)")
    elsif match("011", valStr)
        abort("Add/subtract (immediate, with tags)")
    elsif match("100", valStr)
        lookup_data_processing__immediate__logical(val)
    elsif match("101", valStr)
        abort("Move wide (immediate)")
    elsif match("110", valStr)
        lookup_data_processing__immediate__bitfield(val)
    elsif match("111", valStr)
        abort("Extract")
    else 
        abort("Unknown")
    end 
end

def lookup_branches_exception_generating_and_system_instructions(val)
    puts("Branches, Exception Generating and System instructions: https://developer.arm.com/documentation/ddi0596/2020-12/Index-by-Encoding/Branches--Exception-Generating-and-System-instructions?lang=en")
    op0 = p(val, 3, 29)
    op1 = p(val, 14, 12)
    op2 = p(val, 5, 0)
    valStr = "#{op0} #{op1} #{op2}"
    puts("op0 = #{op0}")
    puts("op1 = #{op1}")
    puts("op2 = #{op2}")

    matches = {
        ["010 0xxxxxxxxxxxxx xxxxx"] => ["Conditional branch (immediate)", "https://developer.arm.com/documentation/ddi0596/2020-12/Index-by-Encoding/Branches--Exception-Generating-and-System-instructions?lang=en#condbranch"],      
     ["110 00xxxxxxxxxxxx xxxxx"] => "Exception generation",
     ["110 01000000110001 xxxxx"] => "System instructions with register argument",
        ["110 01000000110010 11111"] => "Hints",
        ["110 01000000110011 xxxxx"] => "Barriers",
        ["110 0100000xxx0100 xxxxx"] => "PSTATE",
        ["110 0100x01xxxxxxx xxxxx"] => "System instructions",
        ["110 0100x1xxxxxxxx xxxxx"] => "System register move",
        ["110 1xxxxxxxxxxxxx xxxxx"] => ["Unconditional branch (register)", "https://developer.arm.com/documentation/ddi0596/2020-12/Index-by-Encoding/Branches--Exception-Generating-and-System-instructions?lang=en#branch_reg"],
        ["x00   xxxxxxxxxxxxxx xxxxx"] => ["Unconditional branch (immediate)", "https://developer.arm.com/documentation/ddi0596/2020-12/Index-by-Encoding/Branches--Exception-Generating-and-System-instructions?lang=en#branch_imm"],
        ["x01 0xxxxxxxxxxxxx xxxxx"] => "Compare and branch (immediate)",
        ["x01 1xxxxxxxxxxxxx xxxxx"] => "Test and branch (immediate)"
    }
    _handle_matches(matches, valStr, val)
end

def lookup(val)
    op0 = p(val, 4, 25)
    puts("https://developer.arm.com/documentation/ddi0596/2020-12/Index-by-Encoding?lang=en")
    puts("Top-level op0: #{op0}")
    case op0
    when "0000" 
        abort("Reserved")
    when "0001"
        abort("UNALLOCATED")
    when "0010"
        abort("SVE encodings")
    when "0011"
        abort("UNALLOCATED")
    when "1000", "1001" 
        lookup_data_processing__immediate(val)
    when "1010", "1011" 
        lookup_branches_exception_generating_and_system_instructions(val)
    when "0100", "0110", "1100", "1110" 
        lookup_loads_stores(val)
    when "0101", "1101" 
        lookup_data_processing__register(val)
    when "0111", "1111" 
        lookup_data_processing__scalar_floating_point_and_advanced_simd(val)
    else
        abort("Unknown")
    end
end

def lookup_data_processing__scalar_floating_point_and_advanced_simd(val)
    puts("Data Processing -- Scalar Floating-Point and Advanced SIMD: https://developer.arm.com/documentation/ddi0596/2020-12/Index-by-Encoding/Data-Processing----Scalar-Floating-Point-and-Advanced-SIMD?lang=en")
    op0 = p(val, 4, 28)
    op1 = p(val, 2, 23)
    op2 = p(val, 4, 19)
    op3 = p(val, 9, 10)
    valStr = "#{op0} #{op1} #{op2} #{op3}"
    puts("op0 = #{op0}")
    puts("op1 = #{op1}")
    puts("op2 = #{op2}")
    puts("op3 = #{op3}")

    matches = {
        [
            "0000 0x x101 00xxxxx10",
            "0010 0x x101 00xxxxx10",
            "0101 0x x0xx xxx0xxx10",
            "0110 0x x101 00xxxxx10",
            "0111 0x x0xx xxx0xxxx0",
            "0111 0x x101 00xxxxx10",
            "01x1 01 00xx xxx0xxxx1",
            "01x1 0x 0111 00xxxxx10",
            "01x1 0x 10xx xxx01xxx1",
            "01x1 0x x0xx xxx1xxxx0",
            "01x1 0x x1xx 1xxxxxx10",
            "01x1 0x x1xx x1xxxxx10",
            "01x1 11 xxxx xxxxxxxx1",
            "0xx0 01 00xx xxx0xxxx1",
            "0xx0 0x 0111 00xxxxx10",
            "0xx0 0x 10xx xxx01xxx1",
            "0xx0 0x x0xx xxx1xxxx0",
            "0xx0 0x x1xx 1xxxxxx10",
            "0xx0 0x x1xx x1xxxxx10",
            "0xx0 11 xxxx xxxxxxxx1",
            "1xx0 1x xxxx xxxxxxxxx"] => "UNALLOCATED",
        ["0100 0x x101 00xxxxx10"] => "Cryptographic AES",
        ["0101 0x x0xx xxx0xxx00"] => "Cryptographic three-register SHA",
        ["0101 0x x101 00xxxxx10"] => "Cryptographic two-register SHA",
        ["01x1 00 00xx xxx0xxxx1"] => "Advanced SIMD scalar copy",
        ["01x1 0x 10xx xxx00xxx1"] => "Advanced SIMD scalar three same FP16 FEAT_FP16",
        ["01x1 0x 1111 00xxxxx10"] => "Advanced SIMD scalar two-register miscellaneous FP16 FEAT_FP16",
        ["01x1 0x x0xx xxx1xxxx1"] => "Advanced SIMD scalar three same extra FEAT_RDM",
        ["01x1 0x x100 00xxxxx10"] => "Advanced SIMD scalar two-register miscellaneous",
        ["01x1 0x x110 00xxxxx10"] => "Advanced SIMD scalar pairwise FEAT_FP16",
        ["01x1 0x x1xx xxxxxxx00"] => "Advanced SIMD scalar three different",
        ["01x1 0x x1xx xxxxxxxx1"] => "Advanced SIMD scalar three same",
        ["01x1 10 xxxx xxxxxxxx1"] => "Advanced SIMD scalar shift by immediate",
        ["01x1 1x xxxx xxxxxxxx0"] => "Advanced SIMD scalar x indexed element FEAT_FP16",
        ["0x00 0x x0xx xxx0xxx00"] => "Advanced SIMD table lookup",
        ["0x00 0x x0xx xxx0xxx10"] => "Advanced SIMD permute",
        ["0x10 0x x0xx xxx0xxxx0"] => "Advanced SIMD extract",
        ["0xx0 00 00xx xxx0xxxx1"] => "Advanced SIMD copy",
        ["0xx0 0x 10xx xxx00xxx1"] => "Advanced SIMD three same (FP16) FEAT_FP16",
        ["0xx0 0x 1111 00xxxxx10"] => "Advanced SIMD two-register miscellaneous (FP16) FEAT_FP16",
        ["0xx0 0x x0xx xxx1xxxx1"] => "Advanced SIMD three-register extension FEAT_DotProd",
        ["0xx0 0x x100 00xxxxx10"] => "Advanced SIMD two-register miscellaneous FEAT_FRINTTS",
        ["0xx0 0x x110 00xxxxx10"] => "Advanced SIMD across lanes FEAT_FP16",
        ["0xx0 0x x1xx xxxxxxx00"] => "Advanced SIMD three different",
        ["0xx0 0x x1xx xxxxxxxx1"] => "Advanced SIMD three same FEAT_FHM",
        ["0xx0 10 0000 xxxxxxxx1"] => "Advanced SIMD modified immediate FEAT_FP16",
        # ["0xx0 10 != 0000"] => "xxxxxxxx1 Advanced SIMD shift by immediate",
        ["0xx0 1x xxxx xxxxxxxx0"] => "Advanced SIMD vector x indexed element FEAT_DotProd",
        ["1100 00 10xx xxx10xxxx"] => "Cryptographic three-register, imm2 FEAT_SM3",
        ["1100 00 11xx xxx1x00xx"] => "Cryptographic three-register SHA 512 FEAT_SHA512",
        ["1100 00 xxxx xxx0xxxxx"] => "Cryptographic four-register FEAT_SHA3",
        ["1100 01 00xx xxxxxxxxx"] => "XAR FEAT_SHA3",
        ["1100 01 1000 0001000xx"] => "Cryptographic two-register SHA 512 FEAT_SHA512",
        ["x0x1 0x x0xx xxxxxxxxx"] => "Conversion between floating-point and fixed-point FEAT_FP16",
        ["x0x1 0x x1xx xxx000000"] => Proc.new { |val| lookup__conversion_between_floating_point_and_integer(val) },
        ["x0x1 0x x1xx xxxx10000"] => "Floating-point data-processing (1 source) FEAT_FRINTTS: https://developer.arm.com/documentation/ddi0596/2020-12/Index-by-Encoding/Data-Processing----Scalar-Floating-Point-and-Advanced-SIMD?lang=en#floatdp1",
        ["x0x1 0x x1xx xxxxx1000"] => "Floating-point compare FEAT_FP16",
        ["x0x1 0x x1xx xxxxxx100"] => "Floating-point immediate FEAT_FP16",
        ["x0x1 0x x1xx xxxxxxx01"] => "Floating-point conditional compare FEAT_FP16",
        ["x0x1 0x x1xx xxxxxxx10"] => Proc.new { |val| lookup__floating_point_data_processing__2_source(val) },
        ["x0x1 0x x1xx xxxxxxx11"] => "Floating-point conditional select FEAT_FP16",
        ["x0x1 1x xxxx xxxxxxxxx"] => "Floating-point data-processing (3 source)",
    }
    _handle_matches(matches, valStr, val)
end


def lookup__floating_point_data_processing__2_source(val)
    puts("Floating-point data-processing (2 source) FEAT_FP16: https://developer.arm.com/documentation/ddi0596/2020-12/Index-by-Encoding/Data-Processing----Scalar-Floating-Point-and-Advanced-SIMD?lang=en#floatdp2")
    m = p(val, 1, 31)
    s = p(val, 1, 29)
    ptype = p(val, 2, 22)
    rm = p(val, 5, 16)
    opcode = p(val, 4, 12)
    rn = p(val, 5, 5)
    rd = p(val, 5, 0)

    valStr = "#{m} #{s} #{ptype} #{opcode}"
    puts("m = #{m}")
    puts("s = #{s}")
    puts("ptype = #{ptype}")
    puts("rm = #{rm}")
    puts("opcode = #{opcode}")
    puts("rn = #{rn}")
    puts("rd = #{rd}")

    matches = {
        ["x x xx 1xx1"] => "UNALLOCATED -",
        ["x x xx 1x1x"] => "UNALLOCATED -",
        ["x x xx 11xx"] => "UNALLOCATED -",
        ["x x 10 xxxx"] => " UNALLOCATED -",
        ["x 1 xx xxxx"] => " UNALLOCATED -",
        ["0 0 00 0000"] => "FMUL (scalar) — single-precision -",
        ["0 0 00 0001"] => ["FDIV (scalar) — single-precision -", "https://developer.arm.com/documentation/ddi0596/2020-12/SIMD-FP-Instructions/FDIV--scalar---Floating-point-Divide--scalar--?lang=en"],
        ["0 0 00 0010"] => "FADD (scalar) — single-precision -",
        ["0 0 00 0011"] => "FSUB (scalar) — single-precision -",
        ["0 0 00 0100"] => "FMAX (scalar) — single-precision -",
        ["0 0 00 0101"] => "FMIN (scalar) — single-precision -",
        ["0 0 00 0110"] => "FMAXNM (scalar) — single-precision -",
        ["0 0 00 0111"] => "FMINNM (scalar) — single-precision -",
        ["0 0 00 1000"] => "FNMUL (scalar) — single-precision -",
        ["0 0 01 0000"] => "FMUL (scalar) — double-precision -",
        ["0 0 01 0001"] => ["FDIV (scalar) — double-precision -", "https://developer.arm.com/documentation/ddi0596/2020-12/SIMD-FP-Instructions/FDIV--scalar---Floating-point-Divide--scalar--?lang=en"],
        ["0 0 01 0010"] => "FADD (scalar) — double-precision -",
        ["0 0 01 0011"] => "FSUB (scalar) — double-precision -",
        ["0 0 01 0100"] => "FMAX (scalar) — double-precision -",
        ["0 0 01 0101"] => "FMIN (scalar) — double-precision -",
        ["0 0 01 0110"] => "FMAXNM (scalar) — double-precision -",
        ["0 0 01 0111"] => "FMINNM (scalar) — double-precision -",
        ["0 0 01 1000"] => "FNMUL (scalar) — double-precision -",
        ["0 0 11 0000"] => "FMUL (scalar) — half-precision FEAT_FP16",
        ["0 0 11 0001"] => "FDIV (scalar) — half-precision FEAT_FP16",
        ["0 0 11 0010"] => "FADD (scalar) — half-precision FEAT_FP16",
        ["0 0 11 0011"] => "FSUB (scalar) — half-precision FEAT_FP16",
        ["0 0 11 0100"] => "FMAX (scalar) — half-precision FEAT_FP16",
        ["0 0 11 0101"] => "FMIN (scalar) — half-precision FEAT_FP16",
        ["0 0 11 0110"] => "FMAXNM (scalar) — half-precision FEAT_FP16",
        ["0 0 11 0111"] => "FMINNM (scalar) — half-precision FEAT_FP16",
        ["0 0 11 1000"] => "FNMUL (scalar) — half-precision FEAT_FP16",
        ["1 x   xx  xxxx"] => "UNALLOCATED -",
    }
    _handle_matches(matches, valStr, val)
end
        

def lookup__conversion_between_floating_point_and_integer(val)
    puts("Conversion between floating-point and integer: https://developer.arm.com/documentation/ddi0596/2020-12/Index-by-Encoding/Data-Processing----Scalar-Floating-Point-and-Advanced-SIMD?lang=en#float2int")
    sf = p(val, 1, 31)
    s = p(val, 1, 29)
    ptype = p(val, 2, 22)
    rmode = p(val, 2, 19)
    opcode = p(val, 3, 16)

    valStr = "#{sf} #{s} #{ptype} #{rmode} #{opcode}"
    puts("sf = #{sf}")
    puts("s = #{s}")
    puts("ptype = #{ptype}")
    puts("rmode = #{rmode}")
    puts("opcode = #{opcode}")

    matches = {
        ["x x xx x1 01x"] => "UNALLOCATED -",
        ["x x xx x1 10x"] => "UNALLOCATED -",
        ["x x xx 1x 01x"] => "UNALLOCATED -",
        ["x x xx 1x 10x"] => "UNALLOCATED -",
        ["x 0 10 xx 0xx"] => "UNALLOCATED -",
        ["x 0 10 xx 10x"] => "UNALLOCATED -",
        ["x 1 xx xx xxx"] => " UNALLOCATED -",
        ["0 0 00 x1 11x"] => "UNALLOCATED -",
        ["0 0 00 00 000"] => "FCVTNS (scalar) — single-precision to 32-bit -",
        ["0 0 00 00 001"] => "FCVTNU (scalar) — single-precision to 32-bit -",
        ["0 0 00 00 010"] => "SCVTF (scalar, integer) — 32-bit to single-precision -",
        ["0 0 00 00 011"] => ["UCVTF (scalar, integer) — 32-bit to single-precision -", "https://developer.arm.com/documentation/ddi0596/2020-12/SIMD-FP-Instructions/UCVTF--scalar--integer---Unsigned-integer-Convert-to-Floating-point--scalar--?lang=en"],
        ["0 0 00 00 100"] => "FCVTAS (scalar) — single-precision to 32-bit -",
        ["0 0 00 00 101"] => "FCVTAU (scalar) — single-precision to 32-bit -",
        ["0 0 00 00 110"] => "FMOV (general) — single-precision to 32-bit -",
        ["0 0 00 00 111"] => "FMOV (general) — 32-bit to single-precision -",
        ["0 0 00 01 000"] => "FCVTPS (scalar) — single-precision to 32-bit -",
        ["0 0 00 01 001"] => "FCVTPU (scalar) — single-precision to 32-bit -",
        ["0 0 00 1x 11x"] => "UNALLOCATED -",
        ["0 0 00 10 000"] => "FCVTMS (scalar) — single-precision to 32-bit -",
        ["0 0 00 10 001"] => "FCVTMU (scalar) — single-precision to 32-bit -",
        ["0 0 00 11 000"] => "FCVTZS (scalar, integer) — single-precision to 32-bit -",
        ["0 0 00 11 001"] => "FCVTZU (scalar, integer) — single-precision to 32-bit -",
        ["0 0 01 0x 11x"] => "UNALLOCATED -",
        ["0 0 01 00 000"] => "FCVTNS (scalar) — double-precision to 32-bit -",
        ["0 0 01 00 001"] => "FCVTNU (scalar) — double-precision to 32-bit -",
        ["0 0 01 00 010"] => "SCVTF (scalar, integer) — 32-bit to double-precision -",
        ["0 0 01 00 011"] => ["UCVTF (scalar, integer) — 32-bit to double-precision -", "https://developer.arm.com/documentation/ddi0596/2020-12/SIMD-FP-Instructions/UCVTF--scalar--integer---Unsigned-integer-Convert-to-Floating-point--scalar--?lang=en"], 
        ["0 0 01 00 100"] => "FCVTAS (scalar) — double-precision to 32-bit -",
        ["0 0 01 00 101"] => "FCVTAU (scalar) — double-precision to 32-bit -",
        ["0 0 01 01 000"] => "FCVTPS (scalar) — double-precision to 32-bit -",
        ["0 0 01 01 001"] => "FCVTPU (scalar) — double-precision to 32-bit -",
        ["0 0 01 10 000"] => "FCVTMS (scalar) — double-precision to 32-bit -",
        ["0 0 01 10 001"] => "FCVTMU (scalar) — double-precision to 32-bit -",
        ["0 0 01 10 11x"] => "UNALLOCATED -",
        ["0 0 01 11 000"] => "FCVTZS (scalar, integer) — double-precision to 32-bit -",
        ["0 0 01 11 001"] => "FCVTZU (scalar, integer) — double-precision to 32-bit -",
        ["0 0 01 11 110"] => "FJCVTZS FEAT_JSCVT",
        ["0 0 01 11 111"] => "UNALLOCATED -",
        ["0 0 10 xx 11x"] => "UNALLOCATED -",
        ["0 0 11 00 000"] => "FCVTNS (scalar) — half-precision to 32-bit FEAT_FP16",
        ["0 0 11 00 001"] => "FCVTNU (scalar) — half-precision to 32-bit FEAT_FP16",
        ["0 0 11 00 010"] => "SCVTF (scalar, integer) — 32-bit to half-precision FEAT_FP16",
        ["0 0 11 00 011"] => "UCVTF (scalar, integer) — 32-bit to half-precision FEAT_FP16",
        ["0 0 11 00 100"] => "FCVTAS (scalar) — half-precision to 32-bit FEAT_FP16",
        ["0 0 11 00 101"] => "FCVTAU (scalar) — half-precision to 32-bit FEAT_FP16",
        ["0 0 11 00 110"] => "FMOV (general) — half-precision to 32-bit FEAT_FP16",
        ["0 0 11 00 111"] => "FMOV (general) — 32-bit to half-precision FEAT_FP16",
        ["0 0 11 01 000"] => "FCVTPS (scalar) — half-precision to 32-bit FEAT_FP16",
        ["0 0 11 01 001"] => "FCVTPU (scalar) — half-precision to 32-bit FEAT_FP16",
        ["0 0 11 10 000"] => "FCVTMS (scalar) — half-precision to 32-bit FEAT_FP16",
        ["0 0 11 10 001"] => "FCVTMU (scalar) — half-precision to 32-bit FEAT_FP16",
        ["0 0 11 11 000"] => "FCVTZS (scalar, integer) — half-precision to 32-bit FEAT_FP16",
        ["0 0 11 11 001"] => "FCVTZU (scalar, integer) — half-precision to 32-bit FEAT_FP16",
        ["1 0 00 xx 11x"] => "UNALLOCATED -",
        ["1 0 00 00 000"] => "FCVTNS (scalar) — single-precision to 64-bit -",
        ["1 0 00 00 001"] => "FCVTNU (scalar) — single-precision to 64-bit -",
        ["1 0 00 00 010"] => "SCVTF (scalar, integer) — 64-bit to single-precision -",
        ["1 0 00 00 011"] => "UCVTF (scalar, integer) — 64-bit to single-precision -",
        ["1 0 00 00 100"] => "FCVTAS (scalar) — single-precision to 64-bit -",
        ["1 0 00 00 101"] => "FCVTAU (scalar) — single-precision to 64-bit -",
        ["1 0 00 01 000"] => "FCVTPS (scalar) — single-precision to 64-bit -",
        ["1 0 00 01 001"] => "FCVTPU (scalar) — single-precision to 64-bit -",
        ["1 0 00 10 000"] => "FCVTMS (scalar) — single-precision to 64-bit -",
        ["1 0 00 10 001"] => "FCVTMU (scalar) — single-precision to 64-bit -",
        ["1 0 00 11 000"] => "FCVTZS (scalar, integer) — single-precision to 64-bit -",
        ["1 0 00 11 001"] => "FCVTZU (scalar, integer) — single-precision to 64-bit -",
        ["1 0 01 x1 11x"] => "UNALLOCATED -",
        ["1 0 01 00 000"] => "FCVTNS (scalar) — double-precision to 64-bit -",
        ["1 0 01 00 001"] => "FCVTNU (scalar) — double-precision to 64-bit -",
        ["1 0 01 00 010"] => "SCVTF (scalar, integer) — 64-bit to double-precision -",
        ["1 0 01 00 011"] => ["UCVTF (scalar, integer) — 64-bit to double-precision -", "https://developer.arm.com/documentation/ddi0596/2020-12/SIMD-FP-Instructions/UCVTF--scalar--integer---Unsigned-integer-Convert-to-Floating-point--scalar--?lang=en"],
        ["1 0 01 00 100"] => "FCVTAS (scalar) — double-precision to 64-bit -",
        ["1 0 01 00 101"] => "FCVTAU (scalar) — double-precision to 64-bit -",
        ["1 0 01 00 110"] => "FMOV (general) — double-precision to 64-bit -",
        ["1 0 01 00 111"] => "FMOV (general) — 64-bit to double-precision -",
        ["1 0 01 01 000"] => "FCVTPS (scalar) — double-precision to 64-bit -",
        ["1 0 01 01 001"] => "FCVTPU (scalar) — double-precision to 64-bit -",
        ["1 0 01 1x 11x"] => "UNALLOCATED -",
        ["1 0 01 10 000"] => "FCVTMS (scalar) — double-precision to 64-bit -",
        ["1 0 01 10 001"] => "FCVTMU (scalar) — double-precision to 64-bit -",
        ["1 0 01 11 000"] => "FCVTZS (scalar, integer) — double-precision to 64-bit -",
        ["1 0 01 11 001"] => "FCVTZU (scalar, integer) — double-precision to 64-bit -",
        ["1 0 10 x0 11x"] => "UNALLOCATED -",
        ["1 0 10 01 110"] => "FMOV (general) — top half of 128-bit to 64-bit -",
        ["1 0 10 01 111"] => "FMOV (general) — 64-bit to top half of 128-bit -",
        ["1 0 10 1x 11x"] => "UNALLOCATED -",
        ["1 0 11 00 000"] => "FCVTNS (scalar) — half-precision to 64-bit FEAT_FP16",
        ["1 0 11 00 001"] => "FCVTNU (scalar) — half-precision to 64-bit FEAT_FP16",
        ["1 0 11 00 010"] => "SCVTF (scalar, integer) — 64-bit to half-precision FEAT_FP16",
        ["1 0 11 00 011"] => "UCVTF (scalar, integer) — 64-bit to half-precision FEAT_FP16",
        ["1 0 11 00 100"] => "FCVTAS (scalar) — half-precision to 64-bit FEAT_FP16",
        ["1 0 11 00 101"] => "FCVTAU (scalar) — half-precision to 64-bit FEAT_FP16",
        ["1 0 11 00 110"] => "FMOV (general) — half-precision to 64-bit FEAT_FP16",
        ["1 0 11 00 111"] => "FMOV (general) — 64-bit to half-precision FEAT_FP16",
        ["1 0 11 01 000"] => "FCVTPS (scalar) — half-precision to 64-bit FEAT_FP16",
        ["1 0 11 01 001"] => "FCVTPU (scalar) — half-precision to 64-bit FEAT_FP16",
        ["1 0 11 10 000"] => "FCVTMS (scalar) — half-precision to 64-bit FEAT_FP16",
        ["1 0 11 10 001"] => "FCVTMU (scalar) — half-precision to 64-bit FEAT_FP16",
        ["1 0 11 11 000"] => "FCVTZS (scalar, integer) — half-precision to 64-bit FEAT_FP16",
        ["1 0 11 11 001"] => "FCVTZU (scalar, integer) — half-precision to 64-bit",
    }
    _handle_matches(matches, valStr, val)
end

def lookup(val)
    op0 = p(val, 4, 25)
    puts("https://developer.arm.com/documentation/ddi0596/2020-12/Index-by-Encoding?lang=en")
    puts("Top-level op0: #{op0}")
    case op0
    when "0000" 
        abort("Reserved")
    when "0001"
        abort("UNALLOCATED")
    when "0010"
        abort("SVE encodings")
    when "0011"
        abort("UNALLOCATED")
    when "1000", "1001" 
        lookup_data_processing__immediate(val)
    when "1010", "1011" 
        lookup_branches_exception_generating_and_system_instructions(val)
    when "0100", "0110", "1100", "1110" 
        lookup_loads_stores(val)
    when "0101", "1101" 
        lookup_data_processing__register(val)
    when "0111", "1111" 
        lookup_data_processing__scalar_floating_point_and_advanced_simd(val)
    else
        abort("Unknown")
    end
end

# E.g. "02 78 21 78" (output from 'objdump -d ')
def from_objd(str)
    ("0x" + str.split(" ").reverse.join("")).to_i(16)
end

puts lookup(from_objd("41 c0 22 1e"))
#puts lookup(0b11111001010000000000110100100010)
