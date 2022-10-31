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
        abort("STR (immediate) — 64-bit")
    elsif match("11 0 01", valStr)  
        abort("LDR (immediate) — 64-bit")
    elsif match("11 0 10", valStr)  
        abort("PRFM (immediate)")
    elsif match("11 1 00", valStr)  
        abort("STR (immediate, SIMD&FP) — 64-bit")
    elsif match("11 1 01", valStr)  
        abort("LDR (immediate, SIMD&FP) — 64-bit")
    else
        abort("UNALLOCATED")
    end
    
end

# https://developer.arm.com/documentation/ddi0596/2020-12/Index-by-Encoding/Loads-and-Stores?lang=en#ldst_regoff
def lookup_loads_store_register__register_offset(val)
    size = p(val, 2, 30)
    v = p(val, 1, 26)
    opc = p(val, 2, 22)
    rm = p(val, 5, 16)
    opt = p(val, 3, 13)
    s = p(val, 1, 12)
    rn = p(val, 5, 5)
    rt = p(val, 5, 0)

    puts("Loads and stores:")
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
        abort("STRB (register) — shifted register")
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
        abort("LDR (register, SIMD&FP)")
    elsif match("00 1 10 xxx", valStr)
        abort("STR (register, SIMD&FP)")
    elsif match("00 1 11 xxx", valStr)
        abort("LDR (register, SIMD&FP)")
    elsif match("01 0 00 xxx", valStr)
        abort("STRH (register)")
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
        abort("LDR (register, SIMD&FP)")
    elsif match("1x 0 11 xxx", valStr)
        abort("UNALLOCATED")
    elsif match("1x 1 1x xxx", valStr)
        abort("UNALLOCATED")
    elsif match("10 0 00 xxx", valStr)
        abort("STR (register) — 32-bit")
    elsif match("10 0 01 xxx", valStr)
        abort("LDR (register) — 32-bit")
    elsif match("10 0 10 xxx", valStr)
        abort("LDRSW (register)")
    elsif match("10 1 00 xxx", valStr)
        abort("STR (register, SIMD&FP)")
    elsif match("10 1 01 xxx", valStr)
        abort("LDR (register, SIMD&FP)")
    elsif match("11 0 00 xxx", valStr)
        abort("STR (register) — 64-bit")
    elsif match("11 0 01 xxx", valStr)
        abort("LDR (register) — 64-bit")
    elsif match("11 0 10 xxx", valStr)
        abort("PRFM (register)")
    elsif match("11 1 00 xxx", valStr)
        abort("STR (register, SIMD&FP)")
    elsif match("11 1 01 xxx", valStr)
        abort("LDR (register, SIMD&FP)")
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
    elsif match("0	01	0", valStr)
        abort("ORR (immediate) — 32-bit")
    elsif match("0	10	0", valStr)
        abort("EOR (immediate) — 32-bit")
    elsif match("0	11	0", valStr)
        abort("ANDS (immediate) — 32-bit")
    elsif match("1	00 x", valStr) 
        puts("AND (immediate) — 64-bit")
        puts("    https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/AND--immediate---Bitwise-AND--immediate--?lang=en")
    elsif match("1	01 x", valStr)
        abort("ORR (immediate) — 64-bit")
    elsif match("1	10 x", valStr)
        abort("EOR (immediate) — 64-bit")
    elsif match("1	11 x", valStr)
        abort("ANDS (immediate) — 64-bit")
    else 
        abort("Unknown")
    end 
end

# https://developer.arm.com/documentation/ddi0596/2020-12/Index-by-Encoding/Data-Processing----Immediate
def lookup_data_processing__immediate(val)
    op0 = p(val, 3, 23)

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
        abort("Bitfield")
    elsif match("111", valStr)
        abort("Extract")
    else 
        abort("Unknown")
    end 
end

# https://developer.arm.com/documentation/ddi0596/2020-12/Index-by-Encoding?lang=en
def lookup(val)
    op0 = p(val, 4, 25)
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
        abort("Branches, Exception Generating and System instructions")
    when "0100", "0110", "1100", "1110" 
        lookup_loads_stores(val)
    when "0101", "1101" 
        abort("Data Processing -- Register")
    when "0111", "1111" 
        abort("Data Processing -- Scalar Floating-Point and Advanced SIMD")
    else
        abort("Unknown")
    end
end

puts lookup(0x8a020061)

# 0: 61 10 40 92  	and	x1, x3, #0x1f
# 4: 61 00 02 8a  	and	x1, x3, x2
# 8: 61 10 40 b2  	orr	x1, x3, #0x1f
# c: 61 00 02 aa  	orr	x1, x3, x2
