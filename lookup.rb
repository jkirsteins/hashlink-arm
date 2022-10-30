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

# https://developer.arm.com/documentation/ddi0596/2020-12/Index-by-Encoding/Loads-and-Stores?lang=en#ldst_pos
def lookup_loads_store_register__unsigned_immediate(val)
    size = p(val, 2, 30)
    v = p(val, 1, 26)
    opc = p(val, 2, 22)
    imm12 = p(val, 12, 10)
    rn = p(val, 5, 5)
    rt = p(val, 5, 0)

    puts("Loads and stores:")
    puts("    size = #{size}")
    puts("    v = #{v}")
    puts("    opc = #{opc}")
    puts("    imm12 = #{imm12}")
    puts("    rn = #{rn}")
    puts("    rt = #{rt}")
    
    valStr = "#{size}#{v}#{opc}"
    if match("x1 1 1x", valStr)
        abort("UNALLOCATED")
    elsif match("00	0	00", valStr)	
        abort("STRB (immediate)")
    elsif match("00	0	01", valStr)	
        abort("LDRB (immediate)")
    elsif match("00	0	10", valStr)	
        abort("LDRSB (immediate) — 64-bit")
    elsif match("00	0	11", valStr)	
        abort("LDRSB (immediate) — 32-bit")
    elsif match("00	1	00", valStr)	
        abort("STR (immediate, SIMD&FP) — 8-bit")
    elsif match("00	1	01", valStr)		
        abort("LDR (immediate, SIMD&FP) — 8-bit")
    elsif match("00	1	10", valStr)		
        abort("STR (immediate, SIMD&FP) — 128-bit")
    elsif match("00	1	11", valStr)		
        abort("LDR (immediate, SIMD&FP) — 128-bit")
    elsif match("01	0	00", valStr)		
        abort("STRH (immediate)")
    elsif match("01	0	01", valStr)		
        abort("LDRH (immediate)")
    elsif match("01	0	10", valStr)		
        abort("LDRSH (immediate) — 64-bit")
    elsif match("01	0	11", valStr)		
        abort("LDRSH (immediate) — 32-bit")
    elsif match("01	1	00", valStr)		
        abort("STR (immediate, SIMD&FP) — 16-bit")
    elsif match("01	1	01", valStr)		
        abort("LDR (immediate, SIMD&FP) — 16-bit")
    elsif match("1x	0	11", valStr)		
        abort("UNALLOCATED")
    elsif match("1x	1	1x", valStr)		
        abort("UNALLOCATED")
    elsif match("10	0	00", valStr)		
        abort("STR (immediate) — 32-bit")
    elsif match("10	0	01", valStr)		
        abort("LDR (immediate) — 32-bit")
    elsif match("10	0	10", valStr)		
        abort("LDRSW (immediate)")
    elsif match("10	1	00", valStr)		
        abort("STR (immediate, SIMD&FP) — 32-bit")
    elsif match("10	1	01", valStr)		
        abort("LDR (immediate, SIMD&FP) — 32-bit")
    elsif match("11	0	00", valStr)		
        abort("STR (immediate) — 64-bit")
    elsif match("11	0	01", valStr)		
        abort("LDR (immediate) — 64-bit")
    elsif match("11	0	10", valStr)		
        abort("PRFM (immediate)")
    elsif match("11	1	00", valStr)		
        abort("STR (immediate, SIMD&FP) — 64-bit")
    elsif match("11	1	01", valStr)		
        abort("LDR (immediate, SIMD&FP) — 64-bit")
    else
        abort("UNALLOCATED")
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
        abort("Load/store register (register offset)")
    elsif match("xx11 x 0x 1xxxxx x1", valStr)
        abort("Load/store register (pac)")
    elsif match("xx11 x 1x xxxxxx xx", valStr)
        lookup_loads_store_register__unsigned_immediate(val)
    else 
        abort("Unknown #{val}")
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
        abort("Data Processing -- Immediate")
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

lookup(0b0011_1001_0100_0000_0000_0011_1110_0001)
