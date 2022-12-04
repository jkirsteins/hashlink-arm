# swiftasm

Loading and running [hashlink](https://github.com/HaxeFoundation/hashlink) 

Bytecode references:
- <https://github.com/Gui-Yom/hlbc/wiki/Bytecode-file-format>
- <https://github.com/HaxeFoundation/hashlink/blob/0d2561f7805293f0745cd02c5184d43721088bfc/src/code.c>
- <https://github.com/HaxeFoundation/haxe/blob/c35bbd4472c3410943ae5199503c23a2b7d3c5d6/src/generators/hlcode.ml>

## Deviations from libhl

- bin/hl should export:
  - load_code
  - hl_code_free
  - HL_API hl_module *hl_module_alloc( hl_code *code );
- hl_module_init needs to not call JIT 
- hot reloading is ignored/commented out
- hl_get_ustring

## Getting started

To have colorized output first:

    brew install xcbeautify

Then run tests via:

    ./test.sh [--filter <filter>]

## Checking stuff

def p(num, bits, offset)
    mask = bits.times.collect { |x| x }.reduce(0) { |x,y| x | (0b1 << y) }
    res = (num >> offset) & mask
    res = res.to_s(2)
    res.rjust(bits, "0")
end

## TODO

- [ ] Tests for functions mixing floating poing/general purpose stack arguments (e.g. we should pass x0/d0 not x0/x1 or x0/d1)
- [ ] Zero non-argument registers when initializing stack

## hl type C structs

    // See: /usr/local/include/hl.he
    typedef struct {
        hl_type *t;
        uchar *bytes;
        int length;
    } vstring;
    
    
## Numeric operation results

See also: https://haxe.org/blog/hashlink-in-depth-p2/

    E.g. OUDiv is only for integers. OSDiv is for either.

    Operator    Operation       Operand 1    Operand 2    Result type
    %           modulo          Float/Int    Float/Int    Float/Int
    *           multiplication  Float/Int    Float/Int    Float/Int
    /           division        Float/Int    Float/Int    Float
    +           addition        Float/Int    Float/Int    Float/Int
    -           subtraction     Float/Int    Float/Int    Float/Int
    
