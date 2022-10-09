# swiftasm

Loading and running [hashlink](https://github.com/HaxeFoundation/hashlink) 

Bytecode references:
- <https://github.com/Gui-Yom/hlbc/wiki/Bytecode-file-format>
- <https://github.com/HaxeFoundation/hashlink/blob/0d2561f7805293f0745cd02c5184d43721088bfc/src/code.c>
- <https://github.com/HaxeFoundation/haxe/blob/c35bbd4472c3410943ae5199503c23a2b7d3c5d6/src/generators/hlcode.ml>

## hl type C structs

    // See: /usr/local/include/hl.h
    typedef struct {
        hl_type *t;
        uchar *bytes;
        int length;
    } vstring;
    