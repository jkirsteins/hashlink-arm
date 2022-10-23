/**
	Multi-line comments for documentation.
**/

import hl.UI8;
import hl.UI16;

typedef Point = {x:Int, y:Int}

class Path {
    public var test: Int = 0xDEAD; // marker
    public var start:Point;
    public var target:Point;
    public var current:Point;
}

class Path2 extends Path {
    public var test2: Int = 0xBEEF; // marker
    public var childfield:Point;

    public function new() {
        this.target = { x: 0, y: 0};
        this.start = { x: 0, y: 0};
        this.current = { x: 0, y: 0};
        this.childfield = { x: 0, y: 0};
    }
}

// Class for testing offsets for field indexes
class OffsetTests {
    public var test: Int = 0xDEAD; // marker
    public var test_2nd: UI8 = 1;
    public var test_3rd: UI8 = 1;
    public var test_4th: UI16 = 1; // will be memaligned
    public var test2: Float = 0.5;
    public var test3: Bool = true;
    public var test4: String = "Asd";
}

typedef Point3 = { > Point, z : Int }

class Main {
    /*
    29 : fn pathTest@29 () -> (void)@80 (5 regs, 32 ops)
    reg0  Path2@84
    reg1  void@0
    reg2  virtual<x: i32, y: i32>@83
    reg3  virtual<x: i32, y: i32>@83
    reg4  i32@3
     Main.hx:68    0: New         reg0 = new Path2@84
     Main.hx:68    1: Call1       reg1 = __constructor__@28(reg0)
     Main.hx:69    2: Field       reg2 = reg0.target
     Main.hx:69    3: NullCheck   if reg2 == null throw exc
     Main.hx:69    4: Field       reg3 = reg0.target
     Main.hx:69    5: NullCheck   if reg3 == null throw exc
     Main.hx:69    6: Int         reg4 = 101
     Main.hx:69    7: SetField    reg3.y = reg4
     Main.hx:69    8: SetField    reg2.x = reg4
     Main.hx:70    9: Field       reg2 = reg0.start
     Main.hx:70   10: NullCheck   if reg2 == null throw exc
     Main.hx:70   11: Field       reg3 = reg0.start
     Main.hx:70   12: NullCheck   if reg3 == null throw exc
     Main.hx:70   13: Int         reg4 = 202
     Main.hx:70   14: SetField    reg3.y = reg4
     Main.hx:70   15: SetField    reg2.x = reg4
     Main.hx:71   16: Field       reg2 = reg0.current
     Main.hx:71   17: NullCheck   if reg2 == null throw exc
     Main.hx:71   18: Field       reg3 = reg0.current
     Main.hx:71   19: NullCheck   if reg3 == null throw exc
     Main.hx:71   20: Int         reg4 = 303
     Main.hx:71   21: SetField    reg3.y = reg4
     Main.hx:71   22: SetField    reg2.x = reg4
     Main.hx:72   23: Field       reg2 = reg0.childfield
     Main.hx:72   24: NullCheck   if reg2 == null throw exc
     Main.hx:72   25: Field       reg3 = reg0.current
     Main.hx:72   26: NullCheck   if reg3 == null throw exc
     Main.hx:72   27: Int         reg4 = 404
     Main.hx:72   28: SetField    reg3.y = reg4
     Main.hx:72   29: SetField    reg2.x = reg4
     Main.hx:73   30: Call1       reg1 = pathTest2@30(reg0)
     Main.hx:74   31: Ret         reg1
     */
    static public function pathTest():Void {
        var p = new Path2();
        p.target.x = p.target.y = 101;
        p.start.x = p.start.y = 202;
        p.current.x = p.current.y = 303;
        p.childfield.x = p.current.y = 404;
        pathTest2(p);
    }

    /*
    30 : fn pathTest2@30 (Path2) -> (void)@81 (9 regs, 18 ops)
    reg0  Path2@84
    reg1  void@0
    reg2  (dynamic, virtual<className: String, customParams: hl.types.ArrayDyn, fileName: String, lineNumber: i32, methodName: String>) -> (void)@42
    reg3  haxe.$Log@38
    reg4  String@13
    reg5  String@13
    reg6  virtual<className: String, fileName: String, lineNumber: i32, methodName: String>@153
    reg7  i32@3
    reg8  virtual<className: String, customParams: hl.types.ArrayDyn, fileName: String, lineNumber: i32, methodName: String>@40
     Main.hx:107   0: GetGlobal   reg3 = global@9
     Main.hx:107   1: Field       reg2 = reg3.trace
     Main.hx:107   2: NullCheck   if reg2 == null throw exc
     Main.hx:107   3: GetGlobal   reg4 = global@10
     Main.hx:107   4: Call1       reg5 = string@224(reg0)
     Main.hx:107   5: Call2       reg4 = __add__@20(reg4, reg5)
     Main.hx:107   6: New         reg6 = new virtual<className: String, fileName: String, lineNumber: i32, methodName: String>@153
     Main.hx:107   7: GetGlobal   reg5 = global@11
     Main.hx:107   8: SetField    reg6.fileName = reg5
     Main.hx:107   9: Int         reg7 = 107
     Main.hx:107  10: SetField    reg6.lineNumber = reg7
     Main.hx:107  11: GetGlobal   reg5 = global@12
     Main.hx:107  12: SetField    reg6.className = reg5
     Main.hx:107  13: GetGlobal   reg5 = global@13
     Main.hx:107  14: SetField    reg6.methodName = reg5
     Main.hx:107  15: ToVirtual   reg8 = cast reg6
     Main.hx:107  16: CallClosure reg1 = reg2(reg4, reg8)
     Main.hx:107  17: Ret         reg1
     */
     static public function pathTest2(p: Path2):Void {
        trace('received path $p');
    }

    static public function pathTest3(p: Path2):Int {
        var result = p.test2;
        return result;
    }

	static public function main():Void {
		trace("Hello World");
        Main.pathTest();
	}
}
