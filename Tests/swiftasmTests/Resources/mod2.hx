import haxe.io.BytesData;
import hl.I64;
import hl.NativeArray;
import haxe.Exception;
import hl.UI8;
import hl.UI16;

enum Color {
	Red;
	Green;
	Blue;
	Rgb(r:Int, g:Int, b:Int);
}

class Path {
	public var test:Int;

	public function new(test) {
		this.test = test;
	}
}

class _MethodTester {
	public var closure:(Int)->Int;

	public function new(test) {
		this.closure = (mul) -> { test * mul; };
	}

	public function mul(multiplier: Int): Int {
		return this.closure(multiplier);
	}
}

class Main {
	static public function testGetUI8(ix:Int):Int {
		var haxeB = haxe.io.Bytes.ofHex("11121314");
		var a = hl.Bytes.fromBytes(haxeB);
		return a.getUI8(ix);
	}

	static public function testGetUI8_2():Int {
		var haxeB = haxe.io.Bytes.ofHex("11121314");
		return haxeB.getInt32(0);
	}

	static public function testGetUI16(ix:Int):Int {
		var haxeB = haxe.io.Bytes.ofHex("11121314");
		var a = hl.Bytes.fromBytes(haxeB);
		return a.getUI16(ix);
	}

	static public function getInt(ix: Int): Int {
        var haxeB = haxe.io.Bytes.ofHex("11121314");
        var a = hl.Bytes.fromBytes(haxeB);

		var x: hl.Bytes;
		@:privateAccess x = haxeB.b;
		trace('At 0: ${x.getUI8(0)}');

        return a.getUI8(ix);
    }

	static public function testTrap(): Int {
		try {
			throw new Exception("test message");
		} catch (e:Exception) {
			return 1;
		}
        return 0;
	}

	static public function testTrapConditional(shouldThrow: Bool): Int {
		try {
			if (shouldThrow) {
				throw new Exception("test message");
			}
		} catch (e:Exception) {
			return 1;
		}
        return 0;
	}

	static public function testTrapContextEnding(triggerInner: Bool): Int {
		try {
			try {
				if (triggerInner) {
					throw new Exception("throw from inner");
				}
			} catch (e:Exception) {
				return 2;
			}

			if (!triggerInner) {
				throw new Exception("throw from outter");
			}
		} catch (e:Exception) {
			return 1;
		}
        return 0;
	}

	static public function testTrapDifferentTypes(throwInt: Bool, throwString: Bool): Int {
		try {
			if (throwInt) {
				throw 5;
			}
			if (throwString) {
				throw "throwing this";
			}
		} catch (e:Int) {
			return e;
		} catch (e:String) {
			return 3;
		}
		return 0;
	}

	static public function testTrapX(a: Int, msg: String): String {
		try {
			if (a == 1) {
				throw new Exception(msg);
			}
		} catch (e:Exception) {
			return e.message;
		}
        return "nop";
	}

    static public function testTrap2() {
		try {
            throw 5;
		} catch (e:Int) {
			return e;
		}
	}

    static public function testFieldAccess(): Int {
		var obj = new Path(2);
        return obj.test;
	}

	static public function testArrayLength(x: Int): Int {
		var res = new hl.NativeArray(x);
        return res.length;
	}

	static public function testGetArrayInt32(len: Int, start: Int, ix: Int): Int {
		var res: NativeArray<Int> = new hl.NativeArray(len);
		var curVal = 0;
		for (i in (0...len)) {
			res[i] = start + curVal++; 
		}
		return res[ix];
	}

	static public function testGetArrayInt64__haxe(len: Int, start: haxe.Int64, ix: Int): haxe.Int64 {
		var res: NativeArray<haxe.Int64> = new hl.NativeArray(len);
		var curVal = 0;
		for (i in (0...len)) {
			res[i] = start + curVal++; 
		}
		return res[ix];
	}

	static public function testGetArrayInt64__hl(len: Int, start: hl.I64, ix: Int): hl.I64 {
		var res: NativeArray<hl.I64> = new hl.NativeArray(len);
		var curVal = 0;
		for (i in (0...len)) {
			res[i] = start + curVal++; 
		}
		return res[ix];
	}

	static public function testGetSetField(newf: Int): Int {
		var res = new Path(5);
		res.test = newf * 2;
		return res.test;
	}

	// To test fetching globals
	static private function testGlobal():String {
		return "Hello Globals";
	}
	
	static private function testTrace() {
		trace("Hello Trace");
	}

	static private function testStaticClosure(aIn: Int, bIn: Int) {
		var f = (a, b) -> a + b;
        return f(aIn, bIn);
	}

	static private function testInstanceMethod(mul: Int): Int {
		var inst = new _MethodTester(2);
        return inst.mul(mul);
	}

	static private function testFieldClosure(mul: Int): Int {
		var inst = new _MethodTester(2);
        return inst.closure(mul);
	}

	static private function testEnum(): Int {
		return testEnum2(Color.Rgb(0, 4, 2));
	}

	static private function testEnum2(en: Color): Int {
		switch(en) {
			case Color.Red: return 1;
			case Color.Green: return 2;
			case Color.Blue: return 3;
			case Rgb(r, g, b): return r*100 + g*10 + b;
		}
		return -1;
	}

	static private function testStaticVirtual_setField(val: Int, set: Bool): Int {
		var o : { test : Int } = new Path(0);
		if (set) {
			o.test = val;
		}
		return o.test;
	}

	static public function main():Void {
		// var path = new Path(3);
		// trace('${path.test}');
		// path = null;
		// trace('enum ${testEnum()}');

		// trace('static closure: ${testStaticClosure(11, 22)}');
		// trace('instance method: ${testInstanceMethod(3)}');
		// trace('field closure: ${testFieldClosure(14)}');

		// var a = testGetUI8(0);
		// var b = testGetUI8(1);
		// var c = testGetUI8(2);
		var d = testGetUI8(3);
		trace('getUI8 $d');

		// trace('getUI8_2 ${testGetUI8_2()}');

		// // var e = testGetUI16(0);
		// // var f = testGetUI16(1);
		// // var g = testGetUI16(2);
		// // var h = testGetUI16(3);
		// // trace('getUI16 $e $f $g $h');

		// trace('got 32: ${testGetArrayInt32(10, 1234, 5)}');
		// trace('got 64b: ${testGetArrayInt64__haxe(10, 5678, 3)}');
		// trace('got 64n: ${testGetArrayInt64__hl(10, 5678, 3)}');

		// trace('At ${getInt(0)}');
		// trace('At ${getInt(1)}');
		// trace('At ${getInt(2)}');
		// trace('At ${getInt(3)}');
        // testTrap();
        // testTrap2();
	}
}
