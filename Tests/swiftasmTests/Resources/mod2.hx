import hl.I64;
import hl.NativeArray;
import haxe.Exception;
import hl.UI8;
import hl.UI16;

class Path {
	public var test:Int;

	public function new(test) {
		this.test = test;
	}
}

class Main {
	static public function testGetUI8(ix:Int):Int {
		var haxeB = haxe.io.Bytes.ofHex("11121314");
		var a = hl.Bytes.fromBytes(haxeB);
		return a.getUI8(ix);
	}

	static public function testGetUI16(ix:Int):Int {
		var haxeB = haxe.io.Bytes.ofHex("11121314");
		var a = hl.Bytes.fromBytes(haxeB);
		return a.getUI16(ix);
	}

	static public function testTrap(): Int {
		try {
			throw new Exception("test message");
		} catch (e:Exception) {
			return 1;
		}
        return 0;
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

	static public function main():Void {
		// var path = new Path(3);
		// trace('${path.test}');
		// path = null;
		// trace('${path.test}');

		var a = testGetUI8(0);
		var b = testGetUI8(1);
		var c = testGetUI8(2);
		var d = testGetUI8(3);
		trace('getUI8 $a $b $c $d');

		// var e = testGetUI16(0);
		// var f = testGetUI16(1);
		// var g = testGetUI16(2);
		// var h = testGetUI16(3);
		// trace('getUI16 $e $f $g $h');

		trace('got 32: ${testGetArrayInt32(10, 1234, 5)}');
		trace('got 64b: ${testGetArrayInt64__haxe(10, 5678, 3)}');
		trace('got 64n: ${testGetArrayInt64__hl(10, 5678, 3)}');

        // testTrap();
        // testTrap2();
	}
}
