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

	static public function testTrap() {
		try {
			throw new Exception("test message");
		} catch (e:Exception) {
			trace('stack: ${e.stack}');
		}
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

	static public function main():Void {
		var path = new Path(3);
		trace('${path.test}');
		path = null;
		trace('${path.test}');

		var a = testGetUI8(0);
		var b = testGetUI8(1);
		var c = testGetUI8(2);
		var d = testGetUI8(3);
		trace('getUI8 $a $b $c $d');

		var e = testGetUI16(0);
		var f = testGetUI16(1);
		var g = testGetUI16(2);
		var h = testGetUI16(3);
		trace('getUI16 $e $f $g $h');

        testTrap();
        testTrap2();
	}
}
