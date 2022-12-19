import hl.Ref;
import hl.F32;
import hl.F64;
import haxe.io.BytesData;
import hl.I64;
import hl.NativeArray;
import haxe.Exception;
import hl.UI8;
import hl.UI16;

// might be unset if testGlobals called before globals are initialized
var globalString: String = "Global";
	
var globalVirtualTest : { test : Int, second: Bool, third: Int, f64: F64, f32: F32 } = new VirtualTest(123, true, 456, 789.0, 101.0);

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

// NOTE: do not change this class. If the proto layout changes,
// the corresponding tests might become buggy (they have hardcoded
// proto indexes)
class CallTest {
	public function new() {

	}

	public function test(val: Int): Int {
		return this.test2(val);
	}

	public function test2(val: Int): Int {
		return val * 2;
	}
}

class VirtualTest {
	public var test:Int;
	public var second:Bool;
	public var third:Int;
	public var f64:F64;
	public var f32:F32;

	public function new(test: Int = 0, second: Bool = false, third: Int = 0, f64: F64 = 0, f32: F32 = 0) {
		this.test = test;
		this.second = second;
		this.third = third;
		this.f64 = f64;
		this.f32 = f32;
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

	static public function testGlobals(str: String, setIt: Bool): String {
		if (setIt) {
			globalString = str;
		}
		return globalString;
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

	static private function testStaticVirtual_globalVirtual(field: Int): Int {
		switch(field) {
			case 0: return globalVirtualTest.test;
			case 1: return globalVirtualTest.second ? 1 : 0;
			case 2: return globalVirtualTest.third;
			default: return -1;
		}
	}

	static private function testStaticVirtual_globalVirtual_f32(): F32 {
		return globalVirtualTest.f32;
	}

	static private function testStaticVirtual_globalVirtual_f64(): F64 {
		return globalVirtualTest.f64;
	}

	static private function testStaticVirtual_setField(val: Int, set: Bool, field: Int): Int {
		var o : { test : Int, second: Bool, third: Int } = new VirtualTest();
		o.test = 1;
		o.second = true;
		o.third = 2;
		if (set) {
			if (field == 0) {
				o.test = val;
			}
			if (field == 1) {
				o.second = val != 0 ? true : false;
			}
			if (field == 2) {
				o.third = val;
			}
		}
		switch(field) {
			case 0: return o.test;
			case 1: return o.second ? 1 : 0;
			case 2: return o.third;
			default: return -1;
		}
	}

	static public function _testDynGet_getObj(): Dynamic {
		return globalVirtualTest;
	}

	static public function testDynGetSet(set: Bool, val: Int): Int {
		var obj: Dynamic = _testDynGet_getObj();
		if (set) {
			obj.third = val;
		}
		return obj.third;
	}

	static public function testDynGetSet_f64(set: Bool, val: F64): F64 {
		var obj: Dynamic = _testDynGet_getObj();
		if (set) {
			obj.f64 = val;
		}
		return obj.f64;
	}

	static public function testCallClosure_Dynamic_returnFloat64(inval: Int): F64 {
		var myLocalFunction = function(a: Int): F64 { return a * 2.0; };
		var dynFun: Dynamic = myLocalFunction;
		return dynFun(inval);
	}

	static public function testCallClosure_Dynamic_returnFloat32(inval: Int): F32 {
		var myLocalFunction = function(a: Int): F32 { return a * 2.0; };
		var dynFun: Dynamic = myLocalFunction;
		return dynFun(inval);
	}

	static public function testCallClosure_Dynamic_returnInt32(inval: Int): Int {
		var myLocalFunction = function(a: Int): Int { return a * 2; };
		var dynFun: Dynamic = myLocalFunction;
		return dynFun(inval);
	}

	static public function testCallClosure_Dynamic_returnUInt16(inval: Int): UI16 {
		var myLocalFunction = function(a: Int): UI16 { return a * 2; };
		var dynFun: Dynamic = myLocalFunction;
		return dynFun(inval);
	}

	static public function testCallClosure_Dynamic_returnUInt8(inval: Int): UI8 {
		var myLocalFunction = function(a: Int): UI8 { return a * 2; };
		var dynFun: Dynamic = myLocalFunction;
		return dynFun(inval);
	}

	#if !HL_C
	static public function testCallClosure_Dynamic_returnInt64(inval: Int): I64 {
		var myLocalFunction = function(a: Int): I64 { return a * 2; };
		var dynFun: Dynamic = myLocalFunction;
		return dynFun(inval);
	}
	#end

	static public function testRef_fp_internal(r32: Ref<F32>, r64: Ref<F64>) {
		r32.set(101.0);
		r64.set(202.0);
	}

	static public function testRef_fp(): F64 {
		var f32: F32 = 0.0;
		var f64: F64 = 0.0;
		var ref32 = Ref.make(f32);
		var ref64 = Ref.make(f64);
		testRef_fp_internal(ref32, ref64);
		return ref32.get() + ref64.get();
	}

	static public function testRef_i_internal(r32: Ref<Int>, r64: Ref<I64>) {
		r32.set(101);
		r64.set(202);
	}

	static public function testRef_i(): hl.I64 {
		var i32: Int = 0;
		var i64: I64 = 0;
		var ref32 = Ref.make(i32);
		var ref64 = Ref.make(i64);
		testRef_i_internal(ref32, ref64);
		return ref32.get() + ref64.get();
	}

	#if !HL_C
	static public function testVoid(a: Void, b: Void): Void {
		return a;
	}
	#end

	static public function testCallThis(): Int {
		var res: CallTest = new CallTest();
		return res.test(5);
	}

	static public function main():Void {
		trace('testing f64: ${testCallClosure_Dynamic_returnFloat64(123)}');
		trace('testing f32: ${testCallClosure_Dynamic_returnFloat32(123)}');
		trace('testing i32: ${testCallClosure_Dynamic_returnInt32(123)}');
		trace('testing u16: ${testCallClosure_Dynamic_returnUInt16(123)}');
		trace('testing u8: ${testCallClosure_Dynamic_returnUInt8(123)}');
		
		#if !HL_C
		trace('testing i64: ${testCallClosure_Dynamic_returnInt64(123)}');
		#end

		trace('testing virtual init (f32): ${testStaticVirtual_globalVirtual_f32()}');
		trace('testing virtual init (f64): ${testStaticVirtual_globalVirtual_f64()}');

		trace('testing float references (FP): ${testRef_fp()}');
		trace('testing float references (I): ${testRef_i()}');

		trace('testing testTrapDifferentTypes (true, false): ${testTrapDifferentTypes(true, false)}');
		trace('testing testTrapDifferentTypes (false, true): ${testTrapDifferentTypes(false, true)}');
		trace('testing testTrapDifferentTypes (false, false): ${testTrapDifferentTypes(false, false)}');

		trace('testing testCallThis: ${testCallThis()}');
	}
}
