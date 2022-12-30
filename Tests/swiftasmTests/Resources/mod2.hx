import hl.Type;
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
	Rgb(r:UI8, g:I64, b:Int);
	Rgbf(r:F32, g:F64, b:F32);
}

enum Optional<T> {
	None;
	Some(r:T);
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

typedef GetterInterface = { 
	// Note: adding I64 to the param list here causes a segfault in HL/C and HL.intel
	public function get(a: F32, b: F64, c: Int, d: UI8, e: UI16, f: Dynamic): Array<Int>;
	public function get2(): Array<Int>;
}

interface VirtualInterface {
    public function getTest(): Int;
	public function getSecond(): Bool;
	public function getThird(): Int;
	public function getF32(): F32;
	public function getF64(): F64;
	public function getF64Modified(a: F32, b: F64): F64;
	public function getAll(): { test: Int, second: Bool, third: Int, f64: F64, f32: F32 };

	public function getVal(a: Dynamic, b: F32, c: F64, d: UI8, e: UI16, f: Int, g: I64): F64;
}

class VirtualTest implements VirtualInterface {
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

	public function getVal(a: Dynamic, b: F32, c: F64, d: UI8, e: UI16, f: Int, g: I64): F64 {
		var result: F64 = 0.0;
		result += cast(a.test);
		result += b;
		result += c;
		result += d;
		result += e;
		result += f;
		result += g;
		return result;
	}

	public function getTest(): Int {
		return test;
	}

	public function getSecond(): Bool {
		return second;
	}
	
	public function getThird(): Int { return third; }
	public function getF32(): F32 { return f32; }
	public function getF64(): F64 { return f64; }

	public function getF64Modified(a: F32, b: F64): F64 {
		return f64 + a + b;
	}
	
	public function getAll(): { test: Int, second: Bool, third: Int, f64: F64, f32: F32 } {
		return this;
	}
}

class VirtualTestChild extends VirtualTest {

}

class _MethodTester {
	public var closure:(Int)->Int;
	private var value: Int;

	public function new(test) {
		this.value = test;
		this.closure = (mul) -> { test * mul; };
	}

	public function mul(multiplier: Int): Int {
		return this.closure(multiplier);
	}

	public function getval(): Int {
		throw new haxe.exceptions.NotImplementedException();
	}
}

class _MethodTester_Child extends _MethodTester {
	public function new(test) {
		super(test);
		this.value = test;
	}

	public override function getval(): Int {
		return this.value;
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

	static private function testVirtualClosure(v: Int): Int {
		return testVirtualClosure_inner(new _MethodTester_Child(v));
	}

	static private function testVirtualClosure_inner(inst: _MethodTester): Int {
		var cl: Void->Int;
		cl = inst.getval;
        return cl();
	}

	static private function testFieldClosure(mul: Int): Int {
		var inst = new _MethodTester(2);
        return inst.closure(mul);
	}

	static private function testEnum_int(): Int {
		return testEnum2(Color.Rgb(0, 4, 2));
	}

	static private function testEnum_float(): Int {
		return testEnum2(Color.Rgbf(0.0, 4.0, 2.0));
	}

	static private function testEnum_float__setField(): Int {
		var val: Color = Color.Rgbf(0.0, 4.0, 2.0);
		switch(val) {
			case Rgbf(r, g, b): 
				val = Color.Rgbf(0.0, 2.0, 2.0);
			default: 
				return -1;
		}
		
		return testEnum2(val);
	}
	
	static private function testEnum2(en: Color): Int {
		switch(en) {
			case Color.Red: return 1;
			case Color.Green: return 2;
			case Color.Blue: return 3;
			case Rgb(r, g, b): return cast(r*100 + g*10 + b);
			case Rgbf(r, g, b): return cast(r*100 + g*10 + b);
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

		trace('testing testArrayBytes_Float: ${testArrayBytes_Float(1)}');

		trace('testing testVirtualCallMethod: ${testVirtualCallMethod()}');
		trace('testing testVirtualCallMethod2: ${testVirtualCallMethod2()}');

		trace('testing testInterface_1: ${testInterface_1(1, true, 2, 3.3, 4.4)}');
		trace('testing testInterface_2: ${testInterface_2(1, true, 2, 3.3, 4.4))}');
		trace('testing testInterface_3: ${testInterface_3(1, true, 2, 3.3, 4.4))}');

		trace('testing testEnum_int: ${testEnum_int())}');
		trace('testing testEnum_float: ${testEnum_float())}');
		trace('testing testEnum_float__setField: ${testEnum_float__setField())}');
		trace('testing testEnumAssocData: ${testEnumAssocData(123.456))}');

		trace('testing testVirtualClosure: ${testVirtualClosure(5))}');
	}

	public static function testVirtualCallMethod2(): Int {
		var getter: GetterInterface = { 
			get: (a: F32, b: F64, c: Int, d: UI8, e: UI16, f: Dynamic)->{ 
				return [cast(a), cast(b), c, d, e, cast(f)];
			},
			get2: ()->{ 
				return [cast(1.2)];
			} 
		};
		return testVirtualCallMethod2_inner(getter);
	}

	public static function testVirtualCallMethod2_inner(obj: GetterInterface): Int {
		var result = 0;
		var dynamicInt: Int = 6;
		for (i in obj.get(1, 2, 3, 4, 5, dynamicInt))
			result += i;
		return result;
	}

	public static function testVirtualCallMethod(): Int {
		return testVirtualCallMethod_inner([1,2,3]) + testVirtualCallMethod_inner([4,5,6]);
	}

	public static function testVirtualCallMethod_inner(it:Iterable<Int>): Int {
		var result = 0;
		for (i in it)
			result += i;
		return result;
	}

	public static function testInterface_1(test: Int, second: Bool, third: Int, f32: F32, f64: F64): F64 {
		var x = new VirtualTest(test, second, third, f64, f32);
		var v: VirtualInterface = x;
		
		return v.getF64() + v.getF32() + v.getThird() + v.getTest() + (v.getSecond() ? 1 : 0);
	}

	public static function testInterface_2(test: Int, second: Bool, third: Int, f32: F32, f64: F64): F64 {
		var x = new VirtualTest(test, second, third, f64, f32);
		var v: VirtualInterface = x;
		var v2 = v.getAll();
		return v2.test + (v2.second ? 1 : 0) + v2.third + v2.f64 + v2.f32;
	}

	public static function testInterface_3(test: Int, second: Bool, third: Int, f32: F32, f64: F64): F64 {
		var x = new VirtualTest(test, second, third, f64, f32);
		var v: VirtualInterface = x;
		return v.getVal(x, x.f32, x.f64, 1, 2, x.test, 123);
	}

	static public function testArrayBytes_Float(ix: Int): Float {
		var ar: Array<Float> = [123.456, 234.567, 345.678];
		return ar[ix];
	}

	// test OGetType when src register is not .dyn
	static inline public function testGetType_nonDynamicSrc(): Int {
		return hl.Type.Type.getDynamic("a") == hl.Type.Type.getDynamic(5) ? 1 : 2;
	}

	static public function testEnumAssocData(f64: F64): F64 {
		var x = new VirtualTest(0, false, 0, f64*2, 0.0);
		return testEnumAssocData_internal(Optional.Some(x));
	}

	static public function testEnumAssocData_internal<T: VirtualInterface>(value: Optional<T>): F64 {
		switch (value) {
			// matches any Leaf
			case Some(wrappedValue):
				return wrappedValue.getF64();
			default: return -1.0;
		  }
	}

	// two functions that should be the same for purposes of caching
	static public function testCache_funcA(a: F32, b: UI8): F64 {
		return testCache_targetFunc(a, b);
	}
	static public function testCache_funcB(a: F32, b: UI8): F64 {
		return testCache_targetFunc(a, b);
	}
	static public function testCache_targetFunc(a: F32, b: UI8): F64 {
		return a + b;
	}
}
