/**
	Multi-line comments for documentation.
**/
typedef Point = {x:Int, y:Int}

class Path {
    var start:Point;
    var target:Point;
    var current:Point;
}

class Path2 extends Path {
    var childfield:Point;

    public function new() {
        this.target = { x: 0, y: 0};
        this.start = { x: 0, y: 0};
        this.current = { x: 0, y: 0};
        this.childfield = { x: 0, y: 0};
    }
}

typedef Point3 = { > Point, z : Int }

class Main {
	static public function main():Void {
		var user = {
			name: "Nicolas",
			age: 32,
			pos: [{x: 0, y: 0}, {x: 1, y: -1}],
		};
        var x: Point3 = { x: 0, y: 0, z: 0 }

        var x = new Path2();

		// Single line comment
		trace("Hello World " + user);
	}
}
