struct Test<T> {
    typealias Index = Int 
}

var x = Test<Int>.Index(1)
var y = Test<String>.Index(2)

x = y

print(x, y)