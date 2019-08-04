// fn test'(f:(Bool, Bool) -> Bool, x:Bool) -> Bool -> Bool {}

// fn test(f:(Bool, Bool) -> Bool, x:Bool) -> Bool {}

// fn f(a: Bool, b: Bool) -> Bool {}

fn curried = a: Int => b: Int : Int { a + b }

fn higherOrder(f: Int -> Int -> Int) -> Unit { f 3 6; }

// test(f, true)

curried 4 6 !

let partial: Int -> Int = curried 100;

partial 2 !

fn htest(f: Int -> Int) { f 5; }

htest partial;

higherOrder curried;

// fn mc() -> Unit -> Unit { fn count() {  } return count; }

fn makeCounter() -> Unit -> Int { var i: Int = 0; fn count() -> Int { i = i + 1 ! i } return count; }

var counter: Unit -> Int = makeCounter();
counter()! // "1".
counter()! // "2".
counter()! // "1".

// fn fac = n: Int : Int { if n < 2 { 1 } else { n * fac (n - 1) }}
// fac 1 !
// fac 2 !
// fac 20 !

fn fib(n: Int) -> Int { if n < 2 { 1 } else { fib (n - 1) + fib (n - 2) }}
fib 3 !
fib 4 !
fib 8 !

fn f() -> Int { return 5; }

// f() !

var counter = 0;
while counter < 5 { counter! counter = counter + 1; }
