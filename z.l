// fn test'(f:(Bool, Bool) -> Bool, x:Bool) -> Bool -> Bool {}

// fn test(f:(Bool, Bool) -> Bool, x:Bool) -> Bool {}

// fn f(a: Bool, b: Bool) -> Bool {}

// fn curried = a: Int => b: Int : Int { a + b ! }

// fn higherOrder(f: Int -> Int -> Int) -> Unit { f 3 6; }

// test(f, true)

// curried 4 6;

// let partial: Int -> Int = curried 100;

// fn htest(f: Int -> Int) { f 5; }

// htest partial;

// higherOrder curried;

fn makeCounter() {
  var i: Int = 0;
  fn count() {
    i = i + 1;
    i!
  }

  return count;
}

var counter: Unit = makeCounter();
counter(); // "1".
counter(); // "2".
counter(); // "1".