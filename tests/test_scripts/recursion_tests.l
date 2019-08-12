fn fac = n: Int : Int { if n < 2 { 1 } else { n + fac (n - 1) } }

fac 1000 !

// fn recurse(n: Int) {
//    n !
//    recurse (n+1)
// }

// recurse 0;