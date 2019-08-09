data B = T | F;

data List = Nil | Cons (Number, List);

let x = T;

if let y = x { 1! } else { 0! }

if let T = x { 11! } else { 10! }

let xs = Cons (0, Cons(1, Cons(2, Nil)));

xs!

if let Nil = xs { 1! } else { 0! }

if let Cons(a, as) = xs { 10! a! as! } else { 9! }

fn sum_list = xs: List : Number {
    if let Cons(head, tail) = xs {
        head + sum_list tail
    } else {
        0
    }
}

sum_list xs !