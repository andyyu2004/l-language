{
    data B = T | F;

    let x = T;

    if let y = x { 1! } else { 0! }

    if let T = x { 11! } else { 10! }

}

{

    data List = Nil | Cons (Number, List);

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
}

{
    data Tree = TNil | Node (Int, Tree, Tree);

    fn sum_tree = tree: Tree : Number {
        if let Node(x, l, r) = tree {
            x + sum_tree l + sum_tree r
        } else { 0 }
    }

    let t = Node (0, Node(1, TNil, TNil), Node(2, TNil, Node(36, TNil, TNil)));
    sum_tree t !
}

{
    data List = Nil | Cons Int List;
    let xs = Cons 3 Nil;

}

{
    data List = Nil | Cons Number List;
    let xs = Cons 5 (Cons 6 (Cons 7 Nil));

    fn sum_l = xs: List : Number {
        if let Cons x xs = xs {
            x + sum_l xs
        } else {
            0
        }
    }

    sum_l xs !

    // Currying data constructor
    data List = Nil | Cons Number List;
    let l1 = Cons 99; // Partially applied constructor
    let xs = l1 (l1 (l1 (l1 (l1 (l1 Nil)))));
    "Curried"!
    sum_l xs !

    fn map = f: Int -> Int => xs: List : List {
        if let Cons x xs = xs {
            Cons (f x) (map f xs)
        } else {
            Nil
        }
    }

    fn x2 = x: Int : Int { x * 2 }
    "map x2" !
    map x2 xs !

}

{
    data Tree = Nil | Node Int Tree Tree;

    let tree = Node 5 (Node 10 Nil Nil) (Node 15 Nil Nil);

    "tree"!
    tree !

    fn sum_tree = t: Tree : Number {
        if let Node x l r = t {
            x + sum_tree l + sum_tree r
        } else { 0 }
    }
    "sum tree"!
    sum_tree tree !

}

{
    data Foo = Foo Int;
    let foo = Foo 4;
    if let Foo x = foo { x! }

}


{
    data List = Nil | Cons Number List;

    fn map = f: Int -> Int => xs: List : List {if let Cons x xs = xs {Cons (f x) (map f xs)} else {Nil}}

    fn x2 = n: Int : Int { 2 * n }

    let xs = Cons 4 (Cons 7 Nil);

    map x2 xs !
}

{
    data Either = Left Bool | Right Int;

    fn test = e: Either {
        if let Left b = e {
            b !
        } else if let Right i = e {
            i !
        }
    }

    let x = Left false;
    test x;
    let y = Right 5;
    test y;

}

{
    data Either = Left Bool | Right Int;

    let x = Left false;
    let x = Right 22;

    if let Left b = x {
        b !
    } else if let Right i = x {
        i !
    }
}

{
    data List = Nil | Cons Int List;

    fn filter = p: Int -> Bool => xs: List : List {
        if let Cons x xs = xs {
            if p x {
                Cons x (filter p xs)
            } else {
                filter p xs
            }
        } else {
            Nil
        }
    }

    let xs = Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)));

    fn is_even(n: Int) -> Bool { n % 2 == 0 }

    fn is_odd(n: Int) -> Bool { !is_even n}

    "even"!
    filter is_even xs !
    "odd"!
    filter is_odd xs !
}

{
    data Maybe = Just Int | Nothing;

    let x = Just 99;

    if let Just i = x { i ! }
}

























