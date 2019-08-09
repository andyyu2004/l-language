data B = T | F;

let x = T;

if let y = x { 1! } else { 0! }

if let T = x { 11! } else { 10! }

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


data Tree = TNil | Node (Int, Tree, Tree);

fn sum_tree = tree: Tree : Number {
    if let Node(x, l, r) = tree {
        x + sum_tree l + sum_tree r
    } else { 0 }
}

{
    let t = Node (0, Node(1, TNil, TNil), Node(2, TNil, Node(36, TNil, TNil)));
    sum_tree t !
}

{
    data List = Nil | Cons Int List;
    let xs = Cons 3 Nil;

}


