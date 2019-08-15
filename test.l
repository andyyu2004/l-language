data List<'a> = Nil | Cons 'a List<'a>;


fn map = f: 'a -> 'b => xs: List<'a> : List<'b> {
    match xs {
        | Cons x xs -> Cons (f x) (map f xs)
        | Nil       -> Nil
    }
}

let xs = Cons 0 (Cons 1 (Cons 2 (Cons 3 Nil)));
let doublexs = map (\x: Int => x * 2) xs;
doublexs !

map (\x: Int => x % 2 == 0) xs !
