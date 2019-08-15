// Generics

{
    fn identity(x: 'a) -> 'a { x }

    identity false !
    identity 0 !
    identity "identity" !
}
{
    fn f = x: 'a => y: 'a {
        x! y!
    }

    let printnumber = f 0;
    let printstring = f "printstring";

    printnumber 5;
    printstring "str"
}

{
    fn higherorder = f: 'a -> 'b -> 'c => x: 'a => y: 'b : 'c {
        f x y
    }

    fn abc = x: Int => y: Bool : String {
        "abc"
    }

    higherorder abc 5 false ! // This does correctly enforce the types currently 'a -> Int and 'b -> Bool

}


// Polymorphic ADTs
{
    data List<'a> = Nil | Cons 'a List<'a>;
    let xs = Cons 5 (Cons 6 Nil);

    // ADT kinds etc need to be checked in function definitions
    fn map = f: 'a -> 'b => xs: List<'a> : List<'b> {
        match xs {
            | Cons x xs -> Cons (f x) (map f xs)
            | Nil -> Nil
        }
    }

    map (\x: Int => \y: Int => x + y) xs !

}