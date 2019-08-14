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