{
    let destructure_triple = \(a,b,c) : (Int, Int, Int) => {a! b! c!};
    let triple = (1,2,3);
    destructure_triple triple
}

{
    let add = \x: Int => \y: Int => x + y;
    let add_17 = add 17;
    add_17 16 !
}



