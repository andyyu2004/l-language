fn scoping() {
    var c = 0;
    c!
    fn incC() {
        c = c + 1;
        c!
    }
    incC();
    c!
    incC();
    c!
}

scoping();

var a = 100;
{
    fn printa() { a! }
    printa()
    var a = 400;
    printa();
    fn inc_a() { a = a + 1; a! }
}

