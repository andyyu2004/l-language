{
    let pair = (1, false);

    fn print_pair = p: (Int, Bool) {
        if let (a, b) = p {
            a! b!
        }
    }

    print_pair pair
}
