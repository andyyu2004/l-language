
//pub fn f<T, E, F>(x: Result<T, E>, f: F) -> T where F: FnOnce(E) -> T {
//    match x {
//        Ok(x) => x,
//        Err(e) => f(e)
//    }
//}