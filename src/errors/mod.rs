use std::fmt::{Display, Error, Formatter};
use crate::lexing::Token;

#[derive(Debug, Clone)]
pub struct LError {
    message: String,
    line: i32,
    col: i16,
}

impl Display for LError {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        writeln!(f, "{}:{}: {}", self.line, self.col, self.message)
    }
}

//impl FromIterator<LError> for LError {
//    fn from_iter<T: IntoIterator<Item=LError>>(iter: T) -> Self {
//        let mut buf = String::new();
//        for e in iter {
//            println!("{}", e);
//        }
//        LError::new(buf, 0,0)
//    }
//}


impl LError {

    pub(crate) fn from_token(message: String, token: &Token) -> LError {
        let Token { line, col, .. } = token;
        LError {
            message,
            line: *line,
            col: *col,
        }
    }

    pub(crate) fn new(message: String, line: i32, col: i16) -> LError {
        LError {
            line, col, message
        }
    }
}

