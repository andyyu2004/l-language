use std::fmt::{Display, Error, Formatter};

#[derive(Debug, PartialEq, Clone, Eq)]
pub struct Token {
    pub ttype: TokenType,
    pub lexeme: String,
    pub line: i32,
    pub col: i16
}

impl Token {
    pub fn new(ttype: TokenType, lexeme: String, line: i32, col: i16) -> Token {
        Token {
            lexeme,
            ttype,
            line,
            col,
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "{} of {} at {}:{}", self.lexeme, self.ttype, self.line, self.col)
    }
}

// Don't associate values with tokentype, makes parsing more difficult, place data into lexeme field in Token type above
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum TokenType {
    Plus, Minus, Star, Slash, Caret, Number,
    LParen, RParen, LBrace, RBrace,
    EOF,
    Equal, DoubleEqual, Less, Greater, LessEqual, GreaterEqual, Bang, BangEqual,
    True, False,
    Semicolon,
    Var,
    Let,
    Print,
    Identifier,
    Comma,
    Colon,
    Fn,
    Typename,
    Return,
    If, Else,
    RightArrow, RightFatArrow
}


impl Display for TokenType {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            TokenType::Plus => write!(f, "+"),
            TokenType::Minus => write!(f, "-"),
            TokenType::Star => write!(f, "*"),
            TokenType::Caret => write!(f, "^"),
            TokenType::Slash => write!(f, "/"),
            x => write!(f, "{:?}", x)
        }
    }
}