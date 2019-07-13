use std::fmt::{Display, Formatter, Error};

#[derive(Debug, PartialEq, Copy, Clone)]
pub struct  Token {
    pub ttype: TokenType,
    pub line: i32,
    pub col: i16
}

impl Token {
    pub fn new(ttype: TokenType, line: i32, col: i16) -> Token {
        Token {
            ttype,
            line,
            col,
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "{}", self.ttype)
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum TokenType {
    Plus,
    Minus,
    Star,
    Slash,
    Caret,
    Number(f64),
    RParen,
    LParen,
    EOF,
    Equal,
    Semicolon,
    Var,
    Let,
    Print,
    Identifier(&'static str)
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