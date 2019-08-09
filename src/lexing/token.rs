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

    pub fn dummy() -> Token {
        Token {
            lexeme: "generated token".to_string(),
            ttype: TokenType::Nothing,
            line: -1,
            col: -1
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "{}", self.lexeme)
//        write!(f, "{} of {} at {}:{}", self.lexeme, self.ttype, self.line, self.col)
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
    Pipe, DoublePipe,
    Ampersand, DoubleAmpersand,
    PlusEqual, MinusEqual, StarEqual, SlashEqual, Modulo,
    Var, Let, Print, Identifier, Fn,
    Comma, Underscore,
    Colon, DoubleColon,
    Typename,
    Return,
    Dot, DoubleDot,
    If, Else, While, Match,
    Record, Type,
    RightArrow, RightFatArrow,
    Struct, Data,
    Nothing
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