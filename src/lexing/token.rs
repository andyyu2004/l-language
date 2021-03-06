use std::fmt::{Display, Error, Formatter, Debug};
use std::hash::{Hash, Hasher};
use std::cmp::Ordering;

#[derive(Clone, Eq)]
pub struct Token {
    pub ttype: TokenType,
    pub lexeme: String,
    pub line: i32,
    pub col: i16
}

impl Hash for Token {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.ttype.hash(state);
        self.lexeme.hash(state);
    }
}

impl Debug for Token {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "{}", self)
    }
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        self.ttype == other.ttype && self.lexeme == other.lexeme
    }
}

impl PartialOrd for Token {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.lexeme.partial_cmp(&other.lexeme)
    }
}

impl Ord for Token {
    fn cmp(&self, other: &Self) -> Ordering {
        self.lexeme.cmp(&other.lexeme)
    }
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

    pub fn name(name: String) -> Token {
        Token {
            lexeme: name,
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
#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
pub enum TokenType {
    Plus, Minus, Star, Slash, Caret, Number,
    LParen, RParen, LBrace, RBrace,
    EOF,
    Lambda,
    Equal, DoubleEqual, Less, Greater, LessEqual, GreaterEqual, Bang, BangEqual,
    True, False,
    Semicolon,
    Pipe, DoublePipe,
    Ampersand, DoubleAmpersand,
    PlusEqual, MinusEqual, StarEqual, SlashEqual, Modulo, Dollar,
    Var, Let, Print, Identifier, Fn, TypeVar,
    Comma, Underscore,
    Colon, DoubleColon,
    Return, String,
    LSquare, RSquare,
    Dot, DoubleDot,
    If, Else, While, Match,
    Record, Type, Struct, Data, Typename, Class,
    RightArrow, RightFatArrow,
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
            TokenType::RightArrow => write!(f, "->"),
            TokenType::RightFatArrow => write!(f, "=>"),
            x => write!(f, "{:?}", x)
        }
    }
}