use std::collections::HashMap;
use std::option::Option::Some;

use crate::errors::LError;
use crate::lexing::Token;
use crate::lexing::token::TokenType;

pub struct Lexer {
    xs: Vec<char>,
    keywords: HashMap<&'static str, TokenType>,
    line: i32,
    col: i16,
    i: usize
}

impl Lexer {
    pub fn new(xs: String, keywords: HashMap<&'static str, TokenType>) -> Lexer {
        Lexer { xs: xs.chars().collect(), keywords, line: 1, col: 0, i: 0 }
    }

    pub(crate) fn lex(&mut self) -> Result<Vec<Token>, Vec<LError>> {
        let mut tokens = Vec::<Token>::new();
        let mut errors = Vec::<LError>::new();
        while let Some(c) = self.safe_peek() {
            match c {
                '\n' => { self.line += 1; self.col = 0; self.i += 1; continue; },
                '\t' => { self.col += 4; self.i += 1; continue; },
                '\r' => { self.col = 0; self.i += 1; continue; },
                ' ' => {},
                ',' => tokens.push(self.create_token(TokenType::Comma, char::to_string(&c))),
                ':' => tokens.push(self.create_token(TokenType::Colon, char::to_string(&c))),
                '+' => if self.match_next('=') {
                    tokens.push(self.create_token(TokenType::PlusEqual, "+".to_string()))
                } else {
                    tokens.push(self.create_token(TokenType::Plus, char::to_string(&c)))
                },
                '-' => if self.match_next('>') {
                    tokens.push(self.create_token(TokenType::RightArrow, "->".to_string()));
                } else if self.match_next('=') {
                    tokens.push(self.create_token(TokenType::MinusEqual, "-".to_string()))
                } else {
                    tokens.push(self.create_token(TokenType::Minus, char::to_string(&c)))
                },
                '*' => tokens.push(self.create_token(TokenType::Star, char::to_string(&c))),
                '^' => tokens.push(self.create_token(TokenType::Caret, char::to_string(&c))),
                '{' => tokens.push(self.create_token(TokenType::LBrace, char::to_string(&c))),
                '}' => tokens.push(self.create_token(TokenType::RBrace, char::to_string(&c))),
                '(' => tokens.push(self.create_token(TokenType::LParen, char::to_string(&c))),
                ')' => tokens.push(self.create_token(TokenType::RParen, char::to_string(&c))),
                ';' => tokens.push(self.create_token(TokenType::Semicolon, char::to_string(&c))),
                '/' => if self.match_next('/') {
                    while !self.at_end() && self.peek() != '\n' { self.inc_indexes(); }
                    continue;
                } else if self.match_next('*') {

                } else {
                    tokens.push(self.create_token(TokenType::Slash, char::to_string(&c)))
                },
                '=' => if self.match_next('=') {
                    tokens.push(self.create_token(TokenType::DoubleEqual, "==".to_string()));
                } else if self.match_next('>') {
                    tokens.push(self.create_token(TokenType::RightFatArrow, "=>".to_string()));
                } else { tokens.push(self.create_token(TokenType::Equal, char::to_string(&c))); },
                '<' => if self.match_next('=') {
                    tokens.push(self.create_token(TokenType::LessEqual, "<=".to_string()));
                } else {
                    tokens.push(self.create_token(TokenType::Less, char::to_string(&c)))
                },
                '>' => if self.match_next('=') {
                    tokens.push(self.create_token(TokenType::GreaterEqual, ">=".to_string()));
                } else {
                    tokens.push(self.create_token(TokenType::Greater, char::to_string(&c)))
                },
                '!' => if self.match_next('=') {
                    tokens.push(self.create_token(TokenType::BangEqual, "!=".to_string()));
                } else {
                    tokens.push(self.create_token(TokenType::Bang, char::to_string(&c)))
                },
                '|' => if self.match_next('|') {
                    tokens.push(self.create_token(TokenType::DoublePipe, "||".to_string()));
                } else {
                    tokens.push(self.create_token(TokenType::Pipe, char::to_string(&c)))
                },
                '&' => if self.match_next('&') {
                    tokens.push(self.create_token(TokenType::DoubleAmpersand, "&&".to_string()));
                    self.inc_indexes();
                } else {
                    tokens.push(self.create_token(TokenType::Ampersand, char::to_string(&c)))
                },
                '.' => if self.match_next('.') {
                    tokens.push(self.create_token(TokenType::DoubleDot, "..".to_string()));
                    self.inc_indexes();
                } else {
                    tokens.push(self.create_token(TokenType::Dot, char::to_string(&c)))
                },
                '0'...'9' => {
                    let start_col = self.col;
                    let num = self.lex_number();
                    let token = Token::new(TokenType::Number, num, self.line, start_col);
                    tokens.push(token);
                    continue;
                },
                c if Lexer::is_identifier_start(c) => {
                    let start_col = self.col;
                    let id = self.lex_identifier();
                    if self.keywords.contains_key::<str>(&id) {
                        let ttype = self.keywords.get::<str>(&id).unwrap();
                        tokens.push(Token::new(*ttype, id, self.line, start_col));
                    } else {
                        tokens.push(Token::new(TokenType::Identifier, id, self.line, start_col))
                    }
                    continue;
                },
                'A'...'Z' => {
                    let start_col = self.col;
                    let t = self.lex_type();
                    tokens.push(Token::new(TokenType::Typename, t, self.line, start_col));
                    continue;
                },
                x => errors.push(LError::new(format!("Unexpected character: {}", x), self.line, self.col)),
            }
            self.inc_indexes();
        }
        tokens.push(self.create_token(TokenType::EOF, "EOF".to_string()));
        // Declare accumulator locally to allow move out
        if errors.is_empty() { Ok(tokens) }
        else { Err(errors) }

    }

    fn create_token(&self, ttype: TokenType, lexeme: String) -> Token {
        Token {
            ttype,
            lexeme,
            col: self.col,
            line: self.line
        }
    }

    fn lex_identifier(&mut self) -> String {
        let mut acc = String::new();
        while !self.at_end() && Lexer::is_identifier_char(self.peek()) {
            acc.push(self.peek());
            self.inc_indexes();
        }
        acc
    }

    fn lex_type(&mut self) -> String {
        let mut acc = String::new();
        while !self.at_end() && self.peek().is_ascii_alphabetic() {
            acc.push(self.peek());
            self.inc_indexes();
        }
        acc
    }

    fn lex_number(&mut self) -> String {
        let mut acc = String::new();
        while !self.at_end() && self.peek().is_numeric() {
            acc.push(self.peek());
            self.inc_indexes();
        }
        if self.match1('.') {
            if !self.at_end() && self.peek().is_numeric() {
                acc.push(self.previous());
                while !self.at_end() && self.peek().is_numeric() {
                    acc.push(self.peek());
                    self.inc_indexes();
                }
            } else { self.i -= 1; }
        }
        acc


    }

}


impl Lexer {
    // Utility
    fn is_identifier_start(c: char) -> bool {
        'a' <= c && c <= 'z' || c == '_'
    }

    fn is_identifier_char(c: char) -> bool {
        Lexer::is_identifier_start(c) || '0' <= c && c <= '9' || 'A' <= c && c <= 'Z' || c == '\''
    }

//    fn next(&mut self) -> Option<char> {
//        let x = self.safe_peek();
//        if x.is_some() {
//            self.inc_indexes();
//            x
//        } else { None }
//    }

    fn match1(&mut self, c: char) -> bool {
        if self.safe_peek().map_or(false, |x| x == c) {
            self.inc_indexes();
            true
        } else { false }
    }

    fn match_next(&mut self, c: char) -> bool {
//        self.lookahead(1).map_or(false, |x| x == c) // this does not increment
        if self.lookahead(1).map_or(false, |x| x == c) {
            self.inc_indexes();
            true
        } else { false }
    }

//    fn prev(&self) -> char {
//        self.xs[self.i - 1]
//    }

    fn at_end(&self) -> bool {
        self.i >= self.xs.len()
    }

    fn inc_indexes(&mut self) {
        self.i += 1;
        self.col += 1;
    }

    fn peek(&self) -> char {
        self.xs[self.i]
    }

    fn safe_peek(&self) -> Option<char> {
        if self.i >= self.xs.len() { None }
        else { Some(self.peek()) }
    }

    fn lookahead(&self, n: usize) -> Option<char> {
        if self.i + n >= self.xs.len() { None }
        else { Some(self.xs[self.i + n]) }
    }

    fn previous(&self) -> char {
        self.xs[self.i - 1]
    }

}