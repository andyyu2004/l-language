use std::option::Option::{Some};
use crate::lexing::token::TokenType;
use itertools::{MultiPeek,multipeek};
use std::str::Chars;
use std::collections::HashMap;
use crate::lexing::Token;
use crate::L;

pub struct Lexer {
    keywords: HashMap<&'static str, TokenType>,
    line: i32,
    col: i16
}

impl Lexer {
    pub fn new(keywords: HashMap<&'static str, TokenType>) -> Lexer {
        Lexer { keywords, line: 0, col: 0 }
    }

    pub(crate) fn lex(&mut self, xs: &str) -> Result<Vec<Token>, Vec<String>> {
        let mut it = multipeek(xs.chars());
        // Reset peek before and after due to multi peek
        let mut tokens = Vec::<Token>::new();
        let mut errors = Vec::<String>::new();
        while let Some(&c) = it.peek() {
            it.reset_peek();
            match c {
                '\n' => {self.line += 1; self.col = 0 },
                '\r' => self.col = 0,
                ' ' => {},
                '+' => tokens.push(self.create_token(TokenType::Plus)),
                '-' => tokens.push(self.create_token(TokenType::Minus)),
                '*' => tokens.push(self.create_token(TokenType::Star)),
                '/' => tokens.push(self.create_token(TokenType::Slash)),
                '^' => tokens.push(self.create_token(TokenType::Caret)),
                '(' => tokens.push(self.create_token(TokenType::LParen)),
                ')' => tokens.push(self.create_token(TokenType::RParen)),
                '=' => tokens.push(self.create_token(TokenType::Equal)),
                ';' => tokens.push(self.create_token(TokenType::Semicolon)),
                '0'...'9' => {
                    let start_col = self.col;
                    let token = Token::new(TokenType::Number(self.lex_number(&mut it)), self.line, start_col);
                    tokens.push(token);
                    continue;
                },
                c if Lexer::is_identifier_start(c) => {
                    let start_col = self.col;
                    let ttype = self.lex_identifier(&mut it);
                    tokens.push(Token::new(ttype, self.line, start_col));
                    continue;
                },
                x => errors.push(L::error(format!("Unexpected character: {}", x), self.line, self.col)),
            }
            it.next();
            self.col += 1;
        }
        tokens.push(self.create_token(TokenType::EOF));
        // Declare accumulator locally to allow move out
        if errors.is_empty() {
            Ok(tokens)
        } else {
            Err(errors)
        }

    }

    fn create_token(&self, ttype: TokenType) -> Token {
        Token {
            ttype,
            col: self.col,
            line: self.line
        }
    }

    fn lex_identifier(&mut self, it: &mut MultiPeek<Chars>) -> TokenType {
        // Know first character is correct
        let mut acc = it.next().unwrap().to_string();
        while let Some(&c) = it.peek() {
            if !Lexer::is_identifier_char(c) { break }
            acc.push(c);
            it.next();
        }

        it.reset_peek();

        self.col += acc.len() as i16;

        match self.keywords.get::<str>(&acc) {
            Some(&x) => x,
            _ => TokenType::Identifier(Box::leak(acc.into_boxed_str()))
        }

//        if self.keywords.contains_key::<str>(&acc) {
//            *self.keywords.get::<str>(&acc).unwrap()
//        } else {
//            Token::Identifier(Box::leak(acc.into_boxed_str()))
//        }
    }

    fn lex_number(&mut self, it: &mut MultiPeek<Chars>) -> f64 {
        let mut acc = String::new();
        while let Some(&c) = it.peek() {
            if c == '.' {
                if let Some(&next) = it.peek() {
                    if !next.is_numeric() {
                        // Immediately return if non-number after dot
                        break;
                    }
                }
            }
            if !c.is_numeric() && c != '.' { break }
            acc.push(c);
            it.next();
        }

        it.reset_peek();
        self.col += acc.len() as i16;
        acc.parse::<f64>().expect("Failed to lex number")
    }

    // Utility
    fn is_identifier_start(c: char) -> bool {
        'a' <= c && c <= 'z' || c == '_'
    }

    fn is_identifier_char(c: char) -> bool {
        Lexer::is_identifier_start(c) || '0' <= c && c <= '9'
    }

}