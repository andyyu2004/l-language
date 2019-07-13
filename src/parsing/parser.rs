use crate::lexing::token::{TokenType};
use crate::parsing::{Stmt, Expr};
use std::mem::{discriminant, Discriminant};
use crate::lexing::token::TokenType::Semicolon;
use crate::lexing::Token;
use crate::L;

/*
<expr> ::= <addition>
<addition> ::= <addition> +|- <unary>
<unary> ::= - <unary> | <primary>
<primary> ::= <NUMBER>

<expr> ::= <addition>
<addition> ::= <primary> { +|- <primary> }
<primary> ::= <NUMBER>
*/

pub struct Parser {
    tokens: Vec<Token>,
    i: usize,
}

// Utility
impl Parser {
    pub fn current(&self) -> Token { self.tokens[self.i] }

    pub fn previous(&self) -> Token {
        self.tokens[self.i-1]
    }

//    pub fn peek(&self) -> Option<Token> {
//        if self.i + 1 < self.tokens.len() {
//            Some(self.tokens[self.i + 1])
//        } else { None }
//    }

    fn expect(&mut self, ttype: TokenType) -> Result<Token, String> {
        if self.current().ttype == ttype {
            self.i += 1;
            Ok(self.previous())
        } else {
            Err(format!("Expected {:?}, found EOF", ttype))
        }
    }

    fn expect_discriminant(&mut self, r#type: Discriminant<TokenType>) -> Result<Token, String> {
        if &discriminant(&self.current().ttype) == &r#type {
            self.i += 1;
            Ok(self.previous())
        } else {
            Err(format!("Expected {:?}, found {:?}", r#type, self.current()))
        }
    }

    fn match1(&mut self, r#type: TokenType) -> bool {
        if self.current().ttype == r#type {
            self.i += 1;
            true
        } else { false }
    }
    fn r#match(&mut self, types: &[TokenType]) -> bool {
        if types.iter().any(|x| x == &self.current().ttype) {
            self.i += 1;
            true
        } else { false }
    }

    // Matches based on enum variant only, useful for Number(f64)
    fn match_discriminant(&mut self, types: &[Discriminant<TokenType>]) -> bool {
        if types.iter().any(|x| x == &discriminant(&self.current().ttype)) {
            self.i += 1;
            true
        } else { false }
    }
}


// Parsing Statements
impl Parser {

    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser { tokens, i: 0 }
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>, Vec<String>> {
        let mut statements = Vec::<Stmt>::new();
        let mut errors = Vec::<String>::new();
        while self.current().ttype != TokenType::EOF {
            match self.parse_declaration() {
                Ok(stmt) => statements.push(stmt),
                Err(err) => {
                    errors.push(err);
                    self.synchronize();
                }
            }

        }
        if errors.is_empty() { Ok(statements) }
        else { Err(errors) }

    }

    fn synchronize(&self) {

    }

    // Maybe move synchronize to this method?? maybe not
    // <declaration> ::= <varDecl> | <letBinding> | <statement>
    fn parse_declaration(&mut self) -> Result<Stmt, String> {
        if self.r#match1(TokenType::Let) {
            self.parse_let_binding()
        } else if self.r#match1(TokenType::Var) {
            self.parse_var_decl()
        } else {
            self.parse_statement()
        }
    }

    // <varDecl> ::= "var" ID ( = <expr> )? ;
    fn parse_var_decl(&mut self) -> Result<Stmt, String> {
        let token = self.expect_discriminant(discriminant(&TokenType::Identifier("")))?;
        let mut init = None::<Expr>;
        if self.r#match1(TokenType::Equal) {
            init = Some(self.parse_expression()?);
        }
        self.expect(TokenType::Semicolon)?;
        if let TokenType::Identifier(name) = token.ttype {
            Ok(Stmt::Var(name, init))
        } else {
            Err(L::error("Expected identifier, found {}".to_string(), token.line, token.col))
        }
    }

    // <letBinding> ::= "let" ID = <expr> ;
    fn parse_let_binding(&mut self) -> Result<Stmt, String> {
        let token = self.expect_discriminant(discriminant(&TokenType::Identifier("")))?;
        println!("Post token");
        self.expect(TokenType::Equal)?;
        println!("Post equal");
        let init = self.parse_expression()?;
        self.expect(TokenType::Semicolon)?;
        if let TokenType::Identifier(name) = token.ttype {
            Ok(Stmt::Let(name, init))
        } else {
            Err(L::error("Expected identifier, found {}".to_string(), token.line, token.col))
        }


    }

    // <statement> ::= <exprStmt> | <printStmt>
    fn parse_statement(&mut self) -> Result<Stmt, String> {
        self.parse_exprstmt()

    }
    // <exprStmt> ::= <expr> ;
    fn parse_exprstmt(&mut self) -> Result<Stmt, String> {
        let expr = self.parse_expression()?;
        // If there is ; expr_stmt, else expr
        if let Ok(_) = self.expect(Semicolon) {
            Ok(Stmt::ExprStmt(expr))
        } else { Ok(Stmt::PrintStmt(expr)) }

    }

}


// Parsing expr
impl Parser {

    pub fn parse_expression(&mut self) -> Result<Expr, String> {
//        println!("Expr");
        self.parse_addition()
    }

    fn parse_addition(&mut self) -> Result<Expr, String> {
//        println!("Add");
        let mut expr = self.parse_multiplication();
        while self.r#match(&[TokenType::Plus, TokenType::Minus]) {
            let operator = self.previous();
            let right = self.parse_multiplication();
            expr = Ok(Expr::Binary(operator.ttype, Box::new(expr?), Box::new(right?)))
        }
        expr
    }

    fn parse_multiplication(&mut self) -> Result<Expr, String> {
//        println!("Mult");
        let mut expr = self.parse_unary();
        while self.r#match(&[TokenType::Star, TokenType::Slash]) {
            let operator = self.previous();
            let right = self.parse_unary();
            // Unwrap the expressions
//            expr = expr.and_then(|left| right.and_then(|right|
//                Ok(Expr::Binary(Box::new(left), operator, Box::new(right)))
//            ))
            expr = Ok(Expr::Binary(operator.ttype, Box::new(expr?), Box::new(right?)))
        }
        expr
    }

    // <unary> ::= - <unary> | <primary>
    fn parse_unary(&mut self) -> Result<Expr, String> {
//        println!("Unary");
        if self.r#match1(TokenType::Minus) {
            let operator = self.previous();
            let right = self.parse_unary();
            Ok(Expr::Unary(operator.ttype, Box::new(right?)))
        } else {
            self.parse_exponent()
        }
    }

    // <exp> ::= <primary> ^ <exp>
    fn parse_exponent(&mut self) -> Result<Expr, String> {
//        println!("Exp");
        let mut expr = self.parse_primary();
        while self.r#match1(TokenType::Caret) {
            let operator = self.previous();
            let right = self.parse_exponent();
            expr = Ok(Expr::Binary(operator.ttype, Box::new(expr?), Box::new(right?)));
        }
        expr
    }

    fn parse_primary(&mut self) -> Result<Expr, String> {
//        println!("Primary");
        if self.r#match1(TokenType::LParen) {
            let expr = self.parse_expression();
            if let Err(err) = self.expect(TokenType::RParen) { return Err(err) }
            Ok(Expr::Grouping(Box::new(expr?)))
        } else if self.match_discriminant(&[discriminant(&TokenType::Number(0.0))]) {
            if let TokenType::Number(x) = self.previous().ttype {
                Ok(Expr::Operand(x))
            } else { Err("Failed to parse Number".to_string()) }
        } else if self.match_discriminant(&[discriminant(&TokenType::Identifier(""))]) {
            if let TokenType::Identifier(name) = self.previous().ttype {
                Ok(Expr::Variable(name))
            } else { unreachable!() }
        } else {
            let t = self.current();
            Err(L::error(format!("unexpected token: {:?}", self.current()), t.line, t.col))
        }
    }

}