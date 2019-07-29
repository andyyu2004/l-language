//use crate::L;
use crate::lexing::Token;
use crate::lexing::token::TokenType;
use crate::lexing::token::TokenType::Semicolon;
use crate::parsing::{Expr, Stmt};
use crate::errors::LError;
use crate::types::LType;
use crate::types::l_types::NameTypePair;

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
    pub fn current(&self) -> &Token { &self.tokens[self.i] }

    pub fn previous(&self) -> &Token {
        &self.tokens[self.i-1]
    }

//    pub fn peek(&self) -> Option<Token> {
//        if self.i + 1 < self.tokens.len() {
//            Some(self.tokens[self.i + 1])
//        } else { None }
//    }

    fn expect(&mut self, ttype: TokenType) -> Result<&Token, LError> {
        if self.current().ttype == ttype {
            self.i += 1;
            Ok(self.previous())
        } else {
            Err(LError::from_token(format!("Expected {}, found {}", ttype, self.current()), self.current()))
        }
    }

    // fn expect_discriminant(&mut self, r#type: Discriminant<TokenType>) -> Result<Token, String> {
    //     if &discriminant(&self.current().ttype) == &r#type {
    //         self.i += 1;
    //         Ok(self.previous())
    //     } else {
    //         Err(format!("Expected {:?}, found {:?}", r#type, self.current()))
    //     }
    // }

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
    // fn match_discriminant(&mut self, types: &[Discriminant<TokenType>]) -> bool {
    //     if types.iter().any(|x| x == &discriminant(&self.current().ttype)) {
    //         self.i += 1;
    //         true
    //     } else { false }
    // }
}


// Parsing Statements
impl Parser {

    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser { tokens, i: 0 }
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>, Vec<LError>> {
        let mut statements = Vec::<Stmt>::new();
        let mut errors = Vec::<LError>::new();
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

    // Consume tokens to logical place such as end of statement or return statement
    fn synchronize(&mut self) {
        while self.current().ttype != TokenType::EOF {
            self.i += 1;
            match self.previous().ttype {
                TokenType::Semicolon => break,
                _                    => continue,
            }
        }
    }

    // Maybe move synchronize to this method?? maybe not
    // <declaration> ::= <varDecl> | <letBinding> | <statement>
    fn parse_declaration(&mut self) -> Result<Stmt, LError> {
        if self.match1(TokenType::Let) {
            self.parse_let_binding()
        } else if self.match1(TokenType::Var) {
            self.parse_var_decl()
        } else if self.match1(TokenType::Fn) {
            self.parse_fn_decl()
        } else {
            self.parse_statement()
        }
    }

    // <varDecl> ::= "var" ID ( = <expr> )? ;
    fn parse_var_decl(&mut self) -> Result<Stmt, LError> {
        // let token = self.expect_discriminant(discriminant(&TokenType::Identifier("")))?;
        let name = self.expect(TokenType::Identifier)?.clone();
        let mut init = None::<Expr>;
        if self.r#match1(TokenType::Equal) {
            init = Some(self.parse_expression()?);
        }
        self.expect(TokenType::Semicolon)?;
        Ok(Stmt::Var(name, init))
    }

    // <letBinding> ::= "let" ID = <expr> ;
    fn parse_let_binding(&mut self) -> Result<Stmt, LError> {
        let name = self.expect(TokenType::Identifier)?.clone();
        self.expect(TokenType::Equal)?;
        let init = self.parse_expression()?;
        self.expect(TokenType::Semicolon)?;
        Ok(Stmt::Let(name, init))
    }

    fn parse_fn_decl(&mut self) -> Result<Stmt, LError> {
        let name = self.expect(TokenType::Identifier)?.clone();
        self.expect(TokenType::LParen)?;
        let args = self.parse_typed_args()?;
        let mut return_type = LType::TUnit;
        if self.match1(TokenType::RightArrow) { return_type = self.parse_type()?; }
        self.expect(TokenType::LBrace)?;
        let statements = self.parse_block()?;
        Ok(Stmt::Fn(name, args, return_type, statements))
    }

    fn parse_typed_args(&mut self) -> Result<Vec<NameTypePair>, LError> {
        let mut vec = Vec::<NameTypePair>::new();
        while self.current().ttype != TokenType::RParen {
            let name = self.expect(TokenType::Identifier)?.lexeme.clone();
            self.expect(TokenType::Colon)?;
            let ltype = self.parse_type()?;
            vec.push(NameTypePair::new(name, ltype));
            if self.match1(TokenType::Comma) { continue; } else { break; }
        }
        self.expect(TokenType::RParen)?;
        Ok(vec)
    }

    // <type> ::= <typename> | <typename> -> <type>; Right associtive type constructor
    fn parse_type(&mut self) -> Result<LType, LError> {
        let mut ltype = self.parse_primitive_type();
        while self.match1(TokenType::RightArrow) {
            ltype = Ok(LType::TArrow(Box::new(ltype?), Box::new(self.parse_type()?)));
        }
        ltype
    }

    fn parse_primitive_type(&mut self) -> Result<LType, LError> {
        // Not sure how to match strings with runtime types yet as needs to run through evaluator first to generate types?? hm
        if self.match1(TokenType::LParen) {
            let expr = self.parse_type();
            self.expect(TokenType::RParen)?;
            expr
        } else {
            let typename= self.expect(TokenType::Typename)?;
            if &typename.lexeme == "Number" {
                Ok(LType::TNum)
            } else if &typename.lexeme == "Bool" {
                Ok(LType::TBool)
            } else {
                Err(LError::from_token("Unknown type".to_string(), typename))
            }
        }
    }

    fn parse_block(&mut self) -> Result<Vec<Stmt>, LError> {
        let mut vec = Vec::<Stmt>::new();
        while self.current().ttype != TokenType::EOF && self.current().ttype != TokenType::RBrace {
            vec.push(self.parse_declaration()?);
        }
        self.expect(TokenType::RBrace)?;
        Ok(vec)
    }

    // <statement> ::= <exprStmt> | <printStmt>
    fn parse_statement(&mut self) -> Result<Stmt, LError> {
        self.parse_exprstmt()
    }

    // <exprStmt> ::= <expr> ;
    fn parse_exprstmt(&mut self) -> Result<Stmt, LError> {
        let expr = self.parse_expression()?;
        // If there is ; expr_stmt, else expr
        if let Ok(_) = self.expect(Semicolon) {
            Ok(Stmt::ExprStmt(expr))
        } else { Ok(Stmt::PrintStmt(expr)) }

    }

}


// Parsing expr
impl Parser {

    pub fn parse_expression(&mut self) -> Result<Expr, LError> {
//        println!("Expr");
        self.parse_equality()
    }

    // <eq> ::= <comp> { =|!= <comp> }
    fn parse_equality(&mut self) -> Result<Expr, LError> {
        let mut expr = self.parse_comparison();
        while self.r#match(&[TokenType::DoubleEqual, TokenType::BangEqual]) {
            let operator = self.previous().clone();
            let right = self.parse_addition()?;
            expr = Ok(Expr::Binary(operator, Box::new(expr?), Box::new(right)))
        }
        expr
    }

    // <comp> ::= <add> <> <add>
    fn parse_comparison(&mut self) -> Result<Expr, LError> {
        let mut expr = self.parse_addition();
        while self.r#match(&[TokenType::Greater, TokenType::GreaterEqual, TokenType::Less, TokenType::LessEqual]) {
            let operator = self.previous().clone();
            let right = self.parse_addition()?;
            expr = Ok(Expr::Binary(operator, Box::new(expr?), Box::new(right)))
        }
        expr
    }

    fn parse_addition(&mut self) -> Result<Expr, LError> {
//        println!("Add");
        let mut expr = self.parse_multiplication();
        while self.r#match(&[TokenType::Plus, TokenType::Minus]) {
            let operator = self.previous().clone();
            let right = self.parse_multiplication();
            expr = Ok(Expr::Binary(operator, Box::new(expr?), Box::new(right?)))
        }
        expr
    }

    fn parse_multiplication(&mut self) -> Result<Expr, LError> {
//        println!("Mult");
        let mut expr = self.parse_unary();
        while self.r#match(&[TokenType::Star, TokenType::Slash]) {
            let operator = self.previous().clone();
            let right = self.parse_unary();
            // Unwrap the expressions
//            expr = expr.and_then(|left| right.and_then(|right|
//                Ok(Expr::Binary(Box::new(left), operator, Box::new(right)))
//            ))
            expr = Ok(Expr::Binary(operator, Box::new(expr?), Box::new(right?)))
        }
        expr
    }

    // <unary> ::= - <unary> | <primary>
    fn parse_unary(&mut self) -> Result<Expr, LError> {
//        println!("Unary");
        if self.r#match1(TokenType::Minus) {
            let operator = self.previous().clone();
            let right = self.parse_unary();
            Ok(Expr::Unary(operator, Box::new(right?)))
        } else {
            self.parse_exponent()
        }
    }

    // <exp> ::= <primary> ^ <exp>
    fn parse_exponent(&mut self) -> Result<Expr, LError> {
//        println!("Exp");
        let mut expr = self.parse_application();
        while self.r#match1(TokenType::Caret) {
            let operator = self.previous().clone();
            let right = self.parse_exponent();
            expr = Ok(Expr::Binary(operator, Box::new(expr?), Box::new(right?)));
        }
        expr
    }

    // <application> ::= <primary> { (<args>?) }
    fn parse_application(&mut self) -> Result<Expr, LError> {
        let mut expr = self.parse_primary();
        while self.match1(TokenType::LParen) {
            let lparen = self.previous().clone();
            let args = self.parse_args()?;
            expr = Ok(Expr::Application(lparen, Box::new(expr?), args));
            self.expect(TokenType::RParen)?;
        }
        expr
    }

    // <args> ::= <expr> { , <expr> : <type> }
    fn parse_args(&mut self) -> Result<Vec<Expr>, LError> {
        let mut v = Vec::<Expr>::new();
        if self.current().ttype != TokenType::RParen {
            while { // Cheap do-while loop
                v.push(self.parse_expression()?);
                self.match1(TokenType::Comma)
            } {}
        }
        Ok(v)
    }

    fn parse_primary(&mut self) -> Result<Expr, LError> {
//        println!("Primary");
        if self.r#match1(TokenType::LParen) {
            let expr = self.parse_expression();
            if let Err(err) = self.expect(TokenType::RParen) { return Err(err) }
            expr
//            Ok(Expr::Grouping(Box::new(expr?))) // dont think this is explicitly required
        } else if self.match1(TokenType::Number) {
            let token = self.previous().clone();
            Ok(Expr::Literal(token))
        } else if self.match1(TokenType::Identifier) {
            Ok(Expr::Variable(self.previous().clone()))
        } else if self.match1(TokenType::True) {
            Ok(Expr::Literal(self.previous().clone()))
        } else if self.match1(TokenType::False){
            Ok(Expr::Literal(self.previous().clone()))
        } else {
            let t = self.current();
            Err(LError::from_token(format!("unexpected token: {}", t), t))
        }
    }

}