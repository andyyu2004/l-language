//use crate::L;
use crate::lexing::Token;
use crate::lexing::token::TokenType;
use crate::parsing::{Expr, Stmt, ParseMode};
use crate::errors::LError;
use crate::types::LType;
use crate::types::l_types::NameTypePair;
use crate::parsing::stmt::Stmt::{FnStmt, LetStmt, FnCurried, VarStmt, BlockStmt, ReturnStmt};
use crate::types::l_types::LType::{TTuple, TUnit, TNum, TBool};
use crate::parsing::expr::Expr::{EApplication, EBinary, EUnary, ELiteral, EVariable, EAssignment};
use std::borrow::Borrow;

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
    mode: ParseMode
}

// Utility
impl Parser {

    pub fn new(tokens: Vec<Token>, mode: ParseMode) -> Parser {
        Parser { tokens, mode, i: 0 }
    }

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

    // <statement> ::= <exprStmt> | <printStmt>
    fn parse_statement(&mut self) -> Result<Stmt, LError> {
        if self.match1(TokenType::LBrace) {
            Ok(BlockStmt(self.parse_block()?))
        } else if self.match1(TokenType::Return) {
            self.parse_return_stmt()
        } else {
            self.parse_exprstmt()
        }
    }

    // <exprStmt> ::= <expr> ;
    fn parse_exprstmt(&mut self) -> Result<Stmt, LError> {
        let expr = self.parse_expression()?;
        // If there is ; expr_stmt, else expr
        if self.match1(TokenType::Semicolon) {
            return Ok(Stmt::ExprStmt(expr))
        } else if self.match1(TokenType::Bang) {
            return Ok(Stmt::PrintStmt(expr))
        }
        match self.mode {
            ParseMode::Interactive => Ok(Stmt::PrintStmt(expr)),
            ParseMode::Interpreted => Err(LError::from_token("Expected ! or ; to terminate statement".to_string(), self.current()))
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
        self.expect(TokenType::Colon)?;
        let ltype = self.parse_type()?;
        if self.r#match1(TokenType::Equal) {
            init = Some(self.parse_expression()?);
        }
        self.expect(TokenType::Semicolon)?;
        Ok(VarStmt { name, ltype, init })
    }

    // <letBinding> ::= "let" ID = <expr> ;
    fn parse_let_binding(&mut self) -> Result<Stmt, LError> {
        let name = self.expect(TokenType::Identifier)?.clone();
        self.expect(TokenType::Colon)?;
        let ltype = self.parse_type()?;
        self.expect(TokenType::Equal)?;
        let init = self.parse_expression()?;
        self.expect(TokenType::Semicolon)?;
        Ok(LetStmt { name, ltype, init })
    }

    fn parse_fn_decl(&mut self) -> Result<Stmt, LError> {
        let token = self.expect(TokenType::Identifier)?.clone();
        if self.match1(TokenType::LParen) {
            let params = self.parse_typed_params()?;
            let mut ret_type = LType::TUnit;
            if self.match1(TokenType::RightArrow) { ret_type = self.parse_type()?; }
            self.expect(TokenType::LBrace)?;
            let body = self.parse_block()?;
            Ok(FnStmt{ name: Some(token.lexeme.clone()), token, params, ret_type, body })
        } else if self.match1(TokenType::Equal) {
            self.parse_curried_fn_decl(Some(token.lexeme))
        } else {
            Err(LError::from_token("Invalid syntax, expected '=' or '('".to_string(), self.current()))
        }
    }

    // Only name function on first iteration
    fn parse_curried_fn_decl(&mut self, name: Option<String>) -> Result<Stmt, LError> {
        // fn f = x: Int => y: Bool => z: Bool : ret => { body } // Optional colon indicating type
        let token = self.current().clone();
        let param = self.parse_name_type_pair()?;
        if self.match1(TokenType::RightFatArrow) {
            let right = self.parse_curried_fn_decl(None)?;
            Ok(FnCurried { name, token, param, ret: Box::new(right) })
        } else if self.match1(TokenType::Colon) {
            let ret_type = self.parse_type()?;
            self.expect(TokenType::LBrace)?;
            let body = self.parse_block()?;
            Ok(FnStmt { name, token, params: vec![param], ret_type, body })
        } else {
            Err(LError::from_token("Invalid curried function syntax".to_string(), &token))
        }
    }

    fn parse_typed_params(&mut self) -> Result<Vec<NameTypePair>, LError> {
        let mut vec = Vec::<NameTypePair>::new();
        while self.current().ttype != TokenType::RParen {
            vec.push(self.parse_name_type_pair()?);
            if !self.match1(TokenType::Comma) { break; }
        }
        self.expect(TokenType::RParen)?;
        Ok(vec)
    }

    fn parse_name_type_pair(&mut self) -> Result<NameTypePair, LError> {
        let name = self.expect(TokenType::Identifier)?.lexeme.clone();
        self.expect(TokenType::Colon)?;
        let ltype = self.parse_type()?;
        Ok(NameTypePair::new(name, ltype))
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
        if self.match1(TokenType::LParen) {
            let backtrack = self.i;
            let expr = self.parse_type();
            if expr.is_err() || self.expect(TokenType::RParen).is_err() {
                self.i = backtrack;
                return Ok(TTuple(self.parse_tuple(Parser::parse_type)?))
            }
            expr
        } else {
            let typename= self.expect(TokenType::Typename)?;
            if &typename.lexeme == "Number" || &typename.lexeme == "Int" { // Allow int as synonym for now
                Ok(TNum)
            } else if &typename.lexeme == "Bool" {
                Ok(TBool)
            } else if &typename.lexeme == "Unit" {
                Ok(TUnit)
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

    fn parse_return_stmt(&mut self) -> Result<Stmt, LError> {
        let token = self.previous().clone();
        let value = if self.match1(TokenType::Semicolon) { None } else {
            let expr = Some(self.parse_expression()?);
            self.expect(TokenType::Semicolon)?;
            expr
        };
        Ok(ReturnStmt { token, value })
    }

}


// Parsing expr
impl Parser {

    pub fn parse_expression(&mut self) -> Result<Expr, LError> {
        self.parse_assignment()
    }

    // <assn> ::= <lvalue> = <assn> | <eq>
    fn parse_assignment(&mut self) -> Result<Expr, LError> {
        let expr = self.parse_equality()?;
        if self.match1(TokenType::Equal) {
            let token = self.previous().clone();
            let rvalue = self.parse_assignment()?;
            return if let EVariable { name } = &expr {
                Ok(EAssignment { lvalue: name.clone(), expr: Box::new(rvalue) })
            } else {
                Err(LError::from_token(format!("Cannot assign to {}, not an l-value", expr), &token))
            }
        }
        Ok(expr)
    }

    // <eq> ::= <comp> { =|!= <comp> }
    fn parse_equality(&mut self) -> Result<Expr, LError> {
        let mut expr = self.parse_comparison();
        while self.r#match(&[TokenType::DoubleEqual, TokenType::BangEqual]) {
            let operator = self.previous().clone();
            let right = self.parse_addition()?;
            expr = Ok(EBinary { operator, left: Box::new(expr?), right: Box::new(right) })
        }
        expr
    }

    // <comp> ::= <add> <> <add>
    fn parse_comparison(&mut self) -> Result<Expr, LError> {
        let mut expr = self.parse_addition();
        while self.r#match(&[TokenType::Greater, TokenType::GreaterEqual, TokenType::Less, TokenType::LessEqual]) {
            let operator = self.previous().clone();
            let right = self.parse_addition()?;
            expr = Ok(EBinary { operator, left: Box::new(expr?), right: Box::new(right) })
        }
        expr
    }

    fn parse_addition(&mut self) -> Result<Expr, LError> {
//        println!("Add");
        let mut expr = self.parse_multiplication();
        while self.r#match(&[TokenType::Plus, TokenType::Minus]) {
            let operator = self.previous().clone();
            let right = self.parse_multiplication();
            expr = Ok(EBinary { operator, left: Box::new(expr?), right: Box::new(right?) })
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
            expr = Ok(EBinary { operator, left: Box::new(expr?), right: Box::new(right?) })
        }
        expr
    }

    // <unary> ::= - <unary> | <primary>
    fn parse_unary(&mut self) -> Result<Expr, LError> {
//        println!("Unary");
        if self.r#match1(TokenType::Minus) {
            let operator = self.previous().clone();
            let right = self.parse_unary();
            Ok(EUnary { operator, operand: Box::new(right?) })
        } else {
            self.parse_exponent()
        }
    }

    // <exp> ::= <primary> ^ <exp>
    fn parse_exponent(&mut self) -> Result<Expr, LError> {
//        println!("Exp");
        let mut expr = self.parse_curried_application();
        while self.r#match1(TokenType::Caret) {
            let operator = self.previous().clone();
            let right = self.parse_exponent();
            expr = Ok(EBinary { operator, left: Box::new(expr?), right: Box::new(right?) });
        }
        expr
    }

    // <application> ::= <primary> { (<args>?) }
//    fn parse_application(&mut self) -> Result<Expr, LError> {
//        let mut expr = self.parse_primary();
//        while self.match1(TokenType::LParen) {
//            let lparen = self.tokens[self.i-2].clone();
//            let args = self.parse_tuple(Parser::parse_expression)?;
//            expr = Ok(Expr::EApplication(lparen, Box::new(expr?), args));
//        }
//        expr
//    }

    // <app> = <primary> { <primary> }
    fn parse_curried_application(&mut self) -> Result<Expr, LError> {
        let mut expr = self.parse_primary();
        while let Ok(arg) = self.parse_primary() {
            expr = Ok(EApplication { token: self.current().clone(), callee: Box::new(expr?), arg: Box::new(arg) })
        }
        expr
    }

    fn parse_tuple<F, T>(&mut self, f: F) -> Result<Vec<T>, LError>
            where F: Fn(&mut Self) -> Result<T, LError> {
        let mut v = Vec::<T>::new();
        if self.current().ttype != TokenType::RParen {
            while { // Cheap do-while loop
                v.push(f(self)?);
                self.match1(TokenType::Comma)
            } {}
        }
        self.expect(TokenType::RParen)?;
        Ok(v)
    }

//    fn parse_tuple(&mut self) -> Result<Vec<Expr>, LError> {
//        let mut v = Vec::<Expr>::new();
//        if self.current().ttype != TokenType::RParen {
//            while { // Cheap do-while loop
//                v.push(self.parse_expression()?);
//                self.match1(TokenType::Comma)
//            } {}
//        }
//        self.expect(TokenType::RParen)?;
//        Ok(v)
//    }

    fn parse_primary(&mut self) -> Result<Expr, LError> {
        // Parsing as parens will fail if intended to be tuple, and unary tuples probably aren't useful
        if self.r#match1(TokenType::LParen) {
            let backtrack = self.i;
            let expr = self.parse_expression();
            if expr.is_err() || self.expect(TokenType::RParen).is_err() {
                self.i = backtrack;
                return Ok(Expr::ETuple(self.parse_tuple(Parser::parse_expression)?))
            }
            expr
//            Ok(Expr::Grouping(Box::new(expr?))) // dont think this is explicitly required
        } else if self.match1(TokenType::Number) {
            let token = self.previous().clone();
            Ok(ELiteral(token))
        } else if self.match1(TokenType::Identifier) {
            Ok(EVariable { name: self.previous().clone() })
        } else if self.match1(TokenType::True) {
            Ok(ELiteral(self.previous().clone()))
        } else if self.match1(TokenType::False){
            Ok(ELiteral(self.previous().clone()))
        } else {
            let t = self.current();
            Err(LError::from_token(format!("unexpected token: {}", t), t))
        }
    }


}