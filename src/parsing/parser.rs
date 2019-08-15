//use crate::L;
use crate::lexing::Token;
use crate::lexing::token::TokenType;
use crate::parsing::{Expr, Stmt, Mode};
use crate::errors::LError;
use crate::types::LType;
use crate::types::l_types::{Pair, TypeName};
use crate::parsing::stmt::Stmt::*;
use crate::types::l_types::LType::{TTuple, TRecord, TName, TArrow, TUnit, TList, TVar};
use crate::parsing::expr::Expr::*;
use std::fmt::{Display};
use std::collections::{HashMap, VecDeque};
use crate::lexing::token::TokenType::{Plus, Minus, Star, Slash};
use crate::interpreting::pattern_matching::LPattern;
use crate::interpreting::pattern_matching::LPattern::*;
use itertools::Itertools;
use std::rc::Rc;

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

    pub fn new(tokens: Vec<Token>, _mode: Mode) -> Parser {
        Parser { tokens, i: 0 }
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
        if self.match1(TokenType::Return) {
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
            Ok(Stmt::ExprStmt(expr))
        } else if self.match1(TokenType::Bang) {
            Ok(Stmt::PrintStmt(expr))
        } else {
            Ok(Stmt::LStmt(expr))
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
        } else if self.match1(TokenType::Type) {
            self.parse_type_alias()
        } else if self.match1(TokenType::While) {
            self.parse_while()
        } else if self.match1(TokenType::Struct) {
            self.parse_struct()
        } else if self.match1(TokenType::Data) {
            self.parse_data()
//        } else if self.match1(TokenType::Class) {
//            self.parse_type_class()
        } else {
            self.parse_statement()
        }
    }

    fn parse_while(&mut self) -> Result<Stmt, LError> {
        let token = self.previous().clone();
        let condition = self.parse_expression()?;
        self.expect(TokenType::LBrace)?;
        let body = self.parse_block()?;
        Ok(WhileStmt { token, condition, body })
    }

    fn parse_type_alias(&mut self) -> Result<Stmt, LError> {
        let name = self.parse_type_name()?;
        self.expect(TokenType::Equal)?;
        let ltype = self.parse_type()?;
        self.expect(TokenType::Semicolon)?;
        Ok(TypeAlias { name, ltype })
    }

    // <varDecl> ::= "var" ID ( = <expr> )? ;
    fn parse_var_decl(&mut self) -> Result<Stmt, LError> {
        // let token = self.expect_discriminant(discriminant(&TokenType::Identifier("")))?;
        let name = self.expect(TokenType::Identifier)?.clone();
        let mut init = None::<Expr>;
        let mut ltype = None::<LType>;
        if self.match1(TokenType::Colon) {
            ltype = Some(self.parse_type()?);
        }
        if self.r#match1(TokenType::Equal) {
            init = Some(self.parse_expression()?);
        }
        self.expect(TokenType::Semicolon)?;
        Ok(VarStmt { name, ltype, init })
    }

    // <letBinding> ::= "let" ID = <expr> ;
    fn parse_let_binding(&mut self) -> Result<Stmt, LError> {
        let token = self.previous().clone();
        let pattern = self.parse_pattern()?;
        let ltype = if self.match1(TokenType::Colon) {
            Some(self.parse_type()?)
        } else { None };
        self.expect(TokenType::Equal)?;
        let init = self.parse_expression()?;
        self.expect(TokenType::Semicolon)?;
        Ok(LetStmt { token, pattern, ltype, init })
    }

    fn parse_fn_decl(&mut self) -> Result<Stmt, LError> {
        let token = self.expect(TokenType::Identifier)?.clone();
        let tparams = Rc::new(if self.match1(TokenType::Less) {
            self.parse_type_parameters()?
        } else { vec![] });
        // Some holes in the previous design of functional definitions
        // Only allow parameterless or single parameter functions to be declared in shorthand way
        if self.match1(TokenType::LParen) {
            let param = self.parse_name_type_pair().ok();
            self.expect(TokenType::RParen)?;
            let ret_type = if self.match1(TokenType::RightArrow) { self.parse_type()? } else { TUnit };
            self.expect(TokenType::LBrace)?;
            let body = self.parse_block()?;
            Ok(FnStmt{ name: Some(token.lexeme.clone()), token, tparams: Rc::clone(&tparams),param, ret_type, body })
        } else if self.match1(TokenType::Equal) {
            self.parse_curried_fn_decl(Some(token.lexeme), tparams)
        } else {
            Err(LError::from_token(format!("Invalid syntax, expected '=' or '(', found {}", self.current()), self.current()))
        }
    }

    // Only name function on first iteration
    fn parse_curried_fn_decl(&mut self, name: Option<String>, tparams: Rc<Vec<Token>>) -> Result<Stmt, LError> {
        // fn f = x: Int => y: Bool => z: Bool : ret { body } // Optional colon indicating type
        let token = self.current().clone();
        let param = self.parse_name_type_pair()?;
        if self.match1(TokenType::RightFatArrow) {
            let right = self.parse_curried_fn_decl(None, Rc::clone(&tparams))?;
            Ok(FnCurried { name, token, param, tparams, ret: Box::new(right) })
        } else if self.match1(TokenType::Colon) {
            let ret_type = self.parse_type()?;
            self.expect(TokenType::LBrace)?;
            let body = self.parse_block()?;
            Ok(FnStmt { name, token, param: Some(param), tparams, ret_type, body })
        } else if self.match1(TokenType::LBrace) {
            let body = self.parse_block()?;
            Ok(FnStmt { name, token, param: Some(param), tparams, ret_type: TUnit, body })
        } else {
            Err(LError::from_token("Invalid curried function syntax".to_string(), &token))
        }
    }

    // fn parse_typed_params(&mut self) -> Result<Vec<Pair<LType>>, LError> {
    //     let mut vec = Vec::<Pair<LType>>::new();
    //     while self.current().ttype != TokenType::RParen {
    //         vec.push(self.parse_name_type_pair()?);
    //         if !self.match1(TokenType::Comma) { break; }
    //     }
    //     self.expect(TokenType::RParen)?;
    //     Ok(vec)
    // }

    fn parse_name_type_pair(&mut self) -> Result<Pair<LType>, LError> {
        let name = self.expect(TokenType::Identifier)?.lexeme.clone();
        self.expect(TokenType::Colon)?;
        let ltype = self.parse_type()?;
        Ok(Pair::new(name, ltype))
    }

    // <type> ::= <typename> | <typename> -> <type>; Right associtive type constructor
    fn parse_type(&mut self) -> Result<LType, LError> {
        let mut ltype = self.parse_primitive_type();
        while self.match1(TokenType::RightArrow) {
            ltype = Ok(LType::TArrow(Box::new(ltype?), Box::new(self.parse_type()?)));
        }
        ltype
    }

    // Primitive means NON-ARROW in this context
    fn parse_primitive_type(&mut self) -> Result<LType, LError> {
        if self.match1(TokenType::LParen) {
            // Ambiguous between parentheses and tuple
            // Try parse as parens first, if it is tuple it will have comma and fail
            // If its singleton tuple then they are essentially equivalent in this language so doesn't matter
            let backtrack = self.i;
            let expr = self.parse_type();
            if expr.is_err() || self.expect(TokenType::RParen).is_err() {
                self.i = backtrack;
                return Ok(TTuple(self.parse_tuple(&Parser::parse_type)?))
            }
            expr
        } else if self.match1(TokenType::LBrace) {
            Ok(TRecord(self.parse_record(&Parser::parse_type)?))
        } else if self.match1(TokenType::LSquare) {
            Ok(TList(Box::new(self.parse_type()?)))
        } else if self.match1(TokenType::TypeVar){
            Ok(TVar(self.previous().clone()))
        } else {
            Ok(TName(self.parse_type_name()?))
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

    fn parse_struct(&mut self) -> Result<Stmt, LError> {
        let name = self.parse_type_name()?;
        self.expect(TokenType::LBrace)?;
        let fields = self.parse_record(&Parser::parse_type)?;
        Ok(StructDecl { name, fields })
    }

    // data List<T> = Nil | Cons T List<T>
    fn parse_data(&mut self) -> Result<Stmt, LError> {
        let name = self.parse_type_name()?;
        self.expect(TokenType::Equal)?;
        let mut variants = HashMap::new();
        while {
            let vname = self.expect(TokenType::Typename)?.lexeme.clone();
//            let vtype = self.parse_type()?;
//            variants.insert(vname, TArrow(Box::new(vtype), Box::new(TName(name.clone()))));
            let vtype = self.parse_variant_type(LType::TName(name.clone()))?;
            variants.insert(vname, vtype);
            self.match1(TokenType::Pipe)
        } {}
        self.expect(TokenType::Semicolon)?;
        Ok(DataDecl { name, variants })
    }

    fn parse_type_name(&mut self) -> Result<TypeName, LError> {
        // No higher kinds, can't nested type parameters <'m<'a>>
//        let name = if self.r#match(&[TokenType::Typename, TokenType::TypeVar]) {
//            self.previous().clone()
//        } else { return Err(LError::from_token(format!("Expected Typename or Type Variable, found {}", self.current()), self.current())) };
        let name = self.expect(TokenType::Typename)?.clone();
        let tparams = if self.match1(TokenType::Less) {
            self.parse_type_parameters()?
        } else { vec![] };
        Ok(TypeName::new(name, tparams))
    }

    // Allow empty <> as syntactically correct
    fn parse_type_parameters(&mut self) -> Result<Vec<Token>, LError> {
        let mut vec = vec![];
        if self.current().ttype != TokenType::Greater {
            while {
                vec.push(self.expect(TokenType::TypeVar)?.clone());
                self.match1(TokenType::Comma)
            } {}
        }
        self.expect(TokenType::Greater)?;
        Ok(vec)
    }

    fn parse_variant_type(&mut self, mut t: LType) -> Result<LType, LError> {
        let mut v = vec![];
        while self.current().ttype != TokenType::Pipe && self.current().ttype != TokenType::Semicolon && self.current().ttype != TokenType::EOF {
            v.push(self.parse_primitive_type()?);
        }
        for ltype in v.into_iter().rev() {
            t = TArrow(Box::new(ltype), Box::new(t))
        }
//        println!("type: {}", t);
        Ok(t)
    }

}


// Parsing expr
impl Parser {

    pub fn parse_expression(&mut self) -> Result<Expr, LError> {
        self.parse_assignment()
    }

    // <assn> ::= <lvalue> = <assn> | <eq>
    fn parse_assignment(&mut self) -> Result<Expr, LError> {
        let expr = self.parse_match()?;
        if self.match1(TokenType::Equal) {
            let token = self.previous().clone();
            let right = self.parse_assignment()?;
            return if let EVariable { name } = &expr {
                Ok(EAssignment { lvalue: name.clone(), expr: Box::new(right) })
            } else if let EGet { name, expr } = &expr {
                Ok(ESet { name: name.clone(), expr: expr.clone(), value: Box::new(right) })
            } else {
                Err(LError::from_token(format!("Cannot assign to {}, not an l-value", expr), &token))
            }
        } else if self.r#match(&[TokenType::PlusEqual, TokenType::MinusEqual, TokenType::StarEqual, TokenType::SlashEqual]) {
            let mut operator = self.previous().clone();
            operator.ttype = match operator.ttype {
                TokenType::PlusEqual => Plus,
                TokenType::MinusEqual => Minus,
                TokenType::StarEqual => Star,
                TokenType::SlashEqual => Slash,
                _ => unreachable!()
            };
            let right = self.parse_assignment()?;
            return if let EVariable { name } = &expr {
                Ok(EAssignment { lvalue: name.clone(), expr: Box::new(EBinary { operator, left: Box::new(expr), right: Box::new(right) }) })
            } else if let EGet { name, expr } = &expr {
                Ok(ESet { name: name.clone(), expr: expr.clone(), value: Box::new(right) })
            } else {
                Err(LError::from_token(format!("Cannot assign to {}, not an l-value", expr), &operator))
            }
        }
        Ok(expr)
    }

    fn parse_match(&mut self) -> Result<Expr, LError> {
        if self.match1(TokenType::Match) {
            let token = self.previous().clone();
            let expr = self.parse_expression()?;
            self.expect(TokenType::LBrace)?;
            let mut branches = vec![];
            while self.match1(TokenType::Pipe) {
                let pattern = self.parse_pattern()?;
                self.expect(TokenType::RightArrow)?;
                let body = self.parse_block_expr()?;
                branches.push((pattern, body));
            }
            if branches.is_empty() {
                return Err(LError::from_token("Match expression must have at least one branch".to_string(), &token))
            }
            self.expect(TokenType::RBrace)?;
            Ok(EMatch { token, expr: Box::new(expr), branches })
        } else {
            self.parse_conditional()
        }
    }

    fn parse_pattern(&mut self) -> Result<LPattern, LError> {
//        let mut pattern = self.parse_single_pattern()?;
//        if self.match1(TokenType::Pipe) {
//            pattern = POr(Box::new(pattern), self.parse_pattern().map(Box::new)?)
//        }
//        Ok(pattern)
        self.parse_single_pattern()
    }

    fn parse_single_pattern(&mut self) -> Result<LPattern, LError> {
        if self.match1(TokenType::Underscore) {
            Ok(PWildcard)
        } else if self.match1(TokenType::Identifier) {
            Ok(PIdentifier(self.previous().clone()))
        } else if self.match1(TokenType::Typename) {
            let token = self.previous().clone();
            let mut ps = vec![];
            while let Ok(p) = self.parse_pattern() { ps.push(p); }
            let acc = ps.into_iter().rev().fold1( |acc, x| PConstructor(Box::new(x), Box::new(acc)));
            Ok(PVariant(token, acc.map(Box::new)))
        } else if self.match1(TokenType::LParen) {
            let backtrack = self.i;
            let pattern = self.parse_pattern();
            if pattern.is_err() || self.expect(TokenType::RParen).is_err() {
                self.i = backtrack;
                return Ok(PTuple(self.parse_tuple(&Parser::parse_pattern)?))
            }
            pattern
        } else if self.r#match(&[TokenType::Number, TokenType::String]) {
            Ok(PLiteral(self.previous().clone()))
        } else {
            Err(LError::from_token("Bad pattern".to_string(), self.current()))
        }
    }

//    if` expressions without `else` evaluate to `()`
    fn parse_conditional(&mut self) -> Result<Expr, LError> {
        if self.match1(TokenType::If) {
            if self.match1(TokenType::Let) {
                self.parse_if_let()
            } else {
                let token = self.previous().clone();
                let condition = self.parse_expression()?;
                self.expect(TokenType::LBrace)?;
                let left = EBlock(self.parse_block()?);
                // Might be easier to represent no else as empty block instead of an Option type
                let right = self.parse_optional_else()?;
                Ok(EIf {
                    token,
                    condition: Box::new(condition),
                    left: Box::new(left),
                    right: Box::new(right)
                })
            }
        } else {
            self.parse_lambda()
        }
    }

    fn parse_if_let(&mut self) -> Result<Expr, LError> {
        let token = self.previous().clone();
        let pattern = self.parse_pattern()?;
        // Only one pattern allowed for now
        self.expect(TokenType::Equal)?;
        let scrutinee = self.parse_expression()?;
        self.expect(TokenType::LBrace)?;
        let left = self.parse_block()?;
        let right = self.parse_optional_else()?;
        Ok(EIfLet {
            token,
            pattern,
            scrutinee: Box::new(scrutinee),
            left,
            right: Box::new(right)
        })
    }

    fn parse_optional_else(&mut self) -> Result<Expr, LError> {
        if self.match1(TokenType::Else) {
            if self.match1(TokenType::LBrace) {
                self.parse_block().map(EBlock)
            } else if self.match1(TokenType::If) {
                self.i -= 1;
                self.parse_conditional()
            } else {
                return Err(LError::from_token("Expected block or if after else".to_string(), self.current()));
            }
        } else {
            Ok(EBlock(vec![]))
        }
    }

    fn parse_lambda(&mut self) -> Result<Expr, LError> {
        if self.match1(TokenType::Lambda) {
            let token = self.previous().clone();
            let param = self.parse_pattern()?;
            let ltype = if self.match1(TokenType::Colon) { Some(self.parse_type()?) } else { None };
            self.expect(TokenType::RightFatArrow)?;
            let body = self.parse_expression()?;
            Ok(ELambda { token, ltype, param, body: Box::new(body) })
        } else {
            self.parse_block_expr()
        }
    }

    fn parse_block_expr(&mut self) -> Result<Expr, LError> {
        if self.match1(TokenType::LBrace) {
            Ok(EBlock(self.parse_block()?))
        } else {
            self.logical_or()
        }
    }

    // <or> ::= <eq> { || <eq }
    fn logical_or(&mut self) -> Result<Expr, LError> {
        let mut expr = self.logical_and();
        if self.match1(TokenType::DoublePipe) {
            let operator = self.previous().clone();
            let right = self.logical_and()?;
            expr = Ok(ELogic { operator, left: Box::new(expr?), right: Box::new(right) })
        }
        expr
    }

    fn logical_and(&mut self) -> Result<Expr, LError> {
        let mut expr = self.parse_equality();
        if self.match1(TokenType::DoubleAmpersand) {
            let operator = self.previous().clone();
            let right = self.parse_equality()?;
            expr = Ok(ELogic { operator, left: Box::new(expr?), right: Box::new(right) })
        }
        expr
    }

    // <eq> ::= <comp> { ==|!= <comp> }
    fn parse_equality(&mut self) -> Result<Expr, LError> {
        let mut expr = self.parse_comparison();
        while self.r#match(&[TokenType::DoubleEqual, TokenType::BangEqual]) {
            let operator = self.previous().clone();
            let right = self.parse_comparison()?;
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
        let mut expr = self.parse_unary();
        while self.r#match(&[TokenType::Star, TokenType::Slash, TokenType::Modulo]) {
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
        if self.r#match(&[TokenType::Minus, TokenType::Bang]) {
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
        let mut expr = self.parse_app_dot();
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
//    fn parse_curried_application(&mut self) -> Result<Expr, LError> {
//        let mut expr = self.parse_dot();
//        while let Ok(arg) = self.parse_dot() {
//            expr = Ok(EApplication { token: self.current().clone(), callee: Box::new(expr?), arg: Box::new(arg) })
//        }
//        expr
//    }

//    fn parse_dot(&mut self) -> Result<Expr, LError> {
//        let mut expr = self.parse_primary();
//        while self.match1(TokenType::Dot) {
//            let name = self.expect(TokenType::Identifier)?.clone();
//            expr = Ok(EGet { name, expr: Box::new(expr?) });
//        }
//        expr
//    }

    // Applicative space and get/set dot have equal precedence
    fn parse_app_dot(&mut self) -> Result<Expr, LError> {
        let mut expr = self.parse_primary();
        loop {
            if let Ok(arg) = self.parse_primary() {
                expr = Ok(EApplication { token: self.current().clone(), callee: Box::new(expr?), arg: Box::new(arg) })
            } else if self.match1(TokenType::Dot) {
                let name = self.expect(TokenType::Identifier)?.clone();
                expr = Ok(EGet { name, expr: Box::new(expr?) });
            } else {
                break;
            }
        }
        expr
    }


    fn parse_primary(&mut self) -> Result<Expr, LError> {
        // Parsing as parens will fail if intended to be tuple, and unary tuples probably aren't useful
        if self.r#match1(TokenType::LParen) {
            let backtrack = self.i;
            let expr = self.parse_expression();
            if expr.is_err() || self.expect(TokenType::RParen).is_err() {
                self.i = backtrack;
                let token = self.current().clone();
                return Ok(Expr::ETuple(token, self.parse_tuple(&Parser::parse_expression)?))
            }
            expr
//            Ok(Expr::Grouping(Box::new(expr?))) // dont think this is explicitly required
        } else if self.match1(TokenType::Identifier) {
            Ok(EVariable { name: self.previous().clone() })
        } else if self.match1(TokenType::Typename) {
            Ok(EDataConstructor { name: self.previous().clone() })
        } else if self.r#match(&[TokenType::False, TokenType::True, TokenType::Number, TokenType::String]) {
            Ok(ELiteral(self.previous().clone()))
        } else if self.match1(TokenType::Record) {
            self.expect(TokenType::LBrace)?;
            let token = self.previous().clone();
            Ok(ERecord(token, self.parse_record(&Parser::parse_expression)?))
//            self.parse_record(&Parser::parse_expression).map(ERecord)?
        } else if self.match1(TokenType::LSquare) {
            let token = self.previous().clone();
            let partial = |x| EList(token, x);
            self.parse_list().map(partial)
        } else {
            let t = self.current();
            Err(LError::from_token(format!("Unexpected token (primary): {}", t), t))
        }
    }


    fn parse_tuple<F, T>(&mut self, f: &F) -> Result<Vec<T>, LError>
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

    fn parse_list(&mut self) -> Result<VecDeque<Expr>, LError> {
        let mut v = VecDeque::new();
        if self.current().ttype != TokenType::RSquare {
            while { // Cheap do-while loop
                v.push_back(self.parse_expression()?);
                self.match1(TokenType::Comma)
            } {}
        }
        self.expect(TokenType::RSquare)?;
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

    // Expect opening LBrace to be consumed
    fn parse_record<F, T>(&mut self, f: &F) -> Result<HashMap<String, T>, LError>
        where F: Fn(&mut Self) -> Result<T, LError>, T : Display {
        let mut v = HashMap::new();
        while self.current().ttype != TokenType::RBrace {
            let current_line = self.current().line;
            let current_col = self.current().col;
            let pair = self.parse_record_entry(f)?;
            if v.insert(pair.name, pair.value).is_some() {
                return Err(LError::new("All fields in record must be unique".to_string(), current_line, current_col))
            };
            self.match1(TokenType::Comma); // Allows optional final comma
        }
        self.expect(TokenType::RBrace)?;
        Ok(v)
    }

    fn parse_record_entry<F, T>(&mut self, f: &F) -> Result<Pair<T>, LError>
        where F: Fn(&mut Self) -> Result<T, LError>, T : Display {
        let name = self.expect(TokenType::Identifier)?.lexeme.clone();
        self.expect(TokenType::Colon)?;
        let value = f(self)?;
        Ok(Pair::new(name, value))
    }


}