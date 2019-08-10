use crate::parsing::{Expr, Stmt, Mode};
use crate::lexing::{TokenType, Token};
use crate::types::{LType, LTypeError};
use crate::types::l_types::LType::{TBool, TNum, TArrow, TTuple, TUnit, TRecord, TData, TVariant, TTop, TNothing, TList, TString};
use crate::types::l_types::Pair;
use crate::parsing::expr::Expr::{EBinary, ELiteral, EVariable, ETuple, EApplication, EAssignment, EBlock, EIf, ERecord, ELogic, EGet, ESet, EDataConstructor, EMatch, EIfLet, EList, EUnary};
use crate::interpreting::Env;
use crate::types::LTypeError::{TypeError, NonFunction, InvalidDeclaration, TypeMismatch, NonExistentField, NotGettable, NonExistentType, NonExistentDataConstructor, BadPattern};
use crate::parsing::stmt::Stmt::{LStmt, FnStmt, VarStmt, LetStmt, FnCurried, ExprStmt, ReturnStmt, PrintStmt, TypeAlias, WhileStmt, StructDecl, DataDecl};
use crate::lexing::token::TokenType::{Greater, GreaterEqual, Caret, Slash, Plus, Star, LessEqual, Less, Minus, BangEqual, DoubleEqual};
use crate::interpreting::LPattern::*;
use std::mem::{discriminant};
use std::collections::{HashMap, VecDeque};
use std::rc::Rc;
use std::cell::RefCell;
use crate::interpreting::pattern_matching::LPattern;
use itertools::Itertools;
use std::ops::Deref;

pub struct TypeChecker {
    env: Rc<RefCell<Env<LType>>>, // Variable -> Type
    types: Env<LType>, // Typename -> Type
    curr_fn_ret_type: Option<LType>, // Type check return statements
    // Some helper for typechecking recursive curried definitions
    curr_ftype: Option<LType>,
    curr_fname: Option<String>,
    mode: Mode
}

impl TypeChecker {

    pub fn new(mode: Mode) -> TypeChecker {
        let mut types = Env::new(None);
        types.define("Bool".to_string(), TBool);
        types.define("Int".to_string(), TNum);
        types.define("Number".to_string(), TNum);
        types.define("Unit".to_string(), TUnit);
        TypeChecker {
            env: Rc::new(RefCell::new(Env::new(None))),
            curr_fn_ret_type: None,
            curr_ftype: None,
            curr_fname: None,
            types,
            mode
        }
    }
}

impl TypeChecker {


    pub fn type_check(&mut self, statements: &Vec<Stmt>) -> Result<(), Vec<LTypeError>> {
        let mut errors = Vec::<LTypeError>::new();
        for statement in statements {
//        if let Err(err) = type_check_statement(statement) {
//            errors.push(err);
//        }
            match self.type_of_statement(statement) {
                Err(err) => errors.push(err),
                Ok(t) => if let Mode::Interactive = self.mode {
                    println!("Type: {}", t)
                }
            }
        }
        if errors.is_empty() { Ok(()) } else { Err(errors) }
    }

    pub fn type_of_expr(&mut self, expr: &Expr) -> Result<LType, LTypeError> {
        match expr {
            ELiteral(token) => TypeChecker::type_of_literal(token),
            EBinary { operator, left, right } => self.type_of_binary(operator, left, right),
            EUnary { operator, operand } => self.type_of_unary(operator, operand),
            EVariable { name } => self.type_of_variable(name),
            EApplication { token, callee, arg} => self.type_of_curry_application(token, callee, arg),
            EAssignment { lvalue, expr } => self.type_of_assignment(lvalue, expr),
            EIf { condition, left, right, token } => self.type_of_if(token, condition, left, right),
            EBlock(xs) => self.type_of_block_e(xs),
            ETuple(_, xs) => self.type_of_tuple(xs),
            EList(token, xs) => self.type_of_list(token, xs),
            ERecord(_, xs) => self.type_of_record(xs),
            ELogic { operator, left, right} => self.type_of_logical(operator, left, right),
            EGet { name, expr } => self.type_of_get(name, expr),
            ESet { name, expr, value } => self.type_of_set(name, expr, value),
            EDataConstructor { name } => self.env.borrow().resolve(name).map_err(|e| NonExistentDataConstructor(name.clone())),
            EMatch { token, expr, branches} => self.type_of_match(token, expr, branches),
            EIfLet { token, pattern, scrutinee, left, right } =>
                self.type_of_if_let(token, pattern, scrutinee, left, right),
            x => unimplemented!("Unsupported: {}", x)
        }
    }

    fn type_of_statement(&mut self, stmt: &Stmt) -> Result<LType, LTypeError> {
        match stmt {
            LStmt(expr) | ExprStmt(expr) | PrintStmt(expr) => self.type_of_expr(expr),
            FnStmt { name, token, params, ret_type, body } => self.type_of_fn(name, token, params, ret_type, body),
            FnCurried { name, param, ret, .. } => self.type_of_curried_fn(name, param, ret),
            VarStmt { name, ltype, init}  => self.type_of_var_decl(name, ltype, init.as_ref()),
            LetStmt { name, ltype, init} => self.type_of_var_decl(name, ltype, Some(init)),
            ReturnStmt { token, value } => self.type_of_return(token, value),
            TypeAlias { name, ltype } => self.define_type_alias(name, ltype),
            WhileStmt { token, condition, body} => self.type_of_while(token, condition, body),
            StructDecl { name, fields} => self.type_of_struct(name, fields),
            DataDecl { name, variants} => self.type_of_data_decl(name, variants),
            _ => panic!("Unimplented in type_check_stmt")
        }
    }

    fn type_of_match(&mut self, token: &Token, expr: &Expr, branches: &Vec<(LPattern, Expr)>) -> Result<LType, LTypeError> {
        let first_type = self.type_of_expr(&branches[0].1)?;
        for (_, t) in branches {
            if self.type_of_expr(t)? != first_type {
                return Err(TypeMismatch(first_type, self.type_of_expr(t)?, token.clone()));
            }
        }
//        if !branches.iter().all(|(_, t)| self.type_of_expr(t) == first_type)
        Ok(first_type)
    }

    fn type_of_data_decl(&mut self, name: &Token, variants: &HashMap<String, LType>) -> Result<LType, LTypeError> {
        self.types.define(name.lexeme.clone(), TData(name.lexeme.clone()));
        for (k, v) in variants {
            // Define data constructors as functions
            self.env.borrow_mut().define(k.clone(), v.clone().map_string_to_type(&self.types)?)
        }
        Ok(TVariant(variants.clone()))
    }

    fn type_of_get(&mut self, name: &Token, expr: &Expr) -> Result<LType, LTypeError> {
        let t_expr = self.type_of_expr(expr)?;
        let ltype = match &t_expr {
            TRecord(xs) => {
                match xs.get(&name.lexeme) {
                    Some(x) => Ok(x.clone()),
                    None => Err(NonExistentField(name.clone(), t_expr))
                }
            },
            _ => Err(NotGettable(name.clone(), t_expr))
        }?;
        Ok(ltype)
    }

    fn type_of_struct(&mut self, name: &Token, fields: &HashMap<String, LType>) -> Result<LType, LTypeError> {
//        self.env.borrow_mut().define(name.lexeme.clone(), ),
        Ok(TUnit)
    }

    fn type_of_set(&mut self, name: &Token, expr: &Expr, value: &Expr) -> Result<LType, LTypeError> {
        let t_expr = self.type_of_get(name, expr)?;
        let t_value = self.type_of_expr(value)?;
        if t_expr != t_value {
            return Err(TypeError(t_expr, t_value, name.clone()))
        }
        Ok(t_expr)
    }

    fn type_of_logical(&mut self, token: &Token, left: &Expr, right: &Expr) -> Result<LType, LTypeError> {
        let ltype = self.type_of_expr(left)?;
        let rtype = self.type_of_expr(right)?;
        if ltype != TBool {
            Err(TypeError(TBool, ltype, token.clone()))
        } else if rtype != TBool {
            Err(TypeError(TBool, rtype, token.clone()))
        } else {
            Ok(TBool)
        }
    }

    fn type_of_list(&mut self, token: &Token, xs: &VecDeque<Expr>) -> Result<LType, LTypeError> {
        let tfirst = self.type_of_expr(&xs[0])?;
        for x in xs {
            let t = self.type_of_expr(x)?;
            if t != tfirst {
                return Err(TypeMismatch(tfirst, t, token.clone()))
            }
        }
        Ok(TList(Box::new(tfirst)))

    }

    fn type_of_record(&mut self, xs: &HashMap<String, Expr>) -> Result<LType, LTypeError> {
        let mut map = HashMap::new();
        for (k, v) in xs {
            map.insert(k.clone(), self.type_of_expr(v)?);
        }
        Ok(TRecord(map))
    }

    fn type_of_if(&mut self, token: &Token, condition: &Expr, left: &Expr, right: &Expr) -> Result<LType, LTypeError> {
        let tcond = self.type_of_expr(condition)?;
        let tleft = self.type_of_expr(left)?;
        let tright = self.type_of_expr(right)?;
        if tcond != TBool {
            Err(TypeError(TBool, tcond, token.clone()))
        } else if tleft != tright {
            Err(TypeMismatch(tleft, tright, token.clone()))
        } else {
            Ok(tleft)
        }
    }

    fn type_of_if_let(&mut self, token: &Token, pattern: &LPattern, scrutinee: &Expr, left: &Vec<Stmt>, right: &Expr) -> Result<LType, LTypeError> {
//        let ptype = self.type_of_pattern(pattern)?;
//        println!("ptype: {}", ptype);
        let texpr = self.type_of_expr(scrutinee)?;
//        if ptype != texpr { return Err(TypeError(texpr, ptype, token.clone())) }
        let mut env = Env::new(Some(Rc::clone(&self.env)));
        let bindings = self.type_of_pattern_bindings(token, pattern, &texpr)?;
//        println!("TBindings: {:?}", bindings);
        for (k, v) in bindings { env.define(k, v); }
        let tleft = self.type_of_block(left, Rc::new(RefCell::new(env)))?;
        let tright = self.type_of_expr(right)?;
        if tleft != tright { return Err(TypeMismatch(tleft, tright, token.clone())) }
        Ok(tright)
    }

//    if let Some(ref p) = **p {
//    let constructor_type = if let TArrow(l, _) = self.env.borrow().resolve(name).unwrap() { *l }
//    else { ltype.clone() };
//    self.type_of_pattern_bindings(name, p, &constructor_type)
//    } else { Ok(vec![]) },

    // Pattern and the scrutinee it is matching against
    fn type_of_pattern_bindings(&self, token: &Token, pattern: &LPattern, ltype: &LType) -> Result<Vec<(String, LType)>, LTypeError> {
        match pattern {
            PVariant(name, p) => if let Some(p) = p {
                let constructor_type = match self.env.borrow().resolve(name) {
                    Ok(x) => x,
                    Err(_) => return Err(NonExistentDataConstructor(name.clone()))
                };
//                println!("ctype: {}", ltype);
//                let constructor_type =
//                    if let TArrow(l, _) = constructor { *l }
//                    else { ltype.clone() };
//                println!("ctype': {}", constructor_type);
                let adt_type = constructor_type.rightmost_type();
                if  adt_type != ltype { return Err(TypeError(adt_type.clone(), ltype.clone(), token.clone())) }
                // Remove the rightmost as the rightmost type is the type of the adt itself, and we don't want to match that
                // Remove rightmost will always work without panic because if it is a simple type then there is no pattern and the if let Some will fail and skip this block
                self.type_of_pattern_bindings(name, p, &constructor_type.remove_rightmost())
            } else { Ok(vec![]) },
            PConstructor(pl, pr) => {
                if let TArrow(tl, tr) = ltype {
                    Ok(self.type_of_pattern_bindings(token, pl, tl)?
                        .into_iter()
                        .chain(self.type_of_pattern_bindings(token, pr, tr)?)
                        .collect_vec())
                } else {
                    Err(BadPattern(pattern.clone(), ltype.clone(), token.clone()))
                }
            },
            PRecord => Ok(vec![]),
            PTuple(xs) => {
                if let TTuple(ts) = ltype {
                    Ok(xs.iter()
                        .zip(ts)
                        .flat_map(|(p, x)| self.type_of_pattern_bindings(token, p, x))
                        .flatten() // flatmap doesn't leave it very flat for some reason
                        .collect_vec()
                    )
                } else {
                    Err(BadPattern(pattern.clone(), ltype.clone(), token.clone()))
                }
            },
            PLiteral(x) => Ok(vec![]),
            PIdentifier(x) => Ok(vec![(x.lexeme.clone(), ltype.clone())]),
            PWildcard => Ok(vec![])
        }
    }

//    fn type_of_pattern(&self, pattern: &LPattern) -> Result<LType, LTypeError> {
//        match pattern {
//            PIdentifier(x) => Ok(TTop),
//            PWildcard => Ok(TTop),
//            PRecord => Ok(TNothing),
//            PTuple(xs) =>
//                Ok(TTuple(xs.iter().map(|x| self.type_of_pattern(x)).collect::<Result<Vec<_>, LTypeError>>()?)),
//            PLiteral(x) => TypeChecker::type_of_literal(x),
//            PVariant(x) => match self.env.borrow().resolve(x) {
//                Err(_) => Err(NonExistentDataConstructor(x.clone())),
//                Ok(t) => Ok(self.rightmost_type(t))
//            },
//        }
//    }

    fn type_of_variable(&mut self, name: &Token) -> Result<LType, LTypeError> {
        match self.env.borrow().resolve(name) {
            Ok(t) => Ok(t),
            Err(_) => Err(InvalidDeclaration) // If variable is not found here it is due to bad types in declaration
        }
    }

    // Default env created
    fn type_of_block_e(&mut self, block: &Vec<Stmt>) -> Result<LType, LTypeError> {
        self.type_of_block(block, Rc::new(RefCell::new(Env::new(Some(Rc::clone(&self.env))))))
    }

    fn type_of_block(&mut self, block: &Vec<Stmt>, env: Rc<RefCell<Env<LType>>>) -> Result<LType, LTypeError> {
        let enclosing = Rc::clone(&self.env);
        self.env = env;
        let mut block = block.clone();
        let last = block.pop();
        for stmt in &block {
            self.type_of_statement(stmt)?;
        }
        let ret = match last {
            Some(stmt) => match stmt {
                LStmt(ref expr) => self.type_of_expr(expr),
                ref x => { self.type_of_statement(x)?; Ok(TUnit) }
            },
            None => Ok(TUnit)
        };
        self.env = enclosing;
        ret
    }

    fn type_of_curry_application(&mut self, token: &Token, callee: &Expr, arg: &Expr) -> Result<LType, LTypeError> {
        let tcallee = self.type_of_expr(callee)?;
        let targ = self.type_of_expr(arg)?;
        if let TArrow(tparam, tret) = tcallee {
            if targ != *tparam { Err(TypeError(*tparam, targ, token.clone())) }
            else { Ok(*tret) }
        } else {
            Err(NonFunction(tcallee, token.clone()))
        }
    }

    fn type_of_assignment(&mut self, lvalue: &Token, expr: &Expr) -> Result<LType, LTypeError> {
        let tlvalue = self.env.borrow().resolve(lvalue).unwrap();
        let texpr = self.type_of_expr(expr)?;
        if tlvalue == texpr {
            if let TRecord(_) = texpr {
                // Make it easier to index into record if names match
                self.env.borrow_mut().update(&lvalue.lexeme, texpr)
            }
            Ok(tlvalue)
        }
        else { Err(TypeError(tlvalue, texpr, lvalue.clone())) }
    }

    fn type_of_unary(&self, operator: &Token, operand: &Expr) -> Result<LType, LTypeError> {
        Ok(TNum)
    }

    fn type_of_binary(&mut self, operator: &Token, left: &Expr, right: &Expr) -> Result<LType, LTypeError> {
        // Only numbers are comparable currently
        let num_ops = vec![Star, Plus, Minus, Slash, Caret];
        let cmp_ops = vec![Greater, GreaterEqual, Less, LessEqual];
        let eq_ops = vec![DoubleEqual, BangEqual];
        let tleft = self.type_of_expr(left)?;
        let tright = self.type_of_expr(right)?;
        if num_ops.contains(&operator.ttype) {
            if tleft != TNum { Err(TypeError(TNum, tleft, operator.clone())) }
            else if tright != TNum { Err(TypeError(TNum, tright, operator.clone())) }
            else { Ok(TNum) }
        } else if eq_ops.contains(&operator.ttype) {
            if tleft != tright { return Err(TypeMismatch(tleft, tright, operator.clone())) }
            else { Ok(TBool) }
        } else if cmp_ops.contains(&operator.ttype) {
            if tleft != TNum { Err(TypeError(TNum, tleft, operator.clone())) }
            else if tright != TNum { Err(TypeError(TNum, tright, operator.clone())) }
            else { Ok(TBool) }
        } else {
            panic!("Unknown binary op type")
        }
//        if tleft == tright { Ok(tleft) }
//        else { Err(LTypeError::TypeMismatch(tleft, tright, operator.clone())) }
    }

//    fn type_of_application(&self, token: &Token, callee: &Expr, args: &Vec<Expr>) -> Result<LType, LTypeError> {
//        let tcallee = self.type_of_expr(callee)?;
//        let mut vargs = args.iter().map(|x| self.type_of_expr(x)).collect::<Result<Vec<LType>, LTypeError>>()?;
//        if let TArrow(tparams, tret) = tcallee.clone() {
//            if let TArrow(_, _) = *tret {
//                // Curried function application
//                self.type_of_curried_application(token, &tcallee, &mut vargs)
//            } else {
//                // Non-curried function application
//                let targs = TTuple(vargs);
//                if *tparams != targs { Err(TypeError(*tparams, targs, token.clone())) }
//                else { Ok(*tret) }
//            }
//        } else {
//            Err(NonFunction(tcallee, token.clone()))
//        }
//    }
//
//    fn type_of_curried_application(&self, token: &Token, ltype: &LType, args: &mut Vec<LType>) -> Result<LType, LTypeError> {
//        // Assume there are not too many arguments due to static checker
//        if let TArrow(t, ts) = ltype {
//            if args.len() == 0 { // Applied all the given arguments
//                Ok(ltype.clone())
//            } else if **t != args[0] {
//                Err(TypeError(*t.clone(), args[0].clone(), token.clone()))
//            } else {
//                args.remove(0);
//                self.type_of_curried_application(token, ts, args)
//            }
//        } else {
//            Ok(ltype.clone())
//        };
//    }

    fn type_of_tuple(&mut self, xs: &Vec<Expr>) -> Result<LType, LTypeError> {
        let v = xs.iter().map(|x| self.type_of_expr(x)).collect::<Result<Vec<LType>, LTypeError>>()?;
        Ok(TTuple(v))
    }

    fn type_of_literal(x: &Token) -> Result<LType, LTypeError> {
        match x.ttype {
            TokenType::True | TokenType::False => Ok(TBool),
            TokenType::Number => Ok(TNum),
            TokenType::String => Ok(TString),
            _ => unreachable!()
        }
    }

    fn type_of_while(&mut self, token: &Token, condition: &Expr, body: &Vec<Stmt>) -> Result<LType, LTypeError> {
        let tcond = self.type_of_expr(condition)?;
        if tcond != TBool {
            return Err(TypeError(TBool, tcond, token.clone()))
        }
        self.type_of_block_e(body)?;
        Ok(TUnit)
    }

    fn define_type_alias(&mut self, name: &Token, ltype: &LType) -> Result<LType, LTypeError> {
        let ltype = ltype.clone().map_string_to_type(&self.types)?;
        self.types.define(name.lexeme.clone(), ltype.clone().map_string_to_type(&self.types)?);
        Ok(ltype.clone())
    }

    fn type_of_return(&mut self, token: &Token, value: &Option<Expr>) -> Result<LType, LTypeError> {
        let tret = match value {
            Some(expr) => self.type_of_expr(expr),
            None => Ok(TUnit)
        }?;

        match &self.curr_fn_ret_type {
            Some(t) => if &tret != t {
                Err(TypeError(t.clone(), tret, token.clone()))
            } else { Ok(tret) },
            None => { Ok(TUnit)}
        }
    }

    fn type_of_var_decl(&mut self, token: &Token, ltype: &Option<LType>, init: Option<&Expr>) -> Result<LType, LTypeError> {
        match init {
            Some(expr) => {
                let t_init = self.type_of_expr(expr)?;
                if let Some(ltype) = ltype {
                    if ltype.clone().map_string_to_type(&self.types)? != t_init { return Err(TypeError(ltype.clone(), t_init, token.clone())) }
                }
                self.env.borrow_mut().define(token.lexeme.clone(), t_init.clone());
                Ok(t_init)
            },
            None => match ltype {
                Some(ltype) => {
                    let ltype = ltype.clone().map_string_to_type(&self.types)?;
                    self.env.borrow_mut().define(token.lexeme.clone(), ltype.clone());
                    Ok(ltype.clone())
                },
                None => Err(LTypeError::RequireTypeAnnotation(token.clone()))
            }
        }
    }

    fn type_of_fn(&mut self, name: &Option<String>, token: &Token, params: &Vec<Pair<LType>>, ret: &LType, body: &Vec<Stmt>) -> Result<LType, LTypeError> {
        let ret = ret.clone().map_string_to_type(&self.types)?;
        let prev_ret_type = self.curr_fn_ret_type.clone();
        self.curr_fn_ret_type = Some(ret.clone());

        let ptypes = params.iter()
            .map(|x| x.value.clone().map_string_to_type(&self.types))
            .collect::<Result<Vec<LType>, LTypeError>>()?;
        let ftype = TArrow(Box::new(TTuple(ptypes.clone())), Box::new(ret.clone()));

        let enclosing = Rc::clone(&self.env);
        let env = Env::new(Some(Rc::clone(&self.env)));

        for (i, param) in params.iter().enumerate() {
            self.env.borrow_mut().define(param.name.clone(), ptypes[i].clone());
        }

        if let Some(fname) = &self.curr_fname {
            // Takes the accumulated type and adds the parameter here and the return type to form the full type for a curried function
            self.curr_ftype = Some(TArrow(
                Box::new(self.curr_ftype.clone().unwrap()), Box::new(
                    TArrow(Box::new(ptypes[0].clone()), Box::new(ret.clone()))
                )
            ));
            self.env.borrow_mut().define(fname.clone(), self.curr_ftype.clone().unwrap())
        }

        // Allows typechecking of recursive types. Assume it has type stated in fn definition.
        if let Some(name) = name {
            self.env.borrow_mut().define(name.clone(), ftype.clone())
        }

        let dummy_ret = ReturnStmt { token: Token::dummy(), value: Some(ELiteral(Token::dummy())) };
        let has_explicit_ret = body.iter().any(|x| self.match_discriminant(x, &dummy_ret));
        let block_type = self.type_of_block_e(body)?;
        if block_type != ret && !has_explicit_ret {
            return Err(TypeError(ret.clone(), block_type, token.clone()));
        }

        self.env = enclosing;

        self.curr_fn_ret_type = prev_ret_type;
        Ok(ftype)
    }

    fn type_of_curried_fn(&mut self, name: &Option<String>, ntpair: &Pair<LType>, ret: &Stmt) -> Result<LType, LTypeError> {
        let enclosing = self.env.clone();
        self.env = Rc::new(RefCell::new(Env::new(Some(self.env.clone()))));

        let ptype = ntpair.value.clone().map_string_to_type(&self.types)?;

        if let Some(name) = name {
            self.curr_fname = Some(name.clone())
        }

        // Accumulating the type in a self variable to allow for recursive definitions
        match &self.curr_ftype {
            Some(t) => self.curr_ftype = Some(TArrow(Box::new(t.clone()), Box::new(ptype.clone()))),
            None => self.curr_ftype = Some(ptype.clone())
        }

        self.env.borrow_mut().define(ntpair.name.clone(), ptype.clone());
        self.type_of_statement(ret)?;
//        let ftype = TArrow(Box::new(ptype), Box::new(self.type_of_statement(ret)?));

        self.env = enclosing;


        let ftype = self.curr_ftype.clone().unwrap();
        if let Some(name) = name {
            self.env.borrow_mut().define(name.clone(), ftype.clone());
        }
        self.curr_ftype = None;
        self.curr_fname = None;
        Ok(ftype)
    }

//    fn type_of_curried_fn(&mut self, token: &Token, ntpair: &Vec<NameTypePair>, ret: &LType) -> LType {
//        let types = ntpair.iter().map(|x| x.ltype.clone()).collect::<Vec<LType>>();
//        let ftype = types.iter().rev().fold(ret.clone(), |acc,x| TArrow(Box::new(x.clone()), Box::new(acc)));
//        self.env.define(token.lexeme.clone(), ftype.clone());
//        ftype
//    }

    pub fn get_type(&self, name: &str) -> Option<LType> {
        self.env.borrow().resolve_str(name).map(|x| x.clone())
    }

    // Matches based on enum variant only
     fn match_discriminant(&mut self, x: &Stmt, y: &Stmt) -> bool {
        discriminant(x) == discriminant(y)
     }
}
