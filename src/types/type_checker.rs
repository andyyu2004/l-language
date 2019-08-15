use crate::parsing::{Expr, Stmt, Mode};
use crate::lexing::{TokenType, Token};
use crate::types::{LType, LTypeError};
use crate::types::l_types::LType::*;
use crate::types::l_types::{Pair, TypeName};
use crate::parsing::expr::Expr::*;
use crate::interpreting::Env;
use crate::types::LTypeError::*;
use crate::parsing::stmt::Stmt::*;
use crate::lexing::token::TokenType::{Greater, GreaterEqual, Caret, Slash, Plus, Star, LessEqual, Less, Minus, BangEqual, DoubleEqual, Modulo};
use crate::interpreting::LPattern::*;
use std::mem::{discriminant};
use std::collections::{HashMap, VecDeque};
use std::rc::Rc;
use std::cell::RefCell;
use crate::interpreting::pattern_matching::LPattern;
use itertools::Itertools;
use crate::utility::vec_to_type_map;

pub struct TypeChecker {
    pub env: Rc<RefCell<Env<LType>>>, // Variable -> Type
    types: Env<LType>, // Typename -> Type
    curr_fn_ret_type: Option<LType>, // Type check return statements

    mode: Mode
}

impl TypeChecker {

    pub fn new(mode: Mode) -> TypeChecker {
        let mut types = Env::new(None);
        types.define("Bool".to_string(), TBool);
        types.define("Int".to_string(), TNum);
        types.define("Number".to_string(), TNum);
        types.define("String".to_string(), TString);
        types.define("Unit".to_string(), TUnit);
        TypeChecker {
            env: Rc::new(RefCell::new(Env::new(None))),
            curr_fn_ret_type: None,
            types,
            mode
        }
    }
}

impl TypeChecker {


    pub fn type_check(&mut self, statements: &mut Vec<Stmt>) -> Result<(), Vec<LTypeError>> {
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

    pub fn type_of_expr(&mut self, expr: &mut Expr) -> Result<LType, LTypeError> {
        match expr {
            ELiteral(token) => TypeChecker::type_of_literal(token),
            EBinary { operator, left, right } => self.type_of_binary(operator, left, right),
            EUnary { operator, operand } => self.type_of_unary(operator, operand),
            EVariable { name } => self.type_of_variable(name),
            EApplication { token, callee, arg} => self.type_of_application(token, callee, arg),
            EAssignment { lvalue, expr } => self.type_of_assignment(lvalue, expr),
            EIf { condition, left, right, token } => self.type_of_if(token, condition, left, right),
            EBlock(xs) => self.type_of_block_e(xs),
            ETuple(_, xs) => self.type_of_tuple(xs),
            EList(token, xs) => self.type_of_list(token, xs),
            ERecord(_, xs) => self.type_of_record(xs),
            ELogic { operator, left, right} => self.type_of_logical(operator, left, right),
            EGet { name, expr } => self.type_of_get(name, expr),
            ESet { name, expr, value } => self.type_of_set(name, expr, value),
            EDataConstructor { name } => self.env.borrow().resolve(name).map_err(|_| NonExistentDataConstructor(name.clone())),
            EMatch { .. } => panic!("Match should be desugared"), // self.type_of_match(token, expr, branches),
            EIfLet { token, pattern, scrutinee, left, right } =>
                self.type_of_if_let(token, pattern, scrutinee, left, right),
            ELambda { token, ref mut ltype, param, body } => self.type_of_lambda(token, param, ltype, body),
            EPanic => Ok(TTop), // Can represent any type
            x => unimplemented!("Unsupported: {}", x)
        }
    }

    fn type_of_statement(&mut self, stmt: &mut Stmt) -> Result<LType, LTypeError> {
        match stmt {
            LStmt(ref mut expr) | ExprStmt(ref mut expr) | PrintStmt(ref mut expr) => self.type_of_expr(expr),
            FnStmt { name, token, param, ret_type, body, tparams } =>
                self.type_of_fn(name, token, tparams, param,ret_type, body),
            FnCurried { name, tparams, param, ret, token } =>
                self.type_of_curried_fn(name, token, tparams, param, ret),
            VarStmt { name, ltype, init}  => self.type_of_var_decl(name, ltype, init.as_mut()),
            LetStmt { pattern, ltype, init, token} => self.type_of_let_binding(token, pattern, ltype, init),
            ReturnStmt { token, value } => self.type_of_return(token, value),
            TypeAlias { name, ltype } => self.define_type_alias(name, ltype),
            WhileStmt { token, condition, body} => self.type_of_while(token, condition, body),
            StructDecl { name, fields} => self.type_of_struct(name, fields),
            DataDecl { name, variants} => self.type_of_data_decl(name, variants),
            _ => panic!("Unimplented in type_check_stmt")
        }
    }

    fn type_of_data_decl(&mut self, typename: &TypeName, variants: &mut HashMap<String, LType>) -> Result<LType, LTypeError> {
        let tdata = TData(typename.name.clone(), vec_to_type_map(typename.tparams.to_vec()));

        self.types.define(typename.name.lexeme.clone(), tdata);
        let mut variants = variants.clone();
        self.validate_data_declaration(&typename.name, &typename.tparams, &variants)?;
        for (k, v) in &mut variants {
            // Make sure to do this after the type parameter check, otherwise the error may be incorrectly fixed by the mapping
            v.map_string_to_type_ref(&self.types)?;
            self.env.borrow_mut().define(k.clone(), v.clone())
        }
        Ok(TVariant(variants))
    }

    // Check the constructor has correct kind AND all type parameters are declared. These checks in conjunction will take care of most scenarios I can think of
    // Changing the ordering is valid e.g.
    // List<'a, 'b> = Nil | Cons ('a, 'b) List<'b, 'a>;
    fn validate_data_declaration(&self, name: &Token, tparams: &Vec<Token>, variants: &HashMap<String, LType>) -> Result<(), LTypeError> {
        let kind = tparams.len();
        for (k, v) in variants {
            // Check for undeclared type variables
            for p in &v.type_parameters() {
                if !tparams.contains(p) {
                    return Err(UndeclaredTypeParameter(p.clone(), format!("All type parameters in ADT definitions must be declared. Help: {}<{}>", name, p)))
                }
            }
//            if v.kinds(name).iter().any(|x| x != kind) {
            for k in v.kinds(name) {
                if k != kind {
                    return Err(BadKind(kind, k, name.clone(), format!("Help: Please ensure the number of type parameters are correct")))
                }
            }
        }
        Ok(())
    }

    // Takes a type and makes sure all the types contained within have correct kind
    fn validate_adt_kind(&self, ltype: &LType) -> Result<(), LTypeError> {
        match ltype {
            TData(adt_name, map) => {
                let adt = match self.types.resolve(adt_name) {
                    Ok(x) => x,
                    Err(_) => return Err(NonExistentType(TypeName::new(adt_name.clone(), vec![])))
                };
                if let TData(_, tparams) = adt {
                    let defined_kind = tparams.values().len();
                    let kind = map.values().len();
                    if defined_kind != kind {
                        Err(BadKind(defined_kind, kind, adt_name.clone(), format!("")))
                    } else { Ok(()) }
                } else {
                    panic!()
                }

            },
            TArrow(l, r) => {
                self.validate_adt_kind(l)?;
                self.validate_adt_kind(r)
            }
            _ => Ok(())
        }
    }

    fn type_of_get(&mut self, name: &Token, expr: &mut Expr) -> Result<LType, LTypeError> {
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

    fn type_of_struct(&mut self, _name: &TypeName, _fields: &HashMap<String, LType>) -> Result<LType, LTypeError> {
//        self.env.borrow_mut().define(name.lexeme.clone(), ),
        Ok(TUnit)
    }

    fn type_of_set(&mut self, name: &Token, expr: &mut Expr, value: &mut Expr) -> Result<LType, LTypeError> {
        let t_expr = self.type_of_get(name, expr)?;
        let t_value = self.type_of_expr(value)?;
        if t_expr != t_value {
            return Err(TypeError(t_expr, t_value, name.clone(), "Type of field must match the the type of value being assigned to it".to_string()))
        }
        Ok(t_expr)
    }

    fn type_of_logical(&mut self, token: &Token, left: &mut Expr, right: &mut Expr) -> Result<LType, LTypeError> {
        let ltype = self.type_of_expr(left)?;
        let rtype = self.type_of_expr(right)?;
        if ltype != TBool {
            Err(TypeError(TBool, ltype.clone(), token.clone(), format!("Logical operators must be assigned applied to booleans. (Left side is of type {})", ltype)))
        } else if rtype != TBool {
            Err(TypeError(TBool, rtype.clone(), token.clone(), format!("Logical operators must be assigned applied to booleans. (Right side is of type {})", rtype)))
        } else {
            Ok(TBool)
        }
    }

    fn type_of_list(&mut self, token: &Token, xs: &mut VecDeque<Expr>) -> Result<LType, LTypeError> {
        let tfirst = self.type_of_expr(&mut xs[0])?;
        for x in xs {
            let t = self.type_of_expr(x)?;
            if t != tfirst {
                return Err(TypeMismatch(tfirst, t, token.clone(), "Lists must be of homogeneous type".into()))
            }
        }
        Ok(TList(Box::new(tfirst)))

    }

    fn type_of_record(&mut self, xs: &mut HashMap<String, Expr>) -> Result<LType, LTypeError> {
        let mut map = HashMap::new();
        for (k, ref mut v) in xs {
            map.insert(k.clone(), self.type_of_expr(v)?);
        }
        Ok(TRecord(map))
    }

    fn type_of_if(&mut self, token: &Token, condition: &mut Expr, left: &mut Expr, right: &mut Expr) -> Result<LType, LTypeError> {
        let tcond = self.type_of_expr(condition)?;
        let tleft = self.type_of_expr(left)?;
        let tright = self.type_of_expr(right)?;
        if tcond != TBool {
            Err(TypeError(TBool, tcond.clone(), token.clone(), format!("The condition of an if statement must be of type Bool (found type {})", tcond)))
        } else if tleft != tright {
            Err(TypeMismatch(tleft, tright, token.clone(), "If and else branches must have same return type".into()))
        } else {
            Ok(tleft)
        }
    }

    fn type_of_let_binding(&mut self, token: &Token, pattern: &LPattern, ltype: &mut Option<LType>, init: &mut Expr) -> Result<LType, LTypeError> {
        let tright = self.type_of_expr(init)?;
        if let Some(ltype) = ltype {
            ltype.map_string_to_type_ref(&self.types)?;
            // Check annotated type is the type on the right
            if &tright != ltype {
                return Err(TypeError(ltype.clone(), tright.clone(), token.clone(),
                    format!("The type of the value being assigned does not match the annotation, (Annotated {}, found {}", ltype, tright))
                )
            }
        }

        self.type_of_pattern_bindings(token, pattern, &tright)?
            .into_iter()
            .for_each(|(k, v)| self.env.borrow_mut().define(k, v));

        Ok(tright)
    }
//
//    fn type_of_match(&mut self, token: &Token, expr: &Expr, branches: &Vec<(LPattern, Expr)>) -> Result<LType, LTypeError> {
//        let first_type = self.type_of_expr(&branches[0].1)?;
//        for (_, t) in branches {
//            if self.type_of_expr(t)? != first_type {
//                return Err(TypeMismatch(first_type, self.type_of_expr(t)?, token.clone(), "All match arms must have the same return type".into()));
//            }
//        }
//        Ok(first_type)
//    }

    fn type_of_lambda(&mut self, token: &Token, param: &LPattern, ltype: &mut Option<LType>, body: &mut Expr) -> Result<LType, LTypeError> {
        if let Some(ptype) = ltype {
            ptype.map_string_to_type_ref(&self.types)?;
            let bindings = self.type_of_pattern_bindings(token, param, &ptype)?;
            let enclosing = Rc::clone(&self.env);
            self.env = Rc::new(RefCell::new(Env::new(Some(Rc::clone(&self.env)))));
            bindings.into_iter().for_each(|(k,v)| self.env.borrow_mut().define(k, v));
            let ftype = TArrow(Box::new(ptype.clone()), Box::new(self.type_of_expr(body)?));
            self.env = enclosing;
            Ok(ftype)
        } else {
            // Don't have type inference yet
            Err(RequireTypeAnnotation(token.clone(), format!("Lambda binder {} requires explicit type annotation", param)))
        }
    }

    fn type_of_if_let(&mut self, token: &Token, pattern: &LPattern, scrutinee: &mut Expr, left: &Vec<Stmt>, right: &mut Expr) -> Result<LType, LTypeError> {
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
        if tleft != tright { return Err(TypeMismatch(tleft, tright, token.clone(), "All branches must have the same return type".into())) }
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
                if  adt_type != ltype {
                    return Err(TypeError(adt_type.clone(), ltype.clone(), token.clone(),
                         format!("The constructor {} belongs to data type {}. Attempting to pattern match this constructor against {}", name, adt_type, ltype))
                    )
                }
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
                    Err(BadPattern(pattern.clone(), ltype.clone(), token.clone(), format!("")))
                }
            },
            PRecord => Ok(vec![]),
            PTuple(xs) => {
                if let TTuple(ts) = ltype {
                    if xs.len() != ts.len() {
                        return Err(BadPattern(pattern.clone(), ltype.clone(), token.clone(), format!("Pattern matches a {}-tuple, got {}-tuple", xs.len(), ts.len())))
                    }
                    Ok(xs.iter()
                        .zip(ts)
                        .flat_map(|(p, x)| self.type_of_pattern_bindings(token, p, x))
                        .flatten() // flatmap doesn't leave it very flat for some reason
                        .collect_vec()
                    )
                } else {
                    Err(BadPattern(pattern.clone(), ltype.clone(), token.clone(), format!("Note: Cannot match tuple pattern against non-tuple")))
                }
            },
            PLiteral(_) => Ok(vec![]),
            PIdentifier(x) => Ok(vec![(x.lexeme.clone(), ltype.clone())]),
            PWildcard => Ok(vec![]),
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

    fn type_of_variable(&self, name: &Token) -> Result<LType, LTypeError> {
        match self.env.borrow().resolve(name) {
            Ok(t) => Ok(t),
            Err(_) => {
                println!("Couldn't find variable {}", name);
                Err(InvalidDeclaration)
            } // If variable is not found here it is due to bad types in declaration
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
        for stmt in &mut block {
            self.type_of_statement(stmt)?;
        }
        let ret = match last {
            Some(mut stmt) => match stmt {
                LStmt(ref mut expr) => self.type_of_expr(expr),
                ref mut x => { self.type_of_statement(x)?; Ok(TUnit) }
            },
            None => Ok(TUnit)
        };
        self.env = enclosing;
        ret
    }

    fn type_of_application(&mut self, token: &Token, callee: &mut Expr, arg: &mut Expr) -> Result<LType, LTypeError> {
        let tcallee = self.type_of_expr(callee)?;
        let targ = self.type_of_expr(arg)?;
        if let TArrow(tparam, tret) = tcallee {
            if *tparam == targ {
                Ok(*tret)
            } else {
                self.validate_generic_application(token, targ, *tparam, *tret)
//                let note = format!("Expected type {} as annotated in signature, found type {}", tparam, targ);
//                Err(TypeError(*tparam, targ, token.clone(), note))
            }
        } else {
            Err(NonFunction(tcallee, token.clone(), format!("Applying to a type other than a function type or arrow type")))
        }
    }

    // Doesn't even requires the TGeneric construct. Any type parameter can be used without declaration currently
    fn validate_generic_application(&self, token: &Token, targ: LType, tparam: LType, mut tret: LType) -> Result<LType, LTypeError> {
        if !tparam.is_type_matchable(&targ) {
            let note = format!("Cannot match type {} to generic type {}", targ, tparam);
            return Err(TypeMismatch(tparam, targ, token.clone(), note))
        }
        let substitutions = tparam.generate_substitutions(&targ);
//                println!("Substutitons");
//                substitutions.iter().for_each(|(k, v)| println!("{} -> {}", k, v));
        // Can probably be accomplished with fold but keep it simple for more clarity
        for (parameter, ltype) in substitutions {
            tret = tret.substitute_type_parameters(&parameter, &ltype);
        }
        Ok(tret)
    }

    fn type_of_assignment(&mut self, lvalue: &Token, expr: &mut Expr) -> Result<LType, LTypeError> {
        let tlvalue = self.env.borrow().resolve(lvalue).unwrap();
        let texpr = self.type_of_expr(expr)?;
        if tlvalue == texpr {
            if let TRecord(_) = texpr {
                // Make it easier to index into record if labels match (as record equality is done by comparing hashmaps)
                self.env.borrow_mut().update(&lvalue.lexeme, texpr)
            }
            Ok(tlvalue)
        }
        else {
            Err(TypeError(tlvalue.clone(), texpr.clone(), lvalue.clone(),
                  format!("Note: The variable type must match the type of the value being assigned to it. Variable is of type {}, value of type {}", tlvalue, texpr)))
        }
    }

    fn type_of_unary(&mut self, operator: &Token, operand: &mut Expr) -> Result<LType, LTypeError> {
        let t_operand = self.type_of_expr(operand)?;
        match operator.ttype {
            TokenType::Bang => if t_operand != TBool {
                Err(TypeError(TBool, t_operand.clone(), operator.clone(), format!("Note: The logical negation operator must be applied to type Bool, found type {}", t_operand)))
            } else { Ok(TBool) },
            TokenType::Minus => if t_operand != TNum {
                Err(TypeError(TNum, t_operand.clone(), operator.clone(), format!("The numerical negation operator must be applied to type Number, found type {}", t_operand)))
            } else { Ok(TNum) },
            _ => panic!("Invalid unary operator")
        }
    }

    fn type_of_binary(&mut self, operator: &Token, left: &mut Expr, right: &mut Expr) -> Result<LType, LTypeError> {
        // Only numbers are comparable currently
        let num_ops = vec![Star, Plus, Minus, Slash, Caret, Modulo];
        let cmp_ops = vec![Greater, GreaterEqual, Less, LessEqual];
        let eq_ops = vec![DoubleEqual, BangEqual];
        let tleft = self.type_of_expr(left)?;
        let tright = self.type_of_expr(right)?;
        if num_ops.contains(&operator.ttype) {
            if tleft != TNum { Err(TypeError(TNum, tleft.clone(), operator.clone(),
                format!("The operation {} is of type Number -> Number -> Number, but the left argument is of type {}", operator.ttype, tleft))
            )}
            else if tright != TNum { Err(TypeError(TNum, tleft, operator.clone(),
                format!("The operation {} is of type Number -> Number -> Number, but the right argument is of type {}", operator.ttype, tright))
            )}
            else { Ok(TNum) }
        } else if eq_ops.contains(&operator.ttype) {
            if tleft != tright { return Err(TypeMismatch(tleft, tright, operator.clone(), format!("Cannot compare different types for equality"))) }
            else { Ok(TBool) }
        } else if cmp_ops.contains(&operator.ttype) {
            if tleft != TNum { Err(TypeError(TNum, tleft.clone(), operator.clone(),
                format!("The operation {} is of type Number -> Number -> Bool, but the left argument is of type {}", operator.ttype, tleft))
            )}
            else if tright != TNum { Err(TypeError(TNum, tleft, operator.clone(),
                format!("The operation {} is of type Number -> Number -> Bool, but the right argument is of type {}", operator.ttype, tright))
            )}
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

    fn type_of_tuple(&mut self, xs: &mut Vec<Expr>) -> Result<LType, LTypeError> {
        let v = xs.iter_mut().map(|x| self.type_of_expr(x)).collect::<Result<Vec<LType>, LTypeError>>()?;
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

    fn type_of_while(&mut self, token: &Token, condition: &mut Expr, body: &Vec<Stmt>) -> Result<LType, LTypeError> {
        let tcond = self.type_of_expr(condition)?;
        if tcond != TBool {
            return Err(TypeError(TBool, tcond.clone(), token.clone(), format!("The condition on a while loop must be of type Bool, found type {}", tcond)))
        }
        self.type_of_block_e(body)?;
        Ok(TUnit)
    }

    fn define_type_alias(&mut self, typename: &TypeName, ltype: &mut LType) -> Result<LType, LTypeError> {
        ltype.map_string_to_type_ref(&self.types)?;
        self.types.define(typename.name.lexeme.clone(), ltype.clone());
        Ok(ltype.clone())
    }

    fn type_of_return(&mut self, token: &Token, value: &mut Option<Expr>) -> Result<LType, LTypeError> {
        let tret = match value {
            Some(expr) => self.type_of_expr(expr),
            None => Ok(TUnit)
        }?;

        match &self.curr_fn_ret_type {
            Some(t) => if &tret != t {
                Err(TypeError(t.clone(), tret.clone(), token.clone(),
                      format!("The return type must match the return type in the function definition. The function is annotated {} but got {}", t, tret))
                )
            } else { Ok(tret) },
            None => { Ok(TUnit)}
        }
    }

    fn type_of_var_decl(&mut self, token: &Token, ltype: &Option<LType>, init: Option<&mut Expr>) -> Result<LType, LTypeError> {
        match init {
            Some(expr) => {
                let t_init = self.type_of_expr(expr)?;
                if let Some(ltype) = ltype {
                    if ltype.clone().map_string_to_type(&self.types)? != t_init {
                        return Err(TypeError(ltype.clone(), t_init.clone(), token.clone(),
                             format!("The type of the variable annotation must match the type of the initializer. Variable is annotated to be of type {}, but found type {}", ltype, t_init))
                        )
                    }
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
                None => Err(LTypeError::RequireTypeAnnotation(token.clone(), format!("Unintialised variable declaration {} requires explicit type annotation", token)))
            }
        }
    }

    fn type_of_fn(&mut self, name: &Option<String>, token: &Token, _tparams: &Rc<Vec<Token>>, param: &Option<Pair<LType>>, ret: &mut LType, body: &Vec<Stmt>) -> Result<LType, LTypeError> {
        let prev_ret_type = self.curr_fn_ret_type.clone();

        ret.map_string_to_type_ref(&self.types)?; // This is used multiple times below, don't move
        self.validate_adt_kind(ret)?;
        self.curr_fn_ret_type = Some(ret.clone());

        let enclosing = Rc::clone(&self.env);
        self.env = self.wrap(Env::new(Some(Rc::clone(&self.env))));

        // Let parameter type be either the type of the parameter or TUnit it no parameter
        let ptype = if let Some(param) = param {
            let t = param.value.clone().map_string_to_type(&self.types)?;
            self.validate_adt_kind(&t)?;
            self.env.borrow_mut().define(param.name.clone(), t.clone());
            t
        } else { TUnit };

        // Let the function type be the parameter type -> return type
        let ftype = TArrow(Box::new(ptype.clone()), Box::new(ret.clone()));

        // Allows typechecking of recursive functions. Assume it has type stated in fn definition.
        if let Some(name) = name {
            self.env.borrow_mut().define(name.clone(), ftype.clone())
        }

        let dummy_ret = ReturnStmt { token: Token::dummy(), value: Some(ELiteral(Token::dummy())) };
        let has_explicit_ret = body.iter().any(|x| self.match_discriminant(x, &dummy_ret));
        let block_type = self.type_of_block_e(body)?;
        if &block_type != ret && !has_explicit_ret { // Checks the implicit return is type correct
            return Err(TypeError(ret.clone(), block_type.clone(), token.clone(), format!("Function is annotated to have return type {}, but found {}", ret, block_type)));
        }

        self.env = enclosing;
        self.curr_fn_ret_type = prev_ret_type;

        // Define function in the outer scope so it can be referenced
        if let Some(name) = name {
            self.env.borrow_mut().define(name.clone(), ftype.clone())
        }

        Ok(ftype)
    }

    fn type_of_curried_fn(&mut self, name: &Option<String>, _token: &Token, _tparams: &Rc<Vec<Token>>, param: &mut Pair<LType>, ret: &mut Stmt) -> Result<LType, LTypeError> {
        let enclosing = self.env.clone();
        self.env = self.wrap(Env::new(Some(self.env.clone())));

        param.value.map_string_to_type_ref(&self.types)?;
        self.validate_adt_kind(&param.value)?;
        self.env.borrow_mut().define(param.name.clone(), param.value.clone());

        // Do this so recursive type checking works
        if let Some(name) = name {
            let defined_type = TArrow(Box::new(param.value.clone()), Box::new(self.type_of_function_by_definition(ret)?));
            self.env.borrow_mut().define(name.clone(), defined_type)
        }

        let ftype = TArrow(Box::new(param.value.clone()), Box::new(self.type_of_statement(ret)?));

        self.env = enclosing;

        if let Some(name) = name {
            self.env.borrow_mut().define(name.clone(), ftype.clone())
        }

        Ok(ftype)
    }

    fn type_of_function_by_definition(&mut self, stmt: &Stmt) -> Result<LType, LTypeError> {
        match stmt {
            FnCurried { param, ret , .. } =>
                Ok(TArrow(Box::new(param.value.clone().map_string_to_type(&self.types)?), Box::new(self.type_of_function_by_definition(ret)?))),
            FnStmt { name, param, ret_type, .. } => match param {
                Some(t) => Ok(TArrow(Box::new(t.clone().value.map_string_to_type(&self.types)?), Box::new(ret_type.clone().map_string_to_type(&self.types)?))),
                None => Ok(TArrow(Box::new(TUnit), Box::new(ret_type.clone().map_string_to_type(&self.types)?))),
            },
            x => panic!("Badness in type of function by def, got non function stmt {}", x)
        }
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

    fn wrap<T>(&self, x: T) -> Rc<RefCell<T>> {
        Rc::new(RefCell::new(x))
    }

}
