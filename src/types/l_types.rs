use std::fmt::{Display, Formatter, Error};
use crate::parsing::expr::{format_tuple, format_record};
use LType::{TBool,TArrow,TTuple,TUnit,TNum,TTop};
use crate::types::l_types::LType::*;
use crate::interpreting::Env;
use crate::types::LTypeError;
use crate::lexing::Token;
use crate::types::LTypeError::NonExistentType;
use std::collections::HashMap;
use itertools::{join, Itertools};
use std::rc::Rc;
use std::cell::RefCell;
use std::ops::Deref;
use crate::main;
use crate::utility::vec_to_type_map;

#[derive(Clone, Debug)]
pub struct TypeName {
    pub name: Token,
    pub tparams: Vec<Token>, // Type parameters <T, U>
}

impl PartialEq for TypeName {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Display for TypeName {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        if self.tparams.is_empty() {
            write!(f, "{}", self.name)
        } else {
            write!(f, "{}<{}>", self.name, join(&self.tparams, ", "))
        }

    }
}

impl TypeName {
    pub fn new(name: Token, tparams: Vec<Token>) -> TypeName {
        TypeName { tparams, name }
    }
}

#[derive(Clone, Debug)]
pub enum LType {
    TTop,
    TBool,
    TNum,
    TString,
    TArrow(Box<LType>, Box<LType>),
    TTuple(Vec<LType>),
    TRecord(HashMap<String, LType>),
    TList(Box<LType>),
    TUnit,
    TVariant(HashMap<String, LType>),
    TName(TypeName),
    TVar(Token),
    TData(Token, HashMap<Token, Option<LType>>), // Token is name, and map is Mapping from type paraneter to concrete type
//    TGeneric(Box<LType>, Vec<Token>), // The Tokens represents the type parameter of the argument
    // TNothing
}

impl LType {

    // Where name is the name of the ADT we are trying to find the kind of
    // Vec contains each occurance of the kind of the specified adt
    pub fn kinds(&self, name: &Token) -> Vec<usize> {
        match self {
            TName(typename) => if &typename.name == name {
                vec![typename.tparams.len()] } else { vec![] },
            TData(adt_name, map) => if adt_name == name {
                vec![map.values().len()] } else { vec![] },
            TArrow(l, r) => l.kinds(name).into_iter().chain(r.kinds(name).into_iter()).collect_vec(),
            _ => vec![]
        }
    }

    pub fn type_parameters(&self) -> Vec<Token> {
        match self {
            TVar(token) => vec![token.clone()],
//            TGeneric(ltype, tparams) => tparams.clone().into_iter().chain(ltype.type_parameters()).collect(),
            TArrow(l, r) => l.type_parameters().into_iter().chain(r.type_parameters()).collect(),
            TTuple(xs) => xs.iter().flat_map(|x| x.type_parameters()).collect_vec(),
            TRecord(xs)
                | TVariant(xs) => xs.values().flat_map(|x| x.type_parameters()).collect_vec(),
            TList(xs) => xs.type_parameters(),
            TName(typename) => typename.tparams.clone(),
            TData(_, map) => map.clone().into_iter().map(|(k, _)| k).collect_vec(),
            _ => vec![]
        }
    }

    // If only one simple type, it remains. Used to find the type of a variant. i.e. Cons : Int -> List -> List :: List, Nil : List :: List
    pub fn rightmost_type(&self) -> &Self {
        match self {
            TArrow(_, r) => r.rightmost_type(),
            t => t,
        }
    }

    // Number of arrows
    // i.e. the number of parameters the function takes to become fully applied
    pub fn curried_arity(&self) -> i16 {
        match self {
            TArrow(_, r) => 1 + r.curried_arity(),
            _ => 0
        }
    }

    // If only one type to start with, it remains
    pub fn remove_rightmost(self) -> Self {
        match self {
            TArrow(l, r) => match *r {
                TArrow(_, _) => TArrow(l, Box::from(r.remove_rightmost())),
                _ => *l
            }
            t => panic!("Attempting to remove rightmost type on a simple type {}", t)
        }
    }

    pub fn map_string_to_type(self, env: &Env<LType>) -> Result<LType, LTypeError> {
        match self {
            TList(t) => Ok(TList(Box::new(t.map_string_to_type(env)?))),
            TArrow(l, r) => Ok(TArrow(Box::new(l.map_string_to_type(env)?), Box::new(r.map_string_to_type(env)?))),
            TTuple(xs) => Ok(TTuple(xs.into_iter().map(|x| x.map_string_to_type(env)).collect::<Result<Vec<_>, _>>()?)),
            TRecord(xs) | TVariant(xs) => {
                let mut map = HashMap::new();
                for (k, v) in xs {
                    map.insert(k, v.map_string_to_type(env)?);
                }
                Ok(TRecord(map))
            },
            TName(typename) => match env.resolve(&typename.name) {
                Ok(t) => {
                    match t {
                        TData(name, _) => Ok(TData(name, vec_to_type_map(typename.tparams))),
                        t => Ok(t)
                    }
                },
                Err(_) => Err(NonExistentType(typename))
            },
            _ => Ok(self)
        }
    }

    pub fn map_string_to_type_ref(&mut self, env: &Env<LType>) -> Result<(), LTypeError> {
        match self {
            TList(t) => t.map_string_to_type_ref(env),
            TArrow(l, r) => {
                l.map_string_to_type_ref(env)?;
                r.map_string_to_type_ref(env)
            }
            TTuple(xs) => {
                xs.iter_mut().map(|x| x.map_string_to_type_ref(env)).collect::<Result<Vec<_>, _>>()?;
                Ok(())
            }
            TRecord(xs) => {
                xs.iter_mut().map(|(_, t)| t.map_string_to_type_ref(env)).collect::<Result<Vec<_>, _>>()?;
                Ok(())
            }

            TVariant(xs) => {
                xs.iter_mut().map(|(_, t)| t.map_string_to_type_ref(env)).collect::<Result<Vec<_>, _>>()?;
                Ok(())
            },
            TName(typename) => match env.resolve(&typename.name) {
                Ok(t) => {
                    match t {
                        TData(name, _) => *self = TData(name, vec_to_type_map(typename.tparams.to_vec())),
                        _ => *self = t,
                    };
                    Ok(())
                },
                Err(_) => Err(NonExistentType(typename.clone()))
            }
            _ => Ok(())
        }
    }

    // Similar to equality, but allows type parameters to represent any type
    pub fn is_type_matchable(&self, other: &LType) -> bool {
        // If they are already equal, definite match
        if self == other { return true; }
        match (self, other) {
            (TVar(_), t) => true,
            (t, TVar(_)) => true,
            (TArrow(l, r), TArrow(t, u)) => l.is_type_matchable(t) && r.is_type_matchable(u),
            (TTuple(xs), TTuple(ys)) =>
                xs.len() == ys.len() && xs.iter().zip(ys).all(|(x, y)| x.is_type_matchable(y)),
            (TRecord(xs), TRecord(ys)) =>
                xs.values().len() == ys.values().len()
                    && xs.values().zip(ys.values()).all(|(x,y)| x.is_type_matchable(y)),
            (TList(xs), TList(ys)) => xs.is_type_matchable(ys),
            (TVariant(xs), TVariant(ys)) =>
                xs.values().len() == ys.values().len()
                    && xs.values().zip(ys.values()).all(|(x,y)| x.is_type_matchable(y)),
//            (TData(a, xs), TData(b, ys)) =>
            _ => false
        }
    }

    // If matchable from above, generate the substitutions
    // targ is the concrete type
    pub fn generate_substitutions(&self, targ: &LType) -> Vec<(Token, LType)> {
        // Sanity check
        if !self.is_type_matchable(targ) { panic!("Please check the types are matchable before generating substitutions") }
        match self {
            TVar(t) => vec![(t.clone(), targ.clone())],
            TArrow(l, r) => if let TArrow(tl, tr) = targ {
                l.generate_substitutions(tl).into_iter().chain(r.generate_substitutions(tr)).collect_vec()
            } else { panic!("This isn't a successful match, should be filtered out") },
            TTuple(xs) => xs.iter().flat_map(|x| x.generate_substitutions(targ)).collect_vec(),
            TRecord(xs)
            | TVariant(xs) => xs.values().flat_map(|x| x.generate_substitutions(targ)).collect_vec(),
            TList(t) => t.generate_substitutions(targ),
//            TName(typename) => if typename.tparams.contains()
//            TGeneric(_, _) => {} not sure
            _ => vec![]
        }
    }

    // Creates a new one
    pub fn substitute_type_parameters(&self, tparam: &Token, with: &LType) -> LType {
        match self {
            // Substitute if type parameter matches
            TVar(x) => if x == tparam { with.clone() } else { self.clone() },
            TArrow(l, r) => TArrow(Box::new(l.substitute_type_parameters(tparam, with)), Box::new(r.substitute_type_parameters(tparam, with))),
            TTuple(xs) => TTuple(xs.into_iter().map(|x| x.substitute_type_parameters(tparam, with)).collect_vec()),
            TRecord(xs) => TRecord(xs.iter().map(|(k, v)| (k.clone(), v.substitute_type_parameters(tparam, with))).collect()),
            TVariant(xs) => TVariant((xs.iter().map(|(k, v)| (k.clone(), v.substitute_type_parameters(tparam, with)))).collect()),
            TList(xs) => TList(Box::new(xs.substitute_type_parameters(tparam, with))),
            TData(token, map) => if map.contains_key(tparam) {
                if let TVar(new_tparam) = with {
                    // If the substitution is another type parameter, replace
                    let mut map = map.clone();
                    map.remove(tparam);
                    map.insert(new_tparam.clone(), None);
                    TData(token.clone(), map)
                } else {
                    // Else creating a mapping from the type parameter to a concrete type
                    let mut map = map.clone();
                    map.insert(tparam.clone(), Some(with.clone()));
                    TData(token.clone(), map)
                }
            } else { self.clone() }
//            TName(typename) =>
//            TGeneric(ltype, tparams) => if tparams.contains(tparam) {
////                // If generic matches remove generic and substitute
//                if tparams.len() <= 1 { // Will be empty after removal, so skip that step
//                    ltype.substitute_type_parameters(tparam, with)
//                } else {
//                    let mut tparams = tparams.clone();
//                    let index = tparams.iter().position(|x| x == tparam).unwrap();
//                    tparams.remove(index);
//                    TGeneric(ltype.clone(), tparams)
//                }
//            } else {
//                // Else keep generic type but propogate substitution
//                TGeneric(Box::new(ltype.substitute_type_parameters(tparam, with)), tparams.clone())
//            }
            t => t.clone()
        }
    }
}

impl Display for LType {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            TTop => write!(f, "TTop"),
            TBool => write!(f, "Bool"),
            TNum => write!(f, "Number"),
            TUnit => write!(f, "Unit"),
            TString => write!(f, "String"),
            TVar(var) => write!(f, "{}", var),
            TList(x) => write!(f, "[{}]", x),
            TName(s) => write!(f, "'{}", s),
            // TNothing => write!(f, "TNothing"),
            TRecord(xs) | TVariant(xs) => write!(f, "{{{}}}", format_record(xs, ", ")),
            TTuple(xs) =>
                if xs.len() == 0 { write!(f, "{}", TUnit)}
//                else if xs.len() == 1 { write!(f, "{}", xs[0]) }
                else { write!(f, "({})", format_tuple(xs)) },
            TArrow(left, right) => match **left {
                TArrow(_, _) => write!(f, "({}) -> {}", left, right),
                _                   => write!(f, "{} -> {}", left, right),
            },
            TData(name, mapping) => write!(f, "{}<{}>", name, format_data(mapping)),
//            TGeneric(ltype, tparams) => write!(f, "TGeneric<{}> {}", join(tparams, ", "), ltype)
//                write!(f, "TFunc :: {} sub {:?}", ltype, map)
        }
    }
}

// If value is none print the type parameter, else print the concrete type
fn format_data<K, V>(map: &HashMap<K, Option<V>>) -> String where K : Display, V : Display {
    let mut acc = String::new();
    for (i, (k, v)) in map.iter().enumerate() {
        if i == map.len() - 1 {
            match v {
                Some(v) => acc.push_str(&format!("{}", v)),
                None => acc.push_str(&format!("{}", k))
            }
        } else {
            match v {
                Some(v) => acc.push_str(&format!("{}, ", v)),
                None => acc.push_str(&format!("{}, ", k))
            }
        }
    }
    acc
}

//pub fn format_option_hashmap<K, V>(map: &HashMap<K, Option<V>>) -> String where K : Display, V : Display {
//    let mut acc = String::new();
//    for (i, (k, v)) in map.iter().enumerate() {
//        if i == map.len() - 1 { acc.push_str(&format!("{}:{}", k, format_option(v))) }
//        else { acc.push_str(&format!("{}:{}, ", k, format_option(v))) }
//    }
//    acc
//}
//
//pub fn format_option<T>(x: &Option<T>) -> String where T : Display {
//    match x {
//        Some(x) => format!("{}", x),
//        None => String::from("None")
//    }
//}

//impl PartialEq<Vec<LType>> for LType {
//    fn eq(&self, other: &Vec<LType>) -> bool {
//        match (self, other) {
//            (TTuple(xs), ys) => xs == ys,
//            _ => false
//        }
//    }
//}


impl PartialEq for LType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
//            (TGeneric(a, b), TGeneric(x, y)) => a == x && b == y,
            (TVar(x), TVar(y)) => x == y,
            (TTop, _) | (_, TTop) => true, // TTop can be used in place of any type
            (TBool, TBool) => true,
            (TString, TString) => true,
            (TNum, TNum) => true,
            (TArrow(t, u), TArrow(x, y)) => t == x && u == y,
            (TUnit, TUnit) => true,
            (TTuple(xs), TUnit) => xs.is_empty(), // TUnit is just convenient way to represent empty tuple
            (TUnit, TTuple(ys)) => ys.is_empty(),
//            (TTuple(xs), TTuple(ys)) => if xs.len() == 1 { &xs[0] == ys } // Allow nested singleton tuple equivalence
//                else if ys.len() == 1 { &ys[0] == xs } else { xs == ys },
//            (TTuple(xs), t) => xs.len() == 1 && t == &xs[0], // Order matters for some reason for eq
//            (t, TTuple(ys)) => ys.len() == 1 && t == &ys[0],
            (TTuple(xs), TTuple(ys)) => xs == ys,
            (TTuple(xs), t) => xs.len() == 1 && &xs[0] == t, // Singleton tuple is equivalent to the containing type
            (t, TTuple(ys)) => ys.len() == 1 && &ys[0] == t,
            (TData(x, ma), TData(y, mb)) =>
                eq_data(x, y, ma, mb),
            // Order is not important, but naming is in records
            (TRecord(xs), TRecord(ys)) => xs == ys,
//                 xs.iter().map(|x| &x.value).collect::<Vec<&LType>>() == ys.iter().map(|y| &y.value).collect::<Vec<&LType>>(),
            (TList(x), TList(y)) => x == y,
            _ => false
        }
    }
}

// Require ADT<'a> to be equal to any ADT<'x> where 'x is any concrete type
// E.g. data List<'a> = Nil | Cons 'a List<'a>; Nil has no way of getting a concrete type, should be compatible with any list type
// Two TData are equal if both fields are equal or
// Or when ALL mapping that are not equal are due to one being mapped to None
fn eq_data(a: &Token, b: &Token, x: &HashMap<Token, Option<LType>>, y: &HashMap<Token, Option<LType>>) -> bool {
    if a == b && x == y { return true; }
    let mut keys_x = x.keys().collect_vec(); keys_x.sort();
    let mut keys_y = y.keys().collect_vec(); keys_y.sort();
    if keys_x != keys_y { return false; }

    for k in keys_x {
        if x.get(k) == y.get(k) { continue; }
        // else t != u
        // If they are both some and they do not equal, then it is because they are of different types
        // Safely unwrap as we are indexing by their keys
        if x.get(k).unwrap().is_some() && y.get(k).unwrap().is_some() {
            return false;
        }
    }
    true

}

#[derive(Debug, Clone, PartialEq)]
pub struct Pair<T> where T : Display {
    pub name: String,
    pub value: T
}

impl<T> Pair<T> where T : Display {
    pub fn new(name: String, value: T) -> Pair<T> {
        Pair { name, value }
    }
}

impl<T> Display for Pair<T> where T : Display {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "{}: {}", self.name, self.value)
    }
}