use crate::lexing::TokenType;
use crate::types::LType;
use crate::types::l_types::LType::{TArrow, TTuple, TNum};
use std::collections::HashMap;


macro_rules! hashmap {
    ($( $key: expr => $val: expr ),*) => {{
         let mut map = ::std::collections::HashMap::new();
         $( map.insert($key, $val); )*
         map
    }}
}

struct OperatorTypeMap {}

impl OperatorTypeMap {
    // Symbol operators are not curried currently
//    pub fn map() -> HashMap<TokenType, LType> {
//        let binary_numeric_type = TArrow(Box::new(TTuple(vec![TNum, TNum])), Box::new(TNum));
//
//        hashmap![
//            TokenType::Plus => binary_numeric_type,
//            TokenType::Minus => binary_numeric_type,
//            TokenType::Star => binary_numeric_type,
//            TokenType::Slash => binary_numeric_type,
//            TokenType::Caret => binary_numeric_type,
//            TokenType::Plus => binary_numeric_type
//        ]
//    }
}

