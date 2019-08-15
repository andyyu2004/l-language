use std::collections::HashMap;
use std::hash::Hash;
use crate::types::LType;

pub mod result;

//pub fn vec_to_map<T, U>(vec: Vec<T>) -> HashMap<T, Option<U>> where T : Hash + Eq {
//    let mut map = HashMap::new();
//    for x in vec { map.insert(x, None); }
//    map
//}
// Can't infer U with only None

pub fn vec_to_type_map<T>(vec: Vec<T>) -> HashMap<T, Option<LType>> where T : Hash + Eq {
    let mut map = HashMap::new();
    for x in vec { map.insert(x, None); }
    map
}