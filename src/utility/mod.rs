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

pub fn vec_to_vec_map<T>(xs: Vec<T>) -> Vec<(T, Option<LType>)> {
    let mut v = vec![];
    for x in xs { v.push((x, None)) }
    v
}

// Helper functions for a pseudo hashmap implemented with vector. (Because order is important, but not ordered by keys so BTreeMap isn't suitable)
// Linear search is fine as its only for lists of probably average length 2
pub fn vec_contains_key<T, U> (x: &T, xs: &Vec<(T, U)>) -> bool where T : PartialEq {
    for (k, v) in xs {
        if k == x { return true }
    }
    false
}

pub fn vec_replace_key<K, V>(old: &K, new: K, xs: &mut Vec<(K, V)>) where K : PartialEq {
    for (k, v) in xs {
        if k == old {
            *k = new;
            break;
        }
    }
}

pub fn vec_update_value<K, V>(key: &K, value: V, xs: &mut Vec<(K, V)>) where K : PartialEq {
    for (k, v) in xs {
        if k == key {
            *v = value;
            break;
        }
    }
}






