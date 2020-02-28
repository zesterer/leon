use std::collections::HashMap;
use crate::{
    parse::{
        Node,
        Expr,
    },
    util::Interned,
};
use super::heap::HeapRef;

pub enum Value<'a> {
    Null,
    Number(f64),
    Bool(bool),
    String(String),
    Func {
        env: Vec<(Interned<String>, HeapRef)>,
        params: &'a Node<Vec<Node<Interned<String>>>>,
        body: &'a Node<Expr>,
    },
    Structure(HashMap<Interned<String>, HeapRef>),
    List(Vec<HeapRef>),
}
