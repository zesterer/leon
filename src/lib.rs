#![feature(trait_alias, arbitrary_self_types, proc_macro_hygiene)]

mod lex;
mod parse;
mod util;
mod walker;

use std::collections::HashSet;
use self::{
    util::SrcRegion,
    lex::Lexeme,
};

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq)]
pub enum Thing {
    Char(char),
    Lexeme(Lexeme),
    Ident,
    Number,
    String,
    Bool,
    Expr,
    LValue,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ErrorKind {
    UnexpectedChar(char),
    UnclosedDelimiter(char),
    UnknownOperator(String),
    UnexpectedEof,
    Unexpected(Thing),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Error {
    kind: ErrorKind,
    region: Option<SrcRegion>,
    while_parsing: Vec<Thing>,
    expected: HashSet<Thing>,
    hint: Option<&'static str>,
}

impl Error {
    pub fn unexpected(thing: impl Into<Thing>) -> Self {
        Self::from(ErrorKind::Unexpected(thing.into()))
    }

    pub fn unclosed_delimiter(c: char) -> Self {
        Self::from(ErrorKind::UnclosedDelimiter(c))
    }

    pub fn unknown_operator(op: String) -> Self {
        Self::from(ErrorKind::UnknownOperator(op))
    }

    pub fn unexpected_eof() -> Self {
        Self::from(ErrorKind::UnexpectedEof)
    }

    pub fn at(mut self, region: impl Into<Option<SrcRegion>>) -> Self {
        self.region = region.into();
        self
    }

    pub fn while_parsing(mut self, thing: impl Into<Thing>) -> Self {
        self.while_parsing.push(thing.into());
        self
    }

    pub fn expected(mut self, thing: impl Into<Thing>) -> Self {
        self.expected.insert(thing.into());
        self
    }

    pub fn hint(mut self, hint: &'static str) -> Self {
        self.hint = Some(hint);
        self
    }

    pub fn combine(mut self, other: Self) -> Self {
        self.expected.extend(other.expected.into_iter());
        self
    }

    pub fn max(self, other: Self) -> Self {
        match (self.region, other.region) {
            (Some(region_a), Some(region_b)) if region_a.later_than(region_b) => self,
            _ => other,
        }
    }
}

impl From<ErrorKind> for Error {
    fn from(kind: ErrorKind) -> Self {
        Self {
            kind,
            region: None,
            while_parsing: Vec::new(),
            expected: HashSet::default(),
            hint: None,
        }
    }
}

#[derive(Default)]
pub struct Engine;

impl Engine {
    pub fn execute(&mut self, code: &str) -> Result<(), Vec<Error>> {
        let (tokens, ctx) = lex::lex(code)?;

        //println!("--- Tokens ---");
        ctx.print_debug(&tokens);

        let ast = parse::parse(&tokens, &ctx)?;

        println!("--- Syntax Tree ---");
        ast.print_debug(&ctx);

        let result = walker::AbstractMachine::new(ctx.strings, ctx.idents)
            .execute(&ast)
            .unwrap();

        println!("{:?}", result);

        Ok(())
    }
}
