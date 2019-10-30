#![feature(trait_alias)]

mod lex;
mod parse;
mod util;

use self::{
    util::SrcRegion,
    lex::Lexeme,
};

#[derive(Debug)]
pub enum Thing {
    Atom,
    Lexeme(Lexeme),
    Expr,
}

#[derive(Debug)]
pub enum ErrorKind {
    Spurious, // Never revealed to user
    UnexpectedChar(char),
    UnknownOperator(String),
    UnexpectedEof,
    Expected(Thing),
}

#[derive(Debug)]
pub struct Error {
    kind: ErrorKind,
    region: Option<SrcRegion>,
    while_parsing: Vec<Thing>,
}

impl Error {
    pub fn spurious() -> Self {
        Self::from(ErrorKind::Spurious)
    }

    pub fn unexpected_char(c: char) -> Self {
        Self::from(ErrorKind::UnexpectedChar(c))
    }

    pub fn unknown_operator(op: String) -> Self {
        Self::from(ErrorKind::UnknownOperator(op))
    }

    pub fn unexpected_eof() -> Self {
        Self::from(ErrorKind::UnexpectedEof)
    }

    pub fn expected(thing: impl Into<Thing>) -> Self {
        Self::from(ErrorKind::Expected(thing.into()))
    }

    pub fn at(mut self, region: impl Into<Option<SrcRegion>>) -> Self {
        self.region = region.into();
        self
    }

    pub fn while_parsing(mut self, thing: impl Into<Thing>) -> Self {
        self.while_parsing.push(thing.into());
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
        }
    }
}

#[derive(Default)]
pub struct Engine;

impl Engine {
    pub fn execute(&mut self, code: &str) -> Result<Value, Vec<Error>> {
        let (tokens, ctx) = lex::lex(code)?;

        println!("--- Tokens ---");
        ctx.print_debug(&tokens);

        let ast = parse::parse(&tokens)?;

        println!("--- Syntax Tree ---");
        ast.print_debug(&ctx);

        unimplemented!()
    }
}

pub enum Value {}
