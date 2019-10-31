#![feature(trait_alias)]

mod lex;
mod parse;
mod util;
mod walker;

use self::{
    util::SrcRegion,
    lex::Lexeme,
};

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Thing {
    Atom,
    Lexeme(Lexeme),
    Ident,
    Expr,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ErrorKind {
    Spurious, // Never revealed to user
    UnexpectedChar(char),
    UnknownOperator(String),
    UnexpectedEof,
    Expected(Thing),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Error {
    kind: ErrorKind,
    region: Option<SrcRegion>,
    while_parsing: Vec<Thing>,
    hint: Option<&'static str>,
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

    pub fn hint(mut self, hint: &'static str) -> Self {
        self.hint = Some(hint);
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
            hint: None,
        }
    }
}

#[derive(Default)]
pub struct Engine;

impl Engine {
    pub fn execute(&mut self, code: &str) -> Result<walker::Value, Vec<Error>> {
        let (tokens, ctx) = lex::lex(code)?;

        println!("--- Tokens ---");
        ctx.print_debug(&tokens);

        let ast = parse::parse(&tokens)?;

        println!("--- Syntax Tree ---");
        ast.print_debug(&ctx);

        Ok(walker::AbstractMachine::default()
            .execute(&ast, &ctx)
            .unwrap())
    }
}
