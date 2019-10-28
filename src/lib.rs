mod lex;
mod util;

use util::SrcRegion;

#[derive(Debug)]
pub enum ErrorKind {
    UnexpectedChar(char),
}

#[derive(Debug)]
pub struct Error {
    kind: ErrorKind,
    region: Option<SrcRegion>,
}

impl Error {
    pub fn unexpected_char(c: char) -> Self {
        Self::from(ErrorKind::UnexpectedChar(c))
    }

    pub fn at(mut self, region: SrcRegion) -> Self {
        self.region = Some(region);
        self
    }
}

impl From<ErrorKind> for Error {
    fn from(kind: ErrorKind) -> Self {
        Self {
            kind,
            region: None,
        }
    }
}

#[derive(Default)]
pub struct Engine;

impl Engine {
    pub fn execute(&mut self, code: &str) -> Result<Value, Vec<Error>> {
        let (tokens, ctx) = lex::lex(code)?;

        ctx.print_debug(&tokens);

        unimplemented!()
    }
}

pub enum Value {}
