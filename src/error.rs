use crate::{
    util::SrcRegion,
    lex::{Token, Lexeme},
};

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub enum Thing {
    Char(char),
    Ident(String),
    Lexeme(Lexeme),
    Token(Token),
    Expr,
    LValue,
}

impl From<char> for Thing {
    fn from(c: char) -> Self {
        Thing::Char(c)
    }
}

impl<'a> From<&'a str> for Thing {
    fn from(s: &'a str) -> Self {
        Thing::Ident(s.to_string())
    }
}

impl From<Lexeme> for Thing {
    fn from(lexeme: Lexeme) -> Self {
        Thing::Lexeme(lexeme)
    }
}

impl From<Token> for Thing {
    fn from(tok: Token) -> Self {
        Thing::Token(tok)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ErrorKind {
    UnexpectedChar(char),
    UnclosedDelimiter(char),
    UnknownOperator(String),
    UnexpectedEof,
    ExpectedEof(Thing),
    Unexpected(Thing),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Error {
    kind: ErrorKind,
    pub region: Option<SrcRegion>,
    while_parsing: Vec<Thing>,
    expected: Vec<Thing>,
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

    pub fn expected_eof(thing: impl Into<Thing>) -> Self {
        Self::from(ErrorKind::ExpectedEof(thing.into()))
    }

    pub fn at(mut self, region: impl Into<Option<SrcRegion>>) -> Self {
        self.region = region.into();
        self
    }

    pub fn while_parsing(mut self, thing: impl Into<Thing>) -> Self {
        self.while_parsing.push(thing.into());
        self
    }

    pub fn expected(mut self, things: impl IntoIterator<Item=Thing>) -> Self {
        self.expected.extend(things);
        self
    }

    pub fn hint(mut self, hint: &'static str) -> Self {
        self.hint = Some(hint);
        self
    }

    pub fn combine(mut self, other: Self) -> Self {
        for thing in other.expected.into_iter() {
            if !self.expected.contains(&thing) {
                self.expected.push(thing);
            }
        }
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
            expected: Default::default(),
            hint: None,
        }
    }
}
