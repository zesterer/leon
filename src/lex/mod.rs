use std::{
    fmt,
};
use crate::{
    Error,
    util::{Interned, InternTable, SrcLoc, SrcRegion},
};

#[derive(Copy, Clone)]
pub enum Lexeme {
    Ident(Interned<String>),
    String(Interned<String>),
    LBrace,
    RBrace,
    LParen,
    RParen,
    LBrack,
    RBrack,
    Semicolon,
}

impl Lexeme {
    pub fn as_str<'a>(&self, ctx: &'a TokenCtx) -> &'a str {
        match self {
            Lexeme::Ident(i) => ctx.idents.get(*i),
            Lexeme::String(i) => ctx.strings.get(*i),
            Lexeme::LBrace => "{",
            Lexeme::RBrace => "}",
            Lexeme::LParen => "(",
            Lexeme::RParen => ")",
            Lexeme::LBrack => "[",
            Lexeme::RBrack => "]",
            Lexeme::Semicolon => ";",
        }
    }
}

#[derive(Copy, Clone)]
pub struct Token {
    lexeme: Lexeme,
    region: SrcRegion,
}

impl Token {
    pub fn new(lexeme: Lexeme, region: SrcRegion) -> Self {
        Self {
            lexeme,
            region,
        }
    }

    pub fn print_debug(&self, ctx: &TokenCtx) {
        println!("{:?}: '{}'", self.region, self.lexeme.as_str(ctx))
    }
}

pub struct TokenCtx {
    pub idents: InternTable<String>,
    pub strings: InternTable<String>,
}

impl TokenCtx {
    pub fn print_debug(&self, tokens: &[Token]) {
        println!("--- Tokens ---");
        for token in tokens {
            token.print_debug(self);
        }
    }
}

pub fn lex(s: &str) -> Result<(Vec<Token>, TokenCtx), Vec<Error>> {

    let mut tokens = Vec::new();
    let mut errors = Vec::new();
    let mut idents = InternTable::default();
    let mut strings = InternTable::default();

    enum State {
        Default,
        Ident(SrcLoc, String),
        String(SrcLoc, String),
    }

    let mut chars = s.chars();
    let mut state = State::Default;
    let mut loc = SrcLoc::start();

    while let c = chars.clone().next() {
        let mut to_next = true;
        match &mut state {
            State::Default => match c {
                Some(c) if c.is_whitespace() => {},
                Some('{') => tokens.push(Token::new(Lexeme::LBrace, SrcRegion::single(loc))),
                Some('}') => tokens.push(Token::new(Lexeme::RBrace, SrcRegion::single(loc))),
                Some('(') => tokens.push(Token::new(Lexeme::LParen, SrcRegion::single(loc))),
                Some(')') => tokens.push(Token::new(Lexeme::RParen, SrcRegion::single(loc))),
                Some('[') => tokens.push(Token::new(Lexeme::LBrack, SrcRegion::single(loc))),
                Some(']') => tokens.push(Token::new(Lexeme::RBrack, SrcRegion::single(loc))),
                Some(';') => tokens.push(Token::new(Lexeme::Semicolon, SrcRegion::single(loc))),
                Some('"') => state = State::String(loc, String::new()),
                Some(c) if c.is_alphabetic() || c == '_' => state = State::Ident(loc, Some(c).iter().collect()),
                Some(c) => errors.push(Error::unexpected_char(c).at(SrcRegion::single(loc))),
                None => break,
            },
            State::Ident(start, ident) => match c {
                Some(c) if c.is_alphanumeric() || c == '_' => ident.push(c),
                _ => {
                    tokens.push(Token::new(Lexeme::Ident(idents.intern(ident.clone())), SrcRegion::range(*start, loc.next())));
                    to_next = false;
                    state = State::Default;
                },
            },
            State::String(start, s) => match c {
                Some('"') => {
                    tokens.push(Token::new(Lexeme::String(strings.intern(s.clone())), SrcRegion::range(*start, loc.next())));
                    state = State::Default;
                },
                Some(c) => s.push(c),
                None => {
                    to_next = false;
                    state = State::Default;
                },
            },
        }

        if to_next {
            chars.next();
            loc = loc.next();
        }
    }

    if errors.len() == 0 {
        Ok((tokens, TokenCtx {
            idents,
            strings,
        }))
    } else {
        Err(errors)
    }
}
