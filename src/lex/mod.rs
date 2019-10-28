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
    Number(Interned<String>),

    LBrace,
    RBrace,
    LParen,
    RParen,
    LBrack,
    RBrack,

    Semicolon,

    Add,
    Sub,
    Mul,
    Div,
    Rem,
    AddEq,
    SubEq,
    MulEq,
    DivEq,
    RemEq,
    Eq,
    EqEq,

    Let,
    Var,
    Fn,
    If,
    While,
    For,
    Struct,
}

impl Lexeme {
    pub fn as_str<'a>(&self, ctx: &'a TokenCtx) -> &'a str {
        match self {
            Lexeme::Ident(i) => ctx.idents.get(*i),
            Lexeme::String(i) => ctx.strings.get(*i),
            Lexeme::Number(i) => ctx.numbers.get(*i),

            Lexeme::LBrace => "{",
            Lexeme::RBrace => "}",
            Lexeme::LParen => "(",
            Lexeme::RParen => ")",
            Lexeme::LBrack => "[",
            Lexeme::RBrack => "]",

            Lexeme::Semicolon => ";",

            Lexeme::Add => "+",
            Lexeme::Sub => "-",
            Lexeme::Mul => "*",
            Lexeme::Div => "/",
            Lexeme::Rem => "%",
            Lexeme::AddEq => "+=",
            Lexeme::SubEq => "-=",
            Lexeme::MulEq => "*=",
            Lexeme::DivEq => "/=",
            Lexeme::RemEq => "%=",

            Lexeme::Eq => "=",
            Lexeme::EqEq => "==",

            Lexeme::Let => "let",
            Lexeme::Var => "var",
            Lexeme::Fn => "fn",
            Lexeme::If => "if",
            Lexeme::While => "while",
            Lexeme::For => "for",
            Lexeme::Struct => "struct",
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
    pub numbers: InternTable<String>,
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
    fn is_singular(c: char) -> Option<Lexeme> {
        Some(match c {
            '+' => Lexeme::Add,
            '-' => Lexeme::Sub,
            '*' => Lexeme::Mul,
            '/' => Lexeme::Div,
            '%' => Lexeme::Rem,
            '=' => Lexeme::Eq,
            _ => return None,
        })
    }

    struct OpState {
        chars: Vec<(char, SrcLoc)>,
    }

    impl OpState {
        pub fn empty() -> Self {
            Self {
                chars: Vec::new(),
            }
        }

        pub fn push_char(&mut self, c: char, loc: SrcLoc) -> Option<Token> {
            match (self.chars.as_slice(), c) {
                ([('+', start)], '=') => {
                    let tok = Token::new(Lexeme::AddEq, SrcRegion::range(*start, loc));
                    self.chars.clear();
                    Some(tok)
                },
                ([('-', start)], '=') => {
                    let tok = Token::new(Lexeme::SubEq, SrcRegion::range(*start, loc));
                    self.chars.clear();
                    Some(tok)
                },
                ([('=', start)], '=') => {
                    let tok = Token::new(Lexeme::EqEq, SrcRegion::range(*start, loc));
                    self.chars.clear();
                    Some(tok)
                },
                ([(o, start)], _) if is_singular(*o).is_some() => {
                    let tok = Token::new(is_singular(*o).unwrap(), SrcRegion::range(*start, loc));
                    self.chars.clear();
                    self.chars.push((c, loc));
                    Some(tok)
                },
                _ => {
                    self.chars.push((c, loc));
                    None
                },
            }
        }

        pub fn finish(&mut self) -> Option<Result<Token, Error>> {
            match self.chars.as_slice() {
                [(o, start)] if is_singular(*o).is_some() => {
                    let tok = Token::new(is_singular(*o).unwrap(), SrcRegion::range(*start, start.next()));
                    Some(Ok(tok))
                },
                [] => None,
                _ => {
                    let s = self.chars.iter().map(|(c, _)| *c).collect();
                    let region = self.chars.iter().fold(SrcRegion::single(self.chars[0].1), |a, (_, r)| a.extend_to(*r));
                    Some(Err(Error::unknown_operator(s).at(region)))
                },
            }
        }
    }

    fn is_operator_part(c: char) -> bool {
        match c {
            '+' | '-' => true,
            '*' | '/' => true,
            '&' | '|' => true,
            '!' | '=' => true,
            _ => false,
        }
    }

    let mut tokens = Vec::new();
    let mut errors = Vec::new();
    let mut idents = InternTable::default();
    let mut strings = InternTable::default();
    let mut numbers = InternTable::default();

    enum State {
        Default,
        Ident(SrcLoc, String),
        String(SrcLoc, String),
        Number(SrcLoc, String),
        Operator(SrcLoc, OpState),
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
                Some(c) if c.is_numeric() => state = State::Number(loc, Some(c).iter().collect()),
                Some(c) if is_operator_part(c) => {
                    to_next = false;
                    state = State::Operator(loc, OpState::empty());
                },
                Some(c) => errors.push(Error::unexpected_char(c).at(SrcRegion::single(loc))),
                None => break,
            },
            State::String(start, string) => match c {
                Some('"') => {
                    tokens.push(Token::new(Lexeme::String(strings.intern(string.clone())), SrcRegion::range(*start, loc.next())));
                    state = State::Default;
                },
                Some(c) => string.push(c),
                None => {
                    to_next = false;
                    state = State::Default;
                },
            },
            State::Ident(start, ident) => match c {
                Some(c) if c.is_alphanumeric() || c == '_' => ident.push(c),
                _ => {
                    let lexeme = match ident.as_str() {
                        "let" => Lexeme::Let,
                        "var" => Lexeme::Var,
                        "fn" => Lexeme::Fn,
                        "if" => Lexeme::If,
                        "while" => Lexeme::While,
                        "for" => Lexeme::For,
                        "struct" => Lexeme::Struct,
                        _ => Lexeme::Ident(idents.intern(ident.clone())),
                    };
                    tokens.push(Token::new(lexeme, SrcRegion::range(*start, loc.next())));
                    to_next = false;
                    state = State::Default;
                },
            },
            State::Number(start, number) => match c {
                Some(c) if c.is_alphanumeric() || c == '_' => number.push(c),
                _ => {
                    tokens.push(Token::new(Lexeme::Number(numbers.intern(number.clone())), SrcRegion::range(*start, loc.next())));
                    to_next = false;
                    state = State::Default;
                },
            },
            State::Operator(start, op_state) => match c {
                Some(c) if is_operator_part(c) => {
                    if let Some(tok) = op_state.push_char(c, loc) {
                        tokens.push(tok);
                    }
                },
                _ => {
                    match op_state.finish() {
                        Some(Ok(token)) => tokens.push(token),
                        Some(Err(err)) => errors.push(err),
                        None => {},
                    }
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
            numbers,
        }))
    } else {
        Err(errors)
    }
}
