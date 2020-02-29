use internment::LocalIntern;
use crate::{
    util::{Interned, InternTable, SrcLoc, SrcRegion},
    Error, Thing,
};

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum Lexeme {
    Eof,

    Ident(Interned<String>),
    String(Interned<String>),
    Number(LocalIntern<String>),

    LBrace,
    RBrace,
    LParen,
    RParen,
    LBrack,
    RBrack,

    Comma,
    Colon,
    Semicolon,
    Pipe,

    Dot,
    DotDot,

    Not,
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Eq,
    AddEq,
    SubEq,
    MulEq,
    DivEq,
    RemEq,
    Less,
    Greater,
    EqEq,
    NotEq,
    LessEq,
    GreaterEq,

    Let,
    Var,
    Fn,
    If,
    Else,
    Loop,
    While,
    For,
    Break,
    Continue,
    Match,
    Struct,
    Trait,
    This,
    In,
    As,
    And,
    Or,
    Xor,

    Char,
    Num,
    Str,

    True,
    False,
    Null,
}

impl PartialEq<char> for Lexeme {
    fn eq(&self, other: &char) -> bool {
        self.as_str() == &other.to_string()
    }
}

impl<'a> PartialEq<&'a str> for Lexeme {
    fn eq(&self, other: &&'a str) -> bool {
        self.as_str() == &other.to_string()
    }
}

impl Lexeme {
    pub fn as_str<'a>(&self) -> &'a str {
        match self {
            Lexeme::Eof => "EOF",

            Lexeme::Ident(i) => "<identifier>",
            Lexeme::String(i) => "<string>",
            Lexeme::Number(i) => "<number>",

            Lexeme::LBrace => "{",
            Lexeme::RBrace => "}",
            Lexeme::LParen => "(",
            Lexeme::RParen => ")",
            Lexeme::LBrack => "[",
            Lexeme::RBrack => "]",

            Lexeme::Comma => ",",
            Lexeme::Colon => ":",
            Lexeme::Semicolon => ";",
            Lexeme::Pipe => "|",

            Lexeme::Dot => ".",
            Lexeme::DotDot => "..",

            Lexeme::Not => "!",
            Lexeme::Add => "+",
            Lexeme::Sub => "-",
            Lexeme::Mul => "*",
            Lexeme::Div => "/",
            Lexeme::Rem => "%",
            Lexeme::Eq => "=",
            Lexeme::AddEq => "+=",
            Lexeme::SubEq => "-=",
            Lexeme::MulEq => "*=",
            Lexeme::DivEq => "/=",
            Lexeme::RemEq => "%=",

            Lexeme::Less => "<",
            Lexeme::Greater => ">",
            Lexeme::EqEq => "==",
            Lexeme::NotEq => "!=",
            Lexeme::LessEq => "<=",
            Lexeme::GreaterEq => ">=",

            Lexeme::Let => "let",
            Lexeme::Var => "var",
            Lexeme::Fn => "fn",
            Lexeme::If => "if",
            Lexeme::Else => "else",
            Lexeme::Loop => "loop",
            Lexeme::While => "while",
            Lexeme::For => "for",
            Lexeme::Break => "break",
            Lexeme::Continue => "continue",
            Lexeme::Match => "match",
            Lexeme::Struct => "struct",
            Lexeme::Trait => "trait",
            Lexeme::This => "this",
            Lexeme::In => "in",
            Lexeme::As => "as",
            Lexeme::And => "and",
            Lexeme::Or => "or",
            Lexeme::Xor => "xor",

            Lexeme::Char => "char",
            Lexeme::Num => "num",
            Lexeme::Str => "str",

            Lexeme::True => "true",
            Lexeme::False => "false",
            Lexeme::Null => "null",
        }
    }

    pub fn as_str_ctx<'a>(&'a self, ctx: &'a TokenCtx) -> &'a str {
        match self {
            Lexeme::Ident(i) => ctx.idents.get(*i),
            Lexeme::String(i) => ctx.strings.get(*i),
            Lexeme::Number(x) => x.as_str(),
            _ => self.as_str(),
        }
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct Token {
    pub lexeme: Lexeme,
    pub region: SrcRegion,
}

impl PartialEq<char> for Token {
    fn eq(&self, other: &char) -> bool {
        self.lexeme == *other
    }
}

impl PartialEq<Lexeme> for Token {
    fn eq(&self, other: &Lexeme) -> bool {
        &self.lexeme == other
    }
}

impl<'a> PartialEq<&'a str> for Token {
    fn eq(&self, other: &&'a str) -> bool {
        self.lexeme == *other
    }
}

impl Token {
    pub fn new(lexeme: Lexeme, region: SrcRegion) -> Self {
        Self {
            lexeme,
            region,
        }
    }

    pub fn print_debug(&self, ctx: &TokenCtx) {
        println!("{:?}: '{}'", self.region, self.lexeme.as_str_ctx(ctx))
    }
}

#[derive(PartialEq, Debug)]
pub struct TokenCtx {
    pub idents: InternTable<String>,
    pub strings: InternTable<String>,
    pub numbers: InternTable<String>,
}

impl TokenCtx {
    pub fn print_debug(&self, tokens: &[Token]) {
        for token in tokens {
            token.print_debug(self);
        }
    }
}

pub fn lex(s: &str) -> Result<(Vec<Token>, TokenCtx), Vec<Error>> {
    fn is_singular(c: char) -> Option<Lexeme> {
        Some(match c {
            '.' => Lexeme::Dot,
            '!' => Lexeme::Not,
            '+' => Lexeme::Add,
            '-' => Lexeme::Sub,
            '*' => Lexeme::Mul,
            '/' => Lexeme::Div,
            '%' => Lexeme::Rem,
            '=' => Lexeme::Eq,
            '<' => Lexeme::Less,
            '>' => Lexeme::Greater,
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
                ([('.', start)], '.') => {
                    let tok = Token::new(Lexeme::DotDot, SrcRegion::range(*start, loc));
                    self.chars.clear();
                    Some(tok)
                },
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
                ([('*', start)], '=') => {
                    let tok = Token::new(Lexeme::MulEq, SrcRegion::range(*start, loc));
                    self.chars.clear();
                    Some(tok)
                },
                ([('/', start)], '=') => {
                    let tok = Token::new(Lexeme::DivEq, SrcRegion::range(*start, loc));
                    self.chars.clear();
                    Some(tok)
                },
                ([('%', start)], '=') => {
                    let tok = Token::new(Lexeme::RemEq, SrcRegion::range(*start, loc));
                    self.chars.clear();
                    Some(tok)
                },
                ([('=', start)], '=') => {
                    let tok = Token::new(Lexeme::EqEq, SrcRegion::range(*start, loc));
                    self.chars.clear();
                    Some(tok)
                },
                ([('!', start)], '=') => {
                    let tok = Token::new(Lexeme::NotEq, SrcRegion::range(*start, loc));
                    self.chars.clear();
                    Some(tok)
                },
                ([('<', start)], '=') => {
                    let tok = Token::new(Lexeme::LessEq, SrcRegion::range(*start, loc));
                    self.chars.clear();
                    Some(tok)
                },
                ([('>', start)], '=') => {
                    let tok = Token::new(Lexeme::GreaterEq, SrcRegion::range(*start, loc));
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
            '.' | '+' | '-' | '*' | '/' | '%' | '&' | '|' | '!' | '=' | '<' | '>' => true,
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
        LineComment,
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
                Some(',') => tokens.push(Token::new(Lexeme::Comma, SrcRegion::single(loc))),
                Some(':') => tokens.push(Token::new(Lexeme::Colon, SrcRegion::single(loc))),
                Some(';') => tokens.push(Token::new(Lexeme::Semicolon, SrcRegion::single(loc))),
                Some('|') => tokens.push(Token::new(Lexeme::Pipe, SrcRegion::single(loc))),
                Some('"') => state = State::String(loc, String::new()),
                Some('#') => state = State::LineComment,
                Some(c) if c.is_alphabetic() || c == '_' => state = State::Ident(loc, Some(c).iter().collect()),
                Some(c) if c.is_numeric() => state = State::Number(loc, Some(c).iter().collect()),
                Some(c) if is_operator_part(c) => {
                    to_next = false;
                    state = State::Operator(loc, OpState::empty());
                },
                Some(c) => errors.push(Error::unexpected(Thing::Char(c)).at(SrcRegion::single(loc))),
                None => break,
            },
            State::String(start, string) => match c {
                Some('"') => {
                    tokens.push(Token::new(Lexeme::String(strings.intern(string.clone())), SrcRegion::range(*start, loc.next())));
                    state = State::Default;
                },
                Some(c) => string.push(c),
                None => break,
            },
            State::Ident(start, ident) => match c {
                Some(c) if c.is_alphanumeric() || c == '_' => ident.push(c),
                _ => {
                    let lexeme = match ident.as_str() {
                        "let" => Lexeme::Let,
                        "var" => Lexeme::Var,
                        "fn" => Lexeme::Fn,
                        "if" => Lexeme::If,
                        "else" => Lexeme::Else,
                        "loop" => Lexeme::Loop,
                        "while" => Lexeme::While,
                        "for" => Lexeme::For,
                        "break" => Lexeme::Break,
                        "continue" => Lexeme::Continue,
                        "struct" => Lexeme::Struct,
                        "trait" => Lexeme::Trait,
                        "this" => Lexeme::This,
                        "in" => Lexeme::In,
                        "as" => Lexeme::As,
                        "and" => Lexeme::And,
                        "or" => Lexeme::Or,
                        "xor" => Lexeme::Xor,
                        "char" => Lexeme::Char,
                        "num" => Lexeme::Num,
                        "str" => Lexeme::Str,
                        "true" => Lexeme::True,
                        "false" => Lexeme::False,
                        "null" => Lexeme::Null,
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
                    tokens.push(Token::new(Lexeme::Number(LocalIntern::new(number.clone())), SrcRegion::range(*start, loc.next())));
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
            State::LineComment => match c {
                Some('\n') => state = State::Default,
                _ => {},
            },
        }

        if to_next {
            chars.next();
            loc = loc.next();
        }
    }

    match state {
        State::String(start, _) => errors.push(Error::unclosed_delimiter('"').at(SrcRegion::single(start))),
        _ => {},
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

#[test]
fn test_lex() {

    use std::marker::PhantomData;

    assert_eq!(
        lex("println(\"Hello, world!\")"),
        Ok({
            let mut idents = InternTable::default();
            let mut strings = InternTable::default();

            let tokens = vec![
                // println
                Token {
                    lexeme: Lexeme::Ident(idents.intern("println".to_owned())),
                    region: SrcRegion::range(SrcLoc(0), SrcLoc(8)),
                },
                // (
                Token {
                    lexeme: Lexeme::LParen,
                    region: SrcRegion::range(SrcLoc(7), SrcLoc(8)),
                },
                // "Hello, world!"
                Token {
                    lexeme: Lexeme::String(strings.intern("Hello, world!".to_owned())),
                    region: SrcRegion::range(SrcLoc(8), SrcLoc(23)),
                },
                // )
                Token {
                    lexeme: Lexeme::RParen,
                    region: SrcRegion::range(SrcLoc(23), SrcLoc(24)),
                },
            ];

            (tokens, TokenCtx {
                idents,
                strings,
                numbers: InternTable::default(),
            })
        }),
    );
}
