use crate::{
    Error,
    lex::{Token, TokenCtx},
    util::Interned,
};

pub enum Literal {
    String(Interned<String>),
}

impl Literal {
    pub fn as_str<'a>(&self, ctx: &'a TokenCtx) -> &'a str {
        match self {
            Literal::String(i) => ctx.strings.get(*i),
        }
    }
}

pub enum UnaryOp {
    Neg,
    Not,
}

pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,

    And,
    Or,
    Xor,

    BitAnd,
    BitOr,
    BitXor,
}

pub enum TernaryOp {
    If,
}

pub enum Expr {
    Literal(Literal),
    Ident(Interned<String>),
    Call {
        func: Box<Expr>,
        args: Vec<Expr>,
    },
    UnaryOp(UnaryOp, Box<Expr>),
    BinaryOp(BinaryOp, [Box<Expr>; 2]),
    TernaryOp(TernaryOp, [Box<Expr>; 3]),
}

impl Expr {
    fn print_debug_depth(&self, ctx: &TokenCtx, depth: usize) {
        (0..depth).for_each(|_| print!("  "));
        match self {
            Expr::Literal(l) => println!("LITERAL: {}", l.as_str(ctx)),
            _ => unimplemented!(),
        }
    }

    pub fn print_debug(&self, ctx: &TokenCtx) {
        self.print_debug_depth(ctx, 0);
    }
}

pub fn parse(tokens: &[Token]) -> Result<Expr, Vec<Error>> {
    parse_expr(
        &mut tokens
            .iter()
            .copied()
    )
        .map_err(|e| vec![e])
}

trait TokenIter = Iterator<Item=Token> + Clone;

fn parse_expr(tokens: &mut impl TokenIter) -> Result<Expr, Error> {
    unimplemented!()
}
