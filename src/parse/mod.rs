use std::ops::{Deref, DerefMut};
use crate::{
    Error, Thing,
    lex::{Lexeme, Token, TokenCtx},
    util::{Interned, SrcRegion},
};

pub struct Node<T> {
    item: Box<T>,
    region: SrcRegion,
}

impl<T> Node<T> {
    pub fn new(item: T, region: SrcRegion) -> Self {
        Self {
            item: Box::new(item),
            region,
        }
    }
}

impl<T> Deref for Node<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.item
    }
}

impl<T> DerefMut for Node<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.item
    }
}

pub enum Literal {
    String(Interned<String>),
    Number(Interned<String>),
    Bool(bool),
    Null,
}

impl Literal {
    pub fn as_str<'a>(&self, ctx: &'a TokenCtx) -> &'a str {
        match self {
            Literal::String(i) => ctx.strings.get(*i),
            Literal::Number(i) => ctx.numbers.get(*i),
            Literal::Bool(x) => if *x { "true" } else { "false" },
            Literal::Null => "null",
        }
    }
}

#[derive(Debug)]
pub enum UnaryOp {
    Neg,
    Not,
}

#[derive(Debug)]
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

    Eq,
    NotEq,
    Less,
    LessEq,
    Greater,
    GreaterEq,

    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    RemAssign,
}

pub enum TernaryOp {
    If,
}

pub enum Expr {
    Literal(Literal),
    Ident(Interned<String>),
    Call(Node<Expr>, Vec<Node<Expr>>),
    Unary(Node<UnaryOp>, Node<Expr>),
    Binary(Node<BinaryOp>, [Node<Expr>; 2]),
    Ternary(Node<TernaryOp>, [Node<Expr>; 3]),
    Var(Interned<String>, Node<Expr>, Node<Expr>), // let foo = 5; bar
    ThisThen(Node<Expr>, Node<Expr>), // foo; bar
}

impl Expr {
    pub fn literal(literal: Literal, region: SrcRegion) -> Node<Self> {
        Node::new(Expr::Literal(literal), region)
    }

    pub fn ident(ident: Interned<String>, region: SrcRegion) -> Node<Self> {
        Node::new(Expr::Ident(ident), region)
    }

    pub fn unary(op: Node<UnaryOp>, expr: Node<Expr>, region: SrcRegion) -> Node<Self> {
        Node::new(Expr::Unary(op, expr), region)
    }

    pub fn binary(op: Node<BinaryOp>, left: Node<Expr>, right: Node<Expr>, region: SrcRegion) -> Node<Self> {
        Node::new(Expr::Binary(op, [left, right]), region)
    }

    pub fn this_then(this: Node<Expr>, then: Node<Expr>, region: SrcRegion) -> Node<Self> {
        Node::new(Expr::ThisThen(this, then), region)
    }

    fn print_debug_depth(&self, ctx: &TokenCtx, depth: usize) {
        (0..depth * 2).for_each(|_| print!("  "));
        match self {
            Expr::Literal(l) => println!("Literal: {}", l.as_str(ctx)),
            Expr::Ident(i) => println!("Ident: {}", ctx.idents.get(*i)),
            Expr::Unary(op, expr) => {
                println!("Unary Operation: {:?}", **op);
                expr.print_debug_depth(ctx, depth + 1);
            },
            Expr::Binary(op, [l, r]) => {
                println!("Binary Operation: {:?}", **op);
                l.print_debug_depth(ctx, depth + 1);
                r.print_debug_depth(ctx, depth + 1);
            },
            Expr::ThisThen(this, then) => {
                println!("This, Then:");
                this.print_debug_depth(ctx, depth + 1);
                then.print_debug_depth(ctx, depth + 1);
            },
            _ => unimplemented!(),
        }
    }

    pub fn print_debug(&self, ctx: &TokenCtx) {
        self.print_debug_depth(ctx, 0);
    }
}

pub fn parse(tokens: &[Token]) -> Result<Node<Expr>, Vec<Error>> {
    let mut tokens = tokens
        .iter()
        .copied();

    let ast = parse_block_body(&mut tokens)
        .map_err(|e| vec![e])?;

    parse_lexeme(Lexeme::Eof, &mut tokens)
        .map_err(|e| vec![e])?;

    Ok(ast)
}

trait TokenIter = Iterator<Item=Token> + Clone;

fn parse_block(tokens: &mut impl TokenIter) -> Result<Node<Expr>, Error> {
    parse_enclosed(Lexeme::LBrace, Lexeme::RBrace, tokens, parse_block_body)
}

fn parse_block_body(tokens: &mut impl TokenIter) -> Result<Node<Expr>, Error> {
    match parse_expr(tokens) {
        Ok(expr) => match parse_lexeme(Lexeme::Semicolon, tokens) {
            Ok(()) => Ok(Expr::this_then(expr, parse_block_body(tokens)?, SrcRegion::none())),
            Err(_) => Ok(expr),
        },
        Err(_) => Ok(Expr::literal(Literal::Null, SrcRegion::none())),
    }
}

fn parse_expr(tokens: &mut impl TokenIter) -> Result<Node<Expr>, Error> {
    parse_assign(tokens)
        .map_err(|e| e.while_parsing(Thing::Expr))
}

fn parse_assign(tokens: &mut impl TokenIter) -> Result<Node<Expr>, Error> {
    let left = parse_comparison(tokens)?;

    let op = try_parse(tokens, |tok, iter| match tok.lexeme {
        Lexeme::Eq => Ok(Node::new(BinaryOp::Assign, tok.region)),
        Lexeme::AddEq => Ok(Node::new(BinaryOp::AddAssign, tok.region)),
        Lexeme::SubEq => Ok(Node::new(BinaryOp::SubAssign, tok.region)),
        Lexeme::MulEq => Ok(Node::new(BinaryOp::MulAssign, tok.region)),
        Lexeme::DivEq => Ok(Node::new(BinaryOp::DivAssign, tok.region)),
        Lexeme::RemEq => Ok(Node::new(BinaryOp::RemAssign, tok.region)),
        _ => Err(Error::spurious()),
    });

    if let Ok(op) = op {
        let right = parse_comparison(tokens)?;
        let region = left.region.union(right.region);
        Ok(Expr::binary(op, left, right, region))
    } else {
        Ok(left)
    }
}

fn parse_comparison(tokens: &mut impl TokenIter) -> Result<Node<Expr>, Error> {
    let left = parse_sum(tokens)?;

    let op = try_parse(tokens, |tok, iter| match tok.lexeme {
        Lexeme::EqEq => Ok(Node::new(BinaryOp::Eq, tok.region)),
        Lexeme::NotEq => Ok(Node::new(BinaryOp::NotEq, tok.region)),
        Lexeme::Less => Ok(Node::new(BinaryOp::Less, tok.region)),
        Lexeme::LessEq => Ok(Node::new(BinaryOp::LessEq, tok.region)),
        Lexeme::Greater => Ok(Node::new(BinaryOp::Greater, tok.region)),
        Lexeme::GreaterEq => Ok(Node::new(BinaryOp::GreaterEq, tok.region)),
        _ => Err(Error::spurious()),
    });

    if let Ok(op) = op {
        let right = parse_sum(tokens)?;
        let region = left.region.union(right.region);
        Ok(Expr::binary(op, left, right, region))
    } else {
        Ok(left)
    }
}

fn parse_sum(tokens: &mut impl TokenIter) -> Result<Node<Expr>, Error> {
    let left = parse_product(tokens)?;

    let op = try_parse(tokens, |tok, iter| match tok.lexeme {
        Lexeme::Add => Ok(Node::new(BinaryOp::Add, tok.region)),
        Lexeme::Sub => Ok(Node::new(BinaryOp::Sub, tok.region)),
        _ => Err(Error::spurious()),
    });

    if let Ok(op) = op {
        let right = parse_product(tokens)?;
        let region = left.region.union(right.region);
        Ok(Expr::binary(op, left, right, region))
    } else {
        Ok(left)
    }
}

fn parse_product(tokens: &mut impl TokenIter) -> Result<Node<Expr>, Error> {
    let left = parse_unary(tokens)?;

    let op = try_parse(tokens, |tok, iter| match tok.lexeme {
        Lexeme::Mul => Ok(Node::new(BinaryOp::Mul, tok.region)),
        Lexeme::Div => Ok(Node::new(BinaryOp::Div, tok.region)),
        Lexeme::Rem => Ok(Node::new(BinaryOp::Rem, tok.region)),
        _ => Err(Error::spurious()),
    });

    if let Ok(op) = op {
        let right = parse_unary(tokens)?;
        let region = left.region.union(right.region);
        Ok(Expr::binary(op, left, right, region))
    } else {
        Ok(left)
    }
}

fn parse_unary(tokens: &mut impl TokenIter) -> Result<Node<Expr>, Error> {
    let op = try_parse(tokens, |tok, iter| match tok.lexeme {
        Lexeme::Sub => Ok(Node::new(UnaryOp::Neg, tok.region)),
        Lexeme::Not => Ok(Node::new(UnaryOp::Not, tok.region)),
        _ => Err(Error::spurious()),
    });

    let expr = parse_atom(tokens)?;

    if let Ok(op) = op {
        let region = op.region.union(expr.region);
        Ok(Expr::unary(op, expr, region))
    } else {
        Ok(expr)
    }
}

fn parse_atom(tokens: &mut impl TokenIter) -> Result<Node<Expr>, Error> {
    // Atoms
    let atom = try_parse(tokens, |tok, iter| match tok.lexeme {
        Lexeme::True => Ok(Expr::literal(Literal::Bool(true), tok.region)),
        Lexeme::False => Ok(Expr::literal(Literal::Bool(false), tok.region)),
        Lexeme::Null => Ok(Expr::literal(Literal::Null, tok.region)),
        Lexeme::String(i) => Ok(Expr::literal(Literal::String(i), tok.region)),
        Lexeme::Number(i) => Ok(Expr::literal(Literal::Number(i), tok.region)),
        Lexeme::Ident(i) => Ok(Expr::ident(i, tok.region)),
        _ => Err(Error::expected(Thing::Atom).at(tok.region)),
    });

    // Parenthesised expressions
    match atom {
        Ok(atom) => Ok(atom),
        Err(e0) => parse_enclosed(Lexeme::LParen, Lexeme::RParen, tokens, parse_expr)
            .map_err(|e1| e0.max(e1)),
    }
}

fn parse_enclosed<I: TokenIter, R>(
    head: Lexeme,
    tail: Lexeme,
    tokens: &mut I,
    f: impl FnOnce(&mut I) -> Result<Node<R>, Error>,
) -> Result<Node<R>, Error> {

    parse_lexeme(head, tokens)?;

    let inner = f(tokens);

    parse_lexeme(tail, tokens)?;

    inner
}

fn parse_lexeme(lexeme: Lexeme, tokens: &mut impl TokenIter) -> Result<(), Error> {
    try_parse(tokens, |tok, iter| if tok.lexeme == lexeme {
        Ok(())
    } else {
        Err(Error::expected(Thing::Lexeme(lexeme)).at(tok.region))
    })
}

fn try_parse<I: TokenIter, R, F>(iter: &mut I, f: F) -> Result<R, Error>
    where F: FnOnce(&Token, &mut I) -> Result<R, Error>,
{
    let mut iter2 = iter.clone();
    let tok = iter2
        .next()
        .ok_or(Error::unexpected_eof())?;

    let tok = f(&tok, &mut iter2)?;
    *iter = iter2;
    Ok(tok)
}
