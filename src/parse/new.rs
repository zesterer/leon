use std::{
    collections::HashSet,
    ops::{Deref, DerefMut},
};
use parze::prelude::*;
use crate::{
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

    pub fn into_inner(self) -> T {
        *self.item
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
}

#[derive(Debug)]
pub enum Mutation {
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    RemAssign,
}

pub enum Expr {
    Literal(Literal),
    Ident(Interned<String>),
    Unary(Node<UnaryOp>, Node<Expr>),
    Binary(Node<BinaryOp>, Node<Expr>, Node<Expr>),
    Var(Node<Interned<String>>, Node<Expr>, Node<Expr>), // let foo = 5; bar
    ThisThen(Node<Expr>, Node<Expr>), // foo; bar
    IfElse(Node<Expr>, Node<Expr>, Node<Expr>),
    While(Node<Expr>, Node<Expr>),
    Mutation(Node<Mutation>, Node<Expr>, Node<Expr>),
    Func(Node<Vec<Node<Interned<String>>>>, Node<Expr>),
    Call(Node<Expr>, Node<Vec<Node<Expr>>>),
    Index(Node<Expr>, Node<Expr>),
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
        Node::new(Expr::Binary(op, left, right), region)
    }

    pub fn mutation(m: Node<Mutation>, left: Node<Expr>, right: Node<Expr>, region: SrcRegion) -> Node<Self> {
        Node::new(Expr::Mutation(m, left, right), region)
    }

    pub fn this_then(this: Node<Expr>, then: Node<Expr>, region: SrcRegion) -> Node<Self> {
        Node::new(Expr::ThisThen(this, then), region)
    }

    pub fn var(ident: Node<Interned<String>>, expr: Node<Expr>, then: Node<Expr>, region: SrcRegion) -> Node<Self> {
        Node::new(Expr::Var(ident, expr, then), region)
    }

    pub fn if_else(predicate: Node<Expr>, true_block: Node<Expr>, false_block: Node<Expr>, region: SrcRegion) -> Node<Self> {
        Node::new(Expr::IfElse(predicate, true_block, false_block), region)
    }

    pub fn while_loop(predicate: Node<Expr>, body: Node<Expr>, region: SrcRegion) -> Node<Self> {
        Node::new(Expr::While(predicate, body), region)
    }

    pub fn func(params: Node<Vec<Node<Interned<String>>>>, body: Node<Expr>, region: SrcRegion) -> Node<Self> {
        Node::new(Expr::Func(params, body), region)
    }

    pub fn call(func: Node<Expr>, args: Node<Vec<Node<Expr>>>, region: SrcRegion) -> Node<Self> {
        Node::new(Expr::Call(func, args), region)
    }

    pub fn index(expr: Node<Expr>, index: Node<Expr>, region: SrcRegion) -> Node<Self> {
        Node::new(Expr::Index(expr, index), region)
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
            Expr::Binary(op, l, r) => {
                println!("Binary Operation: {:?}", **op);
                l.print_debug_depth(ctx, depth + 1);
                r.print_debug_depth(ctx, depth + 1);
            },
            Expr::Mutation(m, l, r) => {
                println!("Mutation Operation: {:?}", **m);
                l.print_debug_depth(ctx, depth + 1);
                r.print_debug_depth(ctx, depth + 1);
            },
            Expr::ThisThen(this, then) => {
                println!("This, Then:");
                this.print_debug_depth(ctx, depth + 1);
                then.print_debug_depth(ctx, depth);
            },
            Expr::Var(ident, expr, then) => {
                println!("Var: {}", ctx.idents.get(**ident));
                expr.print_debug_depth(ctx, depth + 1);
                then.print_debug_depth(ctx, depth);
            },
            Expr::IfElse(predicate, true_block, false_block) => {
                println!("If/Else:");
                predicate.print_debug_depth(ctx, depth + 1);
                true_block.print_debug_depth(ctx, depth + 1);
                false_block.print_debug_depth(ctx, depth + 1);
            },
            Expr::While(predicate, body) => {
                println!("While:");
                predicate.print_debug_depth(ctx, depth + 1);
                body.print_debug_depth(ctx, depth + 1);
            },
            Expr::Func(params, body) => {
                let params = params
                    .iter()
                    .map(|i| ctx.idents.get(**i).to_string())
                    .collect::<Vec<_>>();
                println!("Func: {:?}", params);
                body.print_debug_depth(ctx, depth + 1);
            },
            Expr::Call(func, args) => {
                println!("Call:");
                func.print_debug_depth(ctx, depth + 1);
                for arg in args.iter() {
                    arg.print_debug_depth(ctx, depth + 1);
                }
            },
            Expr::Index(expr, index) => {
                println!("Index:");
                expr.print_debug_depth(ctx, depth + 1);
                index.print_debug_depth(ctx, depth + 1);
            },
            _ => unimplemented!(),
        }
    }

    pub fn print_debug(&self, ctx: &TokenCtx) {
        self.print_debug_depth(ctx, 0);
    }
}

#[derive(Clone, Hash, Eq, PartialEq)]
enum Thing {
    Number,
    String,
    Bool,
    Ident,
    Lexeme(Lexeme),
}

pub struct Error {
    found: Option<Token>,
    expected: HashSet<Thing>,
}

impl ParseError<Token> for Error {
    fn unexpected(token: Token) -> Self {
        Self::found(token, None)
    }

    fn unexpected_end() -> Self {
        Self {
            found: None,
            expected: HashSet::default(),
        }
    }

    fn combine(mut self, mut other: Self) -> Self {
        self.expected = self.expected
            .union(&other.expected)
            .cloned()
            .collect();
        self
    }
}

impl Error {
    pub fn found(found: Token, expected: impl IntoIterator<Item=Thing>) -> Self {
        Self {
            found: Some(found),
            expected: expected.into_iter().collect(),
        }
    }
}

pub fn parse(tokens: &[Token]) -> Result<Node<Expr>, Vec<Error>> {
    let number = try_map(|tok| match tok {
        Token { lexeme: Lexeme::Number(n), region }
            => Ok(Expr::literal(Literal::Number(n), region)),
        token => Err(Error::found(token, Some(Thing::Number))),
    });

    let string = try_map(|tok| match tok {
        Token { lexeme: Lexeme::String(s), region }
            => Ok(Expr::literal(Literal::String(s), region)),
        token => Err(Error::found(token, Some(Thing::String))),
    });

    let boolean = try_map(|tok| match tok {
        Token { lexeme: Lexeme::True, region }
            => Ok(Expr::literal(Literal::Bool(true), region)),
        Token { lexeme: Lexeme::False, region }
            => Ok(Expr::literal(Literal::Bool(false), region)),
        token => Err(Error::found(token, Some(Thing::Bool))),
    });

    let ident = try_map(|tok| match tok {
        Token { lexeme: Lexeme::Ident(i), region }
            => Ok(Node::new(i, region)),
        token => Err(Error::found(token, Some(Thing::Ident))),
    });

    parsers! {
        atom = {
            | number
            | string
            | boolean
            | "null" => { |t: Token| Node::new(Expr::Literal(Literal::Null), t.region) }
            | '(' -& expr &- ')'
            | block
            | flow_control
        }

        access = {
            atom & (
                | '(' -& list &- ')' => { |list: Node<Vec<Node<Expr>>>| (0, list) }
                | '[' -& list &- ']' => { |list| (1, list) }
            )* :> { |a, (kind, list)| {
                let region = list.region.union(a.region);
                match kind {
                    0 => Expr::call(a, list, region),
                    1 => list
                        .into_inner()
                        .into_iter()
                        .fold(a, |a, index| {
                            let region = a.region.union(index.region);
                            Expr::index(a, index, region)
                        }),
                    _ => unreachable!(),
                }
            } }
        }

        unary = {
            (
                | '-' => { |op: Token| (UnaryOp::Neg, op.region) }
                | '!' => { |op: Token| (UnaryOp::Not, op.region) }
            )* & access <: { |(op, op_region), a| {
                let region = op_region.union(a.region);
                Expr::unary(Node::new(op, op_region), a, region)
            } }
        }

        product = {
            unary & ((
                | '*' => { |op: Token| (BinaryOp::Mul, op.region) }
                | '/' => { |op: Token| (BinaryOp::Div, op.region) }
                | '%' => { |op: Token| (BinaryOp::Rem, op.region) }
            ) & unary)* :> { |a: Node<Expr>, ((op, op_region), b)| {
                let region = op_region.union(a.region).union(b.region);
                Expr::binary(Node::new(op, op_region), a, b, region)
            } }
        }

        sum = {
            product & ((
                | '+' => { |op: Token| (BinaryOp::Add, op.region) }
                | '-' => { |op: Token| (BinaryOp::Sub, op.region) }
            ) & product)* :> { |a: Node<Expr>, ((op, op_region), b)| {
                let region = op_region.union(a.region).union(b.region);
                Expr::binary(Node::new(op, op_region), a, b, region)
            } }
        }

        comparison = {
            sum & ((
                | "==" => { |op: Token| (BinaryOp::Eq, op.region) }
                | "!=" => { |op: Token| (BinaryOp::NotEq, op.region) }
                |  "<" => { |op: Token| (BinaryOp::Less, op.region) }
                | "<=" => { |op: Token| (BinaryOp::LessEq, op.region) }
                |  ">" => { |op: Token| (BinaryOp::Greater, op.region) }
                | ">=" => { |op: Token| (BinaryOp::GreaterEq, op.region) }
            ) & sum)* :> { |a: Node<Expr>, ((op, op_region), b)| {
                let region = op_region.union(a.region).union(b.region);
                Expr::binary(Node::new(op, op_region), a, b, region)
            } }
        }

        mutation = {
            comparison & ((
                |  "=" => { |op: Token| (Mutation::Assign, op.region) }
                | "+=" => { |op: Token| (Mutation::AddAssign, op.region) }
                | "-=" => { |op: Token| (Mutation::SubAssign, op.region) }
                | "*=" => { |op: Token| (Mutation::MulAssign, op.region) }
                | "/=" => { |op: Token| (Mutation::DivAssign, op.region) }
                | "%=" => { |op: Token| (Mutation::RemAssign, op.region) }
            ) & comparison)* :> { |a: Node<Expr>, ((op, op_region), b)| {
                let region = op_region.union(a.region).union(b.region);
                Expr::mutation(Node::new(op, op_region), a, b, region)
            } }
        }

        expr = { mutation }

        list = {
            expr ... ',' => { |exprs| {
                let region = exprs
                    .iter()
                    .fold(SrcRegion::none(), |a, e| a.union(e.region));
                Node::new(exprs, region)
            } }
        }

        flow_control = {
            | "if" & expr & block & block? => { |(((fi, pred), tblock), fblock)| {
                let region = fi.region
                    .union(pred.region)
                    .union(tblock.region)
                    .union(fblock.as_ref().map(|b| b.region).unwrap_or(SrcRegion::none()));
                Expr::if_else(
                    pred,
                    tblock,
                    fblock.unwrap_or(Expr::literal(Literal::Null, SrcRegion::none())),
                    region,
                )
            } }
            | "while" & expr & block => { |((elihw, pred), block)| {
                let region = elihw.region
                    .union(pred.region)
                    .union(block.region);
                Expr::while_loop(
                    pred,
                    block,
                    region,
                )
            } }
        }

        statement_chain = {
            | "var" -& ident &- '=' & expr &- ';' & statement_chain => { |((ident, expr), then)| {
                let region = ident.region.union(expr.region);
                Expr::var(ident, expr, then, region)
            } }
            | expr &- ';' & statement_chain => { |(expr, then)| Expr::this_then(expr, then, SrcRegion::none()) }
            | flow_control & statement_chain => { |(expr, then)| Expr::this_then(expr, then, SrcRegion::none()) }
            | { nothing() } => { |_| Expr::literal(Literal::Null, SrcRegion::none()) }
        }

        block = { '{' -& statement_chain &- '}' }
    }

    expr.parse(tokens).map_err(|err| vec![err])
}
