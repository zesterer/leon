use std::{
    ops::{Deref, DerefMut},
};
use parze::prelude::*;
use crate::{
    lex::{Lexeme, Token},
    util::SrcRegion,
    Error, Thing,
};
use internment::LocalIntern;

#[derive(Debug)]
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

#[derive(Debug)]
pub enum Literal {
    String(LocalIntern<String>),
    Number(f64),
    Bool(bool),
    Null,
}

impl Literal {
    pub fn as_string(&self) -> String {
        match self {
            Literal::String(i) => i.as_ref().clone(),
            Literal::Number(x) => format!("{}", x),
            Literal::Bool(x) => if *x { "true" } else { "false" }.to_owned(),
            Literal::Null => "null".to_owned(),
        }
    }
}

#[derive(Debug)]
#[repr(u8)]
pub enum UnaryOp {
    Neg,
    Not,

    Clone,
    Mirror,
    Ref,
}

#[derive(Debug)]
#[repr(u8)]
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
#[repr(u8)]
pub enum Mutation {
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    RemAssign,
}

#[derive(Debug)]
pub enum Ty {
    Char,
    Num,
    Str,
}

impl Ty {
    fn print_debug_depth(&self, depth: usize) {
        (0..depth * 2).for_each(|_| print!("  "));
        match self {
            Ty::Char => println!("char"),
            Ty::Num => println!("num"),
            Ty::Str => println!("str"),
        }
    }
}

#[derive(Debug)]
pub enum Expr {
    Literal(Literal),
    Ident(LocalIntern<String>),
    Unary(Node<UnaryOp>, Node<Expr>),
    Binary(Node<BinaryOp>, Node<Expr>, Node<Expr>),
    Var(Node<LocalIntern<String>>, Node<Expr>, Node<Expr>), // let foo = 5; bar
    ThisThen(Node<Expr>, Node<Expr>), // foo; bar
    IfElse(Node<Expr>, Node<Expr>, Node<Expr>),
    While(Node<Expr>, Node<Expr>),
    Mutation(Node<Mutation>, Node<Expr>, Node<Expr>),
    Func(Node<Vec<Node<LocalIntern<String>>>>, Node<Expr>),
    Call(Node<Expr>, Node<Vec<Node<Expr>>>),
    CallMethod(Node<Expr>, Node<LocalIntern<String>>, Node<Vec<Node<Expr>>>),
    Index(Node<Expr>, Node<Expr>),
    Field(Node<Expr>, Node<LocalIntern<String>>),
    Structure(Node<Vec<(Node<LocalIntern<String>>, Node<Expr>)>>),
    List(Node<Vec<Node<Expr>>>),
    ListMany(Node<Expr>, Node<Expr>),
    Convert(Node<Expr>, Node<Ty>),
}

impl Expr {
    pub fn at(self, region: SrcRegion) -> Node<Self> {
        Node::new(self, region)
    }

    pub fn literal(literal: Literal, region: SrcRegion) -> Node<Self> {
        Node::new(Expr::Literal(literal), region)
    }

    pub fn ident(ident: LocalIntern<String>, region: SrcRegion) -> Node<Self> {
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

    pub fn var(ident: Node<LocalIntern<String>>, expr: Node<Expr>, then: Node<Expr>, region: SrcRegion) -> Node<Self> {
        Node::new(Expr::Var(ident, expr, then), region)
    }

    pub fn if_else(predicate: Node<Expr>, true_block: Node<Expr>, false_block: Node<Expr>, region: SrcRegion) -> Node<Self> {
        Node::new(Expr::IfElse(predicate, true_block, false_block), region)
    }

    pub fn while_loop(predicate: Node<Expr>, body: Node<Expr>, region: SrcRegion) -> Node<Self> {
        Node::new(Expr::While(predicate, body), region)
    }

    pub fn func(params: Node<Vec<Node<LocalIntern<String>>>>, body: Node<Expr>, region: SrcRegion) -> Node<Self> {
        Node::new(Expr::Func(params, body), region)
    }

    pub fn call(func: Node<Expr>, args: Node<Vec<Node<Expr>>>, region: SrcRegion) -> Node<Self> {
        Node::new(Expr::Call(func, args), region)
    }

    pub fn index(expr: Node<Expr>, index: Node<Expr>, region: SrcRegion) -> Node<Self> {
        Node::new(Expr::Index(expr, index), region)
    }

    pub fn field(expr: Node<Expr>, field: Node<LocalIntern<String>>, region: SrcRegion) -> Node<Self> {
        Node::new(Expr::Field(expr, field), region)
    }

    pub fn structure(fields: Node<Vec<(Node<LocalIntern<String>>, Node<Expr>)>>, region: SrcRegion) -> Node<Self> {
        Node::new(Expr::Structure(fields), region)
    }

    pub fn list(items: Node<Vec<Node<Expr>>>, region: SrcRegion) -> Node<Self> {
        Node::new(Expr::List(items), region)
    }

    pub fn list_many(item: Node<Expr>, count: Node<Expr>, region: SrcRegion) -> Node<Self> {
        Node::new(Expr::ListMany(item, count), region)
    }

    pub fn convert(expr: Node<Expr>, ty: Node<Ty>, region: SrcRegion) -> Node<Self> {
        Node::new(Expr::Convert(expr, ty), region)
    }

    fn print_debug_depth(&self, depth: usize) {
        (0..depth * 2).for_each(|_| print!("  "));
        match self {
            Expr::Literal(l) => println!("Literal: {}", l.as_string()),
            Expr::Ident(i) => println!("Ident: {}", i),
            Expr::Unary(op, expr) => {
                println!("Unary Operation: {:?}", **op);
                expr.print_debug_depth(depth + 1);
            },
            Expr::Binary(op, l, r) => {
                println!("Binary Operation: {:?}", **op);
                l.print_debug_depth(depth + 1);
                r.print_debug_depth(depth + 1);
            },
            Expr::Mutation(m, l, r) => {
                println!("Mutation Operation: {:?}", **m);
                l.print_debug_depth(depth + 1);
                r.print_debug_depth(depth + 1);
            },
            Expr::ThisThen(this, then) => {
                println!("This, Then:");
                this.print_debug_depth(depth + 1);
                then.print_debug_depth(depth);
            },
            Expr::Var(ident, expr, then) => {
                println!("Var: {}", ident.as_ref());
                expr.print_debug_depth(depth + 1);
                then.print_debug_depth(depth);
            },
            Expr::IfElse(predicate, true_block, false_block) => {
                println!("If/Else:");
                predicate.print_debug_depth(depth + 1);
                true_block.print_debug_depth(depth + 1);
                false_block.print_debug_depth(depth + 1);
            },
            Expr::While(predicate, body) => {
                println!("While:");
                predicate.print_debug_depth(depth + 1);
                body.print_debug_depth(depth + 1);
            },
            Expr::Func(params, body) => {
                let params = params
                    .iter()
                    .map(|i| i.to_owned())
                    .collect::<Vec<_>>();
                println!("Func: {:?}", params);
                body.print_debug_depth(depth + 1);
            },
            Expr::Call(func, args) => {
                println!("Call:");
                func.print_debug_depth(depth + 1);
                for arg in args.iter() {
                    arg.print_debug_depth(depth + 1);
                }
            },
            Expr::Index(expr, index) => {
                println!("Index:");
                expr.print_debug_depth(depth + 1);
                index.print_debug_depth(depth + 1);
            },
            Expr::Field(expr, field) => {
                println!("Field: {}", field.as_ref());
                expr.print_debug_depth(depth + 1);
            },
            Expr::Structure(fields) => {
                for (i, (name, expr)) in fields.iter().enumerate() {
                    if i != 0 {
                        (0..depth * 2).for_each(|_| print!("  "));
                    }
                    println!("Field: {}", name.as_ref());
                    expr.print_debug_depth(depth + 1);
                }
            },
            Expr::List(items) => {
                println!("List:");
                for item in items.iter() {
                    item.print_debug_depth(depth + 1);
                }
            },
            Expr::ListMany(item, count) => {
                println!("ListMany:");
                item.print_debug_depth(depth + 1);
                count.print_debug_depth(depth + 1);
            },
            Expr::Convert(expr, ty) => {
                println!("Convert");
                expr.print_debug_depth(depth + 1);
                ty.print_debug_depth(depth + 1);
            },
            _ => unimplemented!(),
        }
    }

    pub fn print_debug(&self) {
        self.print_debug_depth(0);
    }
}

impl parze::error::Error<Token> for Error {
    type Region = SrcRegion;
    type Thing = Thing;
    type Context = ();

    fn unexpected_sym(token: &Token, region: SrcRegion) -> Self {
        Error::unexpected(Thing::Token(token.clone()))
            .at(region)
    }

    fn unexpected_end() -> Self {
        Error::unexpected_eof()
    }

    fn expected_end(token: &Token, region: SrcRegion) -> Self {
        Error::expected_eof(token.clone()).at(region)
    }
}

pub fn parse(tokens: &[Token]) -> Result<Node<Expr>, Vec<Error>> {
    let expr = declare();
    let block = declare();
    let structure = declare();
    let flow_control = declare();
    let statement_chain = declare();

    let number = permit_map(|tok| match tok {
        Token { lexeme: Lexeme::Number(x), region } => Some(Literal::Number(x.parse().unwrap())),
        token => None,
    });

    let string = permit_map(|tok| match tok {
        Token { lexeme: Lexeme::String(i), .. } => Some(Literal::String(i)),
        token => None,
    });

    let boolean = just(Lexeme::True).to(true)
        .or(just(Lexeme::False).to(false))
        .map(|x| Literal::Bool(x));

    let literal = number
        .or(string)
        .or(boolean)
        .or(just("null").map(|_| Literal::Null))
        .map_with_region(|x, region| Expr::Literal(x).at(region));

    let ident = permit_map(|tok| match tok {
        Token { lexeme: Lexeme::Ident(i), region } => Some(i),
        token => None,
    });

    let ty = just("char").map(|_| Ty::Char)
        .or(just("num").map(|_| Ty::Num))
        .or(just("str").map(|_| Ty::Str))
        .map_with_region(|ty, region| Node::new(ty, region));

    let list = expr
        .link()
        .separated_by(just(','))
        .map_with_region(|items, region| Node::new(items, region));

    let atom = literal
        .or(ident.clone().map_with_region(|x, region| Expr::Ident(x).at(region)))
        .or(just('(').padding_for(expr.link()).padded_by(just(')')))
        .or(just('[')
            .padding_for(expr.link())
            .padded_by(just(';'))
            .then(expr.link())
            .padded_by(just(']'))
            .map_with_region(|(item, count), region| Expr::ListMany(item, count).at(region)))
        .or(just('[')
            .padding_for(list.clone())
            .padded_by(just(']'))
            .map_with_region(|list, region| Expr::List(list).at(region)))
        .or(block.link())
        .or(structure.link())
        .or(flow_control.link())
        .or(just('|')
            .padding_for(ident
                .clone()
                .map_with_region(|ident, region| Node::new(ident, region))
                .separated_by(just(',')))
            .padded_by(just('|'))
            .map_with_region(|params, region| Node::new(params, region))
            .then(expr.link())
            .map_with_region(|(params, body), region| Expr::func(params, body, region)))
        .boxed();

    let access = atom
        // Call
        .then(just('(')
            .padding_for(list.clone())
            .padded_by(just(')'))
            .map(|list| Box::new(|expr: Node<Expr>| {
                let region = expr.region.union(list.region);
                Expr::Call(expr, list).at(region)
            }) as Box<dyn FnOnce(_) -> _>)
            // Index
            .or(just('[')
                .padding_for(list.clone())
                .padded_by(just(']'))
                .map(|list| Box::new(move |expr| list
                    .into_inner()
                    .into_iter()
                    .fold(expr, |expr, index| {
                        let region = index.region;
                        Expr::Index(expr, index).at(region)
                    })) as Box<dyn FnOnce(_) -> _>))
            // Method call
            .or(just('.')
                .padding_for(ident.clone().map_with_region(|ident, region| Node::new(ident, region)))
                .padded_by(just('('))
                .then(list.clone())
                .padded_by(just(')'))
                .map(|(field, args)| Box::new(|expr: Node<Expr>| {
                    let region = expr.region.union(field.region);
                    Expr::CallMethod(expr, field, args).at(region)
                }) as Box<dyn FnOnce(_) -> _>))
            // Field access
            .or(just('.')
                .padding_for(ident.clone().map_with_region(|ident, region| Node::new(ident, region)))
                .map(|field| Box::new(|expr: Node<Expr>| {
                    let region = expr.region.union(field.region);
                    Expr::Field(expr, field).at(region)
                }) as Box<dyn FnOnce(_) -> _>))
            .repeated())
        .reduce_left(|expr, f| f(expr))
        .boxed();

    let unary = just('-').map(|_| UnaryOp::Neg)
        .or(just('!').map(|_| UnaryOp::Not))
        .or(just("clone").map(|_| UnaryOp::Clone))
        .or(just("mirror").map(|_| UnaryOp::Mirror))
        .or(just("ref").map(|_| UnaryOp::Ref))
        .map_with_region(|op, region| Node::new(op, region))
        .repeated()
        .then(access)
        .reduce_right(|op, expr| {
            let region = op.region.union(expr.region);
            Expr::Unary(op, expr).at(region)
        });

    let suffix = unary
        .then(just("as").padding_for(ty).repeated())
        .reduce_left(|expr, ty| {
            let region = expr.region.union(ty.region);
            Expr::Convert(expr, ty).at(region)
        })
        .boxed();

    let product_op = just('*').map(|_| BinaryOp::Mul)
        .or(just('/').map(|_| BinaryOp::Div))
        .or(just('%').map(|_| BinaryOp::Rem))
        .map_with_region(|op, region| Node::new(op, region));
    let product = suffix.clone()
        .then(product_op.then(suffix).repeated())
        .reduce_left(|a, (op, b)| {
            let region = a.region.union(b.region);
            Expr::Binary(op, a, b).at(region)
        });

    let sum_op = just('+').map(|_| BinaryOp::Add)
        .or(just('-').map(|_| BinaryOp::Sub))
        .map_with_region(|op, region| Node::new(op, region));
    let sum = product.clone()
        .then(sum_op.then(product).repeated())
        .reduce_left(|a, (op, b)| {
            let region = a.region.union(b.region);
            Expr::Binary(op, a, b).at(region)
        })
        .boxed();

    let comparison_op = just("==").map(|_| BinaryOp::Eq)
        .or(just("!=").map(|_| BinaryOp::NotEq))
        .or(just("<").map(|_| BinaryOp::Less))
        .or(just("<=").map(|_| BinaryOp::LessEq))
        .or(just(">").map(|_| BinaryOp::Greater))
        .or(just(">=").map(|_| BinaryOp::GreaterEq))
        .map_with_region(|op, region| Node::new(op, region));
    let comparison = sum.clone()
        .then(comparison_op.then(sum).repeated())
        .reduce_left(|a, (op, b)| {
            let region = a.region.union(b.region);
            Expr::Binary(op, a, b).at(region)
        });

    let logical_op = just("and").map(|_| BinaryOp::And)
        .or(just("or").map(|_| BinaryOp::Or))
        .or(just("xor").map(|_| BinaryOp::Xor))
        .map_with_region(|op, region| Node::new(op, region));
    let logical = comparison.clone()
        .then(logical_op.then(comparison).repeated())
        .reduce_left(|a, (op, b)| {
            let region = a.region.union(b.region);
            Expr::Binary(op, a, b).at(region)
        });

    let mutation_op = just("=").map(|_| Mutation::Assign)
        .or(just("+=").map(|_| Mutation::AddAssign))
        .or(just("-=").map(|_| Mutation::SubAssign))
        .or(just("*=").map(|_| Mutation::MulAssign))
        .or(just("/=").map(|_| Mutation::DivAssign))
        .or(just("%=").map(|_| Mutation::RemAssign))
        .map_with_region(|op, region| Node::new(op, region));
    let mutation = logical.clone()
        .then(mutation_op.then(logical).repeated())
        .reduce_left(|a, (op, b)| {
            let region = a.region.union(b.region);
            Expr::Mutation(op, a, b).at(region)
        });

    let expr = expr.define(mutation.boxed());

    let flow_control_p = flow_control.link();
    let flow_control = flow_control.define(block.link()
        .or(just("if")
            .padding_for(expr.clone())
            .then(block.link())
            .then(just("else")
                .padding_for(flow_control_p.clone())
                .or_not())
            .map_with_region(|((pred, t_block), f_block), region| {
                Expr::IfElse(pred, t_block, f_block.unwrap_or(Expr::Literal(Literal::Null).at(region)))
                    .at(region)
            }))
        .or(just("while")
            .padding_for(expr.clone())
            .then(block.link())
            .map_with_region(|(pred, block), region| Expr::While(pred, block).at(region))))
        .boxed();

    let statement_chain_p = statement_chain.link();
    let statement_chain = statement_chain.define(just("var")
            .padding_for(ident.clone().map_with_region(|ident, region| Node::new(ident, region)))
            .padded_by(just('='))
            .then(expr.clone())
            .padded_by(just(';'))
            .then(statement_chain_p.clone().or_not())
            .map_with_region(|((ident, expr), then), region|
                Expr::Var(ident, expr, then.unwrap_or(Expr::Literal(Literal::Null).at(region)))
                    .at(region))
        .or(flow_control.clone()
            .then(statement_chain_p.clone().or_not())
            .map_with_region(|(expr, then), region| Expr::ThisThen(expr, then.unwrap_or(Expr::Literal(Literal::Null).at(region)))
                .at(region)))
        .or(expr.clone()
            .then(just(';').padding_for(statement_chain_p.clone().or_not()))
            .map_with_region(|(expr, then), region| Expr::ThisThen(expr, then.unwrap_or(Expr::Literal(Literal::Null).at(region))).at(region)))
        .or(expr.clone()))
        .boxed();

    let block = block.define(just('{')
        .padding_for(statement_chain.clone())
        .padded_by(just('}')));

    let field = ident.clone()
        .map_with_region(|ident, region| Node::new(ident, region))
        .padded_by(just(':'))
        .then(expr.clone());
    let field_list = field
        .separated_by(just(','));
    let structure = structure.define(just('{')
        .padding_for(field_list.map_with_region(|fields, region| Node::new(fields, region)))
        .padded_by(just('}'))
        .map_with_region(|fields, region| {
            Expr::Structure(fields).at(region)
        }))
        .boxed();

    statement_chain.padded_by(end()).parse(tokens.iter().cloned())
}
