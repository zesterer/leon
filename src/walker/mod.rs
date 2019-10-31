use crate::{
    lex::TokenCtx,
    parse::{
        Node,
        Expr,
        Literal,
        UnaryOp,
    },
    util::Interned,
};

#[derive(Debug)]
pub enum ExecError {
    InvalidOperation,
}

#[derive(Debug)]
pub enum Value {
    String(String),
    Number(f64),
    Bool(bool),
    Null,
}

impl Value {
    pub fn from_literal(l: &Literal, ctx: &TokenCtx) -> Self {
        match l {
            Literal::String(i) => Value::String(ctx.strings.get(*i).clone()),
            Literal::Number(i) => Value::Number(ctx.numbers.get(*i).parse().unwrap()),
            Literal::Bool(x) => Value::Bool(*x),
            Literal::Null => Value::Null,
        }
    }

    pub fn apply_not(&self) -> Result<Self, ExecError> {
        match self {
            Value::Bool(x) => Ok(Value::Bool(!*x)),
            _ => Err(ExecError::InvalidOperation),
        }
    }

    pub fn apply_neg(&self) -> Result<Self, ExecError> {
        match self {
            Value::Number(x) => Ok(Value::Number(-*x)),
            _ => Err(ExecError::InvalidOperation),
        }
    }
}

#[derive(Default)]
pub struct AbstractMachine {
    stack: Vec<(Interned<String>, Value)>,
}

impl AbstractMachine {
    fn exec(&mut self, expr: &Node<Expr>, ctx: &TokenCtx) -> Result<Value, ExecError> {
        match &**expr {
            Expr::Literal(l) => Ok(Value::from_literal(&l, ctx)),
            Expr::Unary(op, a) => {
                let a = self.exec(&a, ctx)?;
                match &**op {
                    UnaryOp::Not => a.apply_not(),
                    UnaryOp::Neg => a.apply_neg(),
                }
            },
            _ => unimplemented!(),
        }
    }

    pub fn execute(mut self, expr: &Node<Expr>, ctx: &TokenCtx) -> Result<Value, ExecError> {
        self.exec(expr, ctx)
    }
}
