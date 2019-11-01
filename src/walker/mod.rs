use std::fmt;
use crate::{
    lex::TokenCtx,
    parse::{
        Node,
        Expr,
        Literal,
        UnaryOp,
        BinaryOp,
        Mutation,
    },
    util::Interned,
};

#[derive(Debug)]
pub enum ExecError {
    InvalidOperation,
    NoSuchVar(String),
    NotTruthy,
    NotCallable,
    WrongNumberOfArgs,
}

#[derive(Clone)]
pub enum Value<'a> {
    String(String),
    Number(f64),
    Bool(bool),
    Null,
    Func(&'a Node<Vec<Node<Interned<String>>>>, &'a Node<Expr>),
}

impl<'a> fmt::Debug for Value<'a> {
    fn fmt(&self, mut f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::String(x) => write!(f, "\"{}\"", x),
            Value::Number(x) => write!(f, "{}", x),
            Value::Bool(x) => write!(f, "{}", x),
            Value::Null => write!(f, "null"),
            Value::Func(_, _) => write!(f, "<func>"),
        }
    }
}

impl<'a> Value<'a> {
    pub fn from_literal(l: &Literal, ctx: &TokenCtx) -> Self {
        match l {
            Literal::String(i) => Value::String(ctx.strings.get(*i).clone()),
            Literal::Number(i) => Value::Number(ctx.numbers.get(*i).parse().unwrap()),
            Literal::Bool(x) => Value::Bool(*x),
            Literal::Null => Value::Null,
        }
    }

    pub fn truth(&self) -> Result<bool, ExecError> {
        match self {
            Value::Bool(x) => Ok(*x),
            _ => Err(ExecError::NotTruthy),
        }
    }

    pub fn apply_not(self) -> Result<Self, ExecError> {
        match self {
            Value::Bool(a) => Ok(Value::Bool(!a)),
            _ => Err(ExecError::InvalidOperation),
        }
    }

    pub fn apply_neg(self) -> Result<Self, ExecError> {
        match self {
            Value::Number(a) => Ok(Value::Number(-a)),
            _ => Err(ExecError::InvalidOperation),
        }
    }

    pub fn apply_add(self, rhs: Self) -> Result<Self, ExecError> {
        match (self, rhs) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a + b)),
            (Value::String(mut a), Value::String(b)) => {
                a += &b;
                Ok(Value::String(a))
            },
            _ => Err(ExecError::InvalidOperation),
        }
    }

    pub fn apply_sub(self, rhs: Self) -> Result<Self, ExecError> {
        match (self, rhs) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a - b)),
            _ => Err(ExecError::InvalidOperation),
        }
    }

    pub fn apply_mul(self, rhs: Self) -> Result<Self, ExecError> {
        match (self, rhs) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a * b)),
            _ => Err(ExecError::InvalidOperation),
        }
    }

    pub fn apply_less(self, rhs: Self) -> Result<Self, ExecError> {
        match (self, rhs) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Bool(a < b)),
            _ => Err(ExecError::InvalidOperation),
        }
    }

    pub fn apply_assign(&mut self, rhs: Self) -> Result<(), ExecError> {
        *self = rhs;
        Ok(())
    }

    pub fn apply_add_assign(&mut self, rhs: Self) -> Result<(), ExecError> {
        match (self, &rhs) {
            (Value::Number(a), Value::Number(b)) => {
                *a += b;
                Ok(())
            },
            (Value::String(a), Value::String(b)) => {
                *a += &b;
                Ok(())
            },
            _ => Err(ExecError::InvalidOperation),
        }
    }
}

#[derive(Default)]
pub struct AbstractMachine<'a> {
    stack: Vec<Option<(Interned<String>, Value<'a>)>>,
}

impl<'a> AbstractMachine<'a> {
    fn fetch_lvalue(&mut self, lvalue: &'a Node<Expr>, ctx: &TokenCtx) -> Result<&mut Value<'a>, ExecError> {
        match &**lvalue {
            Expr::Ident(i) => self.stack
                .iter_mut()
                .rev()
                .take_while(|x| x.is_some())
                .filter_map(|x| x.as_mut())
                .find(|(ident, _)| ident == i)
                .map(|(_, v)| v)
                .ok_or(ExecError::NoSuchVar(ctx.idents.get(*i).clone())),
            _ => unimplemented!(),
        }
    }

    fn exec(&mut self, expr: &'a Node<Expr>, ctx: &TokenCtx) -> Result<Value<'a>, ExecError> {
        match &**expr {
            Expr::Literal(l) => Ok(Value::from_literal(&l, ctx)),
            Expr::Ident(i) => self.stack
                .iter()
                .rev()
                .take_while(|x| x.is_some())
                .filter_map(|x| x.as_ref())
                .find(|(ident, _)| ident == i)
                .map(|(_, v)| v.clone())
                .ok_or(ExecError::NoSuchVar(ctx.idents.get(*i).clone())),
            Expr::Func(params, body) => Ok(Value::Func(&params, &body)),
            Expr::Unary(op, a) => {
                let a = self.exec(&a, ctx)?;
                match &**op {
                    UnaryOp::Not => a.apply_not(),
                    UnaryOp::Neg => a.apply_neg(),
                }
            },
            Expr::Binary(op, a, b) => {
                let a = self.exec(&a, ctx)?;
                let b = self.exec(&b, ctx)?;
                match &**op {
                    BinaryOp::Add => a.apply_add(b),
                    BinaryOp::Sub => a.apply_sub(b),
                    BinaryOp::Mul => a.apply_mul(b),
                    BinaryOp::Less => a.apply_less(b),
                    _ => unimplemented!(),
                }
            },
            Expr::Var(ident, expr, tail) => {
                let val = self.exec(&expr, ctx)?;
                self.stack.push(Some((**ident, val)));
                let val = self.exec(&tail, ctx);
                self.stack.pop();
                val
            },
            Expr::ThisThen(head, tail) => {
                self.exec(&head, ctx)?;
                self.exec(&tail, ctx)
            },
            Expr::Mutation(m, lvalue, rhs) => {
                let rhs = self.exec(&rhs, ctx)?;
                let lvalue = self.fetch_lvalue(&lvalue, ctx)?;
                match &**m {
                    Mutation::Assign => lvalue.apply_assign(rhs)?,
                    Mutation::AddAssign => lvalue.apply_add_assign(rhs)?,
                    _ => unimplemented!(),
                }
                Ok(Value::Null)
            },
            Expr::IfElse(predicate, a, b) => if self.exec(&predicate, ctx)?.truth()? {
                self.exec(&a, ctx)
            } else {
                self.exec(&b, ctx)
            },
            Expr::While(predicate, body) => {
                while self.exec(&predicate, ctx)?.truth()? {
                    self.exec(&body, ctx)?;
                }
                Ok(Value::Null)
            },
            Expr::Call(func, args) => {
                let func = self.exec(&func, ctx)?;

                if let Value::Func(params, body) = func {
                    if params.len() == args.len() {
                        let args = args
                            .iter()
                            .map(|arg| self.exec(arg, ctx))
                            .collect::<Result<Vec<_>, _>>()?;
                        self.stack.push(None);
                        for (i, arg) in params.iter().zip(args.into_iter()) {
                            self.stack.push(Some((**i, arg)));
                        }
                        let val = self.exec(&body, ctx);
                        for _ in 0..params.len() {
                            self.stack.pop();
                        }
                        self.stack.pop();
                        val
                    } else {
                        Err(ExecError::WrongNumberOfArgs)
                    }
                } else {
                    Err(ExecError::NotCallable)
                }
            },
            expr => {
                expr.print_debug(ctx);
                unimplemented!()
            },
        }
    }

    pub fn execute(mut self, expr: &'a Node<Expr>, ctx: &TokenCtx) -> Result<Value<'a>, ExecError> {
        self.exec(expr, ctx)
    }
}
