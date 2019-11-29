use std::{
    fmt,
    collections::HashMap,
};
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
    util::{Interned, InternTable},
};

#[derive(Debug)]
pub enum ExecError {
    InvalidOperation,
    NoSuchVar(String),
    NotTruthy,
    NotCallable,
    WrongNumberOfArgs,
    OutOfRange,
    NotAnLValue,
    NoSuchField,
    NotAStructure,
}

#[derive(Clone)]
pub enum Value<'a> {
    String(String),
    Number(f64),
    Bool(bool),
    Null,
    Func(&'a Node<Vec<Node<Interned<String>>>>, &'a Node<Expr>),
    Structure(HashMap<Interned<String>, Value<'a>>),
}

impl<'a> fmt::Debug for Value<'a> {
    fn fmt(&self, mut f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::String(x) => write!(f, "\"{}\"", x),
            Value::Number(x) => write!(f, "{}", x),
            Value::Bool(x) => write!(f, "{}", x),
            Value::Null => write!(f, "null"),
            Value::Func(_, _) => write!(f, "<func>"),
            Value::Structure(_) => write!(f, "<structure>"),
        }
    }
}

impl<'a> Value<'a> {
    pub fn from_literal(l: &Literal, machine: &AbstractMachine<'a>) -> Self {
        match l {
            Literal::String(i) => Value::String(machine.strings.get(*i).clone()),
            Literal::Number(x) => Value::Number(*x),
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

    pub fn apply_div(self, rhs: Self) -> Result<Self, ExecError> {
        match (self, rhs) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a / b)),
            _ => Err(ExecError::InvalidOperation),
        }
    }

    pub fn apply_rem(self, rhs: Self) -> Result<Self, ExecError> {
        match (self, rhs) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a % b)),
            _ => Err(ExecError::InvalidOperation),
        }
    }

    pub fn apply_eq(self, rhs: Self) -> Result<Self, ExecError> {
        match (self, rhs) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Bool(a == b)),
            (Value::String(a), Value::String(b)) => Ok(Value::Bool(a == b)),
            (Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(a == b)),
            (Value::Null, Value::Null) => Ok(Value::Bool(true)),
            _ => Err(ExecError::InvalidOperation),
        }
    }

    pub fn apply_less(self, rhs: Self) -> Result<Self, ExecError> {
        match (self, rhs) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Bool(a < b)),
            _ => Err(ExecError::InvalidOperation),
        }
    }

    pub fn apply_greater(self, rhs: Self) -> Result<Self, ExecError> {
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

    pub fn apply_index(self, rhs: Self) -> Result<Self, ExecError> {
        match (self, rhs) {
            (Value::String(a), Value::Number(b)) => Ok(Value::String(a
                .get(b as usize..b as usize + 1)
                .ok_or(ExecError::OutOfRange)?
                .into(),
            )),
            _ => Err(ExecError::InvalidOperation),
        }
    }

    pub fn field_mut(&mut self, field: &Interned<String>) -> Result<&mut Self, ExecError> {
        match self {
            Value::Structure(fields) => fields
                .get_mut(field)
                .ok_or(ExecError::NoSuchField),
            _ => Err(ExecError::NotAStructure),
        }
    }

    pub fn field(&self, field: &Interned<String>) -> Result<&Self, ExecError> {
        match self {
            Value::Structure(fields) => fields
                .get(field)
                .ok_or(ExecError::NoSuchField),
            _ => Err(ExecError::NotAStructure),
        }
    }
}

pub struct AbstractMachine<'a> {
    strings: InternTable<String>,
    idents: InternTable<String>,
    stack: Vec<Option<(Interned<String>, Value<'a>)>>,
}

impl<'a> AbstractMachine<'a> {
    pub fn new(strings: InternTable<String>, idents: InternTable<String>) -> Self {
        Self {
            strings,
            idents,
            stack: Vec::new(),
        }
    }

    fn value_mut(&mut self, lvalue: &'a Node<Expr>) -> Result<&mut Value<'a>, ExecError> {
        match &**lvalue {
            Expr::Ident(i) => Ok(self.stack
                .iter_mut()
                .rev()
                .take_while(|x| x.is_some())
                .filter_map(|x| x.as_mut())
                .find(|(ident, _)| ident == i)
                .map(|(_, v)| v)
                .ok_or(ExecError::NoSuchVar(self.idents.get(*i).clone()))?),
            Expr::Field(expr, field) => self.value_mut(expr)?.field_mut(&*field),
            _ => Err(ExecError::NotAnLValue)
        }
    }

    fn mutate(&mut self, lvalue: &'a Node<Expr>, mutation: &Mutation, rvalue: Value<'a>) -> Result<(), ExecError> {
        let lvalue = self.value_mut(lvalue)?;
        match mutation {
            Mutation::Assign => lvalue.apply_assign(rvalue),
            Mutation::AddAssign => lvalue.apply_add_assign(rvalue),
            _ => unimplemented!(),
        }
    }

    fn exec(&mut self, expr: &'a Node<Expr>) -> Result<Value<'a>, ExecError> {
        match &**expr {
            Expr::Literal(l) => Ok(Value::from_literal(&l, &self)),
            Expr::Ident(i) => self.stack
                .iter()
                .rev()
                .take_while(|x| x.is_some())
                .filter_map(|x| x.as_ref())
                .find(|(ident, _)| ident == i)
                .map(|(_, v)| v.clone())
                .ok_or(ExecError::NoSuchVar(self.idents.get(*i).clone())),
            Expr::Func(params, body) => Ok(Value::Func(&params, &body)),
            Expr::Structure(fields) => {
                let fields = fields
                    .iter()
                    .map(|(ident, expr)| self.exec(expr)
                        .map(|field| (**ident, field)))
                    .collect::<Result<_, _>>()?;
                Ok(Value::Structure(fields))
            },
            Expr::Unary(op, a) => {
                let a = self.exec(&a)?;
                match &**op {
                    UnaryOp::Not => a.apply_not(),
                    UnaryOp::Neg => a.apply_neg(),
                }
            },
            Expr::Binary(op, a, b) => {
                let a = self.exec(&a)?;
                let b = self.exec(&b)?;
                match &**op {
                    BinaryOp::Add => a.apply_add(b),
                    BinaryOp::Sub => a.apply_sub(b),
                    BinaryOp::Mul => a.apply_mul(b),
                    BinaryOp::Div => a.apply_div(b),
                    BinaryOp::Rem => a.apply_rem(b),
                    BinaryOp::Eq => a.apply_eq(b),
                    BinaryOp::Less => a.apply_less(b),
                    BinaryOp::Greater => a.apply_greater(b),
                    _ => unimplemented!(),
                }
            },
            Expr::Var(ident, expr, tail) => {
                let val = self.exec(&expr)?;
                self.stack.push(Some((**ident, val)));
                let val = self.exec(&tail);
                self.stack.pop();
                val
            },
            Expr::ThisThen(head, tail) => {
                self.exec(&head)?;
                self.exec(&tail)
            },
            Expr::Mutation(m, lvalue, rhs) => {
                let rhs = self.exec(&rhs)?;
                self.mutate(&lvalue, &**m, rhs)?;
                Ok(Value::Null)
            },
            Expr::IfElse(predicate, a, b) => if self.exec(&predicate)?.truth()? {
                self.exec(&a)
            } else {
                self.exec(&b)
            },
            Expr::While(predicate, body) => {
                while self.exec(&predicate)?.truth()? {
                    self.exec(&body)?;
                }
                Ok(Value::Null)
            },
            Expr::Call(func, args) => {
                let func = self.exec(&func)?;

                if let Value::Func(params, body) = func {
                    if params.len() == args.len() {
                        let args = args
                            .iter()
                            .map(|arg| self.exec(arg))
                            .collect::<Result<Vec<_>, _>>()?;
                        self.stack.push(None);
                        for (i, arg) in params.iter().zip(args.into_iter()) {
                            self.stack.push(Some((**i, arg)));
                        }
                        let val = self.exec(&body);
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
            Expr::Index(expr, index) => {
                let expr = self.exec(&expr)?;
                let index = self.exec(&index)?;
                expr.apply_index(index)
            },
            Expr::Field(expr, field) => {
                let expr = self.exec(&expr)?;
                expr.field(&**field).map(|x| x.clone())
            },
            expr => {
                unimplemented!("{:?}", expr)
            },
        }
    }

    pub fn execute(mut self, expr: &'a Node<Expr>) -> Result<Value<'a>, ExecError> {
        self.exec(expr)
    }
}
