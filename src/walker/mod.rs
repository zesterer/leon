mod heap;
mod value;

use std::{
    fmt,
    rc::Rc,
    cell::{RefCell, Ref},
    collections::HashMap,
};
use crate::{
    lex::TokenCtx,
    parse::{
        Node,
        Expr,
        Ty,
        Literal,
        UnaryOp,
        BinaryOp,
        Mutation,
    },
    util::{Interned, InternTable},
    object::{self, Object},
    box_val::BoxVal,
};

#[derive(Clone, Debug)]
pub enum ExecError {
    InvalidOperation,
    // TODO: merge with invalid operation?
    InvalidObjOperation(String),
    NoSuchVar(String),
    NotTruthy,
    NotCallable,
    WrongNumberOfArgs,
    OutOfRange,
    NotAnLValue,
    NoSuchField,
    NotAStructure,
    InvalidIndex,
    InvalidLength,
}

#[derive(Clone)]
pub enum Value<'a> {
    String(String),
    Number(f64),
    Bool(bool),
    Null,
    Func(&'a Node<Vec<Node<Interned<String>>>>, &'a Node<Expr>),
    Structure(HashMap<Interned<String>, Self>),
    Ref(Rc<RefCell<Self>>),
    List(Vec<Self>),
    Custom(BoxVal<dyn Object>),
}

impl<'a> fmt::Debug for Value<'a> {
    fn fmt(&self, mut f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::String(x) => write!(f, "\"{}\"", x),
            Value::Number(x) => write!(f, "{}", x),
            Value::Bool(x) => write!(f, "{}", x),
            Value::Null => write!(f, "null"),
            Value::Func(_, _) => write!(f, "<func>"),
            Value::Structure(fields) => write!(f, "{}", fields
                .iter()
                .map(|(name, val)| format!("{:?}: {:?}", name, val))
                .collect::<Vec<_>>()
                .join(", ")),
            Value::Ref(val) => write!(f, "ref {:?}", *val.borrow()),
            Value::List(items) => write!(f, "{:?}", items),
        }
    }
}

impl<'a> Value<'a> {
    fn from_literal(l: &Literal, machine: &AbstractMachine<'a>) -> Self {
        match l {
            Literal::String(i) => Value::String(machine.strings.get(*i).clone()),
            Literal::Number(x) => Value::Number(*x),
            Literal::Bool(x) => Value::Bool(*x),
            Literal::Null => Value::Null,
        }
    }

    fn truth(&self) -> Result<bool, ExecError> {
        match self {
            Value::Bool(x) => Ok(*x),
            Value::Custom(x) => Ok(x.truth()),
            _ => Err(ExecError::NotTruthy),
        }
    }

    fn apply_not(self) -> Result<Self, ExecError> {
        match self {
            Value::Bool(a) => Ok(Value::Bool(!a)),
            Value::Custom(a) => Ok(a.not()),
            _ => Err(ExecError::InvalidOperation),
        }
    }

    fn apply_neg(self) -> Result<Self, ExecError> {
        match self {
            Value::Number(a) => Ok(Value::Number(-a)),
            Value::Custom(a) => Ok(a.neg()),
            _ => Err(ExecError::InvalidOperation),
        }
    }

    fn apply_add(self, rhs: Self) -> Result<Self, ExecError> {
        match (self, rhs) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a + b)),
            (Value::String(mut a), Value::String(b)) => {
                a += &b;
                Ok(Value::String(a))
            },
            (Value::List(mut a), Value::List(mut b)) => {
                a.append(&mut b);
                Ok(Value::List(a))
            },
            (Value::List(mut a), b) => {
                a.push(b);
                Ok(Value::List(a))
            },
            (Value::Custom(a), b) => Ok(a.add(b)),
            _ => Err(ExecError::InvalidOperation),
        }
    }

    fn apply_sub(self, rhs: Self) -> Result<Self, ExecError> {
        match (self, rhs) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a - b)),
            (Value::Custom(a), b) => Ok(a.sub(b)),
            _ => Err(ExecError::InvalidOperation),
        }
    }

    fn apply_mul(self, rhs: Self) -> Result<Self, ExecError> {
        match (self, rhs) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a * b)),
            (Value::Custom(a), b) => Ok(a.mul(b)),
            _ => Err(ExecError::InvalidOperation),
        }
    }

    fn apply_div(self, rhs: Self) -> Result<Self, ExecError> {
        match (self, rhs) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a / b)),
            (Value::Custom(a), b) => Ok(a.div(b)),
            _ => Err(ExecError::InvalidOperation),
        }
    }

    fn apply_rem(self, rhs: Self) -> Result<Self, ExecError> {
        match (self, rhs) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a % b)),
            (Value::Custom(a), b) => Ok(a.rem(b)),
            _ => Err(ExecError::InvalidOperation),
        }
    }

    fn apply_eq(self, rhs: Self) -> Result<Self, ExecError> {
        match (self, rhs) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Bool(a == b)),
            (Value::String(a), Value::String(b)) => Ok(Value::Bool(a == b)),
            (Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(a == b)),
            (Value::Null, Value::Null) => Ok(Value::Bool(true)),
            (Value::Custom(a), b) => Ok(a.eq(b)),
            _ => Err(ExecError::InvalidOperation),
        }
    }

    fn apply_not_eq(self, rhs: Self) -> Result<Self, ExecError> {
        match (self, rhs) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Bool(a != b)),
            (Value::String(a), Value::String(b)) => Ok(Value::Bool(a != b)),
            (Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(a != b)),
            (Value::Null, Value::Null) => Ok(Value::Bool(false)),
            (Value::Custom(a), b) => Ok(!a.eq(b)),
            _ => Err(ExecError::InvalidOperation),
        }
    }

    fn apply_less(self, rhs: Self) -> Result<Self, ExecError> {
        match (self, rhs) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Bool(a < b)),
            (Value::Custom(a), b) => Ok(a.less(b)),
            _ => Err(ExecError::InvalidOperation),
        }
    }

    fn apply_greater(self, rhs: Self) -> Result<Self, ExecError> {
        match (self, rhs) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Bool(a > b)),
            (Value::Custom(a), b) => Ok(a.greater(b)),
            _ => Err(ExecError::InvalidOperation),
        }
    }

    fn apply_and(self, rhs: Self) -> Result<Self, ExecError> {
        match (self, rhs) {
            (Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(a && b)),
            _ => Err(ExecError::InvalidOperation),
        }
    }

    fn apply_or(self, rhs: Self) -> Result<Self, ExecError> {
        match (self, rhs) {
            (Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(a || b)),
            _ => Err(ExecError::InvalidOperation),
        }
    }

    fn apply_xor(self, rhs: Self) -> Result<Self, ExecError> {
        match (self, rhs) {
            (Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(a != b)),
            _ => Err(ExecError::InvalidOperation),
        }
    }

    fn apply_assign(&mut self, rhs: Self) -> Result<(), ExecError> {
        *self = rhs;
        Ok(())
    }

    fn apply_add_assign(&mut self, mut rhs: Self) -> Result<(), ExecError> {
        match (self, &mut rhs) {
            (Value::Number(a), Value::Number(b)) => {
                *a += *b;
                Ok(())
            },
            (Value::String(a), Value::String(b)) => {
                *a += &b;
                Ok(())
            },
            (Value::List(a), Value::List(b)) => {
                a.append(b);
                Ok(())
            },
            (Value::List(a), _) => {
                a.push(rhs);
                Ok(())
            (Value::Custom(a), b) => {
                *self = a.add(b);
                Ok(())
            },
            _ => Err(ExecError::InvalidOperation),
        }
    }

    fn apply_sub_assign(&mut self, mut rhs: Self) -> Result<(), ExecError> {
        match (self, &mut rhs) {
            (Value::Number(a), Value::Number(b)) => {
                *a -= *b;
                Ok(())
            },
            _ => Err(ExecError::InvalidOperation),
        }
    }

    fn apply_index(self, rhs: Self) -> Result<Self, ExecError> {
        match (self, rhs) {
            (Value::String(a), Value::Number(b)) => Ok(Value::String(a
                .get(b as usize..b as usize + 1)
                .ok_or(ExecError::OutOfRange)?
                .into(),
            )),
            (Value::List(a), Value::Number(b)) => a
                .get(b as usize)
                .cloned()
                .ok_or(ExecError::OutOfRange),
            (Value::Custom(a), b) => Ok(a.index(b)),
            _ => Err(ExecError::InvalidOperation),
        }
    }

    fn field_mutate(&mut self, field: &Interned<String>, f: impl FnOnce(&mut Self) -> Result<(), ExecError>) -> Result<(), ExecError> {
        match self {
            Value::Structure(fields) => f(fields
                .get_mut(field)
                .ok_or(ExecError::NoSuchField)?),
            Value::Ref(val) => val.borrow_mut().field_mutate(field, f),
            _ => Err(ExecError::NotAStructure),
        }
    }

    fn index_mutate(&mut self, index: Self, f: impl FnOnce(&mut Self) -> Result<(), ExecError>) -> Result<(), ExecError> {
        match self {
            Value::List(items) => f(items
                .get_mut(match index {
                    Value::Number(x) => x as usize,
                    _ => return Err(ExecError::InvalidIndex),
                })
                .ok_or(ExecError::OutOfRange)?),
            Value::Ref(val) => val.borrow_mut().index_mutate(index, f),
            _ => Err(ExecError::NotAStructure),
        }
    }

    fn field_and_receiver(&self, field: Interned<String>, special: &SpecialIdents) -> Result<(Option<Self>, Self), ExecError> {
        match self {
            Value::List(items) if field == special.len => Ok((None, Value::Number(items.len() as f64))),
            Value::Structure(fields) => Ok((None, fields
                .get(&field)
                .cloned()
                .ok_or(ExecError::NoSuchField)?)),
            Value::Ref(val) => Ok((Some(self.clone()), val.borrow().field_and_receiver(field, special)?.1)),
            val => Err(ExecError::NotAStructure),
        }
    }

    fn into_ref(self) -> Result<Self, ExecError> {
        // What if it is already Value::Ref?
        Ok(Value::Ref(Rc::new(RefCell::new(self))))
    }

    fn convert(self, ty: &Ty) -> Result<Self, ExecError> {
        match (self, ty) {
            (Value::Number(x), Ty::Char) => Ok(Value::String((x as u8 as char).to_string())),
            _ => Err(ExecError::InvalidOperation),
        }
    }
}

struct SpecialIdents {
    _self: Interned<String>,
    len: Interned<String>,
}

pub struct AbstractMachine<'a> {
    special: SpecialIdents,
    strings: InternTable<String>,
    idents: InternTable<String>,
    stack: Vec<Option<(Interned<String>, Value<'a>)>>,
}

impl<'a> AbstractMachine<'a> {
    pub fn new(strings: InternTable<String>, mut idents: InternTable<String>) -> Self {

        Self {
            special: SpecialIdents {
                _self: idents.intern("self".to_string()),
                len: idents.intern("len".to_string()),
            },
            strings,
            idents,
            stack: Vec::new(),
        }
    }

    fn value_mut_with<'b>(&'b mut self, lvalue: &'a Node<Expr>, f: Box<dyn FnOnce(&mut Value<'a>) -> Result<(), ExecError> + 'b>) -> Result<(), ExecError> {
        match &**lvalue {
            Expr::Ident(i) => f(self.stack
                .iter_mut()
                .rev()
                .take_while(|x| x.is_some())
                .filter_map(|x| x.as_mut())
                .find(|(ident, _)| ident == i)
                .map(|(_, v)| v)
                .ok_or(ExecError::NoSuchVar(self.idents.get(*i).clone()))?),
            Expr::Field(expr, field) => {
                self.value_mut_with(expr, Box::new(|val| val.field_mutate(&*field, f)))
            },
            Expr::Index(expr, index) => {
                let index = self.exec(index)?.1;
                self.value_mut_with(expr, Box::new(|val| val.index_mutate(index, f)))
            },
            _ => Err(ExecError::NotAnLValue)
        }
    }

    fn mutate(&mut self, lvalue: &'a Node<Expr>, mutation: &Mutation, rvalue: Value<'a>) -> Result<(), ExecError> {
        let lvalue = self.value_mut_with(lvalue, Box::new(|lvalue| {
            match mutation {
                Mutation::Assign => lvalue.apply_assign(rvalue),
                Mutation::AddAssign => lvalue.apply_add_assign(rvalue),
                Mutation::SubAssign => lvalue.apply_sub_assign(rvalue),
                m => unimplemented!("{:?}", m),
            }
        }))?;
        Ok(())
    }

    fn exec(&mut self, expr: &'a Node<Expr>) -> Result<(Option<Value<'a>>, Value<'a>), ExecError> {
        let val = match &**expr {
            Expr::Literal(l) => Value::from_literal(&l, &self),
            Expr::Ident(i) => self.stack
                .iter()
                .rev()
                .take_while(|x| x.is_some())
                .filter_map(|x| x.as_ref())
                .find(|(ident, _)| ident == i)
                .map(|(_, v)| v.clone())
                .ok_or(ExecError::NoSuchVar(self.idents.get(*i).clone()))?,
            Expr::Func(params, body) => Value::Func(&params, &body),
            Expr::Structure(fields) => {
                let fields = fields
                    .iter()
                    .map(|(ident, expr)| Ok((**ident, self.exec(expr)?.1)))
                    .collect::<Result<_, _>>()?;
                Value::Structure(fields).into_ref()?
            },
            Expr::List(items) => {
                let items = items
                    .iter()
                    .map(|item| Ok(self.exec(item)?.1))
                    .collect::<Result<_, _>>()?;
                Value::List(items)
            },
            Expr::ListMany(item, count) => {
                let count = self.exec(count)?.1;
                let item = self.exec(item)?.1;
                match count {
                    Value::Number(x) => Value::List(vec![item; x as usize]),
                    _ => return Err(ExecError::InvalidLength),
                }
            },
            Expr::Unary(op, a) => {
                let a = self.exec(&a)?.1;
                match &**op {
                    UnaryOp::Not => a.apply_not(),
                    UnaryOp::Neg => a.apply_neg(),
                }?
            },
            Expr::Binary(op, a, b) => {
                let a = self.exec(&a)?.1;
                let b = self.exec(&b)?.1;
                match &**op {
                    BinaryOp::Add => a.apply_add(b),
                    BinaryOp::Sub => a.apply_sub(b),
                    BinaryOp::Mul => a.apply_mul(b),
                    BinaryOp::Div => a.apply_div(b),
                    BinaryOp::Rem => a.apply_rem(b),
                    BinaryOp::Eq => a.apply_eq(b),
                    BinaryOp::NotEq => a.apply_not_eq(b),
                    BinaryOp::Less => a.apply_less(b),
                    BinaryOp::Greater => a.apply_greater(b),
                    BinaryOp::And => a.apply_and(b),
                    BinaryOp::Or => a.apply_or(b),
                    BinaryOp::Xor => a.apply_xor(b),
                    op => unimplemented!("{:?}", op),
                }?
            },
            Expr::Var(ident, expr, tail) => {
                let val = self.exec(&expr)?.1;
                self.stack.push(Some((**ident, val)));
                let val = self.exec(&tail)?.1;
                self.stack.pop();
                val
            },
            Expr::ThisThen(head, tail) => {
                self.exec(&head)?;
                self.exec(&tail)?.1
            },
            Expr::Mutation(m, lvalue, rhs) => {
                let rhs = self.exec(&rhs)?.1;
                self.mutate(&lvalue, &**m, rhs)?;
                Value::Null
            },
            Expr::IfElse(predicate, a, b) => if self.exec(&predicate)?.1.truth()? {
                self.exec(&a)?.1
            } else {
                self.exec(&b)?.1
            },
            Expr::While(predicate, body) => {
                while self.exec(&predicate)?.1.truth()? {
                    self.exec(&body)?;
                }
                Value::Null
            },
            Expr::Call(func, args) => {
                let (receiver, func) = self.exec(&func)?;

                if let Value::Func(params, body) = func {
                    if params.len() == args.len() {
                        let args = args
                            .iter()
                            .map(|arg| self.exec(arg))
                            .collect::<Result<Vec<_>, _>>()?;
                        self.stack.push(None);
                        self.stack.push(Some((self.special._self, receiver.unwrap_or(Value::Null))));
                        for (i, arg) in params.iter().zip(args.into_iter()) {
                            self.stack.push(Some((**i, arg.1)));
                        }
                        let val = self.exec(&body)?.1;
                        for _ in 0..params.len() {
                            self.stack.pop();
                        }
                        self.stack.pop();
                        self.stack.pop();
                        val
                    } else {
                        return Err(ExecError::WrongNumberOfArgs);
                    }
                } else {
                    return Err(ExecError::NotCallable);
                }
            },
            Expr::Index(expr, index) => {
                let expr = self.exec(&expr)?.1;
                let index = self.exec(&index)?.1;
                expr.apply_index(index)?
            },
            Expr::Convert(expr, ty) => {
                let expr = self.exec(&expr)?.1;
                expr.convert(&*ty)?
            },
            Expr::Field(expr, field) => {
                let expr = self.exec(&expr)?.1;
                return expr.field_and_receiver(**field, &self.special).map(|x| x.clone())
            },
            expr => {
                unimplemented!("{:?}", expr)
            },
        };

        Ok((None, val))
    }

    pub fn execute(mut self, expr: &'a Node<Expr>) -> Result<Value<'a>, ExecError> {
        Ok(self.exec(expr)?.1)
    }
}
