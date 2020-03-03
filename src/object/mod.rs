use std::any::Any;
use crate::walker::{Value, ExecError};

pub struct InvalidOperation(pub String);
impl From<String> for InvalidOperation {
    fn from(string: String) -> Self {
        Self(string)
    }
}
impl From<&str> for InvalidOperation {
    fn from(string: &str) -> Self {
        Self(string.to_owned())
    }
}

pub trait ObjectHelper {
    fn as_any_impl(&self) -> &dyn Any;
    fn cloned_impl(&self) -> Box<dyn Object>;
}

impl<T: Object + Clone + 'static> ObjectHelper for T {
    fn as_any_impl(&self) -> &dyn Any { self }
    fn cloned_impl(&self) -> Box<dyn Object> { Box::new(self.clone()) }
}

#[allow(unused)]
pub trait Object: std::fmt::Debug + ObjectHelper + 'static {
    fn as_any(&self) -> &dyn Any { self.as_any_impl() }
    fn cloned(&self) -> Box<dyn Object> { self.cloned_impl() }
    fn call<'a>(&self, args: &[Value<'a>]) -> Result<Value<'a>, InvalidOperation> { Err("Not callable!".into()) }
    fn call_method<'a>(&self, method: &str, args: &[Value<'a>]) -> Result<Value<'a>, InvalidOperation> { Err("Not callable!".into()) }
    fn truth(&self) -> Result<bool, InvalidOperation> { Err("Not truthable!".into()) }
    fn not<'a>(&self) -> Result<Value<'a>, InvalidOperation> { Err("Not notable!".into()) }
    fn neg<'a>(&self) -> Result<Value<'a>, InvalidOperation> { Err("Not negativeable!".into()) }
    fn add<'a>(&self, rhs: &Value<'a>) -> Result<Value<'a>, InvalidOperation> { Err("Not addable!".into()) }
    fn sub<'a>(&self, rhs: &Value<'a>) -> Result<Value<'a>, InvalidOperation> { Err("Not subable!".into()) }
    fn add_assign<'a>(&mut self, rhs: &Value<'a>) -> Result<(), InvalidOperation> { Err("Not add assignable!".into()) }
    fn sub_assign<'a>(&mut self, rhs: &Value<'a>) -> Result<(), InvalidOperation> { Err("Not sub assignable!".into()) }
    fn mul<'a>(&self, rhs: &Value<'a>) -> Result<Value<'a>, InvalidOperation> { Err("Not multiplyable!".into()) }
    fn div<'a>(&self, rhs: &Value<'a>) -> Result<Value<'a>, InvalidOperation> { Err("Not dividable!".into()) }
    fn rem<'a>(&self, rhs: &Value<'a>) -> Result<Value<'a>, InvalidOperation> { Err("Not remainderable!".into()) }
    fn eq<'a>(&self, rhs: &Value<'a>) -> Result<bool, InvalidOperation> { Err("Not equalable!".into()) }
    fn less<'a>(&self, rhs: &Value<'a>) -> Result<bool, InvalidOperation> { Err("Not lessable!".into()) }
    fn greater<'a>(&self, rhs: &Value<'a>) -> Result<bool, InvalidOperation> { Err("Not greaterable!".into()) }
    fn and<'a>(&self, rhs: &Value<'a>) -> Result<Value<'a>, InvalidOperation> { Err("Not andable!".into()) }
    fn or<'a>(&self, rhs: &Value<'a>) -> Result<Value<'a>, InvalidOperation> { Err("Not orable!".into()) }
    fn xor<'a>(&self, rhs: &Value<'a>) -> Result<Value<'a>, InvalidOperation> { Err("Not xorable!".into()) }
    fn index<'a>(&self, index: &Value<'a>) -> Result<Value<'a>, InvalidOperation> { Err("Not indexable!".into()) }
    fn index_mutate<'a, 'b>(&mut self, index: &Value, f: Box<dyn FnOnce(&mut Value<'a>) -> Result<(), ExecError> +'b>) -> Result<(), ExecError> { Err(InvalidOperation("Not index mutable!".into()).into()) }
    fn field<'a>(&self, field: &str) -> Result<Value<'a>, InvalidOperation> { Err("Not fieldable!".into()) }
    fn field_mutate<'a, 'b>(&self, field: &str, f: Box<dyn FnOnce(&mut Value<'a>) -> Result<(), ExecError> +'b>) -> Result<(), ExecError> { Err(InvalidOperation("Not field mutable!".into()).into()) }
}
