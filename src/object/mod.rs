use crate::walker::Value;

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

pub trait Object: std::fmt::Debug + std::any::Any {
    fn cloned(&self) -> Box<dyn Object>;
    fn call<'a>(&self, args: &[Value<'a>]) -> Result<Value<'a>, InvalidOperation> { Err("Not callable!".into()) }
    fn truth(&self) -> Result<bool, InvalidOperation> { Err("Not truthable!".into()) }
    fn not<'a>(&self) -> Result<Value<'a>, InvalidOperation> { Err("Not notable!".into()) }
    fn neg<'a>(&self) -> Result<Value<'a>, InvalidOperation> { Err("Not negativeable!".into()) }
    // Use &Value ??
    fn add<'a>(&self, rhs: &Value<'a>) -> Result<Value<'a>, InvalidOperation> { Err("Not addable!".into()) }
    fn sub<'a>(&self, rhs: &Value<'a>) -> Result<Value<'a>, InvalidOperation> { Err("Not subable!".into()) }
    fn add_assign(&mut self, rhs: &Value) -> Result<(), InvalidOperation> { Err("Not add assignable!".into()) }
    fn sub_assign(&mut self, rhs: &Value) -> Result<(), InvalidOperation> { Err("Not sub assignable!".into()) }
    fn mul<'a>(&self, rhs: &Value<'a>) -> Result<Value<'a>, InvalidOperation> { Err("Not multiplyable!".into()) }
    fn div<'a>(&self, rhs: &Value<'a>) -> Result<Value<'a>, InvalidOperation> { Err("Not dividable!".into()) }
    fn rem<'a>(&self, rhs: &Value<'a>) -> Result<Value<'a>, InvalidOperation> { Err("Not remainderable!".into()) }
    fn eq<'a>(&self, rhs: &Value<'a>) -> Result<bool, InvalidOperation> { Err("Not equalable!".into()) }
    fn less<'a>(&self, rhs: &Value<'a>) -> Result<bool, InvalidOperation> { Err("Not lessable!".into()) }
    fn greater<'a>(&self, rhs: &Value<'a>) -> Result<bool, InvalidOperation> { Err("Not greaterable!".into()) }
    fn and<'a>(&self, rhs: &Value<'a>) -> Result<Value<'a>, InvalidOperation> { Err("Not andable!".into()) }
    fn or<'a>(&self, rhs: &Value<'a>) -> Result<Value<'a>, InvalidOperation> { Err("Not orable!".into()) }
    fn xor<'a>(&self, rhs: &Value<'a>) -> Result<Value<'a>, InvalidOperation> { Err("Not xorable!".into()) }
    fn index<'a>(&self, rhs: &Value<'a>) -> Result<Value<'a>, InvalidOperation> { Err("Not indexable!".into()) }
    //fn mutate_field(&self, field: &Interned<String>, f: impl FnOnce(&mut Self) -> Result<(), ExecError>)
    //field and reciever?
}
