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

pub trait Object: 'static {
    fn call(&self, args: &[Value]) -> Result<Value, InvalidOperation> { Err("Not callable!".into()) }
    fn truth(&self) -> Result<bool, InvalidOperation> { Err("Not truthable!".into()) }
    fn not(&self) -> Result<Value, InvalidOperation> { Err("Not notable!".into()) }
    fn neg(&self) -> Result<Value, InvalidOperation> { Err("Not negativeable!".into()) }
    // Use &Value ??
    fn add(&self, rhs: &Value) -> Result<Value, InvalidOperation> { Err("Not addable!".into()) }
    fn sub(&self, rhs: &Value) -> Result<Value, InvalidOperation> { Err("Not subable!".into()) }
    fn mul(&self, rhs: &Value) -> Result<Value, InvalidOperation> { Err("Not multiplyable!".into()) }
    fn div(&self, rhs: &Value) -> Result<Value, InvalidOperation> { Err("Not dividable!".into()) }
    fn rem(&self, rhs: &Value) -> Result<Value, InvalidOperation> { Err("Not remainderable!".into()) }
    fn eq(&self, rhs: &Value) -> Result<bool, InvalidOperation> { Err("Not equalable!".into()) }
    fn less(&self, rhs: &Value) -> Result<bool, InvalidOperation> { Err("Not lessable!".into()) }
    fn greater(&self, rhs: &Value) -> Result<bool, InvalidOperation> { Err("Not greaterable!".into()) }
    fn and(&self, rhs: &Value) -> Result<Value, InvalidOperation> { Err("Not andable!".into()) }
    fn or(&self, rhs: &Value) -> Result<Value, InvalidOperation> { Err("Not orable!".into()) }
    fn xor(&self, rhs: &Value) -> Result<Value, InvalidOperation> { Err("Not xorable!".into()) }
    fn index(&self, rhs: &Value) -> Result<Value, InvalidOperation> { Err("Not indexable!".into()) }
    //fn mutate_field(&self, field: &Interned<String>, f: impl FnOnce(&mut Self) -> Result<(), ExecError>)
    //field and reciever?
    fn cloned(&self) -> Box<dyn Object>;
}
