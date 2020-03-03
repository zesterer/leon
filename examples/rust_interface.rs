const SCRIPT: &str = "
    var test = myvec + 4.0 + myvec.sum();
    test
";

use leon::{Object, object::InvalidOperation, Value, Engine};

#[derive(Clone, Copy, Debug)]
struct MyVec3 {
    x: f32,
    y: f32,
    z: f32,
}

impl MyVec3 {
    pub fn new(x: f32, y: f32, z: f32) -> Self {
        Self { x, y, z }
    }
}

impl std::ops::Add<MyVec3> for MyVec3 {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Self {
            x: self.x + other.x,
            y: self.y + other.y,
            z: self.z + other.z,
        }
    }
}

impl std::ops::Add<f32> for MyVec3 {
    type Output = Self;

    fn add(self, other: f32) -> Self {
        Self {
            x: self.x + other,
            y: self.y + other,
            z: self.z + other,
        }
    }
}

impl Object for MyVec3 {
    fn add<'a>(&self, rhs: &Value<'a>) -> Result<Value<'a>, InvalidOperation> {
        match rhs {
            Value::Custom(obj) => obj.as_any().downcast_ref::<Self>().cloned()
                .map(|v| Value::Custom(Box::new(*self + v)))
                .ok_or_else(|| InvalidOperation("Cannot add with provided type".into())),

            Value::Number(num) => Ok(Value::Custom(Box::new(*self + *num as f32))),
            _ => Err(InvalidOperation(format!("Cannot add with {:?}", rhs)))
        }
    }

    fn call_method<'a>(&self, method: &str, args: &[Value<'a>]) -> Result<Value<'a>, InvalidOperation> {
        match method {
            "sum" => Ok(Value::Number((self.x + self.y + self.z) as f64)),
            _ => Err(InvalidOperation(format!("Cannot call method {}", method)))

        }
    }
}

fn main() {
    let vec3 = Engine::default()
        .exec(SCRIPT, vec!((
                "myvec".into(),
                Box::new(MyVec3::new(1.0, 1.0, 1.0)),
        )), |v| v.extract::<MyVec3>().unwrap())
        .unwrap();

    println!("{:?}", vec3);
}
