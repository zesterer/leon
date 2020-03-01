mod gc;
mod vm;

use std::{
    rc::Rc,
    collections::HashMap,
};
use internment::Intern;
use crate::{
    parse::{UnaryOp, BinaryOp, Mutation},
    util::src::SrcRegion,
    Error,
};
use self::{
    gc::{Tracer, Trace, Heap, Handle, Rooted},
    vm::Vm,
};

#[derive(Debug)]
pub enum Value {
    Str(String),
    Num(f64),
    Bool(bool),
    Null,
    Func {
        module: Rc<Module>,
        addr: u32,
        arity: u16,
        env: Vec<Rooted<Self>>,
    },
    List(Vec<Self>),
    Struct(HashMap<u32, Self>),
    Ref(Handle<Self>),
    TransparentRef(Handle<Self>),
}

impl Trace for Value {
    fn trace(&self, tracer: &mut Tracer<Self>) {
        match self {
            Value::Func { env, .. } => env
                .iter()
                .for_each(|val| tracer.mark(val.handle())),
            Value::List(items) => items
                .iter()
                .for_each(|item| item.trace(tracer)),
            Value::Struct(fields) => fields
                .values()
                .for_each(|field| field.trace(tracer)),
            Value::Ref(val) => tracer.mark(*val),
            Value::TransparentRef(val) => tracer.mark(*val),
            _ => {},
        }
    }
}

type Ident = Intern<String>;

#[derive(Debug)]
#[repr(u8)]
pub enum Instr {
    // Push a number.
    Num(f64),
    // Push a string.
    Str(String),
    // Push a bool.
    Bool(bool),
    // Push a null.
    Null,
    // Push a clone of a local.
    Local(u32),
    // Push a reference to the given global.
    PushGlobal(Ident),
    // Make a list using the top N values on the stack (in reverse order).
    // ... D C B A
    MakeList(u32),
    // Perform a unary operation upon the top value on the stack, pushing the result.
    // ... A
    Unary(UnaryOp),
    // Perform a binary operation upon the top two values on the stack (in reverse order), pushing the result.
    // ... B A
    Binary(BinaryOp),
    // Jump to one of the given addresses depending on the truth of the last value on the stack.
    If(u32, u32),
    // Push a function with the function information with the specified index.
    // All of the stack values specified as environments will implicitly become transparent
    // references (i.e: references that perform full copies when cloned).
    MakeFunc(u32),
    // Jump to the given address.
    Jump(u32),
    // Return from the current function with the value at the top of the stack acting as the return value.
    Return,
    // Call the top value on the stack with the N values preceding it.
    // The function's environment will get placed on the locals stack before execution begins.
    //
    // ... A B C F
    Call(u16),
    // Mutate the value one below the top of the stack with the value at the top
    Mutate(Mutation),
    // Push the value at the top of the stack onto the locals stack.
    PushLocal,
    // Remove the last value from the locals stack
    PopLocal,
}

#[derive(Debug)]
pub struct FuncInfo {
    addr: u32,
    arity: u32,
    env: Vec<u32>,
}

#[derive(Debug)]
pub struct Module {
    entry: u32,
    func_info: Vec<FuncInfo>,
    instrs: Vec<Instr>,
    debug_info: HashMap<u32, SrcRegion>,
}

impl Module {
    pub fn region_at(&self, addr: u32) -> SrcRegion {
        self.debug_info
            .get(&addr)
            .copied()
            .unwrap_or(SrcRegion::none())
    }

    pub fn func_info(&self, index: u32) -> &FuncInfo {
        &self.func_info[index as usize]
    }
}

#[derive(Default)]
pub struct Engine {
    heap: Heap<Value>,
    globals: HashMap<Ident, Rooted<Value>>,
}

impl Engine {
    pub fn insert_global(&mut self, name: &str, value: Value) {
        self.globals.insert(Intern::new(name.into()), self.heap.insert(value));
    }

    pub fn with_global(mut self, name: &str, value: Value) -> Self {
        self.insert_global(name, value);
        self
    }

    pub fn eval_str(&mut self, code: &str) -> Result<Value, Error> {
        self.eval(&self.compile_str(code)?)
    }

    pub fn compile_str(&self, code: &str) -> Result<Rc<Module>, Error> {
        todo!()
    }

    pub fn eval(&mut self, module: &Rc<Module>) -> Result<Value, Error> {
        let result = Vm::exec(self, module);
        self.heap.clean();
        result
    }

    fn clone_val(&mut self, handle: impl AsRef<Handle<Value>>) -> Value {
        // This function clones a value
        // 'Cloning' involves deep cloning the entire value tree down to the closest `Ref`.
        // However, `TransparentRef` *does* get deep cloned because it represents a value that
        // is supposed to behave as if it did not have reference semantics.
        // This is done because we don't want things like integers to get magically mutated
        // Therefore, Leon is a 'clone by default' language much like Rust.
        // Unlike Python or Java, taking values by reference must be done explicitly with `ref x`.
        todo!()
    }
}

#[derive(Debug)]
pub struct ExecError {
    kind: ExecErrorKind,
    module: Rc<Module>,
}

#[derive(Debug)]
pub enum ExecErrorKind {
    NoSuchBinding(Ident, SrcRegion),
    InvalidOp(SrcRegion),
}

impl ExecError {
    pub fn no_such_binding(ident: Ident, addr: u32, module: &Rc<Module>) -> Self {
        ExecError {
            kind: ExecErrorKind::NoSuchBinding(ident, module.region_at(addr)),
            module: module.clone(),
        }
    }

    pub fn invalid_op(addr: u32, module: &Rc<Module>) -> Self {
        ExecError {
            kind: ExecErrorKind::InvalidOp(module.region_at(addr)),
            module: module.clone(),
        }
    }
}


