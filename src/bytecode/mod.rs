mod gc;
mod vm;

use std::{
    rc::Rc,
    collections::HashMap,
};
use internment::Intern;
use crate::{
    parse::{UnaryOp, BinaryOp},
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
        env: Vec<Handle<Self>>,
    },
    List(Vec<Handle<Self>>),
    Struct(HashMap<u32, Handle<Self>>),
    Received(Handle<Self>, Ident),
    Ref(Handle<Self>),
}

impl Trace for Value {
    fn trace(&self, tracer: &mut Tracer<Self>) {
        match self {
            Value::Func { env, .. } => env
                .iter()
                .for_each(|val| tracer.mark(*val)),
            Value::Struct(fields) => fields
                .values()
                .for_each(|field| tracer.mark(*field)),
            Value::Received(receiver, field) => tracer.mark(*receiver),
            Value::Ref(val) => tracer.mark(*val),
            _ => {},
        }
    }
}

type Ident = Intern<String>;

#[derive(Debug)]
#[repr(u8)]
pub enum Instr {
    PushValue(u32), // Push a value from rodata on to the stack
    PushNum(f64), // Push a number on to the stack
    PushLocal(u32), // Push a local
    PushGlobal(Ident), // Push a reference to the given global on to the stack
    MakeList(u32), // Make a list using the last N values on the stack (in reverse order)
    Unary(UnaryOp), // Perform a unary operation upon the last val on the stack, pushing the result
    Binary(BinaryOp), // Perform a binary operation upon the last two val on the stack, pushing the result
    If(u32, u32), // Jump to one of the given addresses depending on the truth of the last val on the stack
    MakeFunc(u32, u16, Vec<u32>), // Push a function value on to the stack pointing at the given address and with the given stack environment
    Jump(u32), // Jump to the given address
    Return, // Return from the current function
}

#[derive(Debug)]
pub struct Module {
    entry: u32,
    rodata: Vec<Value>,
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

    pub fn rodata(&self, index: u32) -> &Value {
        &self.rodata[index as usize]
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
}

#[derive(Debug)]
pub struct ExecError {
    kind: ExecErrorKind,
    module: Rc<Module>,
}

#[derive(Debug)]
pub enum ExecErrorKind {
    NoSuchBinding(Ident, SrcRegion, Rc<Module>),
}

impl ExecError {
    pub fn no_such_binding(ident: Ident, addr: u32, module: &Rc<Module>) -> Self {
        ExecError {
            kind: ExecErrorKind::NoSuchBinding(ident, module.region_at(addr), module),
            module: module.clone(),
        }
    }
}


