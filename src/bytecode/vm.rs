use std::{
    rc::Rc,
    ops::{Deref, DerefMut},
};
use super::{
    gc::{Heap, Rooted, Handle},
    Engine, Module, Value, Instr, Ident, ExecError,
};
use crate::Error;

struct Mem<'a> {
    engine: &'a mut Engine,
    stack: Vec<Rooted<Value>>,
}

impl<'a> Deref for Mem<'a> {
    type Target = Heap<Value>;

    fn deref(&self) -> &Self::Target {
        &self.engine.heap
    }
}

impl<'a> DerefMut for Mem<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.engine.heap
    }
}

impl<'a> Mem<'a> {
    pub fn alloc(&mut self, value: Value) -> Rooted<Value> {
        self.engine.insert(value)
    }

    pub fn pop(&mut self) -> Rooted<Value> {
        self.stack.pop().unwrap()
    }

    pub fn push(&mut self, val: Value) {
        self.stack.push(self.alloc(val));
    }

    pub fn push_val(&mut self, val: Value) {
        self.stack.push(self.alloc(val));
    }

    pub fn global(&self, ident: Ident) -> Option<&Value> {
        self.engine.heap.get(self.engine.globals.get(&ident)?).unwrap()
    }

    pub fn local(&self, offset: usize) -> &Value {
        self.engine.heap.get(&self.stack[self.stack.len() - 1 - offset]).unwrap()
    }
}

pub struct Vm<'a> {
    module: &'a Rc<Module>,
    mem: Mem<'a>,
}

impl<'a> Vm<'a> {
    pub fn exec(engine: &'a mut Engine, module: &Rc<Module>) -> Result<Value, Error> {
        Self {
            module,
            mem: &mut Mem::new(engine),
        }
            .exec_routine(module.entry)
    }

    /*
    pub fn with_env_args(
        mut self,
        env: impl Iterator<Item=Rooted<Value>>,
        args: impl Iterator<Item=Rooted<Value>>,
    ) -> Self {
        self.stack.extend(env);
        self.stack.extend(args);
        self
    }
    */

    fn exec_routine(
        &mut self,
        addr: u32,
    ) -> Result<Value, Error> {
        let mut pc = self.module.entry;

        loop {
            let instr = self.module.instrs[pc as usize];
            pc += 1;

            match instr {
                Instr::PushValue(idx) => self.mem.push(self.module.rodata(idx).clone()),
                Instr::PushNum(x) => self.mem.push(Value::Num(x)),
                Instr::PushLocal(o) => self.mem.push(self.mem.local(o)),
                Instr::PushGlobal(i) => self.mem
                    .global(*i)
                    .ok_or_else(|| ExecError::no_such_binding(*i, pc - 1, self.module))?
                    .clone(),
                Instr::MakeList(n) => {
                    let list = Value::List((0..n as usize)
                        .map(|_| self.mem.pop())
                        .collect());
                    self.mem.push(self.mem.alloc(list));
                },
            }
        }
    }
}
