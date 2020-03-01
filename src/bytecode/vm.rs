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
    locals: Vec<Rooted<Value>>,
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
    pub fn get(&self, handle: impl AsRef<Handle<Value>>) -> &Value {
        self.engine.heap.get(handle).unwrap()
    }

    pub fn get_cloned(&mut self, handle: impl AsRef<Handle<Value>>) -> Value {
        self.engine.clone_val(handle)
    }

    pub fn pop_local(&mut self) {
        self.locals.pop().unwrap();
    }

    pub fn push_local(&mut self, val: Rooted<Value>) {
        self.locals.push(val);
    }

    pub fn replace_local(&mut self, offset: usize, val: Rooted<Value>) -> Rooted<Value> {
        let len = self.locals.len();
        std::mem::replace(&mut self.locals[len - 1 - offset], val)
    }

    pub fn local(&self, offset: usize) -> &Rooted<Value> {
        &self.locals[self.locals.len() - 1 - offset]
    }

    pub fn global(&self, ident: Ident) -> Option<&Rooted<Value>> {
        self.engine.globals.get(&ident)
    }
}

pub struct Vm<'a> {
    module: &'a Rc<Module>,
    mem: &'a mut Mem<'a>,
}

impl<'a> Vm<'a> {
    pub fn exec(engine: &'a mut Engine, module: &'a Rc<Module>) -> Result<Value, Error> {
        let mut mem = Mem {
            engine,
            locals: Vec::new(),
        };
        let mut stack = Vec::new();
        Vm {
            module,
            mem: &mut mem,
        }
            .exec_routine(module.entry, &mut stack)?;
        Ok(engine.clone_val(&stack.pop().unwrap()))
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
        stack: &mut Vec<Handle<Value>>,
    ) -> Result<(), Error> {
        let mut pc = addr;

        loop {
            let instr = &self.module.instrs[pc as usize];
            pc += 1;

            match instr {
                Instr::Num(x) => stack.push(self.mem.insert_temp(Value::Num(*x))),
                Instr::Str(x) => stack.push(self.mem.insert_temp(Value::Str(x.clone()))),
                Instr::Bool(x) => stack.push(self.mem.insert_temp(Value::Bool(*x))),
                Instr::Null => stack.push(self.mem.insert_temp(Value::Null)),
                Instr::Local(o) => stack.push(self.mem.local(*o as usize).handle()),
                Instr::PushGlobal(i) => {
                    stack.push(self.mem
                        .global(*i)
                        .ok_or_else(|| ExecError::no_such_binding(*i, pc - 1, self.module))?
                        .handle());
                },
                Instr::MakeList(n) => {
                    let list = Value::List((0..*n as usize)
                        .map(|_| self.mem.get_cloned(&stack.pop().unwrap()))
                        .collect());
                    stack.push(self.mem.insert_temp(list));
                },
                Instr::Unary(op) => {
                    let a = stack.pop().unwrap();
                    match op {
                        _ => todo!(),
                    }
                },
                Instr::Binary(op) => {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    match op {
                        _ => todo!(),
                    }
                },
                Instr::If(tgt_a, tgt_b) => {
                    let a = stack.pop().unwrap();
                    todo!()
                },
                Instr::MakeFunc(info) => {
                    let info = self.module.func_info(*info);
                    //let new_local = self.mem.insert(Value::TransparentRef(self.mem.local(local_offs).handle()));
                    //self.mem.replace_local(local_offs, new_local);
                    //self.mem.push_local(self.mem.local(local_offs).clone());
                    todo!()
                },
                Instr::Jump(addr) => pc = *addr,
                Instr::Return => return Ok(()),
                Instr::Call(n) => {
                    let a = stack.pop().unwrap();
                    match self.mem.get(a) {
                        Value::Func { module, addr, arity, env } if arity == n => {
                            let addr = *addr;
                            let arity = *arity;
                            let env = (*env).clone();
                            let env_len = env.len();
                            // Push locals
                            for local in env {
                                self.mem.push_local(local);
                            }
                            self.exec_routine(addr, stack)?;
                            let ret_val = stack.pop().unwrap();
                            // Pop args
                            for _ in 0..arity { stack.pop().unwrap(); }
                            // Pop locals
                            for _ in 0..env_len { self.mem.pop_local(); }
                            stack.push(ret_val);
                        },
                        _ => Err(ExecError::invalid_op(pc - 1, self.module))?,
                    }
                },
                Instr::Mutate(mutation) => {
                    let a = stack.pop().unwrap();
                    let receiver = *stack.last().unwrap();
                    self.mem.mutate(receiver, |val| {
                        todo!("mutate val")
                    });
                },
                PushLocal => {
                    let a = stack.pop().unwrap();
                    let local = self.mem.make_rooted(a);
                    self.mem.push_local(local);
                },
                PopLocal => self.mem.pop_local(),
            }
        }
    }
}
