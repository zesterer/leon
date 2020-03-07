#![feature(trait_alias, arbitrary_self_types, proc_macro_hygiene)]

mod lex;
mod parse;
mod util;
mod walker;
mod bytecode;
pub mod object;
mod error;

pub use walker::{Value, ExecError};
pub use object::Object;
pub use error::{ErrorKind, Error, Thing};

#[derive(Default)]
pub struct Engine;

pub struct Ast(parse::Node<parse::Expr>);

impl Engine {
    pub fn exec<T>(&mut self, code: &str, globals: Vec<(String, Value<'static>)>, f: impl FnOnce(Value) -> T) -> Result<T, Vec<Error>> {
        let tokens = lex::lex(code)?;

        let ast = parse::parse(&tokens).map_err(|errs| {
            for err in &errs {
                println!("Location: {:?}", err.region.map(|region| region.in_context(code)));
            }
            errs
        })?;

        //println!("--- Syntax Tree ---");
        //ast.print_debug();

        walker::AbstractMachine::new()
            .with_globals(globals)
            .exec(&ast)
            .map(f)
            .map_err(|e| { println!("Error: {:?}", e); Vec::new() }) // TODO
    }
    pub fn parse(code: &str) -> Result<Ast, Vec<Error>> {
        let tokens = lex::lex(code)?;

        let ast = parse::parse(&tokens).map_err(|errs| {
            for err in &errs {
                println!("Location: {:?}", err.region.map(|region| region.in_context(code)));
            }
            errs
        })?;

        Ok(Ast(ast))
    }
    pub fn exec_parsed<T>(&mut self, ast: &Ast, globals: Vec<(String, Value<'static>)>, f: impl FnOnce(Value) -> T) -> Result<T, Vec<Error>> {
        walker::AbstractMachine::new()
            .with_globals(globals)
            .exec(&ast.0)
            .map(f)
            .map_err(|e| { println!("Error: {:?}", e); Vec::new() }) // TODO
    }
}
