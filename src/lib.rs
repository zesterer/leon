#![feature(trait_alias, arbitrary_self_types, proc_macro_hygiene)]

mod lex;
mod parse;
mod util;
mod walker;
mod bytecode;
pub mod object;
mod error;

pub use walker::Value;
pub use object::Object;
pub use error::{ErrorKind, Error, Thing};

#[derive(Default)]
pub struct Engine;

impl Engine {
    pub fn exec<T>(&mut self, code: &str, globals: Vec<(String, Box<dyn Object>)>, f: impl FnOnce(Value) -> T) -> Result<T, Vec<Error>> {
        let tokens = lex::lex(code)?;

        //println!("--- Tokens ---");
        //ctx.print_debug(&tokens);

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
            .map_err(|_| Vec::new()) // TODO
    }
}
