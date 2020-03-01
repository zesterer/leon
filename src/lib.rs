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
    pub fn exec(&mut self, code: &str, globals: Vec<(String, Box<dyn Object>)>) -> Result<Value<'static>, Vec<Error>> {
        let (tokens, ctx) = lex::lex(code)?;

        //println!("--- Tokens ---");
        //ctx.print_debug(&tokens);

        let mut ast = parse::parse(&tokens).map_err(|errs| {
            for err in &errs {
                println!("Location: {:?}", err.region.map(|region| region.in_context(code)));
            }
            errs
        })?;

        //println!("--- Syntax Tree ---");
        //ast.print_debug(&ctx);

        walker::AbstractMachine::new(ctx.strings, ctx.idents)
            .with_globals(globals)
            .execute(&ast)
            .map_err(|_| Vec::new()) // TODO
    }
}
