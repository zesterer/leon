#![feature(trait_alias, arbitrary_self_types, proc_macro_hygiene)]

mod lex;
mod parse;
mod util;
mod walker;
mod object;
mod error;

pub use walker::Value;
pub use object::Object;
pub use error::{ErrorKind, Error, Thing};

#[derive(Default)]
pub struct Engine;

impl Engine {
    pub fn execute(&mut self, code: &str) -> Result<(), Vec<Error>> {
        let (tokens, ctx) = lex::lex(code)?;

        //println!("--- Tokens ---");
        ctx.print_debug(&tokens);

        let ast = parse::parse(&tokens, &ctx)?;

        println!("--- Syntax Tree ---");
        ast.print_debug(&ctx);

        let result = walker::AbstractMachine::new(ctx.strings, ctx.idents)
            .execute(&ast)
            .unwrap();

        println!("{:?}", result);

        Ok(())
    }
}
