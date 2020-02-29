use std::{
    env,
    fs::File,
    io::Read,
};
use leon::Engine;

fn main() {
    for arg in env::args().skip(1) {
        let mut buf = String::new();
        File::open(&arg)
            .unwrap_or_else(|err| panic!("Could not open file '{}': {:?}", arg, err))
            .read_to_string(&mut buf)
            .unwrap_or_else(|err| panic!("Could not read file '{}': {:?}", arg, err));

        Engine::default()
            .execute(&buf, vec![])
            .unwrap();
    }
}
