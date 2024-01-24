use std::io::{self, Write};

mod builtin;
mod interpreter;
mod lexer;
mod parser;
mod pos;
mod user;

fn main() {
    let state = builtin::global_scope();
    loop {
        print!("> ");
        io::stdout().flush().unwrap();

        let mut input = String::new();
        io::stdin().read_line(&mut input).unwrap();

        if input.trim().eq_ignore_ascii_case("quit") {
            break;
        }
        let result = interpreter::eval(
            parser::parse(lexer::Lexer::load(&input).tokens),
            state.clone(),
        );

        if let interpreter::Type::Void = result {
        } else {
            println!("{}", result);
        }
    }
}
