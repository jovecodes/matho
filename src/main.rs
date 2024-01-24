use std::io::{self, Write};

mod builtin;
mod interpreter;
mod lexer;
mod parser;
mod pos;
mod user;

fn main() {
    let state = builtin::global_scope();
    let mut rl = rustyline::DefaultEditor::new().unwrap();

    loop {
        let readline = rl.readline("> ");
        match readline {
            Ok(input) => {
                if input.trim().eq_ignore_ascii_case("quit") {
                    break;
                }

                rl.add_history_entry(input.as_str()).unwrap();

                let result = interpreter::eval(
                    parser::parse(lexer::Lexer::load(&input).tokens),
                    state.clone(),
                );

                if let interpreter::Type::Void = result {
                } else {
                    println!("{}", result);
                }
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
}
