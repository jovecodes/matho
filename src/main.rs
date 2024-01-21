use parser::AstNode;
use std::io::{self, Write};

mod lexer;
mod parser;

fn main() {
    loop {
        print!("> ");
        io::stdout().flush().unwrap();

        let mut input = String::new();
        io::stdin().read_line(&mut input).unwrap();

        if input.trim().eq_ignore_ascii_case("quit") {
            break;
        }

        // Parse input into AST
        let tokens = lexer::lex(&input);
        let ast = parser::parse(tokens);

        // Evaluate AST
        let result = eval(ast);

        println!("{}", result);
    }
}

fn eval(ast: AstNode) -> f64 {
    match ast {
        AstNode::BinOp(op, lhs, rhs) => op.apply(eval(*lhs), eval(*rhs)),
        AstNode::UnaryOp(op, node) => match op {
            parser::UnaryOp::Neg => eval(*node) * -1.0,
        },
        AstNode::Number(n) => n.into(),
    }
}
