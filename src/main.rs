use parser::AstNode;
use std::{
    cell::RefCell,
    collections::HashMap,
    f64::consts,
    io::{self, Write},
    rc::Rc,
};

mod lexer;
mod parser;

const PHI: f64 = 1.618033988749894848204586834365638118;

trait Function {
    fn apply(&self, args: Vec<AstNode>, state: Rc<RefCell<Scope>>) -> f64;
}

struct UserFunction {
    args: Vec<String>,
    body: AstNode,
}

impl UserFunction {
    fn new(args: Vec<String>, body: AstNode) -> Self {
        Self { args, body }
    }
}

impl Function for UserFunction {
    fn apply(&self, args: Vec<AstNode>, state: Rc<RefCell<Scope>>) -> f64 {
        let mut variable = HashMap::new();
        for (i, arg) in self.args.iter().enumerate() {
            variable.insert(arg.clone(), eval(args[i].clone(), state.clone()));
        }
        let scope = Rc::new(RefCell::new(Scope {
            variable,
            functions: HashMap::new(),
            parent: Some(state),
        }));
        eval(self.body.clone(), scope)
    }
}

type Fun = fn(Vec<AstNode>, Rc<RefCell<Scope>>) -> f64;

struct BuiltinFunction(Fun);

impl Function for BuiltinFunction {
    fn apply(&self, args: Vec<AstNode>, state: Rc<RefCell<Scope>>) -> f64 {
        (self.0)(args, state)
    }
}

struct Scope {
    variable: HashMap<String, f64>,
    functions: HashMap<String, Rc<RefCell<dyn Function>>>,
    parent: Option<Rc<RefCell<Scope>>>,
}

impl Scope {
    fn get_variable(&self, ident: &str) -> Option<f64> {
        if let Some(var) = self.variable.get(ident) {
            Some(*var)
        } else if let Some(parent) = &self.parent {
            parent.borrow().get_variable(ident)
        } else {
            None
        }
    }

    fn get_function(&self, ident: &str) -> Option<Rc<RefCell<dyn Function>>> {
        if let Some(fun) = self.functions.get(ident) {
            Some(fun.clone())
        } else if let Some(parent) = &self.parent {
            parent.borrow().get_function(ident)
        } else {
            None
        }
    }
}

fn builtin_echo(args: Vec<AstNode>, state: Rc<RefCell<Scope>>) -> f64 {
    println!("{:?}", args);
    eval(args[0].clone(), state)
}

fn builtin_sqrt(args: Vec<AstNode>, state: Rc<RefCell<Scope>>) -> f64 {
    assert!(args.len() == 1);
    eval(args[0].clone(), state).sqrt()
}

fn builtin_ln(args: Vec<AstNode>, state: Rc<RefCell<Scope>>) -> f64 {
    assert!(args.len() == 1);
    eval(args[0].clone(), state).ln()
}

fn main() {
    let state = Rc::new(RefCell::new(Scope {
        variable: vec![
            ("pi".to_string(), consts::PI),
            ("e".to_string(), consts::E),
            ("tau".to_string(), consts::TAU),
            ("phi".to_string(), PHI),
        ]
        .into_iter()
        .collect(),
        functions: vec![
            (
                "echo".to_string(),
                Rc::new(RefCell::new(BuiltinFunction(builtin_echo as Fun)))
                    as Rc<RefCell<dyn Function>>,
            ),
            (
                "sqrt".to_string(),
                Rc::new(RefCell::new(BuiltinFunction(builtin_sqrt as Fun)))
                    as Rc<RefCell<dyn Function>>,
            ),
            (
                "ln".to_string(),
                Rc::new(RefCell::new(BuiltinFunction(builtin_ln as Fun)))
                    as Rc<RefCell<dyn Function>>,
            ),
        ]
        .into_iter()
        .collect(),
        parent: None,
    }));
    loop {
        print!("> ");
        io::stdout().flush().unwrap();

        let mut input = String::new();
        io::stdin().read_line(&mut input).unwrap();

        if input.trim().eq_ignore_ascii_case("quit") {
            break;
        }
        let result = eval(parser::parse(lexer::lex(&input)), state.clone());

        println!("{}", result);
    }
}

fn eval(ast: AstNode, state: Rc<RefCell<Scope>>) -> f64 {
    match ast {
        AstNode::BinOp(op, lhs, rhs) => op.apply(eval(*lhs, state.clone()), eval(*rhs, state)),
        AstNode::UnaryOp(op, node) => op.apply(eval(*node, state)),
        AstNode::Number(n) => n.into(),
        AstNode::Variable(var) => {
            if let Some(val) = state.borrow().get_variable(&var) {
                val
            } else {
                panic!("Unknown variable '{}'", var)
            }
        }
        AstNode::Funcall(fun, args) => {
            if let Some(val) = state.borrow().get_function(&fun) {
                val.borrow().apply(args, state.clone())
            } else {
                panic!("Unknown function '{}'", fun)
            }
        }
        AstNode::LetStatement(stmt) => match stmt {
            parser::LetStatement::Function(ident, args, body) => {
                state
                    .borrow_mut()
                    .functions
                    .insert(ident, Rc::new(RefCell::new(UserFunction::new(args, *body))));
                0.0
            }
            parser::LetStatement::Variable(ident, expr) => {
                let val = eval(*expr, state.clone());
                state.borrow_mut().variable.insert(ident, val);
                val
            }
        },
    }
}
