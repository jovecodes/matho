use lexer::Number;
use parser::AstNode;
use std::{
    cell::RefCell,
    collections::HashMap,
    f64::consts,
    fmt::Display,
    io::{self, Write},
    ops,
    rc::Rc,
};

mod lexer;
mod parser;
mod pos;

const PHI: f64 = 1.618033988749894848204586834365638118;

trait Function {
    fn apply(&self, args: Vec<AstNode>, state: Rc<RefCell<Scope>>) -> Type;
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
    fn apply(&self, args: Vec<AstNode>, state: Rc<RefCell<Scope>>) -> Type {
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

type Fun = fn(Vec<AstNode>, Rc<RefCell<Scope>>) -> Type;

struct BuiltinFunction(Fun);

impl Function for BuiltinFunction {
    fn apply(&self, args: Vec<AstNode>, state: Rc<RefCell<Scope>>) -> Type {
        (self.0)(args, state)
    }
}

struct Scope {
    variable: HashMap<String, Type>,
    functions: HashMap<String, Rc<RefCell<dyn Function>>>,
    parent: Option<Rc<RefCell<Scope>>>,
}

impl Scope {
    fn get_variable(&self, ident: &str) -> Option<Type> {
        if let Some(var) = self.variable.get(ident) {
            Some(var.clone())
        } else if let Some(parent) = &self.parent {
            parent.borrow().get_variable(ident)
        } else {
            None
        }
    }

    fn set_variable(&mut self, ident: &str, value: Type) -> Result<(), ()> {
        if let Some(var) = self.variable.get_mut(ident) {
            *var = value;
            Ok(())
        } else if let Some(parent) = &self.parent {
            parent.borrow_mut().set_variable(ident, value)
        } else {
            Err(())
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

fn builtin_echo(args: Vec<AstNode>, state: Rc<RefCell<Scope>>) -> Type {
    println!("{:?}", args);
    eval(args[0].clone(), state)
}

fn builtin_sqrt(args: Vec<AstNode>, state: Rc<RefCell<Scope>>) -> Type {
    assert!(args.len() == 1);
    Type::Number(Number::Float(
        match eval(args[0].clone(), state) {
            Type::Number(n) => n.as_float(),
            _ => panic!("expected number!"),
        }
        .sqrt(),
    ))
}

fn builtin_ln(args: Vec<AstNode>, state: Rc<RefCell<Scope>>) -> Type {
    assert!(args.len() == 1);
    Type::Number(Number::Float(
        match eval(args[0].clone(), state) {
            Type::Number(n) => n.as_float(),
            _ => panic!("expected number!"),
        }
        .ln(),
    ))
}

fn builtin_exit(args: Vec<AstNode>, state: Rc<RefCell<Scope>>) -> Type {
    assert!(args.len() == 1);

    let code = match eval(args[0].clone(), state) {
        Type::Number(n) => n.as_float(),
        _ => panic!("expected number!"),
    };
    std::process::exit(code as i32);
}

// fn builtin_do_file(args: Vec<AstNode>, state: Rc<RefCell<Scope>>) -> f64 {
//     assert!(args.len() == 1);
//     let input = "";
//     let result = eval(parser::parse(lexer::lex(&input)), state.clone());
//     std::process::exit(eval(args[0].clone(), state) as i32)
// }

fn main() {
    let state = Rc::new(RefCell::new(Scope {
        variable: vec![
            ("pi".to_string(), Type::Number(Number::Float(consts::PI))),
            ("e".to_string(), Type::Number(Number::Float(consts::E))),
            ("tau".to_string(), Type::Number(Number::Float(consts::TAU))),
            ("phi".to_string(), Type::Number(Number::Float(PHI))),
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
            (
                "exit".to_string(),
                Rc::new(RefCell::new(BuiltinFunction(builtin_exit as Fun)))
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
        let result = eval(
            parser::parse(lexer::Lexer::load(&input).tokens),
            state.clone(),
        );

        println!("{}", result);
    }
}

#[derive(Debug, Clone)]
enum Type {
    Number(Number),
    String(String),
    Void,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Number(n) => write!(f, "{}", n),
            Type::String(s) => write!(f, "{}", s),
            Type::Void => write!(f, "void"),
        }
    }
}

impl ops::Add for Type {
    type Output = Type;

    fn add(self, rhs: Self) -> Self::Output {
        match self {
            Type::Number(l) => match rhs {
                Type::Number(r) => Type::Number(Number::Float(l.as_float() + r.as_float())),
                _ => panic!(),
            },
            Type::String(l) => match rhs {
                Type::String(r) => Type::String(l + &r),
                _ => panic!(),
            },
            Type::Void => todo!(),
        }
    }
}

impl ops::Sub for Type {
    type Output = Type;

    fn sub(self, rhs: Self) -> Self::Output {
        match self {
            Type::Number(l) => match rhs {
                Type::Number(r) => Type::Number(Number::Float(l.as_float() - r.as_float())),
                _ => panic!(),
            },
            Type::String(_) => todo!(),
            Type::Void => todo!(),
        }
    }
}

impl ops::Mul for Type {
    type Output = Type;

    fn mul(self, rhs: Self) -> Self::Output {
        match self {
            Type::Number(l) => match rhs {
                Type::Number(r) => Type::Number(Number::Float(l.as_float() * r.as_float())),
                _ => panic!(),
            },
            Type::String(_) => todo!(),
            Type::Void => todo!(),
        }
    }
}

impl ops::Div for Type {
    type Output = Type;

    fn div(self, rhs: Self) -> Self::Output {
        match self {
            Type::Number(l) => match rhs {
                Type::Number(r) => Type::Number(Number::Float(l.as_float() / r.as_float())),
                _ => panic!(),
            },
            Type::String(_) => todo!(),
            Type::Void => todo!(),
        }
    }
}

impl ops::Neg for Type {
    type Output = Type;

    fn neg(self) -> Self::Output {
        match self {
            Type::Number(l) => Type::Number(Number::Float(-l.as_float())),
            _ => panic!(),
        }
    }
}

fn eval(ast: AstNode, state: Rc<RefCell<Scope>>) -> Type {
    match ast {
        AstNode::BinOp(op, lhs, rhs) => op.apply(eval(*lhs, state.clone()), eval(*rhs, state)),
        AstNode::UnaryOp(op, node) => op.apply(eval(*node, state)),
        AstNode::Number(n) => Type::Number(n),
        AstNode::Variable(var) => {
            if let Some(val) = state.borrow().get_variable(&var) {
                val
            } else {
                panic!("Unknown variable '{}'", var)
            }
        }
        AstNode::Funcall(fun, args) => {
            let val = match state.borrow().get_function(&fun) {
                Some(v) => v,
                None => panic!("Unknown function '{}'", fun),
            };
            val.clone().borrow().apply(args, state.clone())
        }
        AstNode::LetStatement(stmt) => match stmt {
            parser::LetStatement::Function(ident, args, body) => {
                state
                    .borrow_mut()
                    .functions
                    .insert(ident, Rc::new(RefCell::new(UserFunction::new(args, *body))));
                Type::Void
            }
            parser::LetStatement::Variable(ident, expr) => {
                let val = eval(*expr, state.clone());
                state.borrow_mut().variable.insert(ident.clone(), val);
                state.borrow().get_variable(&ident).unwrap()
            }
        },
        AstNode::Assignment(ident, expr) => {
            let value = eval(*expr, state.clone());
            state
                .borrow_mut()
                .set_variable(ident.as_str(), value)
                .expect(&format!(
                    "Could not assign variable '{}' because it does not exist!",
                    ident
                ));
            state.borrow().get_variable(ident.as_str()).unwrap()
        }
        AstNode::String(s) => Type::String(s),
    }
}
