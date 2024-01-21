use dyn_fmt::AsStrFormatExt;
use lexer::Number;
use parser::AstNode;
use std::{
    cell::RefCell,
    collections::HashMap,
    f64::consts,
    fmt::Display,
    fs,
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
    name: String,
}

impl UserFunction {
    fn new(args: Vec<String>, body: AstNode, name: String) -> Self {
        Self { args, body, name }
    }
}

impl Function for UserFunction {
    fn apply(&self, args: Vec<AstNode>, state: Rc<RefCell<Scope>>) -> Type {
        if args.len() != self.args.len() {
            panic!(
                "number of arguments to {} is incorrect expected {} but got {}",
                self.name,
                self.args.len(),
                args.len()
            )
        }
        let mut variables = HashMap::new();
        for (i, arg) in self.args.iter().enumerate() {
            variables.insert(arg.clone(), eval(args[i].clone(), state.clone()));
        }
        let scope = Rc::new(RefCell::new(Scope {
            variables,
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
    variables: HashMap<String, Type>,
    functions: HashMap<String, Rc<RefCell<dyn Function>>>,
    parent: Option<Rc<RefCell<Scope>>>,
}

impl Scope {
    fn get_variable(&self, ident: &str) -> Option<Type> {
        if let Some(var) = self.variables.get(ident) {
            Some(var.clone())
        } else if let Some(parent) = &self.parent {
            parent.borrow().get_variable(ident)
        } else {
            None
        }
    }

    fn set_variable(&mut self, ident: &str, value: Type) -> Result<(), ()> {
        if let Some(var) = self.variables.get_mut(ident) {
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

fn builtin_dbg_echo(args: Vec<AstNode>, state: Rc<RefCell<Scope>>) -> Type {
    println!("{:?}", args);
    eval(args[0].clone(), state)
}

fn builtin_fmt(args: Vec<AstNode>, state: Rc<RefCell<Scope>>) -> Type {
    let fmt = match args.first().expect("print requires at least one argument!") {
        AstNode::String(fmt) => fmt,
        _ => panic!("expected string format but got {:?}", args[0]),
    };

    let mut printable_args = vec![];
    for arg in args.iter().skip(1) {
        printable_args.push(eval(arg.clone(), state.clone()));
    }

    Type::String(fmt.format(&printable_args))
}

fn builtin_print(args: Vec<AstNode>, state: Rc<RefCell<Scope>>) -> Type {
    print!("{}", builtin_fmt(args, state));
    io::stdout().flush().unwrap();
    Type::Void
}

fn builtin_println(args: Vec<AstNode>, state: Rc<RefCell<Scope>>) -> Type {
    println!("{}", builtin_fmt(args, state));
    Type::Void
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

fn builtin_use(args: Vec<AstNode>, state: Rc<RefCell<Scope>>) -> Type {
    assert!(args.len() == 1);

    let path = match eval(args[0].clone(), state.clone()) {
        Type::String(n) => n,
        _ => panic!("expected file path!"),
    };

    let file = fs::read_to_string(path.clone()).expect(&format!("Could not open file {path}!"));
    let tokens = lexer::Lexer::load(&file).tokens;
    let ast = parser::parse_file(tokens);
    eval(ast, state)
}

// fn builtin_do_file(args: Vec<AstNode>, state: Rc<RefCell<Scope>>) -> f64 {
//     assert!(args.len() == 1);
//     let input = "";
//     let result = eval(parser::parse(lexer::lex(&input)), state.clone());
//     std::process::exit(eval(args[0].clone(), state) as i32)
// }

fn main() {
    let state = Rc::new(RefCell::new(Scope {
        variables: vec![
            ("pi".to_string(), Type::Number(Number::Float(consts::PI))),
            ("e".to_string(), Type::Number(Number::Float(consts::E))),
            ("tau".to_string(), Type::Number(Number::Float(consts::TAU))),
            ("phi".to_string(), Type::Number(Number::Float(PHI))),
        ]
        .into_iter()
        .collect(),
        functions: vec![
            (
                "dbg_echo".to_string(),
                Rc::new(RefCell::new(BuiltinFunction(builtin_dbg_echo as Fun)))
                    as Rc<RefCell<dyn Function>>,
            ),
            (
                "print".to_string(),
                Rc::new(RefCell::new(BuiltinFunction(builtin_print as Fun)))
                    as Rc<RefCell<dyn Function>>,
            ),
            (
                "println".to_string(),
                Rc::new(RefCell::new(BuiltinFunction(builtin_println as Fun)))
                    as Rc<RefCell<dyn Function>>,
            ),
            (
                "fmt".to_string(),
                Rc::new(RefCell::new(BuiltinFunction(builtin_fmt as Fun)))
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
            (
                "use".to_string(),
                Rc::new(RefCell::new(BuiltinFunction(builtin_use as Fun)))
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
struct UserType {}

impl Display for UserType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

#[derive(Debug, Clone)]
enum Type {
    Number(Number),
    String(String),
    UserType(UserType),
    Void,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Number(n) => write!(f, "{}", n),
            Type::String(s) => write!(f, "{}", s),
            Type::Void => write!(f, "void"),
            Type::UserType(t) => write!(f, "{}", t),
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
            Type::UserType(_) => todo!(),
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
            Type::UserType(_) => todo!(),
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
            Type::UserType(_) => todo!(),
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
            Type::UserType(_) => todo!(),
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
                state.borrow_mut().functions.insert(
                    ident.clone(),
                    Rc::new(RefCell::new(UserFunction::new(args, *body, ident))),
                );
                Type::Void
            }
            parser::LetStatement::Variable(ident, expr) => {
                let val = eval(*expr, state.clone());
                state.borrow_mut().variables.insert(ident.clone(), val);
                Type::Void
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
            Type::Void
        }
        AstNode::String(s) => Type::String(s),
        AstNode::Block(b) => {
            let inner_scope = Rc::new(RefCell::new(Scope {
                variables: HashMap::new(),
                functions: HashMap::new(),
                parent: Some(state),
            }));
            for stmt in b {
                match stmt {
                    AstNode::Return(r) => return eval(*r, inner_scope),
                    _ => {
                        eval(stmt, inner_scope.clone());
                    }
                }
            }
            Type::Void
        }
        AstNode::Return(stmt) => eval(*stmt, state),
        AstNode::Items(items) => {
            for stmt in items {
                eval(stmt, state.clone());
            }
            Type::Void
        }
    }
}
