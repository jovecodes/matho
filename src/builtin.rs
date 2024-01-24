use std::{
    cell::RefCell,
    f64::consts,
    fs,
    io::{self, Write},
    rc::Rc,
};

use dyn_fmt::AsStrFormatExt;

use crate::{
    interpreter::eval,
    interpreter::Fun,
    interpreter::Function,
    interpreter::Scope,
    interpreter::Type,
    lexer::{self, Number},
    parser::{self, AstNode},
};

const PHI: f64 = 1.618033988749894848204586834365638118;

pub struct BuiltinFunction(pub Fun);

impl Function for BuiltinFunction {
    fn apply(&self, args: Vec<AstNode>, state: Rc<RefCell<Scope>>) -> Type {
        (self.0)(args, state)
    }
}

pub fn global_scope() -> Rc<RefCell<Scope>> {
    Rc::new(RefCell::new(Scope {
        variables: vec![
            add_builtin_constant("pi", consts::PI),
            add_builtin_constant("e", consts::E),
            add_builtin_constant("tau", consts::TAU),
            add_builtin_constant("phi", PHI),
        ]
        .into_iter()
        .collect(),
        functions: vec![
            add_builtin_fn("dbg_echo", builtin_dbg_echo),
            add_builtin_fn("print", builtin_print),
            add_builtin_fn("println", builtin_println),
            add_builtin_fn("fmt", builtin_fmt),
            add_builtin_fn("sqrt", builtin_sqrt),
            add_builtin_fn("ln", builtin_ln),
            add_builtin_fn("exit", builtin_exit),
            add_builtin_fn("include", builtin_include),
        ]
        .into_iter()
        .collect(),
        structures: vec![("type".to_string(), Rc::new(RefCell::new(vec![])))]
            .into_iter()
            .collect(),
        parent: None,
    }))
}

fn builtin_dbg_echo(args: Vec<AstNode>, state: Rc<RefCell<Scope>>) -> Type {
    assert!(args.len() == 1);
    let input = match args.first().unwrap() {
        AstNode::String(string) => string,
        _ => panic!("dbg_echo expects only a string"),
    };

    let scope = Rc::new(RefCell::new(Scope {
        parent: Some(state.clone()),
        ..Default::default()
    }));

    let tokens = lexer::Lexer::load(&input).tokens;
    println!("Tokens: {tokens:?}");
    let ast = parser::parse(tokens);
    println!("Ast: {ast:?}");
    let result = eval(ast, scope);
    println!("Result: {result}");

    Type::Void
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

fn builtin_include(args: Vec<AstNode>, state: Rc<RefCell<Scope>>) -> Type {
    assert!(args.len() == 1);

    let path = match eval(args[0].clone(), state.clone()) {
        Type::String(n) => n,
        _ => panic!("expected file path!"),
    };

    let scope = Rc::new(RefCell::new(Scope {
        parent: Some(state),
        ..Default::default()
    }));

    let file = fs::read_to_string(path.clone()).expect(&format!("Could not open file {path}!"));
    let tokens = lexer::Lexer::load(&file).tokens;
    let ast = parser::parse_file(tokens);
    eval(ast, scope.clone());
    Type::Scope(scope)
}

fn add_builtin_fn(name: &str, fun: Fun) -> (String, Rc<RefCell<dyn Function>>) {
    (
        name.to_string(),
        Rc::new(RefCell::new(BuiltinFunction(fun))) as Rc<RefCell<dyn Function>>,
    )
}

fn add_builtin_constant(name: &str, num: f64) -> (String, Type) {
    (name.to_string(), Type::Number(Number::Float(num)))
}
