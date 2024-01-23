use dyn_fmt::AsStrFormatExt;
use lexer::Number;
use parser::{AstNode, Field};
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
    args: Vec<Field>,
    body: AstNode,
    name: String,
}

impl UserFunction {
    fn new(args: Vec<Field>, body: AstNode, name: String, scope: &Scope) -> Self {
        for arg in &args {
            if arg.kind.is_some() && scope.get_structure(&arg.kind.as_ref().unwrap()).is_none() {
                panic!("Unknown type {}", arg.ident);
            }
        }
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
            let evaled_arg = eval(args[i].clone(), state.clone());
            if arg.kind.is_some() {
                let kind = arg.kind.as_ref().unwrap();
                if &evaled_arg.type_name() != kind {
                    panic!(
                        "Argument '{}' is of type '{}' but was expected to be of type '{}'",
                        evaled_arg,
                        evaled_arg.type_name(),
                        kind
                    )
                }
            }
            variables.insert(arg.ident.clone(), evaled_arg);
        }
        let scope = Rc::new(RefCell::new(Scope {
            variables,
            functions: HashMap::new(),
            structures: HashMap::new(),
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

#[derive(Default)]
struct Scope {
    variables: HashMap<String, Type>,
    functions: HashMap<String, Rc<RefCell<dyn Function>>>,
    structures: HashMap<String, Rc<RefCell<Vec<Field>>>>,
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

    fn get_structure(&self, ident: &str) -> Option<Rc<RefCell<Vec<Field>>>> {
        if let Some(structure) = self.structures.get(ident) {
            Some(structure.clone())
        } else if let Some(parent) = &self.parent {
            parent.borrow().get_structure(ident)
        } else {
            None
        }
    }
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

fn builtin_use(args: Vec<AstNode>, state: Rc<RefCell<Scope>>) -> Type {
    assert!(args.len() == 1);

    let path = match eval(args[0].clone(), state.clone()) {
        Type::String(n) => n,
        _ => panic!("expected file path!"),
    };

    let file = fs::read_to_string(path.clone()).expect(&format!("Could not open file {path}!"));
    let tokens = lexer::Lexer::load(&file).tokens;
    let ast = parser::parse_file(tokens);
    eval(ast, state.clone())
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

fn main() {
    let state = Rc::new(RefCell::new(Scope {
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
            add_builtin_fn("use", builtin_use),
        ]
        .into_iter()
        .collect(),
        structures: vec![("type".to_string(), Rc::new(RefCell::new(vec![])))]
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

        if let Type::Void = result {
        } else {
            println!("{}", result);
        }
    }
}

#[derive(Debug, Clone)]
enum Type {
    Number(Number),
    String(String),
    Bool(bool),
    StructDefinition(Vec<Field>),
    StructInstance(String, HashMap<String, Type>),
    Any,
    Void,
}

impl Type {
    fn type_name(&self) -> String {
        match self {
            Type::Number(n) => match n {
                Number::Int(_) => "int".to_owned(),
                Number::Float(_) => "float".to_owned(),
            },
            Type::String(_) => "string".to_owned(),
            Type::Bool(_) => "bool".to_owned(),
            Type::Void => "void".to_owned(),
            Type::StructDefinition(s) => format!("{:?}", s),
            Type::Any => "any".to_owned(),
            Type::StructInstance(name, _) => name.clone(),
        }
    }

    fn default_from_name(name: Option<String>) -> Self {
        if name.is_none() {
            return Type::Any;
        }
        match name.as_ref().unwrap().as_str() {
            "string" => Self::String(String::new()),
            "int" => Self::Number(Number::Int(0)),
            "float" => Self::Number(Number::Float(0.0)),
            "bool" => Self::Bool(false),
            "void" => Self::Void,
            "any" => Self::Any,
            _ => panic!(),
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Number(n) => write!(f, "{}", n),
            Type::String(s) => write!(f, "{}", s),
            Type::Bool(b) => write!(f, "{}", b),
            Type::Void => write!(f, "void"),
            Type::StructDefinition(fields) => {
                write!(f, "struct(").unwrap();
                for (i, field) in fields.iter().enumerate() {
                    if i != fields.len() - 1 {
                        write!(f, "{field}, ").unwrap();
                    } else {
                        write!(f, "{field}").unwrap();
                    }
                }
                write!(f, ")")
            }
            Type::Any => write!(f, "any"),
            Type::StructInstance(name, fields) => {
                write!(f, "{name}(").unwrap();
                for (i, (field, value)) in fields.iter().enumerate() {
                    if i != fields.len() - 1 {
                        write!(f, "{field} = {value}, ").unwrap();
                    } else {
                        write!(f, "{field} = {value}").unwrap();
                    }
                }
                write!(f, ")")
            }
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
            Type::Bool(_) => todo!(),
            Type::StructDefinition(_) => todo!(),
            Type::Any => todo!(),
            Type::StructInstance(_, _) => todo!(),
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
            Type::Bool(_) => todo!(),
            Type::StructDefinition(_) => todo!(),
            Type::Any => todo!(),
            Type::StructInstance(_, _) => todo!(),
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
            Type::Bool(_) => todo!(),
            Type::StructDefinition(_) => todo!(),
            Type::Any => todo!(),
            Type::StructInstance(_, _) => todo!(),
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
            Type::Bool(_) => todo!(),
            Type::StructDefinition(_) => todo!(),
            Type::Any => todo!(),
            Type::StructInstance(_, _) => todo!(),
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
        AstNode::Bool(b) => Type::Bool(b),
        AstNode::Variable(var) => {
            if let Some(val) = state.borrow().get_variable(&var) {
                val
            } else {
                panic!("Unknown variable '{}'", var)
            }
        }
        AstNode::Funcall(fun, args) => {
            if let Some(f) = state.borrow().get_function(&fun) {
                f.borrow().apply(args, state.clone())
            } else if let Some(v) = state.borrow().get_variable(&fun) {
                match v {
                    Type::StructDefinition(fields) => {
                        let mut variables = HashMap::new();
                        for field in fields {
                            variables.insert(field.ident, Type::default_from_name(field.kind));
                        }

                        let scope = Rc::new(RefCell::new(Scope {
                            variables,
                            parent: Some(state.clone()),
                            ..Default::default()
                        }));

                        for arg in args {
                            eval(arg, scope.clone());
                        }

                        Type::StructInstance(fun, scope.clone().borrow().variables.clone())
                    }
                    _ => panic!(),
                }
            } else {
                panic!("Unknown function '{}'", fun)
            }
        }
        AstNode::LetStatement(stmt) => match stmt {
            parser::LetStatement::Function(ident, args, body) => {
                let definition = Rc::new(RefCell::new(UserFunction::new(
                    args,
                    *body,
                    ident.clone(),
                    &state.borrow(),
                )));
                state.borrow_mut().functions.insert(ident, definition);
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
                parent: Some(state),
                ..Default::default()
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
        AstNode::Struct(fields) => Type::StructDefinition(fields),
    }
}
