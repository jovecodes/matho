use crate::lexer::{Number, Operator};
use crate::parser::{AstNode, Field, LetStatement};
use crate::user;
use std::fmt::Debug;
use std::{cell::RefCell, collections::HashMap, fmt::Display, ops, rc::Rc};

pub trait Function {
    fn apply(&self, args: Vec<AstNode>, state: Rc<RefCell<Scope>>) -> Type;
}

pub type Fun = fn(Vec<AstNode>, Rc<RefCell<Scope>>) -> Type;

#[derive(Default)]
pub struct Scope {
    pub variables: HashMap<String, Type>,
    pub functions: HashMap<String, Rc<RefCell<dyn Function>>>,
    pub structures: HashMap<String, Rc<RefCell<Vec<Field>>>>,
    pub parent: Option<Rc<RefCell<Scope>>>,
}

impl Scope {
    pub fn get_variable(&self, ident: &str) -> Option<Type> {
        if let Some(var) = self.variables.get(ident) {
            Some(var.clone())
        } else if let Some(parent) = &self.parent {
            parent.borrow().get_variable(ident)
        } else {
            None
        }
    }

    pub fn set_variable(&mut self, ident: &str, value: Type) -> Result<(), ()> {
        if let Some(var) = self.variables.get_mut(ident) {
            *var = value;
            Ok(())
        } else if let Some(parent) = &self.parent {
            parent.borrow_mut().set_variable(ident, value)
        } else {
            Err(())
        }
    }

    pub fn get_function(&self, ident: &str) -> Option<Rc<RefCell<dyn Function>>> {
        if let Some(fun) = self.functions.get(ident) {
            Some(fun.clone())
        } else if let Some(parent) = &self.parent {
            parent.borrow().get_function(ident)
        } else {
            None
        }
    }

    pub fn get_structure(&self, ident: &str) -> Option<Rc<RefCell<Vec<Field>>>> {
        if let Some(structure) = self.structures.get(ident) {
            Some(structure.clone())
        } else if let Some(parent) = &self.parent {
            parent.borrow().get_structure(ident)
        } else {
            None
        }
    }
}

impl Debug for Scope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Scope(variables: {:?}, structs: {:?})",
            self.variables, self.structures
        )
    }
}

#[derive(Debug, Clone)]
pub enum Type {
    Number(Number),
    String(String),
    Bool(bool),
    StructDefinition(Vec<Field>),
    StructInstance(String, HashMap<String, Type>),
    Scope(Rc<RefCell<Scope>>),
    Reference(Rc<RefCell<Type>>),
    Any,
    Void,
}

impl Type {
    pub fn type_name(&self) -> String {
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
            Type::Scope(_) => "scope".to_owned(),
            Type::Reference(_) => "reference".to_owned(),
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
            Type::Scope(scope) => write!(f, "{scope:?}"),
            Type::Reference(r) => write!(f, "reference({})", r.borrow()),
        }
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Type::Number(l) => {
                if let Type::Number(r) = other {
                    l.as_float() == r.as_float()
                } else {
                    panic!()
                }
            }
            Type::String(l) => {
                if let Type::String(r) = other {
                    l == r
                } else {
                    panic!()
                }
            }
            Type::Bool(l) => {
                if let Type::Bool(r) = other {
                    l == r
                } else {
                    panic!()
                }
            }
            Type::StructDefinition(_) => todo!(),
            Type::StructInstance(_, _) => todo!(),
            Type::Any => todo!(),
            Type::Void => todo!(),
            Type::Scope(_) => todo!(),
            Type::Reference(r) => r.borrow().eq(other),
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
            Type::Scope(_) => todo!(),
            Type::Reference(_) => todo!(),
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
            Type::Scope(_) => todo!(),
            Type::Reference(_) => todo!(),
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
            Type::Scope(_) => todo!(),
            Type::Reference(_) => todo!(),
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
            Type::Scope(_) => todo!(),
            Type::Reference(_) => todo!(),
        }
    }
}

impl ops::Rem for Type {
    type Output = Type;

    fn rem(self, rhs: Self) -> Self::Output {
        match self {
            Type::Number(l) => match rhs {
                Type::Number(r) => Type::Number(Number::Float(l.as_float() % r.as_float())),
                _ => panic!(),
            },
            Type::String(_) => todo!(),
            Type::Void => todo!(),
            Type::Bool(_) => todo!(),
            Type::StructDefinition(_) => todo!(),
            Type::Any => todo!(),
            Type::StructInstance(_, _) => todo!(),
            Type::Scope(_) => todo!(),
            Type::Reference(_) => todo!(),
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

pub fn eval(ast: AstNode, state: Rc<RefCell<Scope>>) -> Type {
    match ast {
        AstNode::BinOp(op, lhs, rhs) => op.apply(eval(*lhs, state.clone()), eval(*rhs, state)),
        AstNode::Compare(op, lhs, rhs) => match op {
            Operator::Equality => {
                let l = eval(*lhs, state.clone());
                let r = eval(*rhs, state);
                Type::Bool(l.eq(&r))
            }
            Operator::Inequality => {
                let l = eval(*lhs, state.clone());
                let r = eval(*rhs, state);
                Type::Bool(l.ne(&r))
            }
            _ => panic!(),
        },
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
            LetStatement::Function(ident, args, body) => {
                let definition = Rc::new(RefCell::new(user::UserFunction::new(
                    args,
                    *body,
                    ident.clone(),
                    &state.borrow(),
                )));
                state.borrow_mut().functions.insert(ident, definition);
                Type::Void
            }
            LetStatement::Variable(ident, expr) => {
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
        AstNode::Access(outer, inner) => {
            if let Some(var) = state.borrow().get_variable(&outer) {
                match var {
                    Type::StructInstance(s, fields) => fields
                        .get(&inner)
                        .expect(&format!("could not find field {inner} in {s}"))
                        .clone(),
                    Type::Scope(_) => todo!(),
                    Type::Any => todo!("indexing on anys"),
                    _ => panic!(),
                }
            } else {
                panic!()
            }
        }
    }
}
