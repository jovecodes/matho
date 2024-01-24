use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    interpreter::eval,
    interpreter::{Function, Scope, Type},
    parser::{AstNode, Field},
};

pub struct UserFunction {
    pub args: Vec<Field>,
    pub body: AstNode,
    pub name: String,
}

impl UserFunction {
    pub fn new(args: Vec<Field>, body: AstNode, name: String, scope: &Scope) -> Self {
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
