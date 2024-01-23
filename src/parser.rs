use crate::lexer::{Keyword, Number, Operator, Token, TokenKind};
use std::{fmt::Display, iter::Peekable, ops::Neg, slice::Iter};

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    Neg,
}

impl UnaryOp {
    pub fn apply<T>(self, n: T) -> T
    where
        T: Neg<Output = T>,
    {
        match self {
            UnaryOp::Neg => -n,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Field {
    pub ident: String,
    pub kind: Option<String>,
}

impl Display for Field {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let kind_str = match &self.kind {
            Some(k) => k,
            None => "any",
        };
        let name = &self.ident;
        write!(f, "{name}: {kind_str}")
    }
}

#[derive(Debug, Clone)]
pub enum LetStatement {
    Function(String, Vec<Field>, Box<AstNode>),
    Variable(String, Box<AstNode>),
}

#[derive(Debug, Clone)]
pub enum AstNode {
    BinOp(Operator, Box<AstNode>, Box<AstNode>),
    UnaryOp(UnaryOp, Box<AstNode>),
    Number(Number),
    String(String),
    Variable(String),
    Funcall(String, Vec<AstNode>),
    LetStatement(LetStatement),
    Assignment(String, Box<AstNode>),
    Block(Vec<AstNode>),
    Return(Box<AstNode>),
    Items(Vec<AstNode>),
    Struct(Vec<Field>),
    Bool(bool),
}

pub fn parse(input: Vec<Token>) -> AstNode {
    let mut tokens = input.iter().peekable();
    let res = parse_expr(&mut tokens);
    match tokens.next() {
        Some(token) => match token.kind {
            TokenKind::EOF => {}
            _ => panic!("expected EOF but got {token}"),
        },
        None => {}
    }
    res
}

pub fn parse_file(input: Vec<Token>) -> AstNode {
    parse_items(&mut input.iter().peekable())
}

pub fn parse_items(tokens: &mut Peekable<Iter<'_, Token>>) -> AstNode {
    let mut items = vec![];
    while tokens.peek().unwrap().kind != TokenKind::EOF {
        items.push(parse_item(tokens));
    }
    AstNode::Items(items)
}

// item ::= (statement | structure)
pub fn parse_item(tokens: &mut Peekable<Iter<'_, Token>>) -> AstNode {
    let statement = parse_expr(tokens);
    match &statement {
        AstNode::LetStatement(stmt) => match stmt {
            LetStatement::Variable(_, _) => {
                if tokens.peek().unwrap().kind != TokenKind::Semi {
                    panic!("expected ';' but got {:?}", stmt);
                } else {
                    tokens.next();
                }
            }
            _ => {}
        },
        AstNode::Funcall(_, _) => {
            if tokens.peek().unwrap().kind != TokenKind::Semi {
                panic!("expected ';' but got {:?}", tokens.next());
            } else {
                tokens.next();
            }
        }
        _ => panic!("expected item but got {:?}", statement),
    }
    statement
}

// expression ::= equality-expression
pub fn parse_expr(tokens: &mut Peekable<Iter<'_, Token>>) -> AstNode {
    let peek = &tokens.peek().unwrap().kind;
    if let TokenKind::Keyword(Keyword::Let) = peek {
        parse_let_statement(tokens)
    } else if let TokenKind::Keyword(Keyword::Struct) = peek {
        parse_struct(tokens)
    } else {
        parse_equality(tokens)
    }
}

// struct ::= struct( (field,)* (field)? )
pub fn parse_struct(tokens: &mut Peekable<Iter<'_, Token>>) -> AstNode {
    tokens.next(); // struct
    tokens.next(); // (
    let fields = parse_fields(tokens);
    AstNode::Struct(fields)
}

// let-statement ::= (function | variable)
pub fn parse_let_statement(tokens: &mut Peekable<Iter<'_, Token>>) -> AstNode {
    tokens.next();
    let ident = match &tokens.next().unwrap().kind {
        TokenKind::ID(i) => i.clone(),
        _ => panic!(),
    };

    if let TokenKind::LParen = tokens.peek().unwrap().kind {
        parse_function_definition(tokens, ident)
    } else {
        parse_variable_definition(tokens, ident)
    }
}

fn parse_fields(tokens: &mut Peekable<Iter<'_, Token>>) -> Vec<Field> {
    let mut args = vec![];
    loop {
        let current = &tokens.next().unwrap();
        match &current.kind {
            TokenKind::RParen => break,
            TokenKind::ID(ident) => args.push(Field {
                ident: ident.clone(),
                kind: None,
            }),
            TokenKind::Comma => {}
            TokenKind::Colon => {
                args.last_mut().unwrap().kind = Some(parse_type(tokens));
            }
            _ => panic!("expected field but got {current}"),
        }
    }
    args
}

// function ::= let ident(args) = expr
fn parse_function_definition(tokens: &mut Peekable<Iter<'_, Token>>, ident: String) -> AstNode {
    tokens.next();

    let args = parse_fields(tokens);

    match tokens.next().unwrap().kind {
        TokenKind::Op(Operator::Eq) => {}
        _ => panic!("expected '='"),
    }

    let body = parse_expr(tokens);
    AstNode::LetStatement(LetStatement::Function(ident, args, Box::new(body)))
}

fn parse_type(tokens: &mut Peekable<Iter<'_, Token>>) -> String {
    let token = &tokens.next().unwrap().kind;
    match token {
        TokenKind::ID(id) => id.clone(),
        TokenKind::Keyword(Keyword::Int) => "int".to_owned(),
        TokenKind::Keyword(Keyword::Float) => "float".to_owned(),
        TokenKind::Keyword(Keyword::Bool) => "bool".to_owned(),
        TokenKind::Keyword(Keyword::String) => "string".to_owned(),
        _ => panic!("expected type but got {token}",),
    }
}

// variable ::= let ident = expr
fn parse_variable_definition(tokens: &mut Peekable<Iter<'_, Token>>, ident: String) -> AstNode {
    let next = tokens.next().unwrap();
    match next.kind {
        TokenKind::Op(Operator::Eq) => {}
        _ => panic!("expected '=' but got {next}"),
    }

    let value = parse_expr(tokens);
    AstNode::LetStatement(LetStatement::Variable(ident, Box::new(value)))
}

// equality-expression ::= additive-expression ( ( '==' | '!=' ) additive-expression ) *
pub fn parse_equality(tokens: &mut Peekable<Iter<'_, Token>>) -> AstNode {
    parse_additive(tokens)
}

// additive-expression ::= multiplicative-expression ( ( '+' | '-' ) multiplicative-expression ) *
pub fn parse_additive(tokens: &mut Peekable<Iter<'_, Token>>) -> AstNode {
    let mut node = parse_multiplicative(tokens);

    while let TokenKind::Op(Operator::Add) | TokenKind::Op(Operator::Sub) =
        tokens.peek().unwrap().kind
    {
        let op = match tokens.next().unwrap().kind {
            TokenKind::Op(Operator::Add) => Operator::Add,
            TokenKind::Op(Operator::Sub) => Operator::Sub,
            _ => panic!(),
        };
        let rhs = parse_multiplicative(tokens);
        node = AstNode::BinOp(op, Box::new(node), Box::new(rhs));
    }

    node
}

// multiplicative-expression ::= primary ( ( '*' | '/' ) primary ) *
pub fn parse_multiplicative(tokens: &mut Peekable<Iter<'_, Token>>) -> AstNode {
    let mut node = parse_primary(tokens);

    while let TokenKind::Op(Operator::Mul) | TokenKind::Op(Operator::Div) =
        tokens.peek().unwrap().kind
    {
        let op = match tokens.next().unwrap().kind {
            TokenKind::Op(Operator::Mul) => Operator::Mul,
            TokenKind::Op(Operator::Div) => Operator::Div,
            _ => panic!(),
        };
        let rhs = parse_primary(tokens);
        node = AstNode::BinOp(op, Box::new(node), Box::new(rhs));
    }

    node
}

fn parse_statement(tokens: &mut Peekable<Iter<'_, Token>>) -> AstNode {
    let statement = parse_expr(tokens);
    if tokens.peek().unwrap().kind == TokenKind::Semi {
        tokens.next();
        statement
    } else {
        AstNode::Return(Box::new(statement))
    }
}

fn parse_block(tokens: &mut Peekable<Iter<'_, Token>>) -> AstNode {
    let mut statements = vec![];
    while tokens.peek().unwrap().kind != TokenKind::RCurly {
        statements.push(parse_statement(tokens));
    }
    tokens.next();
    AstNode::Block(statements)
}

// primary ::= '(' expression ')' | NUMBER | VARIABLE | '-' primary
pub fn parse_primary(tokens: &mut Peekable<Iter<'_, Token>>) -> AstNode {
    let next = &tokens.next().unwrap();
    match &next.kind {
        TokenKind::Op(Operator::Sub) => {
            AstNode::UnaryOp(UnaryOp::Neg, Box::new(parse_primary(tokens)))
        }
        TokenKind::Keyword(Keyword::True) => AstNode::Bool(true),
        TokenKind::Keyword(Keyword::False) => AstNode::Bool(false),
        TokenKind::LParen => {
            let expr = parse_expr(tokens);
            match tokens.next().unwrap().kind {
                TokenKind::RParen => {}
                _ => panic!(),
            }
            expr
        }
        TokenKind::ID(ident) => {
            if let TokenKind::LParen = tokens.peek().unwrap().kind {
                tokens.next();
                if let TokenKind::RParen = tokens.peek().unwrap().kind {
                    tokens.next();
                    AstNode::Funcall(ident.clone(), vec![])
                } else {
                    let mut args = vec![parse_expr(tokens)];
                    loop {
                        match tokens.peek().unwrap().kind {
                            TokenKind::RParen => break,
                            TokenKind::Comma => {
                                tokens.next();
                                args.push(parse_expr(tokens))
                            }
                            _ => panic!("{:?}", tokens.peek()),
                        }
                    }
                    tokens.next();
                    AstNode::Funcall(ident.clone(), args)
                }
            } else {
                match tokens.peek().unwrap().kind {
                    TokenKind::Op(Operator::Eq) => {
                        tokens.next();
                        AstNode::Assignment(ident.clone(), Box::new(parse_expr(tokens)))
                    }
                    TokenKind::Op(Operator::AddEq) => {
                        tokens.next();
                        let expr = AstNode::BinOp(
                            Operator::Add,
                            Box::new(AstNode::Variable(ident.clone())),
                            Box::new(parse_expr(tokens)),
                        );
                        AstNode::Assignment(ident.clone(), Box::new(expr))
                    }
                    TokenKind::Op(Operator::SubEq) => {
                        tokens.next();
                        let expr = AstNode::BinOp(
                            Operator::Sub,
                            Box::new(AstNode::Variable(ident.clone())),
                            Box::new(parse_expr(tokens)),
                        );
                        AstNode::Assignment(ident.clone(), Box::new(expr))
                    }
                    TokenKind::Op(Operator::MulEq) => {
                        tokens.next();
                        let expr = AstNode::BinOp(
                            Operator::Mul,
                            Box::new(AstNode::Variable(ident.clone())),
                            Box::new(parse_expr(tokens)),
                        );
                        AstNode::Assignment(ident.clone(), Box::new(expr))
                    }
                    TokenKind::Op(Operator::DivEq) => {
                        tokens.next();
                        let expr = AstNode::BinOp(
                            Operator::Div,
                            Box::new(AstNode::Variable(ident.clone())),
                            Box::new(parse_expr(tokens)),
                        );
                        AstNode::Assignment(ident.clone(), Box::new(expr))
                    }
                    _ => AstNode::Variable(ident.clone()),
                }
            }
        }
        TokenKind::Number(n) => AstNode::Number(n.clone()),
        TokenKind::String(s) => AstNode::String(s.clone()),
        TokenKind::LCurly => parse_block(tokens),
        _ => panic!("could not parse primary expression {next}"),
    }
}
