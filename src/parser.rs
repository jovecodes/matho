use crate::lexer::{BinOp, Keyword, Number, Token};
use std::{iter::Peekable, slice::Iter};

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    Neg,
}

impl UnaryOp {
    pub fn apply(&self, n: f64) -> f64 {
        match self {
            UnaryOp::Neg => n * -1.0,
        }
    }
}

#[derive(Debug, Clone)]
pub enum LetStatement {
    Function(String, Vec<String>, Box<AstNode>),
    Variable(String, Box<AstNode>),
}

#[derive(Debug, Clone)]
pub enum AstNode {
    BinOp(BinOp, Box<AstNode>, Box<AstNode>),
    UnaryOp(UnaryOp, Box<AstNode>),
    Number(Number),
    Variable(String),
    Funcall(String, Vec<AstNode>),
    LetStatement(LetStatement),
    Assignment(String, Box<AstNode>),
}

pub fn parse(input: Vec<Token>) -> AstNode {
    parse_expr(&mut input.iter().peekable())
}

// expression ::= equality-expression
pub fn parse_expr(tokens: &mut Peekable<Iter<'_, Token>>) -> AstNode {
    if let Some(Token::Keyword(Keyword::Let)) = tokens.peek() {
        parse_let_statement(tokens)
    } else {
        parse_equality(tokens)
    }
}

// let-statement ::= (function | variable)
// function ::= let ident(args) = expr
// variable ::= let ident = expr
pub fn parse_let_statement(tokens: &mut Peekable<Iter<'_, Token>>) -> AstNode {
    tokens.next();
    let ident = match tokens.next().unwrap() {
        Token::Ident(i) => i.clone(),
        _ => panic!(),
    };
    if let Some(Token::LParen) = tokens.peek() {
        tokens.next();
        let mut args = vec![];
        loop {
            match tokens.next() {
                Some(Token::RParen) => break,
                Some(Token::Ident(ident)) => args.push(ident.clone()),
                Some(Token::Comma) => {}
                _ => panic!(),
            }
        }

        match tokens.next() {
            Some(Token::Assign) => {}
            _ => panic!("expected '='"),
        }

        let body = parse_expr(tokens);
        AstNode::LetStatement(LetStatement::Function(ident, args, Box::new(body)))
    } else {
        match tokens.next() {
            Some(Token::Assign) => {}
            _ => panic!("expected '='"),
        }

        let value = parse_expr(tokens);
        AstNode::LetStatement(LetStatement::Variable(ident, Box::new(value)))
    }
}

// equality-expression ::= additive-expression ( ( '==' | '!=' ) additive-expression ) *
pub fn parse_equality(tokens: &mut Peekable<Iter<'_, Token>>) -> AstNode {
    parse_additive(tokens)
}

// additive-expression ::= multiplicative-expression ( ( '+' | '-' ) multiplicative-expression ) *
pub fn parse_additive(tokens: &mut Peekable<Iter<'_, Token>>) -> AstNode {
    let mut node = parse_multiplicative(tokens);

    while let Some(Token::Op(BinOp::Add)) | Some(Token::Op(BinOp::Sub)) = tokens.peek() {
        let op = match tokens.next().unwrap() {
            Token::Op(BinOp::Add) => BinOp::Add,
            Token::Op(BinOp::Sub) => BinOp::Sub,
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

    while let Some(Token::Op(BinOp::Mul)) | Some(Token::Op(BinOp::Div)) = tokens.peek() {
        let op = match tokens.next().unwrap() {
            Token::Op(BinOp::Mul) => BinOp::Mul,
            Token::Op(BinOp::Div) => BinOp::Div,
            _ => panic!(),
        };
        let rhs = parse_primary(tokens);
        node = AstNode::BinOp(op, Box::new(node), Box::new(rhs));
    }

    node
}

// primary ::= '(' expression ')' | NUMBER | VARIABLE | '-' primary
pub fn parse_primary(tokens: &mut Peekable<Iter<'_, Token>>) -> AstNode {
    match tokens.next() {
        Some(Token::Op(BinOp::Sub)) => {
            AstNode::UnaryOp(UnaryOp::Neg, Box::new(parse_primary(tokens)))
        }
        Some(Token::LParen) => {
            let expr = parse_expr(tokens);
            match tokens.next() {
                Some(Token::RParen) => {}
                _ => panic!(),
            }
            expr
        }
        Some(Token::Ident(ident)) => {
            if let Some(Token::LParen) = tokens.peek() {
                tokens.next();
                if let Some(Token::RParen) = tokens.peek() {
                    tokens.next();
                    AstNode::Funcall(ident.clone(), vec![])
                } else {
                    let mut args = vec![parse_expr(tokens)];
                    loop {
                        match tokens.peek() {
                            Some(Token::RParen) => break,
                            Some(Token::Comma) => {
                                tokens.next();
                                args.push(parse_expr(tokens))
                            }
                            _ => panic!("{:?}", tokens.peek()),
                        }
                    }
                    tokens.next();
                    AstNode::Funcall(ident.clone(), args)
                }
            } else if let Some(Token::Assign) = tokens.peek() {
                tokens.next();
                AstNode::Assignment(ident.clone(), Box::new(parse_expr(tokens)))
            } else {
                AstNode::Variable(ident.clone())
            }
        }
        Some(Token::Number(n)) => AstNode::Number(*n),
        _ => panic!("could not parse primary expression"),
    }
}
