use crate::lexer::{BinOp, Number, Token};
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
pub enum AstNode {
    BinOp(BinOp, Box<AstNode>, Box<AstNode>),
    UnaryOp(UnaryOp, Box<AstNode>),
    Number(Number),
}

pub fn parse(input: Vec<Token>) -> AstNode {
    parse_expr(&mut input.iter().peekable())
}

// expression ::= equality-expression
pub fn parse_expr(tokens: &mut Peekable<Iter<'_, Token>>) -> AstNode {
    parse_equality(tokens)
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
            let number = match tokens.next() {
                Some(Token::Number(n)) => *n,
                _ => panic!(),
            };
            AstNode::UnaryOp(UnaryOp::Neg, Box::new(AstNode::Number(number)))
        }
        Some(Token::LParen) => {
            let expr = parse_expr(tokens);
            match tokens.next() {
                Some(Token::RParen) => {}
                _ => panic!(),
            }
            expr
        }
        Some(Token::Number(n)) => AstNode::Number(*n),
        _ => panic!("could not parse primary expression"),
    }
}
