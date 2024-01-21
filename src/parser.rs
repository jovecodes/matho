use crate::lexer::{Keyword, Number, Operator, Token, TokenKind};
use std::{iter::Peekable, ops::Neg, slice::Iter};

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
pub enum LetStatement {
    Function(String, Vec<String>, Box<AstNode>),
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
}

pub fn parse(input: Vec<Token>) -> AstNode {
    parse_expr(&mut input.iter().peekable())
}

// expression ::= equality-expression
pub fn parse_expr(tokens: &mut Peekable<Iter<'_, Token>>) -> AstNode {
    if let TokenKind::Keyword(Keyword::Let) = tokens.peek().unwrap().kind {
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
    let ident = match &tokens.next().unwrap().kind {
        TokenKind::ID(i) => i.clone(),
        _ => panic!(),
    };

    if let TokenKind::LParen = tokens.peek().unwrap().kind {
        tokens.next();
        let mut args = vec![];
        loop {
            match &tokens.next().unwrap().kind {
                TokenKind::RParen => break,
                TokenKind::ID(ident) => args.push(ident.clone()),
                TokenKind::Comma => {}
                _ => panic!(),
            }
        }

        match tokens.next().unwrap().kind {
            TokenKind::Op(Operator::Eq) => {}
            _ => panic!("expected '='"),
        }

        let body = parse_expr(tokens);
        AstNode::LetStatement(LetStatement::Function(ident, args, Box::new(body)))
    } else {
        let next = tokens.next().unwrap();
        match next.kind {
            TokenKind::Op(Operator::Eq) => {}
            _ => panic!("expected '=' but got {next}"),
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

// primary ::= '(' expression ')' | NUMBER | VARIABLE | '-' primary
pub fn parse_primary(tokens: &mut Peekable<Iter<'_, Token>>) -> AstNode {
    let next = &tokens.next().unwrap();
    match &next.kind {
        TokenKind::Op(Operator::Sub) => {
            AstNode::UnaryOp(UnaryOp::Neg, Box::new(parse_primary(tokens)))
        }
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
            } else if let TokenKind::Op(Operator::Eq) = tokens.peek().unwrap().kind {
                tokens.next();
                AstNode::Assignment(ident.clone(), Box::new(parse_expr(tokens)))
            } else {
                AstNode::Variable(ident.clone())
            }
        }
        TokenKind::Number(n) => AstNode::Number(n.clone()),
        TokenKind::String(s) => AstNode::String(s.clone()),
        _ => panic!("could not parse primary expression {next}"),
    }
}
