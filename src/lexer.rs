pub use crate::pos::Pos;
use std::fmt::Display;
use std::iter::Peekable;
use std::ops::{Add, Div, Mul, Sub};
use std::str::Chars;
use std::str::FromStr;

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    Op(Operator),
    RParen,
    LParen,
    RCurly,
    LCurly,
    Colon,
    Comma,
    Semi,
    Dot,
    Number(Number),
    String(String),
    WhiteSpace,
    ID(String),
    Keyword(Keyword),
    EOF,
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Number {
    Int(i64),
    Float(f64),
}

impl Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Number::Int(n) => write!(f, "{}", n),
            Number::Float(n) => write!(f, "{}", n),
        }
    }
}

impl Number {
    pub fn as_float(self) -> f64 {
        self.into()
    }
}

impl Into<f64> for Number {
    fn into(self) -> f64 {
        match self {
            Number::Int(n) => n as f64,
            Number::Float(n) => n,
        }
    }
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use TokenKind::*;
        match self {
            Op(op) => write!(f, "{op}"),
            RParen => write!(f, "("),
            LParen => write!(f, ")"),
            RCurly => write!(f, "{{"),
            LCurly => write!(f, "}}"),
            Colon => write!(f, ":"),
            Comma => write!(f, ","),
            Semi => write!(f, ";"),
            Dot => write!(f, "."),
            Number(n) => write!(f, "number({n:?})"),
            WhiteSpace => write!(f, "whitespace"),
            ID(id) => write!(f, "identifier({id})"),
            Keyword(keyword) => write!(f, "keyword({keyword})"),
            String(string) => write!(f, "\"{string}\""),
            EOF => write!(f, "EOF"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum Keyword {
    Let,
    Int,
    Float,
    Bool,
    Char,
    Str,
    Return,
    Void,
    Break,
    While,
    If,
    Else,
    Function,
    True,
    False,
    Struct,
}

impl std::fmt::Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Keyword::*;
        match self {
            Int => write!(f, "int"),
            Let => write!(f, "let"),
            Float => write!(f, "float"),
            Bool => write!(f, "bool"),
            Char => write!(f, "char"),
            Str => write!(f, "str"),
            Return => write!(f, "return"),
            Void => write!(f, "void"),
            Break => write!(f, "break"),
            While => write!(f, "while"),
            If => write!(f, "if"),
            Else => write!(f, "else"),
            Function => write!(f, "fn"),
            True => write!(f, "true"),
            False => write!(f, "false"),
            Struct => write!(f, "struct"),
        }
    }
}

impl std::str::FromStr for Keyword {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use Keyword::*;

        match s {
            "let" => Ok(Let),
            "int" => Ok(Int),
            "float" => Ok(Float),
            "bool" => Ok(Bool),
            "char" => Ok(Char),
            "str" => Ok(Str),
            "return" => Ok(Return),
            "void" => Ok(Void),
            "break" => Ok(Break),
            "while" => Ok(While),
            "if" => Ok(If),
            "else" => Ok(Else),
            "fn" => Ok(Function),
            "true" => Ok(True),
            "false" => Ok(False),
            "struct" => Ok(Struct),
            _ => Err(()),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    AddEq,
    SubEq,
    MulEq,
    DivEq,
    ModEq,
    Equality,
    Inequality,
    Reference,
}

impl Operator {
    pub fn apply<T>(self, lhs: T, rhs: T) -> T
    where
        T: Add<Output = T> + Sub<Output = T> + Mul<Output = T> + Div<Output = T>, // + PartialEq
                                                                                  // + Eq
                                                                                  // + Copy,
    {
        match self {
            Operator::Add => lhs + rhs,
            Operator::Sub => lhs - rhs,
            Operator::Mul => lhs * rhs,
            Operator::Div => lhs / rhs,
            Operator::Mod => todo!(),
            Operator::Eq => todo!(),
            Operator::AddEq => todo!(),
            Operator::SubEq => todo!(),
            Operator::MulEq => todo!(),
            Operator::DivEq => todo!(),
            Operator::ModEq => todo!(),
            Operator::Equality => todo!(),
            Operator::Inequality => todo!(),
            Operator::Reference => todo!(),
        }
    }
}

impl std::fmt::Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operator::Add => write!(f, "+"),
            Operator::Sub => write!(f, "-"),
            Operator::Mul => write!(f, "*"),
            Operator::Div => write!(f, "/"),
            Operator::Mod => write!(f, "%"),
            Operator::Eq => write!(f, "="),
            Operator::AddEq => write!(f, "+="),
            Operator::SubEq => write!(f, "-="),
            Operator::MulEq => write!(f, "*="),
            Operator::DivEq => write!(f, "/="),
            Operator::ModEq => write!(f, "%="),
            Operator::Equality => write!(f, "=="),
            Operator::Inequality => write!(f, "!="),
            Operator::Reference => write!(f, "&"),
        }
    }
}

impl Operator {
    pub fn precedence(&self) -> usize {
        use Operator::*;
        // let op_prec = match operator {
        //     "or" => (Or, 0),
        //     "and" => (And, 0),
        //     "<|" => (PipeLeft, 0),
        //     "|>" => (PipeRight, 0),
        //     "==" => (Eq, 1),
        //     "<" => (Lt, 1),
        //     ">" => (Gt, 1),
        //     "!=" => (NEq, 1),
        //     "<=" => (LtEq, 1),
        //     ">=" => (GtEq, 1),
        //     "+" => (Add, 2),
        //     "-" => (Sub, 2),
        //     "++" => (Concat, 2),
        //     "*" => (Mul, 3),
        //     "/" => (Div, 3),
        //     "%" => (Mod, 3),
        //     "^" => (Pow, 4),
        //     _ => return None,
        // };
        match self {
            Add => 2,
            Sub => 2,
            Mul => 3,
            Div => 3,
            Mod => 3,
            Equality => 1,
            Inequality => 1,
            Reference => 10,
            Pointer => 10,
            _ => 0,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub pos: Pos,
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Token(`{}`, {})", self.kind, self.pos)
    }
}

impl Token {
    pub fn new(kind: TokenKind, pos: Pos) -> Token {
        Token { kind, pos }
    }

    pub fn ident_string(&self) -> Result<String, ()> {
        match &self.kind {
            TokenKind::ID(id) => Ok(id.clone()),
            _ => Err(()),
        }
    }
}

#[derive(Debug)]
pub struct Lexer<'a> {
    pub tokens: Vec<Token>,
    chars: Peekable<Chars<'a>>,
    pos: Pos,
}

impl Lexer<'_> {
    fn next(&mut self) -> Option<Token> {
        let next_char = self.chars.next()?;
        match next_char {
            '+' => {
                if self.chars.next_if(|x| x == &'=').is_some() {
                    Some(self.token(TokenKind::Op(Operator::AddEq)))
                } else {
                    Some(self.token(TokenKind::Op(Operator::Add)))
                }
            }
            '-' => {
                if self.chars.next_if(|x| x == &'=').is_some() {
                    Some(self.token(TokenKind::Op(Operator::Sub)))
                } else {
                    Some(self.token(TokenKind::Op(Operator::Sub)))
                }
            }
            '*' => {
                if self.chars.next_if(|x| x == &'=').is_some() {
                    Some(self.token(TokenKind::Op(Operator::Mul)))
                } else {
                    Some(self.token(TokenKind::Op(Operator::Mul)))
                }
            }
            '/' => {
                if self.chars.next_if(|x| x == &'=').is_some() {
                    Some(self.token(TokenKind::Op(Operator::Div)))
                } else if self.chars.next_if(|x| x == &'/').is_some() {
                    loop {
                        if let Some(next) = self.chars.next() {
                            if next == '\n' {
                                break;
                            }
                        } else {
                            break;
                        }
                    }

                    return self.next();
                } else if self.chars.next_if(|x| x == &'*').is_some() {
                    loop {
                        if let Some(next) = self.chars.next() {
                            if next == '*' {
                                if self.chars.next_if(|x| x == &'/').is_some() {
                                    break;
                                }
                            }
                        } else {
                            break;
                        }
                    }
                    return self.next();
                } else {
                    Some(self.token(TokenKind::Op(Operator::Div)))
                }
            }
            '=' => {
                if self.chars.next_if(|x| x == &'=').is_some() {
                    Some(self.token(TokenKind::Op(Operator::Equality)))
                } else {
                    Some(self.token(TokenKind::Op(Operator::Eq)))
                }
            }
            '(' => Some(self.token(TokenKind::LParen)),
            ')' => Some(self.token(TokenKind::RParen)),
            '{' => Some(self.token(TokenKind::RCurly)),
            '}' => Some(self.token(TokenKind::LCurly)),
            ';' => Some(self.token(TokenKind::Semi)),
            '.' => Some(self.token(TokenKind::Dot)),
            ':' => Some(self.token(TokenKind::Colon)),
            ',' => Some(self.token(TokenKind::Comma)),
            '&' => Some(self.token(TokenKind::Op(Operator::Reference))),
            '"' => {
                let mut string = String::new();
                let mut escape = false;
                loop {
                    if escape {
                        match self.chars.next() {
                            Some('n') => string.push('\n'),
                            Some('\\') => string.push('\\'),
                            Some('t') => string.push('\t'),
                            Some('r') => string.push('\r'),
                            Some('\'') => string.push('\''),
                            Some('"') => string.push('"'),
                            Some(c) => panic!("unknown escape character `{}` {}", c, self.pos),
                            None => break,
                        }
                        escape = false;
                    } else {
                        match self.chars.next_if(|x| x != &'"') {
                            Some('\\') => escape = true,
                            Some(c) => string.push(c),
                            None => break,
                        }
                    }
                }
                self.chars.next();
                Some(self.token(TokenKind::String(string)))
            }
            '0'..='9' => {
                let mut number = next_char.to_string();
                let mut is_float = false;

                while let Some(next_char) = self.chars.clone().next() {
                    self.pos.col += 1;
                    if next_char == '.' {
                        is_float = true;
                    }
                    if next_char.is_numeric() || next_char == '.' {
                        number.push(next_char);
                        self.chars.next();
                    } else if next_char.is_ident_start() {
                        panic!(
                            "expected whitespace after number definition at {}",
                            self.pos
                        );
                    } else {
                        break;
                    }
                }
                if is_float {
                    Some(self.token(TokenKind::Number(Number::Float(number.parse().unwrap()))))
                } else {
                    Some(self.token(TokenKind::Number(Number::Int(number.parse().unwrap()))))
                }
            }
            c if c.is_ident_start() => {
                let mut id = next_char.to_string();

                while let Some(next_char) = self.chars.clone().next() {
                    self.pos.col += 1;
                    if next_char.is_ident() {
                        id.push(next_char);
                        self.chars.next();
                    } else {
                        break;
                    }
                }
                let keyword_option = Keyword::from_str(id.as_str()).ok();
                if let Some(keyword) = keyword_option {
                    Some(self.token(TokenKind::Keyword(keyword)))
                } else {
                    Some(self.token(TokenKind::ID(id)))
                }
            }
            c if c.is_whitespace() => {
                self.pos.advance(next_char);
                while let Some(next_char) = self.chars.next_if(|x| x.is_whitespace()) {
                    self.pos.advance(next_char);
                }
                Some(self.token(TokenKind::WhiteSpace))
            }

            c => {
                panic!("unknown character {c} at {}", self.pos);
            }
        }
    }

    fn new<'a>(tokens: Vec<Token>, source: &'a str) -> Lexer<'a> {
        Lexer {
            tokens,
            chars: source.chars().peekable(),
            pos: Pos::new(1, 1),
        }
    }

    fn token(&self, token_type: TokenKind) -> Token {
        Token::new(token_type, self.pos)
    }

    pub fn load<'a>(input: &'a str) -> Lexer<'a> {
        let mut lexer = Lexer::new(vec![], input);
        while let Some(token) = lexer.next() {
            if token.kind != TokenKind::WhiteSpace {
                lexer.tokens.push(token);
            }
        }
        lexer
            .tokens
            .push(Token::new(TokenKind::EOF, Pos::new(0, 0)));
        return lexer;
    }
}

pub trait IsIdentChar {
    fn is_ident_start(&self) -> bool;
    fn is_ident(&self) -> bool;
}

impl IsIdentChar for char {
    fn is_ident_start(&self) -> bool {
        self.is_alphabetic() || self == &'_'
    }

    fn is_ident(&self) -> bool {
        self.is_alphanumeric() || self == &'_'
    }
}
