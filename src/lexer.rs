#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    Mul,
    Div,
    Sub,
    Add,
}

impl BinOp {
    pub fn from_char(c: char) -> Option<Self> {
        match c {
            '*' => Some(Self::Mul),
            '/' => Some(Self::Div),
            '+' => Some(Self::Add),
            '-' => Some(Self::Sub),
            _ => None,
        }
    }

    pub fn apply(&self, lhs: f64, rhs: f64) -> f64 {
        match self {
            BinOp::Mul => lhs * rhs,
            BinOp::Div => lhs / rhs,
            BinOp::Sub => lhs - rhs,
            BinOp::Add => lhs + rhs,
        }
    }
}

#[derive(Debug)]
pub enum Token {
    Op(BinOp),
    Number(Number),
    LParen,
    RParen,
}

pub fn lex(input: &str) -> Vec<Token> {
    let mut tokens = vec![];
    let mut chars = input.chars().peekable();
    loop {
        match chars.next() {
            Some(c) => match c {
                '0'..='9' => {
                    let mut num = String::from(c);

                    while let Some(next) = chars.peek() {
                        if !next.is_ascii_digit() && next != &'.' {
                            break;
                        }
                        num.push(chars.next().unwrap());
                    }
                    if let Ok(n) = num.parse::<i64>() {
                        tokens.push(Token::Number(Number::Int(n)))
                    } else if let Ok(n) = num.parse::<f64>() {
                        tokens.push(Token::Number(Number::Float(n)))
                    }
                }
                ' ' | '\n' | '\t' | '\r' => {}
                _ => {
                    if let Some(op) = BinOp::from_char(c) {
                        tokens.push(Token::Op(op));
                    } else if c == '(' {
                        tokens.push(Token::LParen);
                    } else if c == ')' {
                        tokens.push(Token::RParen);
                    } else {
                        panic!("Unexpected character '{c}'!");
                    }
                }
            },
            None => break,
        }
    }
    return tokens;
}

#[derive(Debug, Clone, Copy)]
pub enum Number {
    Float(f64),
    Int(i64),
}

impl Into<f64> for Number {
    fn into(self) -> f64 {
        match self {
            Number::Float(n) => n,
            Number::Int(n) => n as f64,
        }
    }
}
