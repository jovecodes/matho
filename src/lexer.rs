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

#[derive(Debug, Clone, Copy)]
pub enum Keyword {
    Let,
}

impl TryFrom<&String> for Keyword {
    type Error = ();

    fn try_from(value: &String) -> Result<Self, Self::Error> {
        match value.as_str() {
            "let" => Ok(Keyword::Let),
            _ => Err(()),
        }
    }
}

#[derive(Debug)]
pub enum Token {
    Op(BinOp),
    Number(Number),
    Ident(String),
    Keyword(Keyword),
    Assign,
    Comma,
    LParen,
    RParen,
}

pub fn lex(input: &str) -> Vec<Token> {
    let mut tokens = vec![];
    let mut chars = input.chars().peekable();
    loop {
        match chars.next() {
            Some(c) => match c {
                ' ' | '\n' | '\t' | '\r' => {}
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
                'a'..='z' | 'A'..='Z' | '_' => {
                    let mut ident = String::from(c);

                    while let Some(next) = chars.peek() {
                        if !next.is_ascii_alphanumeric() && next != &'_' {
                            break;
                        }
                        ident.push(chars.next().unwrap());
                    }
                    if let Ok(keyword) = Keyword::try_from(&ident) {
                        tokens.push(Token::Keyword(keyword))
                    } else {
                        tokens.push(Token::Ident(ident));
                    }
                }
                _ => {
                    if let Some(op) = BinOp::from_char(c) {
                        tokens.push(Token::Op(op));
                    } else if c == '(' {
                        tokens.push(Token::LParen);
                    } else if c == ')' {
                        tokens.push(Token::RParen);
                    } else if c == ',' {
                        tokens.push(Token::Comma);
                    } else if c == '=' {
                        tokens.push(Token::Assign);
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
