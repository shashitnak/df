
#[derive(Debug, Clone)]
pub enum Expression {
    Constant(i64),
    Variable,
//    Neg(Box<Expression>),
    Sub(Box<Expression>, Box<Expression>),
    Add(Box<Expression>, Box<Expression>),
    Mul(Box<Expression>, Box<Expression>),
    Div(Box<Expression>, Box<Expression>),
    Exp(Box<Expression>, Box<Expression>),
    Sin(Box<Expression>),
    Cos(Box<Expression>),
    Tan(Box<Expression>),
    Log(Box<Expression>),
}


use Expression::*;
use std::fmt;
use std::ops;

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Sub(e1, e2) => write!(f, "({}-{})", *e1, *e2),
            Add(e1, e2) => write!(f, "({}+{})", *e1, *e2),
            Mul(e1, e2) => write!(f, "({}*{})", *e1, *e2),
            Div(e1, e2) => write!(f, "({}/{})", *e1, *e2),
            Exp(e1, e2) => write!(f, "({}^{})", *e1, *e2),
            Sin(e)      => write!(f, "sin({})", *e),
            Cos(e)      => write!(f, "cos({})", *e),
            Tan(e)      => write!(f, "tan({})", *e),
            Log(e)      => write!(f, "log({})", *e),
            Constant(num) => write!(f, "{}", num),
            Variable    => write!(f, "x")
        }
    }
}

impl ops::Add for Expression {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Add(Box::new(self), Box::new(other))
    }
}

impl ops::Sub for Expression {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        Sub(Box::new(self), Box::new(other))
    }
}

impl ops::Mul for Expression {
    type Output = Self;

    fn mul(self, other: Self) -> Self {
        Mul(Box::new(self), Box::new(other))
    }
}

impl ops::Div for Expression {
    type Output = Self;

    fn div(self, other: Self) -> Self {
        Div(Box::new(self), Box::new(other))
    }
}

impl ops::BitXor for Expression {
    type Output = Self;

    fn bitxor(self, other: Self) -> Self {
        Exp(Box::new(self), Box::new(other))
    }
}

impl Expression {
    fn df(self) -> Self {
        match self {
            Variable    => Constant(1),
            Constant(_) => Constant(0),
            Sin(e)      => Cos(e.clone()) * (*e).df(),
            Cos(e)      => (Constant(0) - Sin(e.clone())) * (*e).df(),
            Tan(e)      => (Sin(e.clone())/Cos(e)).df(),
            Log(e)      => (Constant(1)/(*e.clone())) * (*e).df(),
            Sub(e1, e2) => (*e1).df() - (*e2).df(),
            Add(e1, e2) => (*e1).df() + (*e2).df(),
            Mul(e1, e2) => (*e1.clone()) * (*e2.clone()).df() + (*e2) * (*e1).df(),
            Div(e1, e2) => ((*e2.clone()) * (*e1.clone()).df() - (*e1) * (*e2.clone()).df()) / ((*e2.clone())*(*e2)),
            Exp(e1, e2) => match (*e1, *e2) {
                (Constant(_), Constant(_)) => Constant(0),
                (Constant(n), e)           => (Constant(n)^e.clone())*Log(Box::new(e.clone()))*e.df(),
                (e,           Constant(n)) => Constant(n)*(e.clone()^Constant(n-1))*e.df(),
                _                          => panic!("Net yet... Not yet."),
            }
        }
    }

    fn dnf(self, n: usize) -> Self {
        let mut df = self;
        for i in 0..n {
            df = df.df().evaluate();
            println!("{}", i+1);
        }
        df
    }

    fn reducible(self) -> bool {
        match self {
            Constant(_) => false,
            Variable    => false,
            Sin(e)      => (*e).reducible(),
            Cos(e)      => (*e).reducible(),
            Tan(e)      => (*e).reducible(),
            Log(e)      => (*e).reducible(),
            Sub(e1, e2) => match (*e1, *e2) {
                (Constant(0), Constant(0))   => true,
                (Constant(_), Constant(_))   => true,
                (_,            Constant(0))  => true,
                (e1,            e2)          => e1.reducible() || e2.reducible()
            },
            Add(e1, e2) => match (*e1, *e2) {
                (Constant(0),  Constant(0))  => true,
                (Constant(_), Constant(_))   => true,
                (Constant(0),  _)            => true,
                (_,            Constant(0))  => true,
                (e1,           e2)           => e1.reducible() || e2.reducible()
            },
            Mul(e1, e2) => match (*e1, *e2) {
                (Constant(0), _)             => true,
                (_, Constant(0))             => true,
                (Constant(_), Constant(_))   => true,
                (Constant(1), _)             => true,
                (_, Constant(1))             => true,
                (e1, e2)                     => e1.reducible() || e2.reducible(),
            },
            Div(e1, e2) => match (*e1, *e2) {
                (Constant(0), _)             => true,
                (_, Constant(1))             => true,
                (Constant(_), Constant(_))   => true,
                (e1, e2)                     => e1.reducible() || e2.reducible(),
            },
            Exp(e1, e2) => match (*e1, *e2) {
                (Constant(0|1), _)       => true,
                (_, Constant(0))             => true,
                (_, Constant(1))             => true,
                (Constant(_), Constant(_))   => true,
                (e1, e2)                     => e1.reducible() || e2.reducible(),
            }
        }
    }

    fn reduce(self) -> Self {
        match self {
            Constant(n) => Constant(n),
            Variable    => Variable,
            Sin(e)      => Sin(Box::new((*e).reduce())),
            Cos(e)      => Cos(Box::new((*e).reduce())),
            Tan(e)      => Tan(Box::new((*e).reduce())),
            Log(e)      => Log(Box::new((*e).reduce())),
            Sub(e1, e2) => match (*e1, *e2) {
                (Constant(0), Constant(0))   => Constant(0),
                (Constant(n1), Constant(n2)) => Constant(n1-n2),
                (e,            Constant(0))  => e.reduce(),
                (e1,            e2)          => e1.reduce() - e2.reduce()
            },
            Add(e1, e2) => match (*e1, *e2) {
                (Constant(0),  Constant(0))  => Constant(0),
                (Constant(n1), Constant(n2)) => Constant(n1+n2),
                (Constant(0),  e)            => e.reduce(),
                (e,            Constant(0))  => e.reduce(),
                (e1,           e2)           => e1.reduce() + e2.reduce()
            },
            Mul(e1, e2) => match (*e1, *e2) {
                (Constant(0), _)             => Constant(0),
                (_, Constant(0))             => Constant(0),
                (Constant(n1), Constant(n2)) => Constant(n1*n2),
                (Constant(1), e)             => e.reduce(),
                (e, Constant(1))             => e.reduce(),
                (e1, e2)                     => e1.reduce()*e2.reduce(),
            },
            Div(e1, e2) => match (*e1, *e2) {
                (Constant(0), _)             => Constant(0),
                (e, Constant(1))             => e.reduce(),
                (Constant(n1), Constant(n2)) => Constant(n1/n2),
                (e1, e2)                     => e1.reduce() / e2.reduce(),
            },
            Exp(e1, e2) => match (*e1, *e2) {
                (Constant(n@(0|1)), _)       => Constant(n),
                (_, Constant(0))             => Constant(1),
                (e, Constant(1))             => e.reduce(),
                (Constant(n1), Constant(n2)) => Constant(n1.pow(n2 as u32)),
                (e1, e2)                     => e1.reduce() ^ e2.reduce(),
            }
        }
    }

    fn evaluate(self) -> Self {
        let mut f = self;
        while f.clone().reducible() {
            f = f.reduce();
        }
        f
    }
}

fn tokenize(exp: &str) -> Vec<String> {
    let mut tokens = Vec::new();

    let mut token = String::new();
    for ch in exp.chars() {
        match ch {
            ' ' => {
                tokens.push(token);
                token = String::new();
            },
            '(' | ')' | '+' | '-' | '*' | '/' | '^' => {
                if !token.is_empty() {
                    tokens.push(token);
                    token = String::new();
                }
                tokens.push(ch.to_string());
            },
           _ => token.push(ch)
        }
    }
    tokens
}


fn presidence(op: &str) -> usize {
    match op {
        "-" => 0,
        "+" => 1,
        "*" => 2,
        "/" => 3,
        "^" => 4,
        "sin" | "cos" | "tan" | "log" => 5,
        "(" => 6,
        op  => panic!("Something's fishy here... {}", op)
    }
}


fn to_postfix(tokens: Vec<String>) -> Vec<String> {
    let mut stack:   Vec<&str>   = Vec::new();
    let mut postfix: Vec<String> = Vec::new();

    for token in tokens.iter() {
        match token.as_str() {
            ")" => loop {
                match stack.pop() {
                    None      => panic!("Something's wrong with your parens..."),
                    Some("(") => break,
                    Some(tkn) => postfix.push(tkn.to_string())
                }
            },
            tkn @ ("+" | "-" | "*" | "/" | "^" | "sin" | "cos" | "tan" | "log") => {
                while let Some(op) = stack.last().cloned() {
                    if op == "(" || presidence(op) < presidence(tkn) {
                        break;
                    }
                    postfix.push(op.to_string());
                    stack.pop();
                }
                stack.push(tkn);
            },
            "(" => stack.push("("),
            tkn => postfix.push(tkn.to_string())
        }
    }
    while let Some(op) = stack.pop() {
        postfix.push(op.to_string());
    }

    postfix
}

pub fn parse(exp: &str) -> Expression {
//    println!("===========================");
//    println!("Expression: {}", exp);
    let tokens = tokenize(exp);
//    println!("Tokens: {:?}", tokens);
    let postfixed_tokens  = to_postfix(tokens);
//    println!("Postfix: {:?}", postfixed_tokens);

    let mut exp_stack = Vec::new();
    for token in postfixed_tokens.iter() {
        let next_exp = match token.as_str() {
            op @ ("-" | "+" | "*" | "/" | "^") => {
                let e2 = Box::new(exp_stack.pop().unwrap());
                let e1 = Box::new(exp_stack.pop().unwrap());
                match op {
                    "-" => Sub(e1, e2),
                    "+" => Add(e1, e2),
                    "*" => Mul(e1, e2),
                    "/" => Div(e1, e2),
                    "^" => Exp(e1, e2),
                    _   => panic!("Too far...")
                }
            },
            op @ ("sin" | "cos" | "tan" | "log") => {
                let e = Box::new(exp_stack.pop().unwrap());
                match op {
                    "sin" => Sin(e),
                    "cos" => Cos(e),
                    "tan" => Tan(e),
                    "log" => Log(e),
                    _     => panic!("Too far...")
                }
            },
            exp => match exp.parse() {
                Ok(num) => Constant(num),
                _       => Variable
            }
        };
        exp_stack.push(next_exp);
    }
    exp_stack.pop().unwrap()
}

fn main() {
    use std::io::{stdin, stdout, Write};
    print!("Enter f(x): ");
    stdout().flush().unwrap();
    let mut line = String::new();
    stdin().read_line(&mut line).unwrap();
    let f = parse(&line);

    print!("Enter number of times you want to differentiate: ");
    stdout().flush().unwrap();
    line = String::new();
    stdin().read_line(&mut line).unwrap();
    let n = line.trim().parse().unwrap();
    println!("fn(x) = {}", f.dnf(n))
}
