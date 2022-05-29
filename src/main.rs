
#[derive(Debug)]
pub enum Equation {
    Constant(i64),
    Variable,
//    Neg(Box<Equation>),
    Sub(Box<Equation>, Box<Equation>),
    Add(Box<Equation>, Box<Equation>),
    Mul(Box<Equation>, Box<Equation>),
    Div(Box<Equation>, Box<Equation>),
    Exp(Box<Equation>, Box<Equation>),
    Sin(Box<Equation>),
    Cos(Box<Equation>),
    Tan(Box<Equation>),
    Log(Box<Equation>),
}


use Equation::*;
use std::fmt;

impl fmt::Display for Equation {
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

pub fn parse(exp: &str) -> Equation {
    println!("===========================");
    println!("Expression: {}", exp);
    let tokens = tokenize(exp);
    println!("Tokens: {:?}", tokens);
    let postfixed_tokens  = to_postfix(tokens);
    println!("Postfix: {:?}", postfixed_tokens);

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
    let exps = vec!["(1+2)",
                    "sin(x+(cos(log(e^(x+3234)))))",
                    "sin(x+cos(3*x))+log(sin(x)*cos(x))",
                    "x+4*3/5+(sin(10)*log(x+10*3))"];
    for exp in exps {
        let parsed = parse(exp);
        println!("{:?}", parsed);
        println!("{}", parsed);
    }
}
