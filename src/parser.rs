use std::iter::Peekable;
use std::slice::Iter;

use lazy_static::lazy_static;
use regex::Regex;


lazy_static! {
    static ref CELL_ADDRESS_RGX: Regex = Regex::new(r"^([A-Z])(\d+)$").unwrap();
}

// Grammar:
// E -> T | T + T | T - T
// T -> P | P * P | P / P
// P -> Num | (E) | -E | func(E)

#[derive(Debug, PartialEq)]
pub enum Token {
    Plus,
    Dash,
    Star,
    Slash,
    Coma,
    Value(Value),
    Cell(String),
    RightParen,
    LeftParen,
    FuncIncFrom,
    FuncText,
    FuncSum,
    FuncConcat,
    FuncSpread,
    FuncSplit,
    FuncBte,
    End,
}


#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Operator {
    Add,
    Multiply,
    Divide,
    Subtract,
    Negative,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Func {
    IncFrom,
    Text,
    Sum,
    Concat,
    Split,
    Spread,
    Bte,
}

#[derive(Debug)]
pub struct SyntaxError {
    message: String,
    level: String,
}

impl std::fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(format!("{} err: {}", self.level, self.message).as_str())
    }
}

impl SyntaxError {
    fn new_lex_error(message: String) -> Self {
        SyntaxError {
            message,
            level: "Lex".to_string(),
        }
    }

    fn new_parse_error(message: String) -> Self {
        SyntaxError {
            message,
            level: "Parse".to_string(),
        }
    }
}


pub fn lex(code: String) -> Result<Vec<Token>, SyntaxError> {
    let mut iter = code.chars().peekable();
    let mut tokens: Vec<Token> = Vec::new();
    let mut leftover: Option<char> = None;

    loop {
        let ch = match leftover {
            Some(ch) => ch,
            None => match iter.next() {
                None => break,
                Some(ch) => ch,
            },
        };
        leftover = None;
        match ch {
            ' ' => continue,
            '+' => tokens.push(Token::Plus),
            '*' => tokens.push(Token::Star),
            '/' => tokens.push(Token::Slash),
            ')' => tokens.push(Token::LeftParen),
            '(' => tokens.push(Token::RightParen),
            '-' => tokens.push(Token::Dash),
            ',' => tokens.push(Token::Coma),
            '0'..='9' | '.' => {
                let (number, rest) = parse_number(ch, &mut iter);
                leftover = rest;
                tokens.push(Token::Value(Value::Number(number)));
            }

            'A'..='Z' | '@' => {
                let (cell, rest) = parse_cell(ch, &mut iter);
                leftover = rest;
                tokens.push(Token::Cell(cell));
            }

            '"' => tokens.push(Token::Value(Value::Text(parse_text(&mut iter)))),

            'a'..='z' => {
                let (str, rest) = parse_func_name(ch, &mut iter);
                leftover = rest;

                match str.as_str() {
                    "incFrom" => tokens.push(Token::FuncIncFrom),
                    "text" => tokens.push(Token::FuncText),
                    "concat" => tokens.push(Token::FuncConcat),
                    "sum" => tokens.push(Token::FuncSum),
                    "spread" => tokens.push(Token::FuncSpread),
                    "split" => tokens.push(Token::FuncSplit),
                    "bte" => tokens.push(Token::FuncBte),
                    _ => {
                        return Err(SyntaxError::new_lex_error(format!(
                            "Unrecognized func {}",
                            str
                        )));
                    }
                }
            }
            _ => {
                return Err(SyntaxError::new_lex_error(format!(
                    "Unrecognized character {}",
                    ch
                )));
            }
        }
    }

    tokens.push(Token::End);

    Ok(tokens)
}

fn parse_text<T: Iterator<Item=char>>(iter: &mut Peekable<T>) -> String {
    iter.by_ref().take_while(|c| *c != '"').collect()
}

fn parse_func_name<T: Iterator<Item=char>>(ch: char, iter: &mut Peekable<T>) -> (String, Option<char>) {
    let mut leftover = None;

    let stream: String = iter
        .by_ref()
        .take_while(|c| match c.is_alphabetic() {
            true => true,
            false => {
                leftover = Some(*c);
                false
            }
        })
        .collect();

    (format!("{}{}", ch, stream), leftover)
}

fn parse_number<T: Iterator<Item=char>>(ch: char, iter: &mut Peekable<T>) -> (f64, Option<char>) {
    let mut leftover = None;

    let str: String = iter
        .by_ref()
        .take_while(|c| match matches!(*c, '0'..='9' | '.') {
            true => true,
            false => {
                leftover = Some(*c);
                false
            }
        })
        .collect();

    let number = format!("{}{}", ch, str).parse().unwrap();
    (number, leftover)
}

fn parse_cell<T: Iterator<Item=char>>(ch: char, iter: &mut Peekable<T>) -> (String, Option<char>) {
    let mut leftover = None;

    let str: String = iter
        .by_ref()
        .take_while(|c| match matches!(*c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '<' | '>' | '^' | '_') {
            true => true,
            false => {
                leftover = Some(*c);
                false
            }
        })
        .collect();

    let number = format!("{}{}", ch, str).parse().unwrap();
    (number, leftover)
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Binary(Operator, Box<Expression>, Box<Expression>),
    Unary(Operator, Box<Expression>),
    UnaryFunc(Func, Box<Expression>),
    Func(Func, Vec<Expression>),
    Value(Value),
    Cell(String),
}

#[derive(Debug, Clone)]
pub enum Value {
    Number(f64),
    Text(String),
    NText(Vec<String>),
    NNumber(Vec<f64>),
    Bool(bool),
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        *self.str() == other.str()
    }
}

impl Value {
    fn num(&self) -> f64 {
        match self {
            Value::Number(n) => *n,
            Value::Text(s) => s.parse().unwrap(),
            Value::NText(v) => v[0].parse().unwrap(),
            Value::NNumber(v) => v[0],
            Value::Bool(_) => 0.0
        }
    }

    fn to_string(&self) -> String {
        match self {
            Value::Number(n) => num_to_str(*n),
            Value::Text(s) => format!("\"{}\"", s),
            _ => "".to_string()
        }
    }

    pub fn str(&self) -> String {
        match self {
            Value::Number(n) => num_to_str(*n),
            Value::Text(s) => s.clone(),
            Value::NText(v) => v[0].clone(),
            Value::NNumber(v) => format!("{}", v[0]).to_string(),
            Value::Bool(b) => format!("{}", b).to_string()
        }
    }

    // fn n_str(&self) -> Vec<String> {
    //     match self {
    //         Value::Number(n) => vec![format!("{}", *n).to_string()],
    //         Value::Text(s) => vec![s.clone()],
    //         Value::NText(v) => v.clone(),
    //         Value::NNumber(v) => v.iter().map(|n| format!("{}", *n).to_string()).collect(),
    //         Value::Bool(_) => Vec::new()
    //     }
    // }

    fn n_num(&self) -> Vec<f64> {
        match self {
            Value::Number(n) => vec![*n],
            Value::Text(s) => vec![s.parse().unwrap()],
            Value::NText(v) => v.iter().map(|s| s.parse().unwrap()).collect(),
            Value::NNumber(v) => v.clone(),
            Value::Bool(_) => Vec::new()
        }
    }
}

fn num_to_str(n: f64) -> String {
    let integer = n as i64;
    if (n - integer as f64).abs() < 1e-9 {
        return format!("{}", integer).to_string();
    }

    format!("{}", n).to_string()
}

pub trait Sheet {
    fn get(&self, cell: &str, r: usize) -> String;
}

impl Expression {
    pub fn eval(&self, m: &dyn Sheet, row: usize) -> Value {
        match self {
            Expression::Value(v) => v.clone(),
            Expression::Cell(s) => Value::Text(m.get(s.as_str(), row)),
            Expression::Unary(_negative, expr) => Value::Number(-1.0 * expr.eval(m, row).num()),
            Expression::Binary(Operator::Add, expr1, expr2) => Value::Number(expr1.eval(m, row).num() + expr2.eval(m, row).num()),
            Expression::Binary(Operator::Multiply, expr1, expr2) => Value::Number(expr1.eval(m, row).num() * expr2.eval(m, row).num()),
            Expression::Binary(Operator::Subtract, expr1, expr2) => Value::Number(expr1.eval(m, row).num() - expr2.eval(m, row).num()),
            Expression::Binary(Operator::Divide, expr1, expr2) => Value::Number(expr1.eval(m, row).num() / expr2.eval(m, row).num()),
            Expression::UnaryFunc(Func::IncFrom, expr) => expr.eval(m, row),
            Expression::UnaryFunc(Func::Text, expr) => Value::Text(expr.eval(m, row).str()),
            Expression::Func(Func::Concat, args) => {
                let strings: Vec<String> = args.iter().map(|a| a.eval(m, row).str()).collect();
                Value::Text(strings.join("").to_string())
            }
            Expression::Func(Func::Split, args) => {
                if args.len() != 2 {
                    panic!("func `split` expects 2 arguments");
                }

                let str = args[0].eval(m, row).str();
                let delim = args[1].eval(m, row).str();

                Value::NText(str.split(delim.as_str()).map(|s| s.to_string()).collect())
            }
            Expression::Func(Func::Spread, args) => {
                Value::NNumber(args.iter().flat_map(|arg| arg.eval(m, row).n_num()).collect())
            }
            Expression::Func(Func::Sum, args) => {
                Value::Number(args.iter().flat_map(|arg| arg.eval(m, row).n_num()).sum())
            }
            Expression::Func(Func::Bte, args) => {
                if args.len() != 2 {
                    panic!("func `bte` expects 2 arguments");
                }

                let arg1 = args[0].eval(m, row).num();
                let arg2 = args[1].eval(m, row).num();

                Value::Bool(arg1 >= arg2)
            }

            _ => {
                panic!("Unreachable code: for expr {:?}", self);
            }
        }
    }

    pub fn inc_formula(&mut self, m: &dyn Sheet, row: usize) {
        match self {
            Expression::Cell(s) => {
                if CELL_ADDRESS_RGX.is_match(s.as_str()) {
                    let cap = CELL_ADDRESS_RGX.captures(s.as_str()).unwrap();
                    let num: i32 = cap[2].parse().unwrap();
                    *self = Expression::Cell(format!("{}{}", cap[1].to_string(), num + 1).to_string())
                }
            }
            Expression::Unary(_negative, expr) => expr.inc_formula(m, row),
            Expression::Binary(Operator::Add, expr1, expr2) => {
                expr1.inc_formula(m, row);
                expr2.inc_formula(m, row)
            }
            Expression::Binary(Operator::Multiply, expr1, expr2) => {
                expr1.inc_formula(m, row);
                expr2.inc_formula(m, row)
            }
            Expression::Binary(Operator::Subtract, expr1, expr2) => {
                expr1.inc_formula(m, row);
                expr2.inc_formula(m, row)
            }
            Expression::Binary(Operator::Divide, expr1, expr2) => {
                expr1.inc_formula(m, row);
                expr2.inc_formula(m, row)
            }
            Expression::UnaryFunc(Func::IncFrom, expr) => {
                let new_expr = Expression::Value(Value::Number(expr.eval(m, row).num() + 1.0));
                *self = Expression::UnaryFunc(Func::IncFrom, Box::new(new_expr));
            }
            Expression::UnaryFunc(Func::Text, expr) => expr.inc_formula(m, row),
            Expression::Func(Func::Concat, args) => {
                for i in 0..args.len() {
                    args[i].inc_formula(m, row);
                }
            }
            Expression::Func(Func::Split, args) => {
                for i in 0..args.len() {
                    args[i].inc_formula(m, row);
                }
            }
            Expression::Func(Func::Spread, args) => {
                for i in 0..args.len() {
                    args[i].inc_formula(m, row);
                }
            }
            Expression::Func(Func::Sum, args) => {
                for i in 0..args.len() {
                    args[i].inc_formula(m, row);
                }
            }
            Expression::Func(Func::Bte, args) => {
                for i in 0..args.len() {
                    args[i].inc_formula(m, row);
                }
            }

            _ => {
                return;
            }
        }
    }

    pub fn to_string(&self) -> String {
        match self {
            Expression::Value(v) => v.to_string(),
            Expression::Cell(s) => s.to_string(),
            Expression::Unary(_negative, expr) => format!("-{}", expr.to_string()),
            Expression::Binary(Operator::Add, expr1, expr2) => format!("{}+{}", expr1.to_string(), expr2.to_string()),
            Expression::Binary(Operator::Multiply, expr1, expr2) => format!("{}*{}", expr1.to_string(), expr2.to_string()),
            Expression::Binary(Operator::Subtract, expr1, expr2) => format!("{}-{}", expr1.to_string(), expr2.to_string()),
            Expression::Binary(Operator::Divide, expr1, expr2) => format!("{}/{}", expr1.to_string(), expr2.to_string()),
            Expression::UnaryFunc(Func::IncFrom, expr) => format!("incFrom({})", expr.to_string()),
            Expression::UnaryFunc(Func::Text, expr) => format!("text({})", expr.to_string()),
            Expression::Func(Func::Concat, args) => {
                let items: Vec<String> = args.iter().map(|a| a.to_string()).collect();
                format!("concat({})", items.join(", ").to_string())
            }
            Expression::Func(Func::Split, args) => {
                let items: Vec<String> = args.iter().map(|a| a.to_string()).collect();
                format!("split({})", items.join(", ").to_string())
            }
            Expression::Func(Func::Spread, args) => {
                let items: Vec<String> = args.iter().map(|a| a.to_string()).collect();
                format!("spread({})", items.join(", ").to_string())
            }
            Expression::Func(Func::Sum, args) => {
                let items: Vec<String> = args.iter().map(|a| a.to_string()).collect();
                format!("sum({})", items.join(", ").to_string())
            }
            Expression::Func(Func::Bte, args) => {
                let items: Vec<String> = args.iter().map(|a| a.to_string()).collect();
                format!("bte({})", items.join(", ").to_string())
            }

            _ => {
                panic!("Unreachable code: for expr {:?}", self);
            }
        }
    }
}

pub struct Parser<'a> {
    iter: &'a mut Peekable<Iter<'a, Token>>,
}

impl<'a> Parser<'a> {
    pub fn new(iter: &'a mut Peekable<Iter<'a, Token>>) -> Self {
        Parser { iter }
    }

    fn assert_next(&mut self, token: Token) -> Result<(), SyntaxError> {
        let next = self.iter.next();

        if let None = next {
            return Err(SyntaxError::new_parse_error(
                "Unexpected end of input".to_string(),
            ));
        }

        if *next.unwrap() != token {
            return Err(SyntaxError::new_parse_error(format!(
                "Expected {:?} actual {:?}",
                token,
                next.unwrap(),
            )));
        }

        Ok(())
    }

    fn primary(&mut self) -> Result<Expression, SyntaxError> {
        let next = self.iter.next().unwrap();

        match next {
            Token::Cell(v) => Ok(Expression::Cell(v.clone())),
            Token::Value(v) => Ok(Expression::Value(v.clone())),
            Token::RightParen => {
                let expr = self.expression()?;
                self.assert_next(Token::LeftParen)?;
                Ok(expr)
            }
            Token::Dash => {
                let expr = self.primary()?;
                Ok(Expression::Unary(Operator::Negative, Box::new(expr)))
            }
            Token::FuncIncFrom => {
                self.assert_next(Token::RightParen)?;
                let expr = self.expression()?;
                self.assert_next(Token::LeftParen)?;
                Ok(Expression::UnaryFunc(Func::IncFrom, Box::new(expr)))
            }
            Token::FuncText => {
                self.assert_next(Token::RightParen)?;
                let expr = self.expression()?;
                self.assert_next(Token::LeftParen)?;
                Ok(Expression::UnaryFunc(Func::Text, Box::new(expr)))
            }
            Token::FuncSplit => {
                self.assert_next(Token::RightParen)?;
                let args = self.expressions()?;
                self.assert_next(Token::LeftParen)?;
                Ok(Expression::Func(Func::Split, args))
            }
            Token::FuncSpread => {
                self.assert_next(Token::RightParen)?;
                let args = self.expressions()?;
                self.assert_next(Token::LeftParen)?;
                Ok(Expression::Func(Func::Spread, args))
            }
            Token::FuncConcat => {
                self.assert_next(Token::RightParen)?;
                let args = self.expressions()?;
                self.assert_next(Token::LeftParen)?;
                Ok(Expression::Func(Func::Concat, args))
            }
            Token::FuncSum => {
                self.assert_next(Token::RightParen)?;
                let args = self.expressions()?;
                self.assert_next(Token::LeftParen)?;
                Ok(Expression::Func(Func::Sum, args))
            }
            Token::FuncBte => {
                self.assert_next(Token::RightParen)?;
                let args = self.expressions()?;
                self.assert_next(Token::LeftParen)?;
                Ok(Expression::Func(Func::Bte, args))
            }
            _ => Err(SyntaxError::new_parse_error(format!(
                "Unexpected token {:?}",
                next
            ))),
        }
    }

    fn term(&mut self) -> Result<Expression, SyntaxError> {
        let mut expr: Expression = self.primary()?;

        loop {
            let next = self.iter.peek().unwrap();
            match next {
                Token::Star => {
                    self.iter.next();
                    let rhs = self.primary()?;
                    expr = Expression::Binary(Operator::Multiply, Box::new(expr), Box::new(rhs));
                }
                Token::Slash => {
                    self.iter.next();
                    let rhs = self.primary()?;
                    expr = Expression::Binary(Operator::Divide, Box::new(expr), Box::new(rhs));
                }
                _ => break,
            };
        }

        Ok(expr)
    }

    fn expression(&mut self) -> Result<Expression, SyntaxError> {
        let mut expr: Expression = self.term()?;

        loop {
            let next = self.iter.peek().unwrap();
            match next {
                Token::Plus => {
                    self.iter.next();
                    let rhs = self.term()?;
                    expr = Expression::Binary(Operator::Add, Box::new(expr), Box::new(rhs));
                }
                Token::Dash => {
                    self.iter.next();
                    let rhs = self.term()?;
                    expr = Expression::Binary(Operator::Subtract, Box::new(expr), Box::new(rhs));
                }
                _ => break,
            };
        }

        Ok(expr)
    }

    fn expressions(&mut self) -> Result<Vec<Expression>, SyntaxError> {
        let mut args = vec!(self.term()?);

        loop {
            let next = self.iter.peek().unwrap();
            match next {
                Token::Coma => {
                    self.iter.next();
                    args.push(self.term()?);
                }

                _ => break,
            };
        }

        Ok(args)
    }

    pub fn parse(&mut self) -> Result<Expression, SyntaxError> {
        let ast = self.expression()?;
        self.assert_next(Token::End)?;
        Ok(ast)
    }
}