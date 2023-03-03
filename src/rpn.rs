use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::io::Write;
use std::process::{Command, Stdio};
use std::result::Result;
use regex::Regex;
use rug::{Complex, Float};
use rug::float::Constant::*;
use rug::ops::Pow;
use Function::*;
use Operation::*;
use crate::complex;
use crate::infixtree::{InfixParseTree, PR};
use crate::rpn::EvalError::{EmptyStack, FunctionNotFound, ParseError, VarNotFound};

// TODO: undo?
pub enum RPNCommand {
    Number(Option<Complex>),
    Op(Operation),
    // TODO: make these functions
    VarSet(String),
    VarGet(String),
    Clear,
    Duplicate,
    Swap,
    Clipboard,
    PrettyPrint,
    Infix(PR<InfixParseTree>)
}

pub type RPT = PR<RPNCommand>;

impl Display for RPNCommand {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            RPNCommand::Number(n) => { match n {
                Some(v) => write!(f, "{}", v),
                None => write!(f, "parse error")
            } }
            RPNCommand::Op(o) => { write!(f, "{}", o) }
            RPNCommand::VarSet(n) => { write!(f, "={}", n) }
            RPNCommand::VarGet(n) => { write!(f, "{}", n) }
            RPNCommand::Clear => { f.write_str("clear") }
            RPNCommand::Duplicate => { f.write_str("d") }
            RPNCommand::Swap => { f.write_str("s") }
            RPNCommand::Infix(tree) => { write!(f, "'{}'", tree.item) }
            RPNCommand::Clipboard => f.write_str("clipboard"),
            RPNCommand::PrettyPrint => f.write_str("prettyprint")
        }
    }
}

pub enum Operation {
    Add, Sub, Mul, Div, Pow, UMinus,
    Func(Function)
}

impl Display for Operation {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Add => f.write_str("+"),
            Sub => f.write_str("-"),
            Mul => f.write_str("*"),
            Div => f.write_str("/"),
            Pow => f.write_str("^"),
            UMinus => f.write_str("um"),
            Func(func) => write!(f, "{}", func)
        }
    }
}

pub enum Function {
    Sin, Cos, Tan, Cot,
    ASin, ACos, ATan, ATan2, ACot,
    Sqrt,
    Ln, Log10, Log2, LogB,
    Drop(usize),
    Unknown(String)
}

impl Display for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            Sin => "sin".to_string(),
            Cos => "cos".to_string(),
            Tan => "tan".to_string(),
            Cot => "cot".to_string(),
            ASin => "asin".to_string(),
            ACos => "acos".to_string(),
            ATan => "atan".to_string(),
            ATan2 => "atan2".to_string(),
            ACot => "acot".to_string(),
            Sqrt => "sqrt".to_string(),
            Ln => "ln".to_string(),
            Log10 => "log10".to_string(),
            Function::Log2 => "log2".to_string(),
            LogB => "logb".to_string(),
            Drop(n) => {
                let string = format!("drop*{}", n);
                string.to_string()
            }
            Unknown(name) => name.clone(),
        })
    }
}

pub struct Calc {
    pub stack: Vec<Complex>,
    pub prec: u32,
    pub vars: HashMap<String, Complex>
}

impl Operation {
    fn num_args(&self) -> Option<usize> {
        match self {
            Add | Sub | Mul | Div | Pow => Some(2),
            UMinus => Some(1),
            Func(func) => match func {
                Sin | Cos | Tan | Cot | ASin | ACos | ATan | ACot | Sqrt => Some(1),
                ATan2 => Some(2),
                Ln => Some(1),
                Log10 => Some(1),
                Function::Log2 => Some(1),
                LogB => Some(2),
                Drop(n) => Some(*n),
                Unknown(name) => Func(Function::find(name.clone())?).num_args(),
            }
        }
    }

    pub(crate) fn eval(&self, calc: &Calc, args: Vec<Complex>) -> Result<Vec<Complex>, EvalError> {
        match self {
            Add => Ok(vec![args[0].clone() + args[1].clone()]),
            Sub => Ok(vec![args[0].clone() - args[1].clone()]),
            Mul => Ok(vec![args[0].clone() * args[1].clone()]),
            Div => Ok(vec![args[0].clone() / args[1].clone()]),
            Pow => Ok(vec![args[0].clone().pow(args[1].clone())]),
            UMinus => Ok(vec![args[0].clone() * -1]),
            Func(func) => match func.evaluate(calc, args) {
                Some(v) => Ok(v),
                None => { return Err(FunctionNotFound { name: func.to_string() }) }
            }
        }
    }
}

impl Function {
    // TODO: result
    pub fn evaluate(&self, calc: &Calc, args: Vec<Complex>) -> Option<Vec<Complex>> {
        match self {
            Sin => Some(vec![args[0].clone().sin()]),
            Cos => Some(vec![args[0].clone().cos()]),
            Tan => Some(vec![args[0].clone().tan()]),
            Cot => Some(vec![args[0].clone().tan().recip()]),
            ASin => Some(vec![args[0].clone().asin()]),
            ACos => Some(vec![args[0].clone().acos()]),
            ATan => Some(vec![args[0].clone().atan()]),
            ATan2 => Some(vec![Float::atan2(args[0].real().clone(), args[1].real()).as_complex().clone()]),
            ACot => Some(vec![(calc.pi() / 2) - args[0].clone().atan()]),
            Sqrt => Some(vec![args[0].clone().sqrt()]),
            Ln => Some(vec![args[0].clone().ln()]),
            Log10 => Some(vec![args[0].clone().log10()]),
            Function::Log2 => Some(vec![args[0].clone().ln() / complex!("2", calc.prec).unwrap().ln()]),
            LogB => Some(vec![args[0].clone().ln() / args[1].clone().ln()]),
            Drop(_) => Some(vec![]),
            Unknown(name) => Function::find(name.to_string())?.evaluate(calc, args),
        }
    }

    pub fn find(name: String) -> Option<Function> {
        match name.to_lowercase().as_str() {
            "sin" | "sine" => Some(Sin),
            "cos" | "cosine" => Some(Cos),
            "tan" | "tangent" => Some(Tan),
            "cot" | "cotangent" => Some(Cot),
            "asin" => Some(ASin),
            "acos" => Some(ACos),
            "atan" => Some(ATan),
            "atan2" => Some(ATan2),
            "acot" => Some(ACot),
            "sqrt" => Some(Sqrt),
            "ln" => Some(Ln),
            "log10" => Some(Log10),
            "log2" => Some(Function::Log2),
            "logb" => Some(LogB),
            _ => None
        }
    }
}

impl Calc {
    pub fn new(prec: u32) -> Calc {
        let mut c = Calc { stack: vec![], prec, vars: HashMap::new() };
        c.vars.insert("pi".to_string(), Complex::with_val(prec, Pi));
        let one: Complex = Complex::new(prec) + 1;
        let e = one.exp();
        c.vars.insert("e".to_string(), e);
        c
    }

    pub fn pi(&self) -> Complex {
        self.vars["pi"].clone()
    }

    pub fn exec(&mut self, command: RPNCommand) -> Result<(), EvalError> {
        match command {
            RPNCommand::Number(v) => {
                let n = v.ok_or(ParseError { value: "".to_string() })?;
                if n.real().is_nan() || n.imag().is_nan() {
                    return Err(EvalError::MathError)
                }
                self.stack.push(n)
            }
            RPNCommand::Op(op) => {
                let mut args = vec![];
                let num_args = match op.num_args() {
                    Some(n) => n,
                    None => { return Err(FunctionNotFound { name: op.to_string() }) }
                };
                for _ in 0..num_args {
                    let gotten = self.stack.pop();
                    match gotten {
                        Some(t) => args.push(t),
                        None => return Err(EmptyStack)
                    }
                }
                args.reverse();
                let output = op.eval(self, args)?;
                for item in &output {
                    if item.real().is_nan() || item.imag().is_nan() {
                        return Err(EvalError::MathError)
                    }
                }
                for item in output {
                    self.stack.push(item);
                }
            }
            RPNCommand::Clear => { self.stack.clear(); }
            RPNCommand::VarSet(name) => {
                let item = self.stack.last();  // TODO: decide on pop vs copy
                match item {
                    Some(t) => { self.vars.insert(name.to_string(), t.clone()); },
                    None => return Err(EmptyStack)
                }
            }
            RPNCommand::VarGet(name) => {
                let gotten = self.vars.get(&*name);
                match gotten {
                    None => { return Err(VarNotFound { name: name.to_string() }) }
                    Some(item) => { self.stack.push(item.clone()) }
                }
            }
            RPNCommand::Duplicate => {
                let item = match self.stack.last() {
                    Some(v) => v.clone(),
                    None => return Err(EmptyStack)
                };
                self.stack.push(item);
            }
            RPNCommand::Swap => {
                let l = self.stack.len();
                if l < 2 {
                    return Err(EmptyStack);
                }
                self.stack.swap(l - 2, l - 1);
            }
            RPNCommand::Infix(tree) => {
                let res = tree.item.eval(self)?;
                self.stack.extend(res);
            }
            RPNCommand::Clipboard => {
                let item = self.stack.last().ok_or(EmptyStack)?;
                let stringified = format!("{}", PComplex(item));
                let mut process = Command::new("xclip").args(["-selection", "clipboard"]).stdin(Stdio::piped())/*.stdout(Stdio::piped()) */.spawn().unwrap();

                let mut stdin = process.stdin.take().unwrap();
                stdin.write_all(stringified.as_bytes()).expect("wat");
                drop(stdin);

                process.wait().unwrap();
            }
            RPNCommand::PrettyPrint => {
                let mut mapping = Vec::new();
                macro_rules! ins {
                    ($($n:literal $s:ident)+) => {
                        $(mapping.push(($n, stringify!($s).to_string().to_lowercase())));+
                    };
                }

                ins!(
                    2    Hundred
                    3    Thousand
                    6    Million
                    9    Billion
                    12   Trillion
                    15   Quadrillion
                    18   Quintillion
                    21   Sextillion
                    24   Septillion 
                    24   Septillion 
                    30   Nonillion
                    33   Decillion 
                    36   Undecillion 
                    39   Duodecillion 
                    42   Tredecillion 
                    45   Quattuordecillion 
                    48   Quindecillion 
                    51   Sedecillion 
                    54   Septendecillion
                    57   Octodecillion 
                    60   Novendecillion 
                    63   Vigintillion 
                    66   Unvigintillion 
                    69   Duovigintillion 
                    72   Tresvigintillion 
                    75   Quattuorvigintillion 
                    78   Quinvigintillion 
                    81   Sesvigintillion 
                    84   Septemvigintillion 
                    87   Octovigintillion 
                    90   Novemvigintillion 
                    93   Trigintillion 
                    96   Untrigintillion 
                    99   Duotrigintillion 
                    102  Trestrigintillion 
                    105  Quattuortrigintillion 
                    108  Quintrigintillion 
                    111  Sestrigintillion 
                    114  Septentrigintillion 
                    117  Octotrigintillion 
                    120  Noventrigintillion 
                    123  Quadragintillion 
                    153  Quinquagintillion 
                    183  Sexagintillion 
                    213  Septuagintillion 
                    243  Octogintillion 
                    273  Nonagintillion 
                    303  Centillion 
                    306  Uncentillion 
                    333  Decicentillion 
                    336  Undecicentillion 
                    363  Viginticentillion 
                    366  Unviginticentillion 
                    393  Trigintacentillion 
                    423  Quadragintacentillion 
                    453  Quinquagintacentillion 
                    483  Sexagintacentillion 
                    513  Septuagintacentillion 
                    543  Octogintacentillion 
                    573  Nonagintacentillion 
                    603  Ducentillion 
                    903  Trecentillion 
                    1203 Quadringentillion 
                    1503 Quingentillion 
                    1803 Sescentillion 
                    2103 Septingentillion 
                    2403 Octingentillion 
                    2703 Nongentillion 
                    3003 Millinillion
                );

                println!("{}", PComplex(self.stack.last().ok_or(EmptyStack)?).pretty_print(&mapping))
            }
        }
        Ok(())
    }
}

pub enum EvalError {
    EmptyStack,
    VarNotFound { name: String },
    MathError,
    FunctionNotFound { name: String },
    ParseError { value: String }
}

impl Display for EvalError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            EmptyStack => { f.write_str("empty stack") }
            VarNotFound { name } => { write!(f, "variable '{}' not found", name) }
            EvalError::MathError => { f.write_str("math error") }
            FunctionNotFound { name } => { write!(f, "function '{}' not found", name) }
            ParseError { value } => write!(f, "parse error at '{}'", value)
        }
    }
}

impl Display for Calc {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for item in self.stack.as_slice() {
            writeln!(f, "{}", item)?;
        }
        Ok(())
    }
}

struct PFloat<'a>(&'a Float);
pub struct PComplex<'a>(pub &'a Complex);

impl<'a> Display for PFloat<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let trailing_zeros: Regex = Regex::new("\\.?0+$").unwrap();
        let trailing_zeros_exp: Regex = Regex::new("\\.?0+e").unwrap();
        let count = (self.0.prec() as f64 / 10.0_f64.log2()) as usize + 2;
        let (sign, digits, exp) = self.0.to_sign_string_exp(10, Some(count - 2));
        if exp.unwrap_or(0).abs() > 500 {
            return write!(f, "{}", trailing_zeros_exp.replace(self.0.to_string_radix(10, Some(count - 2)).as_str(), "e"));
        }
        let point_location = digits.len() as i32 - exp.unwrap_or(0) + 1;
        let string = match point_location {
            x if x <= 0 as i32 => {
                let zeros = (-x) as usize + 1;
                let mut d = digits.clone();
                for _ in 0..zeros {
                    d.push('0');
                }
                d.push_str(".0");
                d
            }
            x if x > digits.len() as i32 => {
                let count = point_location as usize - digits.len();
                let mut output = String::new();
                output.push_str("0.");
                for _ in 0..count - 1 {
                    output.push('0');
                }
                output.push_str(&*digits);
                output
            }
            _ => {
                let mut d = digits.clone();
                d.insert(exp.unwrap() as usize, '.');
                d
            }
        };
        let s = if sign { "-" } else { "" };
        write!(f, "{}{}", s, &*trailing_zeros.replace(string.as_str(), ""))
    }
}

impl<'a> PFloat<'a> {
    fn pretty_print(&self, mapping: &Vec<(i32, String)>) -> String {
        let count = (self.0.prec() as f64 / 10.0_f64.log2()) as usize + 2;
        let (_, _, Some(exp)) = self.0.to_sign_string_exp(10, Some(count - 2)) else { return format!("{}", self) };
        let mut found = None;
        for arr in mapping.iter().collect::<Vec<_>>().windows(2) {
            let a = arr[0];
            let b = arr[1];
            if b.0 > exp - 1 {
                found = Some(a);
                break;
            }
        }

        match found {
            Some((e, n)) => {
                let pow = Float::with_val(self.0.prec(), 10).pow(e);
                format!("{} {}", PFloat(&(self.0 / pow)), n)
            }
            None => {
                if exp < 3 {
                    format!("{}", self)
                } else {
                    let biggest = mapping.last().unwrap();
                    let pow = Float::with_val(self.0.prec(), 10).pow(biggest.0);
                    format!("{} {}", PFloat(&(self.0 / pow)), biggest.1)
                }
            }
        }
    }
}

impl<'a> Display for PComplex<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.0.imag().is_zero() {
            write!(f, "{}", PFloat(self.0.real()))
        } else if self.0.real().is_zero() {
            let img = self.0.imag();
            if img.is_integer() && img.to_f64() == 1.0 {
                f.write_str("i")
            } else {
                write!(f, "{}", PFloat(self.0.imag()))?;
                f.write_str("i")
            }
        } else {
            write!(f, "{}", PFloat(self.0.real()))?;
            f.write_str(" + ")?;
            write!(f, "{}", PFloat(self.0.imag()))?;
            f.write_str("i")
        }
    }
}

impl<'a> PComplex<'a> {
    fn pretty_print(&self, mapping: &Vec<(i32, String)>) -> String {
        if self.0.imag().is_zero() {
            format!("{}", PFloat(self.0.real()).pretty_print(mapping))
        } else if self.0.real().is_zero() {
            let img = self.0.imag();
            if img.is_integer() && img.to_f64() == 1.0 {
                "i".to_string()
            } else {
                format!("{}i", PFloat(self.0.imag()).pretty_print(mapping))
            }
        } else {
            format!("{} + {}i", PFloat(self.0.real()).pretty_print(mapping), PFloat(self.0.imag()).pretty_print(mapping))
        }
    }
}
