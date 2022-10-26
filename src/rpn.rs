use std::collections::HashMap;
use std::fmt::{Display, format, Formatter};
use std::ops::Add;
use std::result::Result;
use rug::{Complex, Float};
use rug::float::Constant::*;
use rug::ops::Pow;
use Function::*;
use Operation::*;
use crate::rpn::EvalError::{EmptyStack, VarNotFound};

// TODO: undo?
pub enum RPNCommand {
    Number(Complex),
    Op(Operation),
    // TODO: make these functions
    VarSet(String),
    VarGet(String),
    Clear,
    Duplicate
}

impl Display for RPNCommand {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            RPNCommand::Number(n) => { write!(f, "{}", n) }
            RPNCommand::Op(o) => { write!(f, "{}", o) }
            RPNCommand::VarSet(n) => { write!(f, "={}", n) }
            RPNCommand::VarGet(n) => { write!(f, "{}", n) }
            RPNCommand::Clear => { f.write_str("clear") }
            RPNCommand::Duplicate => { f.write_str("d") }
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
    Drop(usize)
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
            Drop(n) => {
                let string = format!("drop*{}", n);
                string.to_string()
            }
        })
    }
}

pub struct Calc {
    pub stack: Vec<Complex>,
    pub prec: u32,
    pub vars: HashMap<String, Complex>
}

impl Operation {
    fn num_args(&self) -> usize {
        match self {
            Add | Sub | Mul | Div | Pow => 2,
            UMinus => 1,
            Func(func) => match func {
                Sin | Cos | Tan | Cot | ASin | ACos | ATan | ACot | Sqrt => 1,
                ATan2 => 2,
                Drop(n) => *n
            }
        }
    }
}

impl Function {
    // TODO: result
    pub fn evaluate(&self, calc: &Calc, args: Vec<Complex>) -> Vec<Complex> {
        match self {
            Sin => vec![args[0].clone().sin()],
            Cos => vec![args[0].clone().cos()],
            Tan => vec![args[0].clone().tan()],
            Cot => vec![args[0].clone().tan().recip()],
            ASin => vec![args[0].clone().asin()],
            ACos => vec![args[0].clone().acos()],
            ATan => vec![args[0].clone().atan()],
            ATan2 => vec![Float::atan2(args[0].real().clone(), args[1].real()).as_complex().clone()],
            ACot => vec![(calc.pi() / 2) - args[0].clone().atan()],
            Sqrt => vec![args[0].clone().sqrt()],
            Drop(_) => vec![]
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
            RPNCommand::Number(n) => {
                if n.real().is_nan() || n.imag().is_nan() {
                    return Err(EvalError::MathError)
                }
                self.stack.push(n)
            }
            RPNCommand::Op(op) => {
                let mut args = vec![];
                for _ in 0..op.num_args() {
                    let gotten = self.stack.pop();
                    match gotten {
                        Some(t) => args.push(t),
                        None => return Err(EmptyStack)
                    }
                }
                args.reverse();
                let output = match op {
                    Add => vec![args[0].clone() + args[1].clone()],
                    Sub => vec![args[0].clone() - args[1].clone()],
                    Mul => vec![args[0].clone() * args[1].clone()],
                    Div => vec![args[0].clone() / args[1].clone()],
                    Pow => vec![args[0].clone().pow(args[1].clone())],
                    UMinus => vec![args[0].clone() * -1],
                    Func(func) => func.evaluate(self, args)
                };
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
        }
        Ok(())
    }
}

pub enum EvalError {
    EmptyStack,
    VarNotFound { name: String },
    MathError
}

impl Display for EvalError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            EmptyStack => { f.write_str("empty stack") }
            VarNotFound { name } => { write!(f, "variable '{}' not found", name) }
            EvalError::MathError => { f.write_str("math error") }
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
