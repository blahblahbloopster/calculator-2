use std::fmt::{Display, Formatter};
use std::ops::Range;
use rug::Complex;
use crate::{Calc, complex};
use crate::rpn::{EvalError, Operation};
use crate::rpn::EvalError::VarNotFound;

pub struct PR<T> {
    range: Range<usize>,
    pub(crate) item: Box<T>
}

impl<T> PR<T> {
    pub fn new(range: Range<usize>, item: T) -> PR<T> {
        return PR { range, item: Box::new(item) }
    }
}

pub type IPT = PR<InfixParseTree>;

pub enum InfixParseTree {
    Op(Vec<IPT>, PR<Operation>),
    Literal(String),
    Parens(PR<char>, IPT, PR<char>),
    VarGet(PR<String>)
}

impl Display for InfixParseTree {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            InfixParseTree::Op(args, op) => {
                write!(f, "{}(", op.item)?;
                for arg in args {
                    write!(f, "{}, ", *arg.item)?
                }
                write!(f, ")")
            }
            InfixParseTree::Literal(v) => write!(f, "{}", v),
            InfixParseTree::Parens(_, v, _) => write!(f, "({})", *v.item),
            InfixParseTree::VarGet(v) => write!(f, "{}", *v.item)
        }
    }
}

impl InfixParseTree {
    pub fn eval(&self, calc: &Calc) -> Result<Vec<Complex>, EvalError> {
        match self {
            InfixParseTree::Op(args, op) => {
                let mut vec = vec![];
                for item in args {
                    vec.extend(item.item.eval(calc)?);
                }
                op.item.eval(calc, vec)
            }
            InfixParseTree::Literal(value) => {
                let func = || {
                    complex!(value, calc.prec)
                };
                let res = func().ok_or(EvalError::MathError)?;
                Ok(vec![res])
            }
            InfixParseTree::Parens(_, v, _) => v.item.eval(calc),
            InfixParseTree::VarGet(name) => {
                let var = calc.vars.get(name.item.as_str());
                let found = var.ok_or(VarNotFound { name: *name.item.clone() })?;
                Ok(vec![found.clone()])
            }
        }
    }
}
