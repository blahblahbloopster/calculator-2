use std::io;
use std::io::Write;
use std::time::Duration;
use regex::Regex;
use rug::Float;
use rustyline::{Behavior, Config, Editor};
use rustyline::error::ReadlineError;
use rustyline::highlight::Highlighter;
use crate::rpn::Calc;
extern crate lalrpop_util;

mod rpn;
mod infix;
mod rpncommand;
mod units;
use crate::rpncommand::CommandsParser;

#[macro_export]
macro_rules! complex {
    ($num:expr) => {
        {
            Some(Complex::with_val(prec, Complex::parse(($num).to_string()).ok()?))
        }
    };
    ($num:expr, $prec:expr) => {
        {
            Some(Complex::with_val($prec, Complex::parse(($num).to_string()).ok()?))
        }
    };
}

struct Completioninator {}

impl Highlighter for Completioninator {
}

fn main() -> rustyline::Result<()> {
    let mut r1: Editor<()> = Editor::new()?;

    // let mut inp = String::new();
    // let stdin = io::stdin();

    let mut prec: u32 = 0;
    loop {
        // print!("precision: ");
        // io::stdout().flush().expect("failed to flush stdout???");
        // inp.clear();
        // stdin.read_line(&mut inp).expect("failed to read from stdin");
        let inp = r1.readline("precision: ")?.replace("\n", "");
        if inp.is_empty() {
            prec = 1024;
            break;
        } else {
            match inp.parse::<u32>() {
                Ok(v @ 10..=65536) => {
                    prec = v;
                    break
                }
                _ => println!("Enter a valid number!")
            }
        }
    }
    let mut calc = Calc::new(prec);

    let parser = CommandsParser::new();
    let trailing_zeros = Regex::new("\\.?0+$").unwrap();
    let trailing_zeros_exp = Regex::new("\\.?0+e").unwrap();
    let remove_zeros = |inp: &Float| {
        let count = (inp.prec() as f64 / 10.0_f64.log2()) as usize + 2;
        let (sign, digits, exp) = inp.to_sign_string_exp(10, Some(count - 2));
        if exp.unwrap_or(0).abs() > 500 {
            return trailing_zeros_exp.replace(inp.to_string_radix(10, Some(count - 2)).as_str(), "e").to_string()
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
        return s.to_string() + &*trailing_zeros.replace(string.as_str(), "");
    };
    loop {
        // print!("> ");
        // io::stdout().flush().expect("failed to flush stdout???");
        // inp.clear();
        // stdin.read_line(&mut inp).expect("failed to read from stdin");
        let line = match r1.readline("> ") {
            Ok(l) => l,
            Err(ReadlineError::Interrupted) => {
                // std::thread::sleep(Duration::new(1, 0));
                // io::stdout().write(&[0x7F])?;
                // io::stdout().flush()?;
                // std::thread::sleep(Duration::new(1, 0));
                // print!("^C");
                // io::stdout().flush()?;
                // std::thread::sleep(Duration::new(1, 0));
                continue;
            },
            Err(ReadlineError::Eof) => {
                return Ok(());
            },
            Err(_) => {
                continue;
            }
        };
        let inp = line.replace("\n", "");
        match inp.as_str() {
            "q" | "quit" | "exit" => break,
            "clear" => print!("\x1B[2J\x1B[1;1H"),  // clear the screen as well as the stack
            _ => {}
        }
        r1.add_history_entry(inp.clone());
        match parser.parse(&calc, inp.as_str()) {
            Ok(v) => {
                let mut mapped = vec![];
                for c in v {
                    match c {
                        Some(t) => mapped.push(t),
                        None => {
                            println!("\nParse error!");
                            continue
                        }
                    }
                }

                let pre = calc.stack.clone();
                for c in mapped {
                    match calc.exec(c) {
                        Ok(_) => {}
                        Err(e) => { println!("\nEvaluation error! {}", e); calc.stack = pre; break; }
                    }
                }
            }
            Err(e) => {
                println!("\nParse error! {}", e);
                continue
            }
        }
        for item in &calc.stack {
            let out =
                if item.imag().is_zero() { remove_zeros(item.real()) }
                else if item.real().is_zero() {
                    let img = item.imag();
                    if img.is_integer() && img.to_f64() == 1.0 {
                        "i".to_string()
                    } else {
                        format!("{}i", remove_zeros(item.imag()))
                    }
                }
                else { format!("{} + {}i", remove_zeros(item.real()), remove_zeros(item.imag())) };
            println!("{}", out)
        }
    }

    Ok(())
}
