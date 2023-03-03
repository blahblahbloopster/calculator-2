use std::borrow::Cow;
use std::borrow::Cow::Owned;
use std::collections::HashMap;
use std::io;
use std::io::Write;
use rustyline::Editor;
use rustyline::error::ReadlineError;
use rustyline::highlight::Highlighter;
use crate::rpn::{Calc, PComplex, RPNCommand};
use rustyline_derive::{Completer, Helper, Hinter, Validator};
use crate::infixtree::{InfixParseTree, PR};
extern crate lalrpop_util;

mod rpn;
mod rpncommand;
mod units;
mod infixtree;
mod infix;

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

#[derive(Helper, Completer, Hinter, Validator)]
struct Completioninator {}

enum Color {
    Operator, Variable, Literal, Reset
}

impl Color {
    fn ansi(&self) -> &str {
        match self {
            Color::Operator => { "\x1B[38;5;167m" }
            Color::Variable => { "\x1B[38;5;2m" }
            Color::Literal => { "\x1B[38;5;69m" }
            Color::Reset => { "\x1B[0m" }
        }
    }
}

struct ModifiableString {
    original: String,
    modifications: HashMap<usize, String>
}

impl ModifiableString {
    fn string(&self) -> String {
        let mut output = String::new();
        for (i, c) in self.original.chars().enumerate() {
            let addition = self.modifications.get(&i);
            match addition {
                Some(v) => output.push_str(v.as_str()),
                None => {}
            };
            output.push(c);
        }
        match self.modifications.get(&self.original.len()) {
            Some(v) => output.push_str(v.as_str()),
            None => {}
        };
        output
    }

    fn before(&mut self, index: usize, value: String) {
        match self.modifications.get_mut(&index) {
            Some(v) => { v.push_str(value.as_str()) }
            None => { self.modifications.insert(index, value); }
        }
    }

    fn after(&mut self, index: usize, value: String) {
        match self.modifications.get_mut(&index) {
            Some(v) => { v.insert_str(0, value.as_str()) }
            None => { self.modifications.insert(index, value); }
        }
    }

    fn new(original: String) -> ModifiableString {
        ModifiableString { original, modifications: HashMap::new() }
    }
}

impl Completioninator {
    fn highlight_infix(line: &mut ModifiableString, tree: &PR<InfixParseTree>) {
        match &*tree.item {
            InfixParseTree::Op(args, operation) => {
                for item in args {
                    Completioninator::highlight_infix(line, &item);
                    line.before(operation.range.start, Color::Operator.ansi().to_string());
                    line.after(operation.range.end, Color::Reset.ansi().to_string());
                }
            }
            InfixParseTree::Literal(_) => {
                line.before(tree.range.start, Color::Literal.ansi().to_string());
                line.after(tree.range.end, Color::Reset.ansi().to_string());
            }
            InfixParseTree::Parens(_, content, _) => {
                Completioninator::highlight_infix(line, &content)
            }
            InfixParseTree::VarGet(_) => {
                line.before(tree.range.start, Color::Variable.ansi().to_string());
                line.after(tree.range.end, Color::Reset.ansi().to_string());
            }
        }
    }
}

impl Highlighter for Completioninator {
    fn highlight<'l>(&self, line: &'l str, _: usize) -> Cow<'l, str> {
        let calc = Calc::new(1);
        let parsed = match CommandsParser::new().parse(&calc, line.strip_prefix("> ").unwrap_or(line)) {
            Ok(v) => v,
            Err(_) => { /*println!("{}", e); */return Owned(line.to_string()) }
        };
        let mut output = ModifiableString::new(line.to_string());
        for item in parsed.iter().rev() {
            match item {
                Some(v) => {
                    match &*v.item {
                        RPNCommand::Number(_) => {
                            output.before(v.range.start, Color::Literal.ansi().to_string());
                            output.after(v.range.end, Color::Reset.ansi().to_string());
                        }
                        RPNCommand::Op(_) => {
                            output.before(v.range.start, Color::Operator.ansi().to_string());
                            output.after(v.range.end, Color::Reset.ansi().to_string());
                        }
                        RPNCommand::VarSet(_) => {
                            output.before(v.range.start, Color::Variable.ansi().to_string());
                            output.after(v.range.end, Color::Reset.ansi().to_string());
                        }
                        RPNCommand::VarGet(_) => {
                            output.before(v.range.start, Color::Variable.ansi().to_string());
                            output.after(v.range.end, Color::Reset.ansi().to_string());
                        }
                        RPNCommand::Clear => {
                            output.before(v.range.start, Color::Operator.ansi().to_string());
                            output.after(v.range.end, Color::Reset.ansi().to_string());
                        }
                        RPNCommand::Duplicate => {
                            output.before(v.range.start, Color::Operator.ansi().to_string());
                            output.after(v.range.end, Color::Reset.ansi().to_string());
                        }
                        RPNCommand::Swap => {
                            output.before(v.range.start, Color::Operator.ansi().to_string());
                            output.after(v.range.end, Color::Reset.ansi().to_string());
                        }
                        RPNCommand::Infix(tree) => {
                            Completioninator::highlight_infix(&mut output, tree)
                        }
                        RPNCommand::Clipboard => {
                            output.before(v.range.start, Color::Operator.ansi().to_string());
                            output.after(v.range.end, Color::Reset.ansi().to_string());
                        }
                        RPNCommand::PrettyPrint => {
                            output.before(v.range.start, Color::Operator.ansi().to_string());
                            output.after(v.range.end, Color::Reset.ansi().to_string());
                        }
                    }
                }
                None => {}
            }
        }
        Owned(output.string())
    }

    fn highlight_char(&self, _: &str, _: usize) -> bool {
        true
    }
}

fn main() -> rustyline::Result<()> {
    let mut r1: Editor<Completioninator> = Editor::new()?;
    let helper = Completioninator {};
    r1.set_helper(Some(helper));

    let prec: u32;
    loop {
        let inp = r1.readline("precision: ")?.replace("\n", "");
        if inp.is_empty() {
            prec = 1024;
            break;
        } else {
            match inp.parse::<u32>() {
                Ok(v @ 10..=16777216) => {
                    prec = v;
                    break
                }
                _ => println!("Enter a valid number!")
            }
        }
    }
    let mut calc = Calc::new(prec);

    let parser = CommandsParser::new();
    loop {
        let mut print_stack = true;
        let line = match r1.readline("> ") {
            Ok(l) => l,
            Err(ReadlineError::Interrupted) => {
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
            "clear" => {
                // clear the screen as well as the stack
                io::stdout().write(b"\x1b[H\x1b[2J")?;
                io::stdout().flush()?;
            }
            "" => {
                for item in &calc.stack {
                    println!("{}", PComplex(item))
                }
                continue;        
            }
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
                    match *c.item {
                        RPNCommand::PrettyPrint => {
                            print_stack = false;
                        }
                        _ => {}
                    }
                    match calc.exec(*c.item) {
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
        if !print_stack {
            continue;
        }
        for item in &calc.stack {
            println!("{}", PComplex(item))
        }
    }

    Ok(())
}
