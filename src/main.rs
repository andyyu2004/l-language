#[allow(dead_code)]
use rustyline::Editor;
use rustyline::error::ReadlineError;

use lexing::{Keywords, Lexer};
use parsing::Parser;

use crate::interpreting::{Interpreter};

mod parsing;
mod lexing;
mod utility;
mod interpreting;
mod errors;
mod types;
mod internal;
mod static_analysis;
mod generation;

use std::{env, fs, process};
use crate::types::type_checker::TypeChecker;
use crate::parsing::Mode;
use crate::static_analysis::StaticAnalyser;
use crate::generation::{Desugarer};

//fn recurse(n: i32) {
//    println!("depth: {}", n);
//    recurse(n + 1)
//}


fn main() {
    let args = env::args().collect::<Vec<String>>();
    if args.len() == 2 {
        let path = &args[1];
        match fs::read_to_string(path) {
            Err(err) => eprintln!("{}", err),
            Ok(contents) => execute(contents)
        }
        process::exit(0)
    } else if args.len() != 1 {
        eprintln!("Invalid args");
        process::exit(1);
    }

    let mut rl = Editor::<()>::new();
    if rl.load_history("interpreterhistory.txt").is_err() {}

    let mut analyser = StaticAnalyser::new();
    let mut typechecker = TypeChecker::new(Mode::Interactive);
//    let mut env = Env::new(None);
    let mut interpreter = Interpreter::new(Mode::Interactive);


    loop {
        let input = match rl.readline("\\>>: ") {
            Ok(ref line) if line.is_empty() => { continue; }
            Ok (ref line) if line == ":e" => {
                rl.add_history_entry(line.as_str());
                if rl.save_history("interpreterhistory.txt").is_err() {
                    eprintln!("Failed to save history");
                }
                println!("{:#?}", interpreter.env);
                continue;
            },
            Ok(ref line) if line.starts_with(":t") => {
                let var = &line[3..].trim_start();
                match typechecker.get_type(var) {
                    Some(t) => println!("{}", t),
                    None => eprintln!("Undefined variable")
                }
                continue;
            },
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                if rl.save_history("interpreterhistory.txt").is_err() {
                    eprintln!("Failed to save history");
                }
                line
            }
            Err(ReadlineError::Interrupted) => {
                continue;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        };


        let mut lexer = Lexer::new(input, Keywords::map());
        let tokens = match lexer.lex() {
            Ok(x) => x,
            Err(errors) => {
                println!("Lex Error: ");
                errors.iter().for_each(|x| eprintln!("{}", x));
                continue
            },
        };

        // println!("{:#?}", tokens);

        let mut parser = Parser::new(tokens, Mode::Interactive);

        let mut statements = match parser.parse() {
            Ok(x) => x,
            Err(errors) => {
                println!("Parse Error: ");
                errors.iter().for_each(|x| eprintln!("{}", x));
                continue;
            }
        };

//        println!("{:?}", statements);
//        println!("{:#?}", statements);

        statements.iter().for_each(|x| println!("{}", x));

        Desugarer::new().desugar(&mut statements);
//        println!("Desugared");
//        statements.iter().for_each(|x| println!("{}", x));

        if let Err(err) = analyser.analyse(&statements) {
            eprintln!("Error in static analyser");
            err.iter().for_each(|e| eprint!("{}", e));
            continue;
        }

        if let Err(err) = typechecker.type_check(&statements) {
            eprintln!("Typecheck error");
            err.iter().for_each(|e| eprint!("{}", e));
            continue;
        }


        if let Err(errors) = interpreter.interpret(statements) {
            println!("Interpreter Error: ");
            errors.iter().for_each(|x| eprint!("{}", x));
        }

    }

}

// Can't bring upper functionality into function as requires break and continue
pub fn execute(input: String) {

    let mut analyser = StaticAnalyser::new();
    let mut typechecker = TypeChecker::new(Mode::Interpreted);
//    let mut env = Env::new(None);
    let mut interpreter = Interpreter::new(Mode::Interpreted);

    let mut lexer = Lexer::new(input, Keywords::map());

    let tokens = match lexer.lex() {
        Ok(t) => t,
        Err(errors) => {
            println!("Lex Error: ");
            errors.iter().for_each(|x| eprint!("{}", x));
            process::exit(1);
        }
    };

    let mut parser = Parser::new(tokens, Mode::Interpreted);

    let mut statements = match parser.parse() {
        Ok(s) => s,
        Err(errors) => {
            println!("Parse Error: ");
            errors.iter().for_each(|x| eprint!("{}", x));
            process::exit(1);
        }
    };

    Desugarer::new().desugar(&mut statements);
//    println!("Desugared");
//    statements.iter().for_each(|x| println!("{}", x));

    if let Err(err) = analyser.analyse(&statements) {
        eprintln!("Error in static analyser");
        err.iter().for_each(|e| eprint!("{}", e));
        process::exit(1)
    }

    if let Err(err) = typechecker.type_check(&statements) {
        eprintln!("Typecheck error");
        err.iter().for_each(|e| eprint!("{}", e));
        process::exit(1)
    }


    if let Err(errors) = interpreter.interpret(statements) {
        println!("Interpreter Error: ");
        errors.iter().for_each(|x| eprint!("{}", x));
        process::exit(1)
    }

}



