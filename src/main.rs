use rustyline::Editor;
use rustyline::error::ReadlineError;

use lexing::{Keywords, Lexer};
use parsing::Parser;

use crate::interpreting::Interpreter;
//use crate::types::type_checker::type_check;

mod parsing;
mod lexing;
mod utility;
mod interpreting;
mod errors;
mod types;
mod internal;

use std::{env, fs, process};

fn main() {

    let args = env::args().collect::<Vec<String>>();
    let mut interpreter = Interpreter::new();

    if args.len() == 2 {
        let path = &args[1];
        match fs::read_to_string(path) {
            Err(err) => println!("{}", err),
            Ok(contents) => execute(contents, &mut interpreter)
        }
        process::exit(0)
    } else if args.len() != 1 {
        println!("Invalid args");
        process::exit(1);
    }

    let mut rl = Editor::<()>::new();
    if rl.load_history("interpreterhistory.txt").is_err() {}


    loop {
        let input = match rl.readline("\\>>: ") {
            Ok(ref line) if line.is_empty() => { continue; }
            Ok (ref line) if line == ":e" => {
                println!("{:#?}", interpreter.env);
                continue;
            }
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                if rl.save_history("interpreterhistory.txt").is_err() {
                    println!("Failed to save history");
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
                errors.iter().for_each(|x| println!("{}", x));
                continue
            },
        };

//        println!("{:#?}", tokens);

        let mut parser = Parser::new(tokens);

        let statements = match parser.parse() {
            Ok(x) => x,
            Err(errors) => {
                println!("Parse Error: ");
                errors.iter().for_each(|x| println!("{}", x));
                continue;
            }
        };

//        println!("{:?}", statements);
//        println!("{:#?}", statements);

        statements.iter().for_each(|x| println!("{}", x));

//        if let Err(err) = type_check(statements) {
//            err.iter().for_each(|e| println!("{:?}", e));
//            continue;
//        }


        if let Err(errors) = interpreter.interpret(&statements) {
            println!("Interpreter Error: ");
            errors.iter().for_each(|x| println!("{}", x));
        }



        // Parse Expressions only

//        let mut parser = Parser::new(tokens);
//        let expr = match parser.parse_expression() {
//            Ok(x) => x,
//            Err(err) => {
//                println!("Parse Error: {:?}", err);
//                continue;
//            }
//        };
//
//        println!("{}", expr);
//        println!("{}", evaluate(&expr));


    }

}

fn execute(input: String, interpreter: &mut Interpreter) {
    let mut lexer = Lexer::new(input, Keywords::map());
    let tokens = match lexer.lex() {
        Ok(t) => t,
        Err(e) => {
            println!("{:?}", e);
            process::exit(1);
        }
    };

    let mut parser = Parser::new(tokens);
    let statements = match parser.parse() {
        Ok(s) => s,
        Err(e) => {
            println!("{:?}", e);
            process::exit(1);
        }
    };

    statements.iter().for_each(|x| println!("{}", x));

}



