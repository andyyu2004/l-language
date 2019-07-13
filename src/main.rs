mod parsing;
mod lexing;
mod utility;
mod interpreting;

use std::io;
use lexing::{TokenType, Lexer, Keywords};
use parsing::{Parser};
use crate::interpreting::{Interpreter};
use std::io::Write;

struct L {
}

impl L {
    fn error(message: String, line: i32, col: i16) -> String {
        format!("error:{}:{}: {}", line, col, message)
    }
}

fn main() {

    loop {
        let stdout = io::stdout();
        let mut handle = stdout.lock();
        handle.write(b">>=: ").expect("Failed to write to stdout");;
        handle.flush().expect("Failed to flush stdout");

        let mut input = String::new();
        io::stdin().read_line(&mut input).expect("Failed to read line");
        println!("{}", input);

        let mut lexer = Lexer::new(Keywords::map());
        let tokens = match lexer.lex(&input) {
            Ok(x) => x,
            Err(errors) => {
                println!("Lex Error: ");
                errors.iter().for_each(|x| println!("{}", x));
                continue
            },
        };

        println!("{:#?}", tokens);

        let mut parser = Parser::new(tokens);
        let statements = match parser.parse() {
            Ok(x) => x,
            Err(errors) => {
                println!("Parse Error: ");
                errors.iter().for_each(|x| println!("{}", x));
                continue;
            }
        };

        let mut interpreter = Interpreter::new();
//        println!("{:?}", statements);
        println!("{:#?}", statements);
        statements.iter().for_each(|x| println!("{}", x));
        if let Err(errors) = interpreter.interpret(&statements) {
            println!("Interpreter Error: {:?}", errors);
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



