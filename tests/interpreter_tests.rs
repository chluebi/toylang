use std::fs::File;
use std::io::{self, Read};

use toylang::interpreter;
use toylang::parser;
use toylang::ast;

fn read_file(file_path: &str) -> io::Result<String> {
    let mut file = File::open(file_path)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    Ok(contents)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn recursive_fib() {
        let program_text = read_file("programs/recursive_fib").unwrap();
        let program = parser::GrammarParser::new().parse(&program_text);
        let res = interpreter::interpret(&program.unwrap()).unwrap();

        match res {
            ast::Expression::IntLiteral(4181) => assert!(true),
            _ => assert!(false)
        }
    }

    #[test]
    fn factorial() {
        let program_text = read_file("programs/factorial").unwrap();
        let program = parser::GrammarParser::new().parse(&program_text);
        let res = interpreter::interpret(&program.unwrap()).unwrap();

        match res {
            ast::Expression::IntLiteral(3628800) => assert!(true),
            _ => assert!(false)
        }
    }

    #[test]
    fn collatz() {
        let program_text = read_file("programs/collatz").unwrap();
        let program = parser::GrammarParser::new().parse(&program_text);
        let res = interpreter::interpret(&program.unwrap()).unwrap();

        match res {
            ast::Expression::IntLiteral(1) => assert!(true),
            _ => assert!(false)
        }
    }

    #[test]
    fn square() {
        let program_text = read_file("programs/square").unwrap();
        let program = parser::GrammarParser::new().parse(&program_text);
        let res = interpreter::interpret(&program.unwrap()).unwrap();

        match res {
            ast::Expression::BoolLiteral(true) => assert!(true),
            _ => assert!(false)
        }
    }

    #[test]
    fn tuples() {
        let program_text = read_file("programs/tuples").unwrap();
        let program = parser::GrammarParser::new().parse(&program_text);
        let res = interpreter::interpret(&program.unwrap()).unwrap();

        match res {
            ast::Expression::IntLiteral(4950) => assert!(true),
            _ => assert!(false)
        }
    }
}