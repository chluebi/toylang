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

    #[test]
    fn lists() {
        let program_text = read_file("programs/lists").unwrap();
        let program = parser::GrammarParser::new().parse(&program_text);
        let res = interpreter::interpret(&program.unwrap()).unwrap();

        match res {
            ast::Expression::IntLiteral(9900) => assert!(true),
            _ => assert!(false)
        }
    }

    #[test]
    fn circular() {
        let program_text = read_file("programs/circular").unwrap();
        let program = parser::GrammarParser::new().parse(&program_text);
        let res = interpreter::interpret(&program.unwrap()).unwrap();

        match res {
            ast::Expression::IntLiteral(1) => assert!(true),
            _ => assert!(false)
        }
    }

    #[test]
    fn powerset() {
        let program_text = read_file("programs/powerset").unwrap();
        let program = parser::GrammarParser::new().parse(&program_text);
        interpreter::interpret(&program.unwrap()).unwrap();

        assert!(true)
    }

    #[test]
    fn typecheck() {
        let program_text = read_file("programs/typecheck").unwrap();
        let program = parser::GrammarParser::new().parse(&program_text);
        interpreter::interpret(&program.unwrap()).unwrap();

        assert!(true)
    }

    #[test]
    fn treesum() {
        let program_text = read_file("programs/treesum").unwrap();
        let program = parser::GrammarParser::new().parse(&program_text);
        let res = interpreter::interpret(&program.unwrap()).unwrap();

        match res {
            ast::Expression::IntLiteral(11) => assert!(true),
            _ => assert!(false)
        }
    }

    #[test]
    fn mmult() {
        let program_text = read_file("programs/mmult").unwrap();
        let program = parser::GrammarParser::new().parse(&program_text);
        let res = interpreter::interpret(&program.unwrap()).unwrap();

        match res {
            ast::Expression::IntLiteral(1884) => assert!(true),
            _ => assert!(false)
        }
    }

    #[test]
    fn forloop() {
        let program_text = read_file("programs/forloop").unwrap();
        let program = parser::GrammarParser::new().parse(&program_text);
        let res = interpreter::interpret(&program.unwrap()).unwrap();

        match res {
            ast::Expression::IntLiteral(2047) => assert!(true),
            _ => assert!(false)
        }
    }
}