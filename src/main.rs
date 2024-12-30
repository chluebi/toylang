mod ast;
mod interpreter;

use std::fs::File;
use std::io::{self, Read};
use lalrpop_util::lalrpop_mod;

lalrpop_mod!(pub parser); // synthesized by LALRPOP


fn read_file(file_path: &str) -> io::Result<String> {
    let mut file = File::open(file_path)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    Ok(contents)
}

pub fn main() {
    let program_text = read_file("programs/forloop").unwrap();
    let program = parser::GrammarParser::new().parse(&program_text);
    match interpreter::interpret(&program.unwrap()) {
        Ok(v) => println!("Program Executed with result {}", v),
        Err(e) => println!("Program Failed {}", e)
    };
}