use std::fs::File;
use std::io::{self, Read};
use std::str;

use crate::parser;
use crate::interpreter;


fn get_error_snippet(source: &str, start: usize, end: usize) -> String {
    let lines = source.lines().collect::<Vec<&str>>();

    let mut current_pos = 0;
    let mut line_number = 1;

    for line in &lines {
        let line_len = line.len() + 1;

        if current_pos + line_len > start {
            let column_start = start - current_pos;
            let column_end = std::cmp::min(end - current_pos, line.len());

            let snippet = format!(
                "{}\n{}{}",
                line,
                " ".repeat(column_start),
                "^".repeat(column_end - column_start)
            );

            return format!("Line {}:\n{}", line_number, snippet);
        }

        current_pos += line_len;
        line_number += 1;
    }

    "Error position out of range".to_string()
}


fn read_file(file_path: &str) -> io::Result<String> {
    let mut file = File::open(file_path)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    Ok(contents)
}


pub fn eval(path: String) -> Result<interpreter::Value, interpreter::InterpreterErrorMessage> {
    let program_text = read_file(&path).unwrap();
    let program = parser::GrammarParser::new().parse(&program_text);
    interpreter::interpret(&program.unwrap())
}


pub fn run(path: String) -> () {
    let program_text = read_file(&path).unwrap();
    let program = parser::GrammarParser::new().parse(&program_text);
    match interpreter::interpret(&program.unwrap()) {
        Ok(v) => println!("Program Executed with result {}", v),
        Err(e) => {
            match e.range.clone() {
                Some(range) => {
                    let original_code_string = get_error_snippet(&program_text, range.start, range.end);
                    println!("Program Failed: {}\n{}", e, original_code_string)
                },
                _ => println!("Program Failed {}: [Unknown Location]", e)
            }
        }
    };
}