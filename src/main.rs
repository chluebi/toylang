mod ast;
mod interpreter;
mod programs;

fn main() {
    let program = programs::long_while::get_program();

    let res = interpreter::interpret(&program);

    match res {
        Ok(v) => println!("Program Executed with result {}", v),
        Err(e) => println!("Program Failed {}", e)
    }
}
