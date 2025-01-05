use lalrpop_util::lalrpop_mod;
lalrpop_mod!(pub parser); // synthesized by LALRPOP

mod parse_ast;
mod ast;
mod interpreter;
mod builtins;
mod preprocess;
mod runner;





pub fn main() {
    runner::run("programs/function_statements".to_string());
}