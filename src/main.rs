mod ast;
mod parse_ast;
mod interpreter;
mod runner;
mod preprocess;


use lalrpop_util::lalrpop_mod;

lalrpop_mod!(pub parser); // synthesized by LALRPOP

pub fn main() {
    runner::run("programs/ret_in_loop".to_string());
}