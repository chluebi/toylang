mod ast;
mod interpreter;
mod runner;

use lalrpop_util::lalrpop_mod;

lalrpop_mod!(pub parser); // synthesized by LALRPOP

pub fn main() {
    runner::run("programs/forloop".to_string());
}