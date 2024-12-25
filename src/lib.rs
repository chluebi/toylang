pub mod ast;
pub mod interpreter;

use lalrpop_util::lalrpop_mod;

lalrpop_mod!(pub parser); // synthesized by LALRPOP