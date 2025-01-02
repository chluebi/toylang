use std::fmt;
use std::collections::HashMap;
use lazy_static::lazy_static;

use crate::ast;
use crate::interpreter;


#[derive(Debug, Clone)]
pub struct BuiltinFunction {
    pub name: String,
    pub contract: ast::FunctionContract,
    pub function: fn(&interpreter::InterpreterState) -> Result<interpreter::Value, interpreter::InterpreterErrorMessage>
}

impl fmt::Display for BuiltinFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Built-in: {} {} {:?}", self.name, self.contract, self.function)
    }
}


fn len(state: &interpreter::InterpreterState) -> Result<interpreter::Value, interpreter::InterpreterErrorMessage> {
    let arg = state.values.get(&"x".to_string()).unwrap();

    match arg {
        interpreter::Value::Tuple { elements } => Ok(interpreter::Value::Int(elements.len() as i64)),
        interpreter::Value::ListReference { elements_ref } => Ok(interpreter::Value::Int(elements_ref.borrow().len() as i64)),
        interpreter::Value::DictionaryReference { index_ref } => Ok(interpreter::Value::Int(index_ref.borrow().len() as i64)),
        x => Err(
            interpreter::InterpreterErrorMessage {
                error: interpreter::InterpreterError::InvalidType(x.clone(), "with length".to_string(), "".to_string()),
                range: None
            }
        )
    }
}


lazy_static!{
    pub static ref BUILTINS: HashMap<String, BuiltinFunction> = [
        ("len".to_string(), BuiltinFunction {
            name: "len".to_string(), 
            contract: ast::FunctionContract {
                positional_arguments: vec![ast::Argument {name: "x".to_string(), loc: 0..0}],
                variadic_argument: None,
                keyword_arguments: vec![],
                keyword_variadic_argument: None
            },
            function: len
        })
    ].iter().cloned().collect();
}
