use std::collections::HashMap;
use std::rc::Rc;
use std::fmt;

use crate::ast;

pub struct InterpreterState {
    values: HashMap<String, ast::Expression>,
    return_value: Option<ast::Expression>
}

impl fmt::Display for InterpreterState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Variables {:?}, Returns {}", self.values, 
            match &self.return_value {
                Some(v) => v.to_string(),
                None => "[Empty]".to_string()
            }
        )
    }
}

#[derive(Debug)]
enum InterpreterError {
    Panic(String),
    VariableNotFound(String)
}

impl fmt::Display for InterpreterError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            InterpreterError::Panic(err) => write!(f, "Interpreter Panic: {}", err),
            InterpreterError::VariableNotFound(var) => write!(f, "Unknown Variable: {}", var)
        }
    }
}

#[derive(Debug)]
pub struct InterpreterErrorMessage {
    error: InterpreterError,
    statement: Option<Rc<ast::Statement>>
}

impl fmt::Display for InterpreterErrorMessage {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.statement.clone() {
            Some(s) => write!(f, "Error {}\n in {}", self.error, s),
            _ => write!(f, "Error {}", self.error)
        }
    }
}

impl std::error::Error for InterpreterErrorMessage {}


pub fn eval_expression(state: &InterpreterState, expression: ast::Expression, statement: Rc<ast::Statement>) -> Result<ast::Expression, InterpreterErrorMessage> {
    match expression {
        ast::Expression::IntLiteral(i) => Ok(ast::Expression::IntLiteral(i)),
        ast::Expression::Variable(var) => {
            match (*state).values.get(&var) {
                Some(v) => Ok(v.clone()),
                _ => return Err(InterpreterErrorMessage {
                    error: InterpreterError::VariableNotFound(var.clone()),
                    statement: Some(statement)
                })
            }
        },
        ast::Expression::BinaryOperation { left, operator, right } => {
            let left = eval_expression(state, *left, statement.clone())?;
            let right = eval_expression(state, *right, statement.clone())?;
            match operator {
                ast::Operator::Add 
                | ast::Operator::Sub 
                | ast::Operator::Mul
                | ast::Operator::Div => {
                    let evaled = match (left, right) {
                        (ast::Expression::IntLiteral(left), ast::Expression::IntLiteral(right)) => Ok((left, right)),
                        _ => Err(
                            InterpreterErrorMessage {
                                error: InterpreterError::Panic("Evaluated statements was not literal".to_string()),
                                statement: Some(statement)
                            }
                        )
                    };

                    let (left, right) = evaled?;

                    let res = match operator {
                        ast::Operator::Add => left + right,
                        ast::Operator::Sub => left - right,
                        ast::Operator::Mul => left * right,
                        ast::Operator::Div => left / right,
                    };

                    return Ok(ast::Expression::IntLiteral(res));
                }
            }
        }
    }
}


pub fn interpret(program: ast::Program) -> Result<ast::Expression, InterpreterErrorMessage> {

    println!("{}", program);

    let mut state = InterpreterState {
        values: HashMap::new(),
        return_value: None
    };

    for stmt in program.body.clone() {
        println!("{}", state);

        let stmtRef = Rc::new(stmt.clone());
        let eval = match stmt {
            ast::Statement::Assignment {variable, expression} => {
                let v = eval_expression(&state, expression, stmtRef)?;
                state.values.entry(variable).and_modify(|value| *value = v.clone()).or_insert(v);
                Ok(())
            },
            ast::Statement::Return { expression } => {
                match eval_expression(&state, expression, stmtRef) {
                    Ok(v) => {
                        state.return_value = Some(v);
                        Ok(())
                    }
                    Err(err) => Err(err)
                }
            }
        };

        if let Err(err) = eval {
            return Err(err);
        }

        if let Some(v) = &state.return_value {
            println!("{}", state);
            return Ok(v.clone());
        }
    }

    return Err(
        match program.body.last() {
            Some(s) => InterpreterErrorMessage {
                    error: InterpreterError::Panic("No return statement".to_string()),
                    statement: Some(Rc::new(s.clone()))
            },
            _ => InterpreterErrorMessage {
                error: InterpreterError::Panic("Empty Body".to_string()),
                statement: None
            }
        }
    )
}