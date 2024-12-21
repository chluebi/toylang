use std::collections::HashMap;
use std::rc::Rc;
use std::fmt;

use crate::ast::{self, Expression};

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


pub fn eval_expression(state: &InterpreterState, expression: &ast::Expression, statement: Rc<ast::Statement>) -> Result<ast::Expression, InterpreterErrorMessage> {
    match expression {
        ast::Expression::IntLiteral(_)
        | ast::Expression::BoolLiteral(_) => Ok(expression.clone()),
        ast::Expression::Variable(var) => {
            match (*state).values.get(var) {
                Some(v) => Ok(v.clone()),
                _ => return Err(InterpreterErrorMessage {
                    error: InterpreterError::VariableNotFound(var.clone()),
                    statement: Some(statement)
                })
            }
        },
        ast::Expression::BinaryOperation { operator, left, right } => {
            let left = eval_expression(state, left, statement.clone())?;
            let right = eval_expression(state, right, statement.clone())?;
            match operator {
                ast::BinOperator::Add 
                | ast::BinOperator::Sub 
                | ast::BinOperator::Mul
                | ast::BinOperator::Div
                | ast::BinOperator::Mod
                | ast::BinOperator::Eq
                | ast::BinOperator::Neq
                | ast::BinOperator::Lt
                | ast::BinOperator::Gt
                | ast::BinOperator::Leq
                | ast::BinOperator::Geq
                => {
                    let evaled = match (left, right) {
                        (ast::Expression::IntLiteral(left), ast::Expression::IntLiteral(right)) => Ok((left, right)),
                        (ast::Expression::IntLiteral(_), x) => Err(
                            InterpreterErrorMessage {
                                error: InterpreterError::Panic(format!("Using Int Operation on non-int: {}", x)),
                                statement: Some(statement)
                            }
                        ),
                        (x, _) => Err(
                            InterpreterErrorMessage {
                                error: InterpreterError::Panic(format!("Using Int Operation on non-int: {}", x)),
                                statement: Some(statement)
                            }
                        )
                    };

                    let (left, right) = evaled?;

                    Ok(match operator {
                        ast::BinOperator::Add => ast::Expression::IntLiteral(left + right),
                        ast::BinOperator::Sub => ast::Expression::IntLiteral(left - right),
                        ast::BinOperator::Mul => ast::Expression::IntLiteral(left * right),
                        ast::BinOperator::Div => ast::Expression::IntLiteral(left / right),
                        ast::BinOperator::Mod => ast::Expression::IntLiteral(left % right),

                        ast::BinOperator::Eq => ast::Expression::BoolLiteral(left == right),
                        ast::BinOperator::Neq => ast::Expression::BoolLiteral(left != right),
                        ast::BinOperator::Lt => ast::Expression::BoolLiteral(left < right),
                        ast::BinOperator::Gt => ast::Expression::BoolLiteral(left > right),
                        ast::BinOperator::Leq => ast::Expression::BoolLiteral(left <= right),
                        ast::BinOperator::Geq => ast::Expression::BoolLiteral(left >= right),

                        _ => unreachable!("This should never be reached")
                    })
                },
                ast::BinOperator::And
                | ast::BinOperator::Or => {
                    let evaled = match (left, right) {
                        (ast::Expression::BoolLiteral(left), ast::Expression::BoolLiteral(right)) => Ok((left, right)),
                        (ast::Expression::BoolLiteral(_), x) => Err(
                            InterpreterErrorMessage {
                                error: InterpreterError::Panic(format!("Using Bool Operation on non-bool: {}", x)),
                                statement: Some(statement)
                            }
                        ),
                        (x, _) => Err(
                            InterpreterErrorMessage {
                                error: InterpreterError::Panic(format!("Using Bool Operation on non-bool: {}", x)),
                                statement: Some(statement)
                            }
                        )
                    };

                    let (left, right) = evaled?;

                    let res = match operator {
                        ast::BinOperator::And => left && right,
                        ast::BinOperator::Or => left || right,
                        _ => unreachable!("This should never be reached")
                    };

                    return Ok(ast::Expression::BoolLiteral(res));
                }                
            }
        },
        ast::Expression::UnaryOperation { operator, expression } => {
            let expression = eval_expression(state, expression, statement.clone())?;

            match operator {
                ast::UnOperator::Neg => {
                    let evaled = match expression {
                        ast::Expression::IntLiteral(expression) => Ok(expression),
                        x => Err(
                            InterpreterErrorMessage {
                                error: InterpreterError::Panic(format!("Using Int Operation on non-int: {}", x)),
                                statement: Some(statement)
                            }
                        )
                    }?;

                    let res = match operator {
                        ast::UnOperator::Neg => -evaled,
                        _ => unreachable!("This should never be reached")
                    };

                    return Ok(ast::Expression::IntLiteral(res));
                },
                ast::UnOperator::Not => {
                    let evaled = match expression {
                        ast::Expression::BoolLiteral(expression) => Ok(expression),
                        x => Err(
                            InterpreterErrorMessage {
                                error: InterpreterError::Panic(format!("Using Bool Operation on non-bool: {}", x)),
                                statement: Some(statement)
                            }
                        )
                    }?;

                    let res = match operator {
                        ast::UnOperator::Not => !evaled,
                        _ => unreachable!("This should never be reached")
                    };

                    return Ok(ast::Expression::BoolLiteral(res));
                }
            }
        }
    }
}

pub fn interpret_statement(state: &mut InterpreterState, stmt: ast::Statement) -> Result<(), InterpreterErrorMessage> {
    let stmt_ref = Rc::new(stmt.clone());
    let eval = match stmt {
        ast::Statement::Assignment {variable, expression} => {
            let v = eval_expression(&state, &expression, stmt_ref)?;
            state.values.entry(variable).and_modify(|value| *value = v.clone()).or_insert(v);
            Ok(())
        },
        ast::Statement::Return { expression } => {
            match eval_expression(&state, &expression, stmt_ref) {
                Ok(v) => {
                    state.return_value = Some(v);
                    Ok(())
                }
                Err(err) => Err(err)
            }
        },
        ast::Statement::IfElse { condition, if_body, else_body } => {
            let eval_condition = eval_expression(&state, &condition, stmt_ref.clone())?;

            match eval_condition {
                ast::Expression::BoolLiteral(b) => {
                    match b {
                        true => interpret_body(state, &if_body),
                        false => interpret_body(state, &else_body),
                    }?;
                    Ok(())
                }
                x => {
                    return Err(InterpreterErrorMessage {
                        error: InterpreterError::Panic(format!("Using non-bool value in loop: {}", x)),
                        statement: Some(stmt_ref)
                    });
                }
            }
        },
        ast::Statement::While { ref condition, ref body } => {
            let eval_condition = eval_expression(&state, &condition, stmt_ref.clone())?;

            match eval_condition {
                ast::Expression::BoolLiteral(b) => {
                    if b {
                        interpret_body(state, &body)?;
                        interpret_statement(state, stmt)?;
                    };
                    Ok(())
                }
                x => {
                    return Err(InterpreterErrorMessage {
                        error: InterpreterError::Panic(format!("Using non-bool value in loop: {}", x)),
                        statement: Some(stmt_ref)
                    });
                }
            }
        }
    };

    if let Err(err) = eval {
        return Err(err);
    }

    Ok(())
}

pub fn interpret_body(state: &mut InterpreterState, body: &ast::Body) -> Result<(), InterpreterErrorMessage> {
    for stmt in body.statements.clone() {
        println!("{}", state);
        println!("{}", stmt);
        interpret_statement(state, stmt)?;
    }
    return Ok(());
}

pub fn interpret(program: &ast::Program) -> Result<ast::Expression, InterpreterErrorMessage> {

    println!("{}", program);

    let mut state = InterpreterState {
        values: HashMap::new(),
        return_value: None
    };

    let program_clone = program.clone();
    let last_statement = program_clone.body.statements.last();

    interpret_body(&mut state, &program.body)?;
    
    if let Some(v) = &state.return_value {
        println!("{}", state);
        return Ok(v.clone());
    }

    return Err(
        match last_statement {
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