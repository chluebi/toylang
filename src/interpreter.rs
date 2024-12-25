use std::collections::HashMap;
use std::rc::Rc;
use std::fmt;

use crate::ast;

type FunctionFrame = HashMap<String, ast::Expression>;
pub struct InterpreterState {
    stack: Vec<FunctionFrame>,
    values: FunctionFrame,
    return_value: Option<ast::Expression>
}

impl fmt::Display for InterpreterState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Stack Size {}, Variables {:?}, Returns {}", self.stack.len(), self.values, 
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


pub fn eval_expression(state: &mut InterpreterState, expression: &ast::Expression, statement: Rc<ast::Statement>, program: &ast::Program) -> Result<ast::Expression, InterpreterErrorMessage> {
    match expression {
        ast::Expression::IntLiteral(_)
        | ast::Expression::BoolLiteral(_)
        | &ast::Expression::FunctionLiteral(_) => Ok(expression.clone()),
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
            let left = eval_expression(state, left, statement.clone(), program)?;
            let right = eval_expression(state, right, statement.clone(), program)?;
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
            let expression = eval_expression(state, expression, statement.clone(), program)?;

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
        },
        ast::Expression::FunctionCall { function_name, arguments } => {
            let function = match program.functions.get(function_name) {
                Some(f) => f,
                _ => return Err(InterpreterErrorMessage {
                    error: InterpreterError::VariableNotFound(function_name.clone()),
                    statement: Some(statement)
                })
            };

            let argument_values: Result<Vec<ast::Expression>, InterpreterErrorMessage>
                = arguments.iter().map(|arg| eval_expression(state, arg, statement.clone(), program)).collect();
            let argument_values: Vec<ast::Expression> = argument_values?;

            state.stack.push(state.values.clone());

            let new_values: HashMap<String, ast::Expression> = function.arguments.clone().into_iter().zip(argument_values.into_iter()).collect();
            state.values = new_values;
            state.return_value = None;

            interpret_function_body(state, function, function_name, program)
        }
    }
}

pub fn interpret_statement(state: &mut InterpreterState, stmt: ast::Statement, program: &ast::Program) -> Result<Option<ast::Expression>, InterpreterErrorMessage> {
    let stmt_ref = Rc::new(stmt.clone());
    let eval = match stmt {
        ast::Statement::Assignment {variable, expression} => {
            let v = eval_expression(state, &expression, stmt_ref, program)?;
            state.values.entry(variable).and_modify(|value| *value = v.clone()).or_insert(v);
            Ok(None)
        },
        ast::Statement::Return { expression } => {
            match eval_expression(state, &expression, stmt_ref.clone(), program) {
                Ok(v) => {
                    state.return_value = Some(v.clone());
                    match state.stack.pop() {
                        Some(s) => {state.values = s},
                        _ => {
                            return Err(InterpreterErrorMessage {
                                error: InterpreterError::Panic("Returning with empty stack frame".to_string()),
                                statement: Some(stmt_ref)
                            })
                        }
                    }
                    Ok(Some(v))
                }
                Err(err) => Err(err)
            }
        },
        ast::Statement::IfElse { condition, if_body, else_body } => {
            let eval_condition = eval_expression(state, &condition, stmt_ref.clone(), program)?;

            match eval_condition {
                ast::Expression::BoolLiteral(b) => {
                    match b {
                        true => interpret_body(state, &if_body, program),
                        false => interpret_body(state, &else_body, program),
                    }?;
                    Ok(None)
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
            let eval_condition = eval_expression(state, &condition, stmt_ref.clone(), program)?;

            let mut cond = match eval_condition {
                ast::Expression::BoolLiteral(b) => b,
                x => {
                    return Err(InterpreterErrorMessage {
                        error: InterpreterError::Panic(format!("Using non-bool value in loop: {}", x)),
                        statement: Some(stmt_ref)
                    });
                }
            };

            while cond  {
                interpret_body(state, &body, program)?;

                let eval_condition = eval_expression(state, &condition, stmt_ref.clone(), program)?;
                cond = match eval_condition {
                    ast::Expression::BoolLiteral(b) => b,
                    x => {
                        return Err(InterpreterErrorMessage {
                            error: InterpreterError::Panic(format!("Using non-bool value in loop: {}", x)),
                            statement: Some(stmt_ref)
                        });
                    }
                };
            }


            Ok(None)
        }
    };

    if let Err(err) = eval {
        return Err(err);
    }

    eval
}

pub fn interpret_body(state: &mut InterpreterState, body: &ast::Body, program: &ast::Program) -> Result<Option<ast::Expression>, InterpreterErrorMessage> {
    for stmt in body.statements.clone() {
        println!("{}", state);
        println!("{}", stmt);
        match interpret_statement(state, stmt, program) {
            Err(e) => return Err(e),
            Ok(Some(v)) => return Ok(Some(v)),
            Ok(_) => {}
        };
    }
    return Ok(None);
}

pub fn interpret_function_body(state: &mut InterpreterState, function: &ast::Function, function_name: &String, program: &ast::Program) -> Result<ast::Expression, InterpreterErrorMessage>{
    interpret_body(state, &function.body, program)?;
    
    if let Some(v) = &state.return_value {
        println!("returning {} from function with {}", v, state);
        return Ok(v.clone());
    }

    return Err(
        InterpreterErrorMessage {
            error: InterpreterError::Panic(format!("Function {} did not return", function_name)),
            statement: None
        }
    )
}

pub fn interpret(program: &ast::Program) -> Result<ast::Expression, InterpreterErrorMessage> {

    println!("{}", program);

    let mut state = InterpreterState {
        stack: vec![HashMap::new()],
        values: HashMap::new(),
        return_value: None
    };

    let main = program.functions.get(&"main".to_string()).unwrap();

    interpret_function_body(&mut state, &main, &"main".to_string(), program)
}