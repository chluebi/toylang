use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;
use std::hash::{Hash, Hasher};
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
    InvalidType(ast::Expression, String, String),
    VariableNotFound(String),
    IndexOutofBounds(ast::Expression, usize),
    IndexNotInDict(ast::Expression, ast::Expression),
    Unhashable(ast::Expression)
}

impl fmt::Display for InterpreterError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            InterpreterError::Panic(err) => write!(f, "Interpreter Panic: {}", err),
            InterpreterError::InvalidType(expr, expected_type, comment) => write!(f, "Using {} operation on {}: {}", expected_type, expr, comment),
            InterpreterError::VariableNotFound(var) => write!(f, "Unknown Variable: {}", var),
            InterpreterError::IndexOutofBounds(expr, index) => write!(f, "Index of {} is out of bounds on {}", index, expr),
            InterpreterError::IndexNotInDict(expr, index) => write!(f, "Index of {} is not in {}", index, expr),
            InterpreterError::Unhashable(expr) => write!(f, "Expression {} is unhashable", expr)
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


impl Hash for ast::Expression {
    fn hash<H: Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state); // Hash the enum variant

        match self {
            ast::Expression::IntLiteral(i) => i.hash(state),
            ast::Expression::BoolLiteral(b) => b.hash(state),
            ast::Expression::Tuple { elements } => {
                for el in elements {
                    el.hash(state);
                }
            },
            ast::Expression::ListReference { elements_ref } => {
                for el in elements_ref.borrow().iter() {
                    el.hash(state);
                }
            },
            ast::Expression::DictionaryReference { index_ref } => {
                for (key, value) in index_ref.borrow().iter() {
                    key.hash(state);
                    value.hash(state);
                }
            },
            _ => unreachable!("Hashing unhashable expression!")
        }
    }
}



pub fn eval_expression(state: &mut InterpreterState, expression: &ast::Expression, statement: Rc<ast::Statement>, program: &ast::Program) -> Result<ast::Expression, InterpreterErrorMessage> {
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
        ast::Expression::Typecheck { expression, expected_type } => {
            let expression = eval_expression(state, expression, statement.clone(), program)?;

            return Ok(ast::Expression::BoolLiteral( match (expression, expected_type) {
                (ast::Expression::IntLiteral(_), ast::Type::Int) 
                | (ast::Expression::BoolLiteral(_), ast::Type::Bool)
                | (ast::Expression::Tuple { elements: _ }, ast::Type::Tuple)
                | (ast::Expression::ListReference { elements_ref: _ }, ast::Type::List) => true,
                (_, _) => false
            }));
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
                    let evaled = match (left.clone(), right) {
                        (ast::Expression::IntLiteral(left), ast::Expression::IntLiteral(right)) => Ok((left, right)),
                        (ast::Expression::IntLiteral(_), x) => Err(
                            InterpreterErrorMessage {
                                error: InterpreterError::InvalidType(x, "int".to_string(), "".to_string()),
                                statement: Some(statement)
                            }
                        ),
                        (x, _) => Err(
                            InterpreterErrorMessage {
                                error:InterpreterError::InvalidType(x, "int".to_string(), "".to_string()),
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
                                error: InterpreterError::InvalidType(x, "bool".to_string(), "".to_string()),
                                statement: Some(statement)
                            }
                        ),
                        (x, _) => Err(
                            InterpreterErrorMessage {
                                error: InterpreterError::InvalidType(x, "bool".to_string(), "".to_string()),
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
                },
                ast::BinOperator::ListConcat => {
                    let evaled = match (left, right) {
                        (ast::Expression::ListReference {elements_ref: left}, ast::Expression::ListReference{elements_ref: right}) => Ok((left, right)),
                        (ast::Expression::ListReference {elements_ref: _}, x) => Err(
                            InterpreterErrorMessage {
                                error: InterpreterError::InvalidType(x, "list".to_string(), "".to_string()),
                                statement: Some(statement)
                            }
                        ),
                        (x, _) => Err(
                            InterpreterErrorMessage {
                                error: InterpreterError::InvalidType(x, "list".to_string(), "".to_string()),
                                statement: Some(statement)
                            }
                        )
                    };

                    let (left, right) = evaled?;

                    let res = match operator {
                        ast::BinOperator::ListConcat => {
                            left.borrow().iter().chain(right.borrow().iter()).cloned().collect()
                        },
                        _ => unreachable!("This should never be reached")
                    };

                    return Ok(ast::Expression::ListReference {elements_ref: Rc::new(RefCell::new(res))});
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
                                error: InterpreterError::InvalidType(x, "int".to_string(), "".to_string()),
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
                                error: InterpreterError::InvalidType(x, "bool".to_string(), "".to_string()),
                                statement: Some(statement)
                            }
                        )
                    }?;

                    let res = match operator {
                        ast::UnOperator::Not => !evaled,
                        _ => unreachable!("This should never be reached")
                    };

                    return Ok(ast::Expression::BoolLiteral(res));
                },
                ast::UnOperator::Length => {
                    let evaled = match expression {
                        ast::Expression::ListReference { elements_ref } => Ok(elements_ref),
                        x => Err(
                            InterpreterErrorMessage {
                                error: InterpreterError::InvalidType(x, "list".to_string(), "".to_string()),
                                statement: Some(statement)
                            }
                        )
                    }?;

                    let res = match operator {
                        ast::UnOperator::Length => evaled.borrow().len() as i64,
                        _ => unreachable!("This should never be reached")
                    };

                    return Ok(ast::Expression::IntLiteral(res));
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
        },
        ast::Expression::Tuple { elements } => {
            let values: Result<Vec<ast::Expression>, InterpreterErrorMessage>
                = elements.iter().map(|arg| eval_expression(state, arg, statement.clone(), program)).collect();
            let values: Vec<ast::Expression> = values?;
            Ok(ast::Expression::Tuple { elements: values })
        },
        ast::Expression::ListReference { elements_ref } => {
            let values: Result<Vec<ast::Expression>, InterpreterErrorMessage>
                = elements_ref.borrow().iter().map(|arg| eval_expression(state, arg, statement.clone(), program)).collect();
            let values: Vec<ast::Expression> = values?;
            Ok(ast::Expression::ListReference { elements_ref: Rc::new(RefCell::new(values)) })
        },
        ast::Expression::DictionaryReference { index_ref: _ } => {
            Ok(expression.clone()) // dictionary references should always already be fully evaluated
        },
        ast::Expression::DictionaryInitialisation { elements } => {
            let mut map: HashMap<ast::Expression, ast::Expression> = HashMap::new();

            for (key, value) in elements {
                let key = eval_expression(state, key, statement.clone(), program)?;
                let value = eval_expression(state, value, statement.clone(), program)?;

                match key {
                    ast::Expression::IntLiteral(_)
                    | ast::Expression::BoolLiteral(_)
                    | ast::Expression::Tuple { elements: _ }
                    | ast::Expression::ListReference { elements_ref: _ }
                    | ast::Expression::DictionaryReference { index_ref: _ } => (),
                    _ => return Err(InterpreterErrorMessage {
                        error: InterpreterError::Unhashable(key),
                        statement: Some(statement.clone())
                    })
                }

                map.insert(key, value);
            }

            Ok(ast::Expression::DictionaryReference { index_ref: Rc::new(RefCell::new(map)) })
        },
        ast::Expression::Indexing { indexed, indexer } => {
            let original_indexed = eval_expression(state, indexed, statement.clone(), program)?;
            let indexer = eval_expression(state, indexer, statement.clone(), program)?;

            let indexed = match original_indexed {
                ast::Expression::Tuple { ref elements } => elements,
                ast::Expression::ListReference { ref elements_ref } => &elements_ref.borrow(),
                ast::Expression::DictionaryReference { ref index_ref } => {
                    match index_ref.borrow().get(&indexer) {
                        Some(x) => return Ok(x.clone()),
                        _ => return Err(InterpreterErrorMessage {
                            error: InterpreterError::IndexNotInDict(original_indexed.clone(), indexer),
                            statement: Some(statement.clone())
                        })
                    };
                },
                x => return Err(InterpreterErrorMessage {
                    error: InterpreterError::InvalidType(x, "indexable".to_string(), "Only tuples, lists and dictionaries can be indexed".to_string()),
                    statement: Some(statement)
                })
            };
            

            let mut indexer = match indexer {
                ast::Expression::IntLiteral(i) => i,
                x => return Err(InterpreterErrorMessage {
                    error: InterpreterError::InvalidType(x, "int".to_string(), "".to_string()),
                    statement: Some(statement)
                })
            };

            if indexer < 0 {
                indexer = (indexed.len() as i64) - 1 - indexer;
            }

            let indexer = indexer as usize;

            indexed.get(indexer)
                .map(|value| value.clone())
                .ok_or_else(|| InterpreterErrorMessage {
                    error: InterpreterError::IndexOutofBounds(original_indexed.clone(), indexer),
                    statement: Some(statement)
                })
        },
    }
}

pub fn interpret_statement(state: &mut InterpreterState, stmt: ast::Statement, program: &ast::Program) -> Result<Option<ast::Expression>, InterpreterErrorMessage> {
    let stmt_ref = Rc::new(stmt.clone());
    let eval = match stmt {
        ast::Statement::Assignment {target, expression} => {
            match target {
                ast::Expression::Variable(variable) => {
                    let v = eval_expression(state, &expression, stmt_ref, program)?;
                    state.values.entry(variable).and_modify(|value| *value = v.clone()).or_insert(v);
                    Ok(None)
                },
                ast::Expression::Indexing { indexed, indexer } => {
                    let indexer = eval_expression(state, &indexer, stmt_ref.clone(), program)?;

                   
                    let original_indexed =  match *indexed {
                        ast::Expression::Variable(_)
                        | ast::Expression::Indexing { .. } => {
                            eval_expression(state, &indexed, stmt_ref.clone(), program)?
                        }
                        x => return Err(InterpreterErrorMessage {
                            error: InterpreterError::InvalidType(x, "indexable".to_string(), "Only lists and dictionaries can be index assigned".to_string()),
                            statement: Some(stmt_ref)
                        })
                    };

                    match original_indexed {
                        ast::Expression::ListReference { elements_ref: ref indexed } => {
                            let mut indexer = match indexer {
                                ast::Expression::IntLiteral(i) => i,
                                x => return Err(InterpreterErrorMessage {
                                    error: InterpreterError::InvalidType(x, "int".to_string(), "".to_string()),
                                    statement: Some(stmt_ref)
                                })
                            };

                            if indexer < 0 {
                                indexer = (indexed.borrow().len() as i64) - 1 - indexer;
                            }

                            let value = eval_expression(state, &expression, stmt_ref.clone(), program)?;

                            let indexer = indexer as usize;

                            indexed
                                .borrow_mut()
                                .get_mut(indexer)
                                .ok_or_else(|| InterpreterErrorMessage {
                                    error: InterpreterError::IndexOutofBounds(original_indexed.clone(), indexer),
                                    statement: Some(stmt_ref)
                                })
                                .map(|element| *element = value)?;

                            Ok(None)
                        },
                        ast::Expression::DictionaryReference { index_ref: ref indexed } => {
                            let value = eval_expression(state, &expression, stmt_ref.clone(), program)?;
                            indexed.borrow_mut().insert(indexer, value);

                            Ok(None)
                        },
                        x => return Err(InterpreterErrorMessage {
                            error: InterpreterError::InvalidType(x, "indexable".to_string(), "Only lists and dictionaries can be index assigned".to_string()),
                            statement: Some(stmt_ref)
                        })
                    }
                },
                x => return Err(InterpreterErrorMessage {
                    error: InterpreterError::InvalidType(x, "indexable".to_string(), "Only lists and dictionaries can be index assigned".to_string()),
                    statement: Some(stmt_ref)
                })
            }
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
        },
        ast::Statement::ListAppend { target, value } => {
            let original_indexed =  match target {
                ast::Expression::Variable(_)
                | ast::Expression::Indexing { .. } => {
                    eval_expression(state, &target, stmt_ref.clone(), program)?
                }
                x => return Err(InterpreterErrorMessage {
                    error: InterpreterError::InvalidType(x, "indexable".to_string(), "Only lists and dictionaries can be index assigned".to_string()),
                    statement: Some(stmt_ref)
                })
            };

            let indexed = match original_indexed {
                ast::Expression::ListReference { ref elements_ref } => elements_ref,
                x => return Err(InterpreterErrorMessage {
                    error: InterpreterError::InvalidType(x, "indexable".to_string(), "Only lists can be empty-indexed".to_string()),
                    statement: Some(stmt_ref)
                })
            };

            let value = eval_expression(state, &value, stmt_ref.clone(), program)?;

            indexed
                .borrow_mut()
                .push(value);

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
        // println!("{}", state);
        // println!("{}", stmt);
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
        // println!("returning {} from function with {}", v, state);
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