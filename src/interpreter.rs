use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;
use std::hash::{Hash, Hasher};
use std::ops::Range;
use std::fmt;

use crate::ast;


#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Value {
    Int(i64),
    Bool(bool),
    String(String),
    Tuple {
        elements: Vec<Value>
    },
    ListReference {
        elements_ref: Rc<RefCell<Vec<Value>>>
    },
    DictionaryReference {
        index_ref: Rc<RefCell<HashMap<Value,Value>>>
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Int(i) => write!(f, "{}", i),
            Value::Bool(b) => write!(f, "{}", b),
            Value::String(s) => write!(f, "{}", s),
            Value::Tuple {elements} => {
                write!(f, "(")?;
                for (i, elt) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", elt)?;
                }
                write!(f, ")")
            },
            Value::ListReference { elements_ref } => {
                write!(f, "[")?;
                for (i, elt) in elements_ref.borrow().iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", elt)?;
                }
                write!(f, "]")
            },
            Value::DictionaryReference { index_ref } => {
                write!(f, "{{")?;
                for (i, (key, value)) in index_ref.borrow().iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", key, value)?;
                }
                write!(f, "}}")
            }
        }
    }
}

type FunctionFrame = HashMap<String, Value>;
pub struct InterpreterState {
    stack: Vec<FunctionFrame>,
    values: FunctionFrame,
    return_value: Option<Value>
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
    InvalidType(Value, String, String),
    InvalidLHS(ast::Expression, String),
    VariableNotFound(String),
    IndexOutofBounds(Value, usize),
    IndexNotInDict(Value, Value),
    Unhashable(Value)
}

impl fmt::Display for InterpreterError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            InterpreterError::Panic(err) => write!(f, "Interpreter Panic: {}", err),
            InterpreterError::InvalidType(expr, expected_type, comment) => write!(f, "Using {} operation on {} {}", expected_type, expr, comment),
            InterpreterError::InvalidLHS(expr, comment) => write!(f, "Assigning to {}: {}", expr, comment),
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
    pub range: Option<Range<usize>>
}



impl fmt::Display for InterpreterErrorMessage {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.error)
    }
}

impl std::error::Error for InterpreterErrorMessage {}


impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state); // Hash the enum variant

        match self {
            Value::Int(i) => i.hash(state),
            Value::Bool(b) => b.hash(state),
            Value::String(s) => s.hash(state),
            Value::Tuple { elements } => {
                for el in elements {
                    el.hash(state);
                }
            },
            _ => panic!("Unhashable")
        }
    }
}



pub fn eval_expression(state: &mut InterpreterState, expression: &ast::LocExpression, program: &ast::Program) -> Result<Value, InterpreterErrorMessage> {
    match &expression.expression {
        ast::Expression::IntLiteral(i) => Ok(Value::Int(*i)),
        ast::Expression::BoolLiteral(b) => Ok(Value::Bool(*b)),
        ast::Expression::StringLiteral(s) => Ok(Value::String(s.clone())),
        ast::Expression::Variable(var) => {
            match (*state).values.get(var) {
                Some(v) => Ok(v.clone()),
                _ => return Err(InterpreterErrorMessage {
                    error: InterpreterError::VariableNotFound(var.clone()),
                    range: Some(expression.loc.clone())
                })
            }
        },
        ast::Expression::Typecheck { expression, expected_type } => {
            let value = eval_expression(state, &expression, program)?;

            return Ok(Value::Bool( match (value, expected_type) {
                (Value::Int(_), ast::Type::Int) 
                | (Value::Bool(_), ast::Type::Bool)
                | (Value::String(_), ast::Type::String)
                | (Value::Tuple { elements: _ }, ast::Type::Tuple)
                | (Value::ListReference { elements_ref: _ }, ast::Type::List) 
                | (Value::DictionaryReference { index_ref: _ }, ast::Type::Dict)=> true,
                (_, _) => false
            }));
        },
        ast::Expression::BinaryOperation { operator, left, right } => {
            let left_value = eval_expression(state, &left, program)?;
            let right_value = eval_expression(state, &right, program)?;
            match operator {
                ast::BinOperator::Add  => {
                    match (left_value.clone(), right_value) {
                        (Value::Int(left), Value::Int(right)) => {
                            Ok(Value::Int(left + right))
                        },
                        (Value::String(left), Value::String(right)) => {
                            Ok(Value::String(format!("{}{}", left, right)))
                        },
                        (Value::Int(_), x) => Err(
                            InterpreterErrorMessage {
                                error: InterpreterError::InvalidType(x, "int".to_string(), "".to_string()),
                                range: Some(right.loc.clone())
                            }
                        ),
                        (x, _) => Err(
                            InterpreterErrorMessage {
                                error:InterpreterError::InvalidType(x, "int".to_string(), "".to_string()),
                                range: Some(left.loc.clone())
                            }
                        )
                    }
                }
                ast::BinOperator::Sub 
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
                    let evaled = match (left_value.clone(), right_value) {
                        (Value::Int(left), Value::Int(right)) => Ok((left, right)),
                        (Value::Int(_), x) => Err(
                            InterpreterErrorMessage {
                                error: InterpreterError::InvalidType(x, "int".to_string(), "".to_string()),
                                range: Some(right.loc.clone())
                            }
                        ),
                        (x, _) => Err(
                            InterpreterErrorMessage {
                                error:InterpreterError::InvalidType(x, "int".to_string(), "".to_string()),
                                range: Some(left.loc.clone())
                            }
                        )
                    };

                    let (left, right) = evaled?;

                    Ok(match operator {
                        ast::BinOperator::Add => Value::Int(left + right),
                        ast::BinOperator::Sub => Value::Int(left - right),
                        ast::BinOperator::Mul => Value::Int(left * right),
                        ast::BinOperator::Div => Value::Int(left / right),
                        ast::BinOperator::Mod => Value::Int(left % right),

                        ast::BinOperator::Eq => Value::Bool(left == right),
                        ast::BinOperator::Neq => Value::Bool(left != right),
                        ast::BinOperator::Lt => Value::Bool(left < right),
                        ast::BinOperator::Gt => Value::Bool(left > right),
                        ast::BinOperator::Leq => Value::Bool(left <= right),
                        ast::BinOperator::Geq => Value::Bool(left >= right),

                        _ => unreachable!("This should never be reached")
                    })
                },
                ast::BinOperator::And
                | ast::BinOperator::Or => {
                    let evaled = match (left_value, right_value) {
                        (Value::Bool(left), Value::Bool(right)) => Ok((left, right)),
                        (Value::Bool(_), x) => Err(
                            InterpreterErrorMessage {
                                error: InterpreterError::InvalidType(x, "bool".to_string(), "".to_string()),
                                range: Some(right.loc.clone())
                            }
                        ),
                        (x, _) => Err(
                            InterpreterErrorMessage {
                                error: InterpreterError::InvalidType(x, "bool".to_string(), "".to_string()),
                                range: Some(left.loc.clone())
                            }
                        )
                    };

                    let (left, right) = evaled?;

                    let res = match operator {
                        ast::BinOperator::And => left && right,
                        ast::BinOperator::Or => left || right,
                        _ => unreachable!("This should never be reached")
                    };

                    return Ok(Value::Bool(res));
                },
                ast::BinOperator::ListConcat => {
                    let evaled = match (left_value, right_value) {
                        (Value::ListReference {elements_ref: left}, Value::ListReference{elements_ref: right}) => Ok((left, right)),
                        (Value::ListReference {elements_ref: _}, x) => Err(
                            InterpreterErrorMessage {
                                error: InterpreterError::InvalidType(x, "list".to_string(), "".to_string()),
                                range: Some(right.loc.clone())
                            }
                        ),
                        (x, _) => Err(
                            InterpreterErrorMessage {
                                error: InterpreterError::InvalidType(x, "list".to_string(), "".to_string()),
                                range: Some(left.loc.clone())
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

                    return Ok(Value::ListReference {elements_ref: Rc::new(RefCell::new(res))});
                }            
            }
        },
        ast::Expression::UnaryOperation { operator, expression } => {
            let value = eval_expression(state, &expression, program)?;

            match operator {
                ast::UnOperator::Neg => {
                    let evaled = match value {
                        Value::Int(v) => Ok(v),
                        x => Err(
                            InterpreterErrorMessage {
                                error: InterpreterError::InvalidType(x, "int".to_string(), "".to_string()),
                                range: Some(expression.loc.clone())
                            }
                        )
                    }?;

                    let res = match operator {
                        ast::UnOperator::Neg => -evaled,
                        _ => unreachable!("This should never be reached")
                    };

                    return Ok(Value::Int(res));
                },
                ast::UnOperator::Not => {
                    let evaled = match value {
                        Value::Bool(v) => Ok(v),
                        x => Err(
                            InterpreterErrorMessage {
                                error: InterpreterError::InvalidType(x, "bool".to_string(), "".to_string()),
                                range: Some(expression.loc.clone())
                            }
                        )
                    }?;

                    let res = match operator {
                        ast::UnOperator::Not => !evaled,
                        _ => unreachable!("This should never be reached")
                    };

                    return Ok(Value::Bool(res));
                },
                ast::UnOperator::Length => {
                    let evaled = match value {
                        Value::ListReference { elements_ref } => Ok(elements_ref),
                        x => Err(
                            InterpreterErrorMessage {
                                error: InterpreterError::InvalidType(x, "list".to_string(), "".to_string()),
                                range: Some(expression.loc.clone())
                            }
                        )
                    }?;

                    let res = match operator {
                        ast::UnOperator::Length => evaled.borrow().len() as i64,
                        _ => unreachable!("This should never be reached")
                    };

                    return Ok(Value::Int(res));
                }
            }
        },
        ast::Expression::FunctionCall { function_name, arguments } => {
            let function = match program.functions.get(function_name) {
                Some(f) => f,
                _ => return Err(InterpreterErrorMessage {
                    error: InterpreterError::VariableNotFound(function_name.clone()),
                    range: Some(expression.loc.clone())
                })
            };

            let argument_values: Result<Vec<Value>, InterpreterErrorMessage>
                = arguments.iter().map(|arg| eval_expression(state, arg, program)).collect();
            let argument_values: Vec<Value> = argument_values?;

            state.stack.push(state.values.clone());

            let new_values: HashMap<String, Value> = function.positional_arguments.clone().into_iter().zip(argument_values.into_iter()).map(|(arg, value)| (arg.name, value)).collect();
            state.values = new_values;
            state.return_value = None;

            interpret_function_body(state, function, &function_name, program)
        },
        ast::Expression::TupleDefinition { elements } => {
            let values: Result<Vec<Value>, InterpreterErrorMessage>
                = elements.iter().map(|arg| eval_expression(state, arg, program)).collect();
            let values: Vec<Value> = values?;
            Ok(Value::Tuple { elements: values })
        },
        ast::Expression::ListDefinition { elements } => {
            let values: Result<Vec<Value>, InterpreterErrorMessage>
                = elements.iter().map(|arg| eval_expression(state, arg, program)).collect();
            let values: Vec<Value> = values?;
            Ok(Value::ListReference { elements_ref: Rc::new(RefCell::new(values)) })
        },
        ast::Expression::DictionaryDefinition { elements } => {
            let mut map: HashMap<Value, Value> = HashMap::new();

            for (key, value) in elements {
                let key = eval_expression(state, &key, program)?;
                let value = eval_expression(state, &value, program)?;

                map.insert(key, value);
            }

            Ok(Value::DictionaryReference { index_ref: Rc::new(RefCell::new(map)) })
        },
        ast::Expression::Indexing { indexed, indexer } => {
            let original_indexed_value = eval_expression(state, &indexed, program)?;
            let original_indexer_value = eval_expression(state, &indexer, program)?;

            if let Value::DictionaryReference { ref index_ref } = original_indexed_value {
                match index_ref.borrow().get(&original_indexer_value) {
                    Some(x) => return Ok(x.clone()),
                    _ => return Err(InterpreterErrorMessage {
                        error: InterpreterError::IndexNotInDict(original_indexed_value.clone(), original_indexer_value),
                        range: Some(expression.loc.clone())
                    })
                };
            }

            let indexed_length = match &original_indexed_value {
                Value::String(s) => s.len(),
                Value::Tuple { ref elements } => elements.len(),
                Value::ListReference { ref elements_ref } => elements_ref.borrow().len(),
                x => return Err(InterpreterErrorMessage {
                    error: InterpreterError::InvalidType(x.clone(), "indexable".to_string(), "Only strings, tuples, lists and dictionaries can be indexed".to_string()),
                    range: Some(indexed.loc.clone())
                })
            };
            
            let mut indexer_value = match original_indexer_value {
                Value::Int(i) => i,
                x => return Err(InterpreterErrorMessage {
                    error: InterpreterError::InvalidType(x, "int".to_string(), "".to_string()),
                    range: Some(indexer.loc.clone())
                })
            };

            if indexer_value < 0 {
                indexer_value = (indexed_length as i64) - 2 - indexer_value;
            }

            let indexer_value = indexer_value as usize;

            let indexed_value = match &original_indexed_value {
                Value::String(s) => {
                    match s.chars().nth(indexer_value) {
                        Some(c) => return Ok(Value::String(c.to_string())),
                        _ => return Err(InterpreterErrorMessage {
                            error: InterpreterError::IndexOutofBounds(original_indexed_value.clone(), indexer_value),
                            range: Some(indexed.loc.clone())
                        })
                    }
                },
                Value::Tuple { ref elements } => elements,
                Value::ListReference { ref elements_ref } => &elements_ref.borrow(),
                x => return Err(InterpreterErrorMessage {
                    error: InterpreterError::InvalidType(x.clone(), "indexable".to_string(), "Only strings, tuples, lists and dictionaries can be indexed".to_string()),
                    range: Some(indexed.loc.clone())
                })
            };

            indexed_value.get(indexer_value)
                .map(|value| value.clone())
                .ok_or_else(|| InterpreterErrorMessage {
                    error: InterpreterError::IndexOutofBounds(original_indexed_value.clone(), indexer_value),
                    range: Some(indexer.loc.clone())
                })
        },
    }
}

pub fn interpret_statement(state: &mut InterpreterState, stmt: ast::LocStatement, program: &ast::Program) -> Result<Option<Value>, InterpreterErrorMessage> {
    let eval = match stmt.statement {
        ast::Statement::Assignment {target, expression} => {
            match target.expression {
                ast::Expression::Variable(variable) => {
                    let v = eval_expression(state, &expression, program)?;
                    state.values.entry(variable).and_modify(|value| *value = v.clone()).or_insert(v);
                    Ok(None)
                },
                ast::Expression::Indexing { indexed, indexer } => {
                    let indexer_value = eval_expression(state, &indexer, program)?;

                   
                    let original_indexed_value =  match indexed.expression {
                        ast::Expression::Variable(_)
                        | ast::Expression::Indexing { .. } => {
                            eval_expression(state, &indexed, program)?
                        }
                        x => return Err(InterpreterErrorMessage {
                            error: InterpreterError::InvalidLHS(x, "Can only assign to variables or indexing".to_string()),
                            range: Some(indexed.loc)
                        })
                    };

                    match original_indexed_value {
                        Value::ListReference { elements_ref: ref indexed_value } => {
                            let mut indexer_value = match indexer_value {
                                Value::Int(i) => i,
                                x => return Err(InterpreterErrorMessage {
                                    error: InterpreterError::InvalidType(x, "int".to_string(), "".to_string()),
                                    range: Some(indexer.loc)
                                })
                            };

                            if indexer_value < 0 {
                                indexer_value = (indexed_value.borrow().len() as i64) - 2 - indexer_value;
                            }

                            let value = eval_expression(state, &expression, program)?;

                            let indexer = indexer_value as usize;

                            indexed_value
                                .borrow_mut()
                                .get_mut(indexer)
                                .ok_or_else(|| InterpreterErrorMessage {
                                    error: InterpreterError::IndexOutofBounds(original_indexed_value.clone(), indexer),
                                    range: Some(stmt.loc)
                                })
                                .map(|element| *element = value)?;

                            Ok(None)
                        },
                        Value::DictionaryReference { index_ref: ref indexed } => {
                            let value = eval_expression(state, &expression, program)?;

                            match indexer_value {
                                Value::Int(_)
                                | Value::Bool(_)
                                | Value::Tuple { .. } => (),
                                x => return Err(InterpreterErrorMessage {
                                    error: InterpreterError::Unhashable(x),
                                    range: Some(indexer.loc)
                                })
                            };

                            indexed.borrow_mut().insert(indexer_value, value);

                            Ok(None)
                        },
                        x => return Err(InterpreterErrorMessage {
                            error: InterpreterError::InvalidType(x, "indexable".to_string(), "Only lists and dictionaries can be index assigned".to_string()),
                            range: Some(indexed.loc)
                        })
                    }
                },
                x => return Err(InterpreterErrorMessage {
                    error: InterpreterError::InvalidLHS(x, "Can only assign to variables and indexing".to_string()),
                    range: Some(target.loc)
                })
            }
        },
        ast::Statement::Return { expression } => {
            match eval_expression(state, &expression, program) {
                Ok(v) => {
                    state.return_value = Some(v.clone());
                    match state.stack.pop() {
                        Some(s) => {state.values = s},
                        _ => {
                            return Err(InterpreterErrorMessage {
                                error: InterpreterError::Panic("Returning with empty stack frame".to_string()),
                                range: Some(stmt.loc)
                            })
                        }
                    }
                    Ok(Some(v))
                }
                Err(err) => Err(err)
            }
        },
        ast::Statement::IfElse { condition, if_body, else_body } => {
            let eval_condition = eval_expression(state, &condition, program)?;

            match eval_condition {
                Value::Bool(b) => {
                    match b {
                        true => interpret_body(state, &if_body, program),
                        false => interpret_body(state, &else_body, program),
                    }?;
                    Ok(None)
                }
                x => {
                    return Err(InterpreterErrorMessage {
                        error: InterpreterError::Panic(format!("Using non-bool value in loop: {}", x)),
                        range: Some(condition.loc)
                    });
                }
            }
        },
        ast::Statement::While { ref condition, ref body } => {
            let eval_condition = eval_expression(state, &condition, program)?;

            let mut cond = match eval_condition {
                Value::Bool(b) => b,
                x => {
                    return Err(InterpreterErrorMessage {
                        error: InterpreterError::Panic(format!("Using non-bool value in loop: {}", x)),
                        range: Some(condition.loc.clone())
                    });
                }
            };

            while cond  {
                interpret_body(state, &body, program)?;

                let eval_condition = eval_expression(state, &condition, program)?;
                cond = match eval_condition {
                    Value::Bool(b) => b,
                    x => {
                        return Err(InterpreterErrorMessage {
                            error: InterpreterError::Panic(format!("Using non-bool value in loop: {}", x)),
                            range: Some(condition.loc.clone())
                        });
                    }
                };
            }


            Ok(None)
        },
        ast::Statement::ListAppend { target, value: appended } => {
            let original_indexed =  match target.expression {
                ast::Expression::Variable(_)
                | ast::Expression::Indexing { .. } => {
                    eval_expression(state, &target, program)?
                }
                x => return Err(InterpreterErrorMessage {
                    error: InterpreterError::InvalidLHS(x, "Can only append to variables and indexing".to_string()),
                    range: Some(target.loc)
                })
            };

            let indexed = match original_indexed {
                Value::ListReference { ref elements_ref } => elements_ref,
                x => return Err(InterpreterErrorMessage {
                    error: InterpreterError::InvalidType(x, "indexable".to_string(), "Only lists can be empty-indexed".to_string()),
                    range: Some(appended.loc)
                })
            };

            let value = eval_expression(state, &appended, program)?;

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

pub fn interpret_body(state: &mut InterpreterState, body: &ast::Body, program: &ast::Program) -> Result<Option<Value>, InterpreterErrorMessage> {
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

pub fn interpret_function_body(state: &mut InterpreterState, function: &ast::Function, function_name: &String, program: &ast::Program) -> Result<Value, InterpreterErrorMessage>{
    interpret_body(state, &function.body, program)?;
    
    if let Some(v) = &state.return_value {
        // println!("returning {} from function with {}", v, state);
        return Ok(v.clone());
    }

    return Err(
        InterpreterErrorMessage {
            error: InterpreterError::Panic(format!("Function {} did not return", function_name)),
            range: None
        }
    )
}

pub fn interpret(program: &ast::Program) -> Result<Value, InterpreterErrorMessage> {

    println!("{}", program);

    let mut state = InterpreterState {
        stack: vec![HashMap::new()],
        values: HashMap::new(),
        return_value: None
    };

    let main = program.functions.get(&"main".to_string()).unwrap();

    interpret_function_body(&mut state, &main, &"main".to_string(), program)
}