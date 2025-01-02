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
    values: FunctionFrame
}

impl fmt::Display for InterpreterState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Variables {:?}", self.values)
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
    MissingArgument(String),
    UnexpectedArgument(ast::LocExpression),
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
            InterpreterError::MissingArgument(s) => write!(f, "Missing argument {}", s),
            InterpreterError::UnexpectedArgument(e) => write!(f, "Unexpected argument {}", e),
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



pub fn eval_expression(state: &InterpreterState, expression: &ast::LocExpression, program: &ast::Program) -> Result<Value, InterpreterErrorMessage> {

    match expression.expression {
        ast::Expression::IntLiteral(ref i) => Ok(Value::Int(i.clone())),
        ast::Expression::BoolLiteral(ref b) => Ok(Value::Bool(b.clone())),
        ast::Expression::StringLiteral(ref s) => Ok(Value::String(s.clone())),
        ast::Expression::Variable(ref var) => {
            match state.values.get(var) {
                Some(v) => Ok(v.clone()),
                _ => return Err(InterpreterErrorMessage {
                    error: InterpreterError::VariableNotFound(var.clone()),
                    range: Some(expression.loc.clone())
                })
            }
        },
        ast::Expression::Typecheck { ref expression, ref expected_type } => {
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
        ast::Expression::BinaryOperation { ref operator, ref left, ref right } => {
            let left_value = eval_expression(state, &left, program)?;
            let right_value = eval_expression(state, &right, program)?;
            match operator {
                ast::BinOperator::Add  => {
                    match (left_value, right_value) {
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
                    let evaled = match (left_value, right_value) {
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
        ast::Expression::UnaryOperation { ref operator, ref expression } => {
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
        ast::Expression::FunctionCall { 
            ref function_name,
            ref positional_arguments,
            ref variadic_argument,
            ref keyword_arguments,
            ref keyword_variadic_argument 
        } => {
            let function = match program.functions.get(function_name) {
                Some(f) => f,
                _ => return Err(InterpreterErrorMessage {
                    error: InterpreterError::VariableNotFound(function_name.clone()),
                    range: Some(expression.loc.clone())
                })
            };

            let argument_values: Result<Vec<Value>, InterpreterErrorMessage>
                = positional_arguments.iter().map(|arg| eval_expression(state, &arg.expression, program)).collect();

            let mut argument_values: Vec<Value> = argument_values?;

            match &variadic_argument {
                Some(arg) => {
                    let value = eval_expression(state, &arg.expression, program)?;
                    let extra_args: Vec<Value> = match value {
                        Value::Tuple { elements } => elements,
                        Value::ListReference { elements_ref } => elements_ref.borrow().clone(),
                        x => return Err(InterpreterErrorMessage {
                            error: InterpreterError::InvalidType(x, "iterable".to_string(), "Only tuples or list can be passed as variadic arguments".to_string()),
                            range: Some(arg.loc.clone())
                        })
                    };
                    argument_values.extend(extra_args);
                },
                _ => ()
            }

            let keyword_values: Result<HashMap<String, Value>, InterpreterErrorMessage> = keyword_arguments.iter()
                .map(|arg| {
                    eval_expression(state, &arg.expression, program).map(|value| (arg.name.clone(), value))
                })
                .collect();

            let mut keyword_values: HashMap<String, Value> = keyword_values?;

            match keyword_variadic_argument {
                Some(arg) => {
                    let value = eval_expression(state, &arg.expression, program)?;
                    match value {
                        Value::DictionaryReference { ref index_ref } => {
                            for (key, value) in index_ref.borrow().iter() {
                                match key {
                                    Value::String(s) => {
                                        if !keyword_values.contains_key(&s.clone()) {
                                            keyword_values.insert(s.clone(), value.clone());
                                        }
                                    },
                                    x => return Err(InterpreterErrorMessage {
                                        error: InterpreterError::InvalidType(x.clone(), "string".to_string(), "A dict passed as keyword variadic needs to only contain string as keys".to_string()),
                                        range: Some(arg.loc.clone())
                                    })                               
                                }
                            }
                        },
                        x => return Err(InterpreterErrorMessage {
                            error: InterpreterError::InvalidType(x, "dict".to_string(), "Only dict can be passed as keyword variadic arguments".to_string()),
                            range: Some(arg.loc.clone())
                        })
                    };
                },
                _ => ()
            }

            if argument_values.len() < function.positional_arguments.len() {
                let pos = argument_values.len();
                let missing_arg = function.positional_arguments.get(pos).unwrap();
                return Err(InterpreterErrorMessage {
                    error: InterpreterError::MissingArgument(missing_arg.name.clone()),
                    range: Some(missing_arg.loc.clone())
                })
            }

            if function.variadic_argument.is_none() && argument_values.len() > function.positional_arguments.len() {
                let pos = function.positional_arguments.len();
                
                let extra_arg_expression = match pos < positional_arguments.len() {
                    true => positional_arguments.get(pos).unwrap(),
                    false => &variadic_argument.clone().unwrap()
                };

                return Err(InterpreterErrorMessage {
                    error: InterpreterError::UnexpectedArgument(*extra_arg_expression.expression.clone()),
                    range: Some(extra_arg_expression.loc.clone())
                })
            }

            let mut new_values: HashMap<String, Value> = function.positional_arguments.iter().zip(argument_values.iter()).map(|(arg, value)| (arg.name.clone(), value.clone())).collect();

            if argument_values.len() > function.positional_arguments.len() {
                // from previous logic the only way this happens if we have a variadic accepting argument in the function
                let extra_args = &argument_values[function.positional_arguments.len()..];
                new_values.insert(function.variadic_argument.clone().unwrap().name, Value::ListReference { elements_ref: Rc::new(RefCell::new(extra_args.to_vec()))});
            }
            

            let new_state = InterpreterState {
                values: new_values
            };

            let ret = interpret_function_body(new_state, function, &function_name, program);

            Ok(ret?)
        },
        ast::Expression::TupleDefinition { ref elements } => {
            let values: Result<Vec<Value>, InterpreterErrorMessage>
                = elements.into_iter().map(|arg| eval_expression(state, &arg, program)).collect();
            let values: Vec<Value> = values?;
            Ok(Value::Tuple { elements: values })
        },
        ast::Expression::ListDefinition { ref elements } => {
            let values: Result<Vec<Value>, InterpreterErrorMessage>
                = elements.into_iter().map(|arg| eval_expression(state, &arg, program)).collect();
            let values: Vec<Value> = values?;
            Ok(Value::ListReference { elements_ref: Rc::new(RefCell::new(values)) })
        },
        ast::Expression::DictionaryDefinition { ref elements } => {
            let mut map: HashMap<Value, Value> = HashMap::new();

            for (key, value) in elements {
                let key = eval_expression(state, &key, program)?;
                let value = eval_expression(state, &value, program)?;

                map.insert(key, value);
            }

            Ok(Value::DictionaryReference { index_ref: Rc::new(RefCell::new(map)) })
        },
        ast::Expression::Indexing { ref indexed, ref indexer } => {
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

            let original_index_clone = original_indexed_value.clone();
            let indexed_value = match original_index_clone {
                Value::String(ref s) => {
                    match s.chars().nth(indexer_value) {
                        Some(c) => return Ok(Value::String(c.to_string())),
                        _ => return Err(InterpreterErrorMessage {
                            error: InterpreterError::IndexOutofBounds(original_indexed_value, indexer_value),
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
                    error: InterpreterError::IndexOutofBounds(original_indexed_value, indexer_value),
                    range: Some(indexer.loc.clone())
                })
        },
    }
}

pub fn interpret_statement(state: &mut InterpreterState, stmt: &ast::LocStatement, program: &ast::Program) -> Result<Option<Value>, InterpreterErrorMessage> {
    let eval = match &stmt.statement {
        ast::Statement::Assignment {target, expression} => {
            match &target.expression {
                ast::Expression::Variable(variable) => {
                    let v = eval_expression(&state, &expression, program)?;
                    match state.values.entry(variable.clone()) {
                        std::collections::hash_map::Entry::Occupied(mut o) => {
                            *o.get_mut() = v
                        }
                        std::collections::hash_map::Entry::Vacant(entry) => {
                            entry.insert(v);
                        }
                    }
                    Ok(None)
                },
                ast::Expression::Indexing { indexed, indexer } => {
                    let indexer_value = eval_expression(&state, &indexer, program)?;

                   
                    let original_indexed_value =  match &indexed.expression {
                        ast::Expression::Variable(_)
                        | ast::Expression::Indexing { .. } => {
                            eval_expression(&state, &indexed, program)?
                        }
                        x => return Err(InterpreterErrorMessage {
                            error: InterpreterError::InvalidLHS(x.clone(), "Can only assign to variables or indexing".to_string()),
                            range: Some(indexed.loc.clone())
                        })
                    };
                    
                    match original_indexed_value.clone() {
                        Value::ListReference { elements_ref: ref indexed_value } => {
                            let mut indexer_value = match indexer_value {
                                Value::Int(i) => i,
                                x => return Err(InterpreterErrorMessage {
                                    error: InterpreterError::InvalidType(x, "int".to_string(), "".to_string()),
                                    range: Some(indexer.loc.clone())
                                })
                            };

                            if indexer_value < 0 {
                                indexer_value = (indexed_value.borrow().len() as i64) - 2 - indexer_value;
                            }

                            let value = eval_expression(&state, &expression, program)?;

                            let indexer = indexer_value as usize;

                            indexed_value
                                .borrow_mut()
                                .get_mut(indexer)
                                .ok_or_else(|| InterpreterErrorMessage {
                                    error: InterpreterError::IndexOutofBounds(original_indexed_value, indexer),
                                    range: Some(stmt.loc.clone())
                                })
                                .map(|element| *element = value)?;

                            Ok(None)
                        },
                        Value::DictionaryReference { index_ref: ref indexed } => {
                            let value = eval_expression(&state, &expression, program)?;

                            match indexer_value {
                                Value::Int(_)
                                | Value::Bool(_)
                                | Value::Tuple { .. } => (),
                                x => return Err(InterpreterErrorMessage {
                                    error: InterpreterError::Unhashable(x),
                                    range: Some(indexer.loc.clone())
                                })
                            };

                            indexed.borrow_mut().insert(indexer_value, value);

                            Ok(None)
                        },
                        x => return Err(InterpreterErrorMessage {
                            error: InterpreterError::InvalidType(x, "indexable".to_string(), "Only lists and dictionaries can be index assigned".to_string()),
                            range: Some(indexed.loc.clone())
                        })
                    }
                },
                x => return Err(InterpreterErrorMessage {
                    error: InterpreterError::InvalidLHS(x.clone(), "Can only assign to variables and indexing".to_string()),
                    range: Some(target.loc.clone())
                })
            }
        },
        ast::Statement::Return { expression } => {
            let return_eval = eval_expression(&state, &expression, program);
            match return_eval {
                Ok(v) => {
                    Ok(Some(v.clone()))
                }
                Err(err) => Err(err)
            }
        },
        ast::Statement::IfElse { condition, if_body, else_body } => {
            let eval_condition = eval_expression(&state, &condition, program)?;

            let ret = match eval_condition {
                Value::Bool(b) => {
                    match b {
                        true => interpret_body(state, &if_body, program),
                        false => interpret_body(state, &else_body, program),
                    }
                }
                x => {
                    return Err(InterpreterErrorMessage {
                        error: InterpreterError::Panic(format!("Using non-bool value in loop: {}", x)),
                        range: Some(condition.loc.clone())
                    });
                }
            };

            ret
        },
        ast::Statement::While { ref condition, ref body } => {
            let eval_condition = eval_expression(&state, condition, program)?;

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
                let ret = interpret_body(state, &body, program)?;
                if let Some(v) = ret {
                    return Ok(Some(v));
                }

                let eval_condition = eval_expression(&state, condition, program)?;
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
            let original_indexed =  match &target.expression {
                ast::Expression::Variable(_)
                | ast::Expression::Indexing { .. } => {
                    eval_expression(&state, &target, program)?
                }
                x => return Err(InterpreterErrorMessage {
                    error: InterpreterError::InvalidLHS(x.clone(), "Can only append to variables and indexing".to_string()),
                    range: Some(target.loc.clone())
                })
            };

            let indexed = match original_indexed {
                Value::ListReference { ref elements_ref } => elements_ref,
                x => return Err(InterpreterErrorMessage {
                    error: InterpreterError::InvalidType(x, "indexable".to_string(), "Only lists can be empty-indexed".to_string()),
                    range: Some(appended.loc.clone())
                })
            };

            let value = eval_expression(&state, &appended, program)?;

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
    for stmt in body.statements.iter() {
        let ret = interpret_statement(state, stmt, program);
        match ret {
            Err(e) => return Err(e),
            Ok(Some(v)) => {
                return Ok(Some(v))
            },
            Ok(_) => {}
        };
    }
    return Ok(None);
}

pub fn interpret_function_body(state: InterpreterState, function: &ast::Function, function_name: &String, program: &ast::Program) -> Result<Value, InterpreterErrorMessage>{
    
    let mut state = state;
    let ret = interpret_body(&mut state, &function.body, program)?;
    if let Some(v) = ret {
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

    let state = InterpreterState {
        values: HashMap::new(),
    };

    let main = program.functions.get(&"main".to_string()).unwrap();

    interpret_function_body(state, &main, &"main".to_string(), program)
}