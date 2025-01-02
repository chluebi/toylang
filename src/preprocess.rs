use std::ops::Range;
use std::fmt;

use crate::parse_ast;
use crate::ast;

#[derive(Debug, Clone)]
pub enum PreprocessingError {
    FunctionProcessingError(String)
}

impl fmt::Display for PreprocessingError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::FunctionProcessingError(s) => write!(f, "Error Processing Function: {}", s)
        }
    }
}

#[derive(Debug, Clone)]
pub struct PreprocessingErrorMessage {
    pub error: PreprocessingError,
    pub loc: Option<Range<usize>>
}

impl fmt::Display for PreprocessingErrorMessage {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.error)
    }
}




fn process_type(t: parse_ast::Type) -> Result<ast::Type, PreprocessingErrorMessage> {
    match t {
        parse_ast::Type::Int => Ok(ast::Type::Int),
        parse_ast::Type::Bool => Ok(ast::Type::Bool),
        parse_ast::Type::String => Ok(ast::Type::String),
        parse_ast::Type::Tuple => Ok(ast::Type::Tuple),
        parse_ast::Type::List => Ok(ast::Type::List),
        parse_ast::Type::Dict => Ok(ast::Type::Dict),
    }
}

fn process_bin_operator(op: parse_ast::BinOperator) -> Result<ast::BinOperator, PreprocessingErrorMessage> {
    match op {
        parse_ast::BinOperator::Add => Ok(ast::BinOperator::Add),
        parse_ast::BinOperator::Sub => Ok(ast::BinOperator::Sub),
        parse_ast::BinOperator::Mul => Ok(ast::BinOperator::Mul),
        parse_ast::BinOperator::Div => Ok(ast::BinOperator::Div),
        parse_ast::BinOperator::Mod => Ok(ast::BinOperator::Mod),
        parse_ast::BinOperator::And => Ok(ast::BinOperator::And),
        parse_ast::BinOperator::Or => Ok(ast::BinOperator::Or),
        parse_ast::BinOperator::Eq => Ok(ast::BinOperator::Eq),
        parse_ast::BinOperator::Neq => Ok(ast::BinOperator::Neq),
        parse_ast::BinOperator::Lt => Ok(ast::BinOperator::Lt),
        parse_ast::BinOperator::Gt => Ok(ast::BinOperator::Gt),
        parse_ast::BinOperator::Leq => Ok(ast::BinOperator::Leq),
        parse_ast::BinOperator::Geq => Ok(ast::BinOperator::Geq),
        parse_ast::BinOperator::ListConcat => Ok(ast::BinOperator::ListConcat),
    }
}

fn process_un_operator(op: parse_ast::UnOperator) -> Result<ast::UnOperator, PreprocessingErrorMessage> {
    match op {
        parse_ast::UnOperator::Neg => Ok(ast::UnOperator::Neg),
        parse_ast::UnOperator::Not => Ok(ast::UnOperator::Not),
        parse_ast::UnOperator::Length => Ok(ast::UnOperator::Length),
    }
}

fn process_expression(expr: parse_ast::Expression) -> Result<ast::Expression, PreprocessingErrorMessage> {
    match expr {
        parse_ast::Expression::IntLiteral(i) => Ok(ast::Expression::IntLiteral(i)),
        parse_ast::Expression::BoolLiteral(b) => Ok(ast::Expression::BoolLiteral(b)),
        parse_ast::Expression::StringLiteral(s) => Ok(ast::Expression::StringLiteral(s)),
        parse_ast::Expression::Variable(var) => Ok(ast::Expression::Variable(var)),
        parse_ast::Expression::Typecheck { expression, expected_type } => {
            Ok(ast::Expression::Typecheck {
                expression: Box::new(process_loc_expression(*expression)?),
                expected_type: process_type(expected_type)?,
            })
        }
        parse_ast::Expression::BinaryOperation { operator, left, right } => {
            Ok(ast::Expression::BinaryOperation {
                operator: process_bin_operator(operator)?,
                left: Box::new(process_loc_expression(*left)?),
                right: Box::new(process_loc_expression(*right)?),
            })
        }
        parse_ast::Expression::UnaryOperation { operator, expression } => {
            Ok(ast::Expression::UnaryOperation {
                operator: process_un_operator(operator)?,
                expression: Box::new(process_loc_expression(*expression)?),
            })
        }
        parse_ast::Expression::FunctionCall { function_name, arguments } => {
            let mut positional_arguments: Vec<ast::CallArgument> = Vec::new();
            let mut variadic_argument: Option<ast::CallArgument> = None;
            let mut keyword_arguments: Vec<ast::CallKeywordArgument> = Vec::new();
            let mut keyword_variadic_argument: Option<ast::CallArgument> = None;
        
            for arg in arguments {

                match arg.argument {
                    parse_ast::CallArgument::PositionalArgument(expr) => {
                        if !variadic_argument.is_none() || !keyword_arguments.is_empty() || !keyword_variadic_argument.is_none() {
                            return Err(PreprocessingErrorMessage {
                                error: PreprocessingError::FunctionProcessingError(
                                    String::from("Unexpected Positional Argument")
                                ),
                                loc: Some(arg.loc),
                            });
                        }
        
                        positional_arguments.push(ast::CallArgument {
                            expression: Box::new(process_loc_expression(expr)?),
                            loc: arg.loc.clone(),
                        });
                    }
                    parse_ast::CallArgument::Variadic(expr) => {
                        if !keyword_arguments.is_empty() || !keyword_variadic_argument.is_none() {
                            return Err(PreprocessingErrorMessage {
                                error: PreprocessingError::FunctionProcessingError(
                                    String::from("Unexpected Variadic Argument")
                                ),
                                loc: Some(arg.loc),
                            });
                        }
        
                        if variadic_argument.is_none() {
                            variadic_argument = Some(ast::CallArgument {
                                expression: Box::new(process_loc_expression(expr)?),
                                loc: arg.loc.clone(),
                            });
                        } else {
                            return Err(PreprocessingErrorMessage {
                                error: PreprocessingError::FunctionProcessingError(
                                    String::from("Duplicate variadic argument")
                                ),
                                loc: Some(arg.loc),
                            });
                        }
                    }
                    parse_ast::CallArgument::KeywordArgument(name, expression) => {
                        if !keyword_variadic_argument.is_none() {
                            return Err(PreprocessingErrorMessage {
                                error: PreprocessingError::FunctionProcessingError(
                                    String::from("Unexpected Keyword Argument")
                                ),
                                loc: Some(arg.loc),
                            });
                        }

                        match keyword_arguments.iter().filter(|x| x.name == name).collect::<Vec<&ast::CallKeywordArgument>>().get(0) {
                            Some(other_arg) => {
                                return Err(PreprocessingErrorMessage {
                                    error: PreprocessingError::FunctionProcessingError(
                                        format!("Keyword Argument with name {} already exists", other_arg.name)
                                    ),
                                    loc: Some(other_arg.loc.start..arg.loc.end),
                                });
                             }
                            _ => ()
                        }
        
                        keyword_arguments.push(ast::CallKeywordArgument {
                            name: name.clone(),
                            expression: Box::new(process_loc_expression(expression)?),
                            loc: arg.loc.clone(),
                        });
                    }
                    parse_ast::CallArgument::KeywordVariadic(expr) => {
                        if keyword_variadic_argument.is_none() {
                            keyword_variadic_argument = Some(ast::CallArgument {
                                expression: Box::new(process_loc_expression(expr)?),
                                loc: arg.loc.clone(),
                            });
                        } else {
                            return Err(PreprocessingErrorMessage {
                                error: PreprocessingError::FunctionProcessingError(
                                    String::from("Duplicate keyword variadic argument")
                                ),
                                loc: Some(arg.loc),
                            });
                        }
                    }
                }
            }

            Ok(ast::Expression::FunctionCall {
                function_name,
                positional_arguments: positional_arguments,
                variadic_argument: variadic_argument,
                keyword_arguments: keyword_arguments,
                keyword_variadic_argument: keyword_variadic_argument
            })
        }
        parse_ast::Expression::TupleDefinition { elements } => {
            let mut processed_elements = Vec::new();
            for elt in elements {
                processed_elements.push(process_loc_expression(elt)?);
            }
            Ok(ast::Expression::TupleDefinition { elements: processed_elements })
        }
        parse_ast::Expression::ListDefinition { elements } => {
            let mut processed_elements = Vec::new();
            for elt in elements {
                processed_elements.push(process_loc_expression(elt)?);
            }
            Ok(ast::Expression::ListDefinition { elements: processed_elements })
        }
        parse_ast::Expression::DictionaryDefinition { elements } => {
            let mut processed_elements = Vec::new();
            for (key, value) in elements {
                processed_elements.push((process_loc_expression(key)?, process_loc_expression(value)?));
            }
            Ok(ast::Expression::DictionaryDefinition { elements: processed_elements })
        }
        parse_ast::Expression::Indexing { indexed, indexer } => Ok(ast::Expression::Indexing {
            indexed: Box::new(process_loc_expression(*indexed)?),
            indexer: Box::new(process_loc_expression(*indexer)?),
        }),
    }
}

fn process_loc_expression(loc_expr: parse_ast::LocExpression) -> Result<ast::LocExpression, PreprocessingErrorMessage> {
    Ok(ast::LocExpression {
        expression: process_expression(loc_expr.expression)?,
        loc: loc_expr.loc,
    })
}

fn process_statement(stmt: parse_ast::Statement) -> Result<ast::Statement, PreprocessingErrorMessage> {
    match stmt {
        parse_ast::Statement::Assignment { target, expression } => Ok(ast::Statement::Assignment {
            target: process_loc_expression(target)?,
            expression: process_loc_expression(expression)?,
        }),
        parse_ast::Statement::ListAppend { target, value } => Ok(ast::Statement::ListAppend {
            target: process_loc_expression(target)?,
            value: process_loc_expression(value)?,
        }),
        parse_ast::Statement::Return { expression } => Ok(ast::Statement::Return {
            expression: process_loc_expression(expression)?,
        }),
        parse_ast::Statement::IfElse { condition, if_body, else_body } => Ok(ast::Statement::IfElse {
            condition: process_loc_expression(condition)?,
            if_body: process_body(if_body)?,
            else_body: process_body(else_body)?,
        }),
        parse_ast::Statement::While { condition, body } => Ok(ast::Statement::While {
            condition: process_loc_expression(condition)?,
            body: process_body(body)?,
        }),
    }
}

fn process_loc_statement(loc_stmt: parse_ast::LocStatement) -> Result<ast::LocStatement, PreprocessingErrorMessage> {
    Ok(ast::LocStatement {
        statement: process_statement(loc_stmt.statement)?,
        loc: loc_stmt.loc,
    })
}

fn process_body(body: parse_ast::Body) -> Result<ast::Body, PreprocessingErrorMessage> {
    let mut statements = Vec::new();
    for loc_statement in body.statements {
        statements.push(process_loc_statement(loc_statement)?);
    }
    Ok(ast::Body {
        statements
    })
}

fn process_function(func: parse_ast::Function) -> Result<ast::Function, PreprocessingErrorMessage> {
    let mut positional_arguments: Vec<ast::Argument> = Vec::new();
    let mut variadic_argument: Option<ast::Argument> = None;
    let mut keyword_arguments: Vec<ast::KeywordArgument> = Vec::new();
    let mut keyword_variadic_argument: Option<ast::Argument> = None;

    for arg in func.arguments {

        let name = match arg.argument.clone() {
            parse_ast::Argument::PositionalArgument(name)
            | parse_ast::Argument::Variadic(name)
            | parse_ast::Argument::KeywordArgument(name, _)
            | parse_ast::Argument::KeywordVariadic(name)
            => name,
        };

        match positional_arguments.iter().filter(|x| x.name == name).collect::<Vec<&ast::Argument>>().get(0) {
            Some(other_arg) => {
                return Err(PreprocessingErrorMessage {
                    error: PreprocessingError::FunctionProcessingError(
                        format!("Argument with name {} already exists", other_arg.name)
                    ),
                    loc: Some(other_arg.loc.start..arg.loc.end),
                });
            },
            _ => ()
        }

        match variadic_argument.as_ref().or_else(|| None) {
            Some(other_arg) => {
                return Err(PreprocessingErrorMessage {
                    error: PreprocessingError::FunctionProcessingError(
                        format!("Argument with name {} already exists", other_arg.name)
                    ),
                    loc: Some(other_arg.loc.start..arg.loc.end),
                });
            },
            _ => {}
        }

        match keyword_arguments.iter().filter(|x| x.name == name).collect::<Vec<&ast::KeywordArgument>>().get(0) {
            Some(other_arg) => {
                return Err(PreprocessingErrorMessage {
                    error: PreprocessingError::FunctionProcessingError(
                        format!("Argument with name {} already exists", other_arg.name)
                    ),
                    loc: Some(other_arg.loc.start..arg.loc.end),
                });
            },
            _ => ()
        }

        match &keyword_variadic_argument.as_ref().or_else(|| None) {
            Some(other_arg) => {
                return Err(PreprocessingErrorMessage {
                    error: PreprocessingError::FunctionProcessingError(
                        format!("Argument with name {} already exists", other_arg.name)
                    ),
                    loc: Some(other_arg.loc.start..arg.loc.end),
                });
            },
            _ => {}
        }
        
        match arg.argument {
            parse_ast::Argument::PositionalArgument(name) => {
                if !variadic_argument.is_none() || !keyword_arguments.is_empty() || !keyword_variadic_argument.is_none() {
                    return Err(PreprocessingErrorMessage {
                        error: PreprocessingError::FunctionProcessingError(
                            String::from("Unexpected Positional Argument")
                        ),
                        loc: Some(arg.loc),
                    });
                }

                positional_arguments.push(ast::Argument {
                    name: name.clone(),
                    loc: arg.loc.clone(),
                });
            }
            parse_ast::Argument::Variadic(name) => {
                if !keyword_arguments.is_empty() || !keyword_variadic_argument.is_none() {
                    return Err(PreprocessingErrorMessage {
                        error: PreprocessingError::FunctionProcessingError(
                            String::from("Unexpected Variadic Argument")
                        ),
                        loc: Some(arg.loc),
                    });
                }

                if variadic_argument.is_none() {
                    variadic_argument = Some(ast::Argument {
                        name: name.clone(),
                        loc: arg.loc.clone(),
                    });
                } else {
                    return Err(PreprocessingErrorMessage {
                        error: PreprocessingError::FunctionProcessingError(
                            String::from("Duplicate variadic argument")
                        ),
                        loc: Some(arg.loc),
                    });
                }
            }
            parse_ast::Argument::KeywordArgument(name, expression) => {
                if !keyword_variadic_argument.is_none() {
                    return Err(PreprocessingErrorMessage {
                        error: PreprocessingError::FunctionProcessingError(
                            String::from("Unexpected Keyword Argument")
                        ),
                        loc: Some(arg.loc),
                    });
                }

                keyword_arguments.push(ast::KeywordArgument {
                    name: name.clone(),
                    expression: process_loc_expression(expression.clone())?,
                    loc: arg.loc.clone(),
                });
            }
            parse_ast::Argument::KeywordVariadic(name) => {
                if keyword_variadic_argument.is_none() {
                    keyword_variadic_argument = Some(ast::Argument {
                        name: name.clone(),
                        loc: arg.loc.clone(),
                    });
                } else {
                    return Err(PreprocessingErrorMessage {
                        error: PreprocessingError::FunctionProcessingError(
                            String::from("Duplicate keyword variadic argument")
                        ),
                        loc: Some(arg.loc),
                    });
                }
            }
        }
    }

    Ok(ast::Function {
        name: func.name.clone(),
        contract: ast::FunctionContract {
            positional_arguments,
            variadic_argument,
            keyword_arguments,
            keyword_variadic_argument,
        },
        body: process_body(func.body)?,
        loc: func.loc,
    })
}

pub fn process_program(program: parse_ast::Program) -> Result<ast::Program, PreprocessingErrorMessage> {
    let mut functions = std::collections::HashMap::new();
    for (name, func) in program.functions {
        functions.insert(name, process_function(func)?);
    }
    Ok(ast::Program { functions })
}