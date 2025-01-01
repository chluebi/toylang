use crate::parse_ast;
use crate::ast;

fn process_type(t: parse_ast::Type) -> ast::Type {
    match t {
        parse_ast::Type::Int => ast::Type::Int,
        parse_ast::Type::Bool => ast::Type::Bool,
        parse_ast::Type::Tuple => ast::Type::Tuple,
        parse_ast::Type::List => ast::Type::List,
        parse_ast::Type::Dict => ast::Type::Dict,
    }
}

fn process_bin_operator(op: parse_ast::BinOperator) -> ast::BinOperator {
    match op {
        parse_ast::BinOperator::Add => ast::BinOperator::Add,
        parse_ast::BinOperator::Sub => ast::BinOperator::Sub,
        parse_ast::BinOperator::Mul => ast::BinOperator::Mul,
        parse_ast::BinOperator::Div => ast::BinOperator::Div,
        parse_ast::BinOperator::Mod => ast::BinOperator::Mod,
        parse_ast::BinOperator::And => ast::BinOperator::And,
        parse_ast::BinOperator::Or => ast::BinOperator::Or,
        parse_ast::BinOperator::Eq => ast::BinOperator::Eq,
        parse_ast::BinOperator::Neq => ast::BinOperator::Neq,
        parse_ast::BinOperator::Lt => ast::BinOperator::Lt,
        parse_ast::BinOperator::Gt => ast::BinOperator::Gt,
        parse_ast::BinOperator::Leq => ast::BinOperator::Leq,
        parse_ast::BinOperator::Geq => ast::BinOperator::Geq,
        parse_ast::BinOperator::ListConcat => ast::BinOperator::ListConcat,
    }
}

fn process_un_operator(op: parse_ast::UnOperator) -> ast::UnOperator {
    match op {
        parse_ast::UnOperator::Neg => ast::UnOperator::Neg,
        parse_ast::UnOperator::Not => ast::UnOperator::Not,
        parse_ast::UnOperator::Length => ast::UnOperator::Length,
    }
}

fn process_expression(expr: parse_ast::Expression) -> ast::Expression {
    match expr {
        parse_ast::Expression::IntLiteral(i) => ast::Expression::IntLiteral(i),
        parse_ast::Expression::BoolLiteral(b) => ast::Expression::BoolLiteral(b),
        parse_ast::Expression::Variable(var) => ast::Expression::Variable(var),
        parse_ast::Expression::Typecheck { expression, expected_type } => {
            ast::Expression::Typecheck {
                expression: Box::new(process_loc_expression(*expression)),
                expected_type: process_type(expected_type),
            }
        }
        parse_ast::Expression::BinaryOperation { operator, left, right } => {
            ast::Expression::BinaryOperation {
                operator: process_bin_operator(operator),
                left: Box::new(process_loc_expression(*left)),
                right: Box::new(process_loc_expression(*right)),
            }
        }
        parse_ast::Expression::UnaryOperation { operator, expression } => {
            ast::Expression::UnaryOperation {
                operator: process_un_operator(operator),
                expression: Box::new(process_loc_expression(*expression)),
            }
        }
        parse_ast::Expression::FunctionCall { function_name, arguments } => {
            let mut processed_args = Vec::new();
            for arg in arguments {
                processed_args.push(process_loc_expression(arg));
            }
            ast::Expression::FunctionCall {
                function_name,
                arguments: processed_args,
            }
        }
        parse_ast::Expression::TupleDefinition { elements } => {
            let mut processed_elements = Vec::new();
            for elt in elements {
                processed_elements.push(process_loc_expression(elt));
            }
            ast::Expression::TupleDefinition { elements: processed_elements }
        }
        parse_ast::Expression::ListDefinition { elements } => {
            let mut processed_elements = Vec::new();
            for elt in elements {
                processed_elements.push(process_loc_expression(elt));
            }
            ast::Expression::ListDefinition { elements: processed_elements }
        }
        parse_ast::Expression::DictionaryDefinition { elements } => {
            let mut processed_elements = Vec::new();
            for (key, value) in elements {
                processed_elements.push((process_loc_expression(key), process_loc_expression(value)));
            }
            ast::Expression::DictionaryDefinition { elements: processed_elements }
        }
        parse_ast::Expression::Indexing { indexed, indexer } => ast::Expression::Indexing {
            indexed: Box::new(process_loc_expression(*indexed)),
            indexer: Box::new(process_loc_expression(*indexer)),
        },
    }
}

fn process_loc_expression(loc_expr: parse_ast::LocExpression) -> ast::LocExpression {
    ast::LocExpression {
        expression: process_expression(loc_expr.expression),
        loc: loc_expr.loc,
    }
}

fn process_statement(stmt: parse_ast::Statement) -> ast::Statement {
    match stmt {
        parse_ast::Statement::Assignment { target, expression } => ast::Statement::Assignment {
            target: process_loc_expression(target),
            expression: process_loc_expression(expression),
        },
        parse_ast::Statement::ListAppend { target, value } => ast::Statement::ListAppend {
            target: process_loc_expression(target),
            value: process_loc_expression(value),
        },
        parse_ast::Statement::Return { expression } => ast::Statement::Return {
            expression: process_loc_expression(expression),
        },
        parse_ast::Statement::IfElse { condition, if_body, else_body } => ast::Statement::IfElse {
            condition: process_loc_expression(condition),
            if_body: process_body(if_body),
            else_body: process_body(else_body),
        },
        parse_ast::Statement::While { condition, body } => ast::Statement::While {
            condition: process_loc_expression(condition),
            body: process_body(body),
        },
    }
}

fn process_loc_statement(loc_stmt: parse_ast::LocStatement) -> ast::LocStatement {
    ast::LocStatement {
        statement: process_statement(loc_stmt.statement),
        loc: loc_stmt.loc,
    }
}

fn process_body(body: parse_ast::Body) -> ast::Body {
    ast::Body {
        statements: body.statements.into_iter().map(process_loc_statement).collect(),
    }
}


fn process_function(func: parse_ast::Function) -> ast::Function {
    let positional_arguments: Vec<ast::Argument> = func.arguments.iter().filter_map(|arg| {
        if let parse_ast::Argument::PositionalArgument(name) = &arg.argument {
            Some(ast::Argument {
                    name: name.clone(),
                    loc: arg.loc.clone()
                })
        } else {
            None
        }
    }).collect();

    ast::Function {
        name: func.name.clone(), // Important to clone the String
        arguments: positional_arguments,
        body: process_body(func.body),
        loc: func.loc
    }
}

pub fn process_program(program: parse_ast::Program) -> ast::Program {
    let mut functions = std::collections::HashMap::new();
    for (name, func) in program.functions {
        functions.insert(name, process_function(func));
    }
    ast::Program { functions }
}