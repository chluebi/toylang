use std::collections::HashMap;

use crate::ast;

pub fn get_program() -> ast::Program {
    let fibonacci = ast::Function {
        arguments: vec!["n".to_string()],
        body: ast::Body {
            statements: vec![
                ast::Statement::IfElse {
                    condition: ast::Expression::BinaryOperation {
                        operator: ast::BinOperator::Or,
                        left: Box::new(ast::Expression::BinaryOperation {
                            operator: ast::BinOperator::Eq,
                            left: Box::new(ast::Expression::Variable("n".to_string())),
                            right: Box::new(ast::Expression::IntLiteral(0)),
                        }),
                        right: Box::new(ast::Expression::BinaryOperation {
                            operator: ast::BinOperator::Eq,
                            left: Box::new(ast::Expression::Variable("n".to_string())),
                            right: Box::new(ast::Expression::IntLiteral(1)),
                        }),
                    },
                    if_body: ast::Body {
                        statements: vec![
                            ast::Statement::Return {
                                expression: ast::Expression::Variable("n".to_string()),
                            },
                        ],
                    },
                    else_body: ast::Body {
                        statements: vec![
                            ast::Statement::Return {
                                expression: ast::Expression::BinaryOperation {
                                    operator: ast::BinOperator::Add,
                                    left: Box::new(ast::Expression::FunctionCall {
                                        function_name: "fibonacci".to_string(),
                                        arguments: vec![ast::Expression::BinaryOperation {
                                            operator: ast::BinOperator::Sub,
                                            left: Box::new(ast::Expression::Variable("n".to_string())),
                                            right: Box::new(ast::Expression::IntLiteral(1)),
                                        }],
                                    }),
                                    right: Box::new(ast::Expression::FunctionCall {
                                        function_name: "fibonacci".to_string(),
                                        arguments: vec![ast::Expression::BinaryOperation {
                                            operator: ast::BinOperator::Sub,
                                            left: Box::new(ast::Expression::Variable("n".to_string())),
                                            right: Box::new(ast::Expression::IntLiteral(2)),
                                        }],
                                    }),
                                },
                            },
                        ],
                    },
                },
            ],
        },
    };

    let main = ast::Function {
        arguments: vec![],
        body: ast::Body {
            statements: vec![
                ast::Statement::Return {
                    expression: ast::Expression::FunctionCall {
                        function_name: "fibonacci".to_string(),
                        arguments: vec![ast::Expression::IntLiteral(10)],
                    },
                },
            ],
        },
    };

    let mut functions = HashMap::new();
    functions.insert("main".to_string(), main);
    functions.insert("fibonacci".to_string(), fibonacci);

    ast::Program {
        functions,
    }
}
