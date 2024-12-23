use std::collections::HashMap;

use crate::ast;


pub fn get_program() -> ast::Program {
    let factorial = ast::Function {
        arguments: vec!["x".to_string()],
        body: ast::Body {statements: vec![
            ast::Statement::IfElse {
                condition: ast::Expression::BinaryOperation {
                    operator: ast::BinOperator::Eq,
                    left: Box::new(ast::Expression::Variable ("x".to_string())),
                    right: Box::new(ast::Expression::IntLiteral(1))
                },
                if_body: ast::Body {
                    statements: vec![
                        ast::Statement::Return {
                            expression: ast::Expression::Variable ("x".to_string())
                        }
                    ]
                },
                else_body: ast::Body {
                    statements: vec![
                        ast::Statement::Return {
                            expression:ast::Expression::BinaryOperation {
                                operator: ast::BinOperator::Mul,
                                left: Box::new(ast::Expression::Variable ("x".to_string())),
                                right: Box::new(
                                    ast::Expression::FunctionCall {
                                        arguments: vec![
                                            ast::Expression::BinaryOperation {
                                                operator: ast::BinOperator::Sub,
                                                left: Box::new(ast::Expression::Variable ("x".to_string())),
                                                right: Box::new(ast::Expression::IntLiteral(1))
                                            }
                                        ],
                                        function_name: "factorial".to_string()
                                    }
                                )
                            }
                        }
                    ]
                }
            }
        ]}
    };
    let main = ast::Function {
        arguments: vec![],
        body: ast::Body {statements: vec![
            ast::Statement::Return {
                expression: ast::Expression::FunctionCall {
                    arguments: vec![ast::Expression::IntLiteral(10)],
                    function_name: "factorial".to_string()
                }
            }
        ]}
    };

    let mut functions = HashMap::new();
    functions.insert("main".to_string(), main);
    functions.insert("factorial".to_string(), factorial);

    ast::Program {
        functions: functions
    }
}
