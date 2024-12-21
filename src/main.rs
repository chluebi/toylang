mod ast;
mod interpreter;

fn main() {
    let program = ast::Program {
        body: ast::Body {statements: vec![
            ast::Statement::Assignment { variable: "x".to_string(), expression: 
                ast::Expression::IntLiteral(1000)
            },
            ast::Statement::While {
                condition: ast::Expression::BinaryOperation {
                    operator: ast::BinOperator::Gt,
                    left: Box::new(ast::Expression::Variable ("x".to_string())),
                    right: Box::new(ast::Expression::IntLiteral(1)),
                },
                body: ast::Body {
                    statements: vec![
                        ast::Statement::IfElse {
                            condition: ast::Expression::BinaryOperation {
                                operator: ast::BinOperator::Eq,
                                left: Box::new(ast::Expression::BinaryOperation {
                                    operator: ast::BinOperator::Mod,
                                    left: Box::new(ast::Expression::Variable ("x".to_string())),
                                    right: Box::new(ast::Expression::IntLiteral(2)),
                                }),
                                right: Box::new(ast::Expression::IntLiteral(0))
                            },
                            if_body: ast::Body {
                                statements: vec![
                                    ast::Statement::Assignment { variable: "x".to_string(), expression: 
                                        ast::Expression::BinaryOperation {
                                            operator: ast::BinOperator::Div,
                                            left: Box::new(ast::Expression::Variable ("x".to_string())),
                                            right: Box::new(ast::Expression::IntLiteral (2)),
                                        }
                                    }
                                ]
                            },
                            else_body: ast::Body {
                                statements: vec![
                                    ast::Statement::Assignment { variable: "x".to_string(), expression: 
                                        ast::Expression::BinaryOperation {
                                            operator: ast::BinOperator::Add,
                                            left: Box::new(
                                                ast::Expression::BinaryOperation {
                                                    operator: ast::BinOperator::Mul,
                                                    left: Box::new(ast::Expression::IntLiteral (3)),
                                                    right: Box::new(
                                                        ast::Expression::Variable ("x".to_string())
                                                    ),
                                                }
                                            ),
                                            right: Box::new(ast::Expression::IntLiteral (1)),
                                        }
                                    }
                                ]
                            }
                        }
                    ]
                }
            },
            ast::Statement::Return {
                expression: ast::Expression::Variable ("x".to_string())
            }
        ]}
    };

    let res = interpreter::interpret(&program);

    match res {
        Ok(v) => println!("Program Executed with result {}", v),
        Err(e) => println!("Program Failed {}", e)
    }
}
