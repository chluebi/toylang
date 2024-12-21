mod ast;
mod interpreter;

fn main() {
    let program = ast::Program {
        body: vec![
            ast::Statement::Assignment { variable: "x".to_string(), expression: 
                ast::Expression::BinaryOperation { 
                    left: Box::new(ast::Expression::IntLiteral(1)),
                    operator: ast::Operator::Add,
                    right: Box::new(ast::Expression::IntLiteral(1)), 
                }
            },
            ast::Statement::Return {
                expression: ast::Expression::Variable("x".to_string())
            }
        ]
    };

    let res = interpreter::interpret(program);

    match res {
        Ok(v) => println!("Program Executed with result {}", v),
        Err(e) => println!("Program Failed {}", e)
    }
}
