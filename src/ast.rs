use std::fmt;

#[derive(Debug, Clone, Copy)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Operator::Add => write!(f, "+"),
            Operator::Sub => write!(f, "-"),
            Operator::Mul => write!(f, "*"),
            Operator::Div => write!(f, "/")
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    IntLiteral(i64),
    Variable(String),
    BinaryOperation {
        left: Box<Expression>,
        operator: Operator,
        right: Box<Expression>,
    },
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::IntLiteral(i) => write!(f, "{}", i),
            Expression::Variable(var) => write!(f, "{}", var),
            Expression::BinaryOperation { left, operator, right } => {
                write!(f, "({} {} {})", left, operator, right)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    Assignment {
        variable: String,
        expression: Expression,
    },
    Return {
        expression: Expression
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Assignment { variable, expression } => write!(f, "{} = {}", variable, expression),
            Statement::Return { expression } => write!(f, "return {}", expression)
        }
    }
}


#[derive(Debug, Clone)]
pub struct Program {
    pub body: Vec<Statement>
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, statement) in self.body.iter().enumerate() {
            if i > 0 {
                write!(f, "\n")?;
            }
            write!(f, "{};", statement)?;
        }
        Ok(())
    }
}