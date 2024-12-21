use std::fmt;

#[derive(Debug, Clone, Copy)]
pub enum BinOperator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,

    And,
    Or,

    Eq,
    Neq,
    Lt,
    Gt,
    Leq,
    Geq
}

impl fmt::Display for BinOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinOperator::Add => write!(f, "+"),
            BinOperator::Sub => write!(f, "-"),
            BinOperator::Mul => write!(f, "*"),
            BinOperator::Div => write!(f, "/"),
            BinOperator::Mod => write!(f, "%"),

            BinOperator::And => write!(f, "&&"),
            BinOperator::Or => write!(f, "||"),

            BinOperator::Eq => write!(f, "="),
            BinOperator::Neq => write!(f, "!="),
            BinOperator::Lt => write!(f, "<"),
            BinOperator::Gt => write!(f, ">"),
            BinOperator::Leq => write!(f, "<="),
            BinOperator::Geq => write!(f, ">="),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum UnOperator {
    Neg,
    Not
}

impl fmt::Display for UnOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnOperator::Neg => write!(f, "-"),
            UnOperator::Not => write!(f, "!")
        }
    }
}


#[derive(Debug, Clone)]
pub enum Expression {
    IntLiteral(i64),
    BoolLiteral(bool),
    Variable(String),
    BinaryOperation {
        operator: BinOperator,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    UnaryOperation {
        operator: UnOperator,
        expression: Box<Expression>
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::IntLiteral(i) => write!(f, "{}", i),
            Expression::BoolLiteral(b) => write!(f, "{}", b),
            Expression::Variable(var) => write!(f, "{}", var),
            Expression::BinaryOperation {  operator, left, right } => {
                write!(f, "({} {} {})", left, operator, right)
            },
            Expression::UnaryOperation { operator, expression } => {
                write!(f, "({} {})", operator, expression)
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
    },
    IfElse {
        condition: Expression,
        if_body: Body,
        else_body: Body
    },
    While {
        condition: Expression,
        body: Body
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Assignment { variable, expression } => write!(f, "{} = {}", variable, expression),
            Statement::Return { expression } => write!(f, "return {}", expression),
            Statement::IfElse { condition, if_body, else_body } => {
                write!(f, "if ({}) {{\n{}\n}} else {{\n{}\n}}", condition, if_body, else_body)
            },
            Statement::While { condition, body } => {
                write!(f, "while ({}) {{\n{}\n}}", condition, body)
            },
        }
    }
}

#[derive(Debug, Clone)]
pub struct Body {
    pub statements: Vec<Statement>
}

impl fmt::Display for Body {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, statement) in self.statements.iter().enumerate() {
            if i > 0 {
                write!(f, "\n")?;
            }
            write!(f, "{};", statement)?;
        }
        Ok(())
    }
}


#[derive(Debug, Clone)]
pub struct Program {
    pub body: Body
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.body)
    }
}