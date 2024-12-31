use std::fmt;
use std::ops::Range;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum Type {
    Int,
    Bool,
    Tuple,
    List,
    Dict
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Int => write!(f, "int"),
            Type::Bool => write!(f, "bool"),
            Type::Tuple => write!(f, "tuple"),
            Type::List => write!(f, "list"),
            Type::Dict => write!(f, "dict")
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
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
    Geq,

    ListConcat,
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

            BinOperator::ListConcat => write!(f, "++"),
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum UnOperator {
    Neg,
    Not,
    Length
}

impl fmt::Display for UnOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnOperator::Neg => write!(f, "-"),
            UnOperator::Not => write!(f, "!"),
            UnOperator::Length => write!(f, "?")
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    IntLiteral(i64),
    BoolLiteral(bool),
    Variable(String),
    Typecheck {
        expression: Box<LocExpression>,
        expected_type: Type
    },
    BinaryOperation {
        operator: BinOperator,
        left: Box<LocExpression>,
        right: Box<LocExpression>,
    },
    UnaryOperation {
        operator: UnOperator,
        expression: Box<LocExpression>
    },
    FunctionCall {
        function_name: String,
        arguments: Vec<LocExpression>
    },
    TupleDefinition {
        elements: Vec<LocExpression>
    },
    ListDefinition {
        elements: Vec<LocExpression>
    },
    DictionaryDefinition {
        elements: Vec<(LocExpression, LocExpression)>
    },
    Indexing {
        indexed: Box<LocExpression>,
        indexer: Box<LocExpression>
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::IntLiteral(i) => write!(f, "{}", i),
            Expression::BoolLiteral(b) => write!(f, "{}", b),
            Expression::Variable(var) => write!(f, "{}", var),
            Expression::Typecheck { expression, expected_type } => {
                write!(f, "({} ?? {})", expression, expected_type)
            }
            Expression::BinaryOperation {  operator, left, right } => {
                write!(f, "({} {} {})", left, operator, right)
            },
            Expression::UnaryOperation { operator, expression } => {
                write!(f, "({} {})", operator, expression)
            },
            Expression::FunctionCall { function_name: function, arguments } => {
                write!(f, "{}(", function)?;
                for (i, arg) in arguments.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")
            },
            Expression::TupleDefinition {elements} => {
                write!(f, "(")?;
                for (i, elt) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", elt)?;
                }
                write!(f, ")")
            },
            Expression::ListDefinition { elements } => {
                write!(f, "[")?;
                for (i, elt) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", elt)?;
                }
                write!(f, "]")
            },
            Expression::DictionaryDefinition { elements } =>  {
                write!(f, "{{")?;
                for (i, (key, value)) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", key, value)?;
                }
                write!(f, "}}")
            },
            Expression::Indexing { indexed, indexer } => {
                write!(f, "{}[{}]", indexed, indexer)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct LocExpression {
    pub expression: Expression,
    pub loc: Range<usize>
}

impl fmt::Display for LocExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.expression)
    }
}


#[derive(Debug, Clone)]
pub enum Statement {
    Assignment {
        target: LocExpression,
        expression: LocExpression,
    },
    ListAppend {
        target: LocExpression,
        value: LocExpression
    },
    Return {
        expression: LocExpression
    },
    IfElse {
        condition: LocExpression,
        if_body: Body,
        else_body: Body
    },
    While {
        condition: LocExpression,
        body: Body
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Assignment { target, expression } => write!(f, "{} = {}", target, expression),
            Statement::ListAppend { target, value } => write!(f, "{}[] = {}", target, value),
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
pub struct LocStatement {
    pub statement: Statement,
    pub loc: Range<usize>
}

impl fmt::Display for LocStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.statement)
    }
}

#[derive(Debug, Clone)]
pub struct Body {
    pub statements: Vec<LocStatement>
}

impl fmt::Display for Body {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, statement) in self.statements.iter().enumerate() {
            if i > 0 {
                write!(f, "\n")?;
            }
            for line in format!("{};", statement).split('\n') {
                write!(f, "\t{}\n", line)?;
            }
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Argument {
    pub name: String,
    pub loc: Range<usize>
}

impl fmt::Display for Argument {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}


#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub arguments: Vec<Argument>,
    pub body: Body,
    pub loc: Range<usize>
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(")?;
        for (i, arg) in self.arguments.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{};", arg)?;
        }
        write!(f, ") {{\n{}\n}}", self.body)?;
        Ok(())
    }
}


#[derive(Debug, Clone)]
pub struct Program {
    pub functions: HashMap<String, Function>
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, (name, function)) in self.functions.iter().enumerate() {
            if i > 0 {
                write!(f, "\n\n")?;
            }
            write!(f, "{}{}", name, function)?;
        }
        Ok(())
    }
}