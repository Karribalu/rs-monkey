use crate::ast::Statement::Let;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
}
impl Display for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for statement in &self.statements {
            writeln!(f, "{}\n", &statement)?
        }
        write!(f, "")
    }
}
impl Program {
    pub(crate) fn new() -> Self {
        Self { statements: vec![] }
    }
}
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct BlockStatement {
    pub(crate) statements: Vec<Statement>,
}

impl Display for BlockStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for statement in &self.statements {
            writeln!(f, "{}\n", &statement)?
        }
        write!(f, "")
    }
}
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Let(let_statement) => {
                write!(f, "let {}", let_statement)
            }
            Statement::Return(return_statement) => {
                write!(f, "return {}", return_statement)
            }
            Statement::Expression(ex) => {
                write!(f, "{}", ex)
            }
        }
    }
}
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Expression {
    Identifier(String),
    Integer(i64),
    Boolean(bool),
    Prefix(Box<PrefixExpression>),
    Infix(Box<InfixExpression>),
    If(Box<IfExpression>),
    Something,
}
impl Display for Expression {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Expression::Identifier(ident) => {
                write!(f, "{}", ident)
            }
            Expression::Integer(integer) => {
                write!(f, "{}", integer)
            }
            Expression::Prefix(prefix) => {
                write!(f, "{}", prefix)
            }
            Expression::Infix(infix) => {
                write!(f, "{}", infix)
            }
            Expression::Boolean(boolean) => {
                write!(f, "{}", boolean.to_string())
            }
            Expression::If(if_expression) => write!(f, "if {}", if_expression),
            Expression::Something => {
                write!(f, "something")
            }
        }
    }
}
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct LetStatement {
    pub(crate) name: String,
    pub(crate) value: Expression,
}
impl Display for LetStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} = {}", self.name, self.value)
    }
}
// #[derive(Debug, Clone, Eq, PartialEq)]
// pub struct Identifier {
//     pub(crate) value: String,
// }
// impl Display for Identifier {
//     fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
//         write!(f, "{}", self.value)
//     }
// }
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ReturnStatement {
    pub(crate) value: Expression,
}
impl Display for ReturnStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ExpressionStatement {
    pub(crate) expression: Expression,
}

impl Display for ExpressionStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.expression)
    }
}
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct PrefixExpression {
    pub(crate) operator: String,
    pub(crate) right: Box<Expression>,
}
impl Display for PrefixExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} {})", self.operator, self.right)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct InfixExpression {
    pub(crate) left: Box<Expression>,
    pub(crate) operator: String,
    pub(crate) right: Box<Expression>,
}
impl Display for InfixExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} {} {})", self.left, self.operator, self.right)
    }
}
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct IfExpression {
    pub(crate) condition: Expression,
    pub(crate) consequence: BlockStatement,
    pub(crate) alternative: Option<BlockStatement>,
}
impl Display for IfExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.alternative.is_some() {
            write!(
                f,
                "{} {{ {} }}  else {{ {} }}",
                self.condition, self.consequence, self.alternative.clone().unwrap()
            )
        } else {
            write!(
                f,
                "{} {{ {} }}",
                self.condition, self.consequence
            )
        }
    }
}
