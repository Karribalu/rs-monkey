use crate::ast::Statement::Let;
use std::fmt::{Display, Formatter};
// enum Node {
//     Program(),
//     Statement
// }
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
}
impl Display for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for statement in &self.statements {
            writeln!(f, "{}", &statement)?
        }
        write!(f, "")
    }
}
impl Program {
    pub(crate) fn new() -> Self {
        Self {
            statements: vec![]
        }
    }
}
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Let(let_statement) => {
                write!(f, "let {};", let_statement)
            }
            Statement::Return(return_statement) => {
                write!(f, "return {};", return_statement)
            }
        }
    }
}
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Expression {
    Something
}
impl Display for Expression {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}", "something")
    }
}
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct LetStatement {
    pub(crate) name: Identifier,
    pub(crate) value: Expression,
}
impl Display for LetStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} = {}", self.name, self.value)
    }
}
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Identifier {
    pub(crate) value: String,
}
impl Display for Identifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ReturnStatement {
    pub(crate) value: Expression,
}
impl Display for ReturnStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct ExpressionStatement {
    pub(crate) value: Expression,
}

impl Display for ExpressionStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}