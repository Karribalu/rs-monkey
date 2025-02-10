use crate::token::Token;
// enum Node {
//     Program(),
//     Statement
// }
#[derive(Debug, Eq, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
}
impl Program {
    pub(crate) fn new() -> Self {
        Self {
            statements: vec![]
        }
    }
}
#[derive(Debug, Eq, PartialEq)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
}
#[derive(Debug, Eq, PartialEq)]
pub enum Expression {
    Something
}
#[derive(Debug, Eq, PartialEq)]
pub struct LetStatement {
    pub(crate) name: Identifier,
    pub(crate) value: Expression,
}
#[derive(Debug, Eq, PartialEq)]
pub struct Identifier {
    pub(crate) value: String,
}

#[derive(Debug, Eq, PartialEq)]
pub struct ReturnStatement {
    pub(crate) value: Expression,
}
