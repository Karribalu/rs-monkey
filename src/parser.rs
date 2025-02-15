use crate::ast::{Expression, ExpressionStatement, LetStatement, Program, ReturnStatement, Statement};
use crate::lexer::Lexer;
use crate::token::Token;
use std::cmp::PartialEq;
use std::collections::HashMap;
use std::num::ParseIntError;
use thiserror::Error;

type ParseResult<T> = Result<T, ParseError>;
#[derive(Error, Clone, Debug, Eq, PartialEq)]
pub enum ParseError {
    #[error("Error while parsing the let statement: {0}")]
    LetSyntaxError(String),
    #[error("Expected expression but got {0}")]
    ExpressionFailed(String),
    #[error(transparent)]
    InvalidIntegerParse(#[from] ParseIntError),
    #[error("Expected integer but got {0}")]
    IntegerParsingFailed(String),
    #[error("Error while parsing")]
    ParsingFailed,
}
#[derive(Debug)]
pub struct Parser<'a> {
    pub lexer: Lexer<'a>,
    pub curr_token: Token,
    pub peek_token: Token,
    // pub prefix_parse_fns: HashMap<Token, dyn Fn<(), Output=Expression>>,
    // pub suffix_parse_fns: HashMap<Token, dyn Fn<Expression, Output = Expression>>,
}
enum Precedence {
    Lowest,
    Equals,
    LessGreater, // > or <
    Sum, // +
    Product, // *
    Prefix, // -X or !X
    Call,
}
impl Precedence {
    fn value(&self) -> u8 {
        match &self {
            Precedence::Lowest => { 1 }
            Precedence::Equals => { 2 }
            Precedence::LessGreater => { 3 }
            Precedence::Sum => { 4 }
            Precedence::Product => { 5 }
            Precedence::Prefix => { 6 }
            Precedence::Call => { 7 }
        }
    }
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        let mut l = lexer;
        let curr = l.next_token();
        let next = l.next_token();
        Self {
            lexer: l,
            curr_token: curr,
            peek_token: next,
            // prefix_parse_fns: HashMap::new(),
            // suffix_parse_fns: HashMap::new(),
        }
    }
    pub fn next_token(&mut self) {
        self.curr_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    pub fn parse_program(&mut self) -> ParseResult<Program> {
        let mut program = Program::new();
        while self.curr_token != Token::Eof {
            let stmt = self.parse_statement()?;
            program.statements.push(stmt);
            self.next_token();
        }
        Ok(program)
    }
    pub fn parse_statement(&mut self) -> ParseResult<Statement> {
        match self.curr_token {
            Token::Let => {
                self.parse_let_statement()
            }
            Token::Return => {
                self.parse_return_statement()
            }
            _ => {
                self.parse_expression_statement()
            }
        }
        // Err(ParseError::ParsingFailed)
    }
    pub fn parse_let_statement(&mut self) -> ParseResult<Statement> {
        let name = self.expect_ident()?;;
        let value;
        self.next_token();
        self.expect_peek(Token::Assign)?;
        while !self.is_curr_token(Token::Semicolon) {
            self.next_token();
        }
        value = Expression::Something;
        Ok(Statement::Let(LetStatement {
            name,
            value,
        }))
    }
    pub fn parse_return_statement(&mut self) -> ParseResult<Statement> {
        let expression = Expression::Something;
        self.next_token();
        while &self.curr_token != &Token::Semicolon {
            self.next_token();
        }
        Ok(Statement::Return(ReturnStatement {
            value: expression
        }))
    }
    pub fn parse_expression_statement(&mut self) -> ParseResult<Statement> {
        let exp = ExpressionStatement {
            expression: self.parse_expression(Precedence::Lowest)?
        };
        if self.peek_token == Token::Semicolon {
            self.next_token();
        }
        Ok(Statement::Expression(exp))
    }
    pub fn parse_expression(&self, precedence: Precedence) -> ParseResult<Expression> {
        match &self.curr_token {
            Token::Ident(identifier) => {
                Ok(Expression::Identifier(identifier.clone()))
            }
            Token::Int(integer) => {
                Ok(Expression::Integer(*integer))
            }
            _ => Err(ParseError::ExpressionFailed(self.curr_token.to_string()))?
        }
    }
    // Currently not being used
    pub fn parse_integer(&self) -> ParseResult<Expression> {
        match &self.curr_token {
            Token::Ident(ident) => {
                let integer = ident.parse::<i64>()?;
                Ok(Expression::Integer(integer))
            }
            _ => Err(ParseError::IntegerParsingFailed(self.curr_token.to_string()))
        }
    }
    fn is_curr_token(&self, token: Token) -> bool {
        self.curr_token == token
    }
    fn is_peek_token(&self, token: Token) -> bool {
        self.peek_token == token
    }
    fn expect_peek(&mut self, token: Token) -> ParseResult<()> {
        if self.is_peek_token(token) {
            self.next_token();
            return Ok(());
        }
        Err(ParseError::LetSyntaxError("Assignment operator not found in the let statement".to_string()))
    }
    fn expect_ident(&mut self) -> Result<String, ParseError> {
        match &self.peek_token {
            Token::Ident(name) => {
                Ok(name.clone())
            }
            _ => {
                Err(ParseError::LetSyntaxError(format!("Expected next token to be identifier but found {:?}", &self.peek_token)))
            }
        }
    }
}


mod tests {
    use crate::ast::{Expression, ExpressionStatement, LetStatement, ReturnStatement, Statement};
    use crate::lexer::Lexer;
    use crate::parser::{ParseError, Parser};
    use crate::token::Token;

    #[test]
    fn test_let_parser() {
        let input = "let x = 5;
            let y = 10;
            let foobar = 838383;";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        assert!(program.is_ok());
        let statements = program.unwrap().statements;
        assert_eq!(statements.len(), 3);
        let expected = vec![
            Statement::Let(LetStatement {
                name: String::from("x"),
                value: Expression::Something
            }),
            Statement::Let(LetStatement {
                name: String::from("y"),
                value: Expression::Something
            }),
            Statement::Let(LetStatement {
                name: String::from("foobar"),
                value: Expression::Something
            }),
        ];
        for (idx, expected_statement) in expected.iter().enumerate() {
            let actual_statement = &statements[idx];
            assert_eq!(expected_statement, actual_statement)
        }
    }
    #[test]
    fn test_let_parser_error() {
        let input = "let x = 5;
            let = 10;
            let foobar = 838383;";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        assert!(program.is_err());
        assert_eq!(program, Err(ParseError::LetSyntaxError(format!("Expected next token to be identifier but found {:?}", Token::Assign))));
    }
    #[test]
    fn test_return_parser() {
        let input = "return 10;
            return 20;
            return 3000;";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        assert!(program.is_ok());
        let statements = program.unwrap().statements;
        let expected = vec![
            Statement::Return(ReturnStatement {
                value: Expression::Something,
            }),
            Statement::Return(ReturnStatement {
                value: Expression::Something,
            }),
            Statement::Return(ReturnStatement {
                value: Expression::Something,
            }),
        ];
        for (idx, expected_statement) in expected.iter().enumerate() {
            let actual_statement = &statements[idx];
            assert_eq!(expected_statement, actual_statement)
        }
    }
    #[test]
    fn test_expression_parser() {
        let input = "foobar;";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        assert!(program.is_ok());
        let statements = program.unwrap().statements;
        let expected = vec![Statement::Expression(ExpressionStatement {
            expression: Expression::Identifier("foobar".to_string()),
        })];
        for (idx, expected_statement) in expected.iter().enumerate() {
            let actual_statement = &statements[idx];
            assert_eq!(expected_statement, actual_statement)
        }
    }
    #[test]
    fn test_integer_parser() {
        let input = "10;";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        assert!(program.is_ok());
        let statements = program.unwrap().statements;
        let expected = vec![Statement::Expression(ExpressionStatement {
            expression: Expression::Integer(10),
        })];
        for (idx, expected_statement) in expected.iter().enumerate() {
            let actual_statement = &statements[idx];
            assert_eq!(expected_statement, actual_statement)
        }
    }
}

