use crate::ast::{
    Expression, ExpressionStatement, InfixExpression, LetStatement, PrefixExpression, Program,
    ReturnStatement, Statement,
};
use crate::lexer::Lexer;
use crate::token::Token;
use std::cmp::{PartialEq, PartialOrd};
use std::num::ParseIntError;
use thiserror::Error;

type ParseResult<T> = Result<T, ParseError>;
type PrefixFn = fn(parser: &mut Parser) -> ParseResult<Expression>;
type InfixFn = fn(parser: &mut Parser, left: Expression) -> ParseResult<Expression>;
#[derive(Error, Clone, Debug, Eq, PartialEq)]
pub enum ParseError {
    #[error("Error while parsing the let statement: {0}")]
    LetSyntaxError(String),
    #[error("Expected expression but found {0}")]
    ExpressionFailed(String),
    #[error("Expected identifier but found {0}")]
    InvalidIdentifier(String),
    #[error(transparent)]
    InvalidIntegerParse(#[from] ParseIntError),
    #[error("Expected integer but found {0}")]
    IntegerParsingFailed(String),
    #[error("Error while parsing: {0}")]
    ParsingFailed(String),
}
#[derive(Debug)]
pub struct Parser<'a> {
    pub lexer: Lexer<'a>,
    pub curr_token: Token,
    pub peek_token: Token,
    // pub prefix_parse_fns: HashMap<Token, dyn Fn<(), Output=Expression>>,
    // pub suffix_parse_fns: HashMap<Token, dyn Fn<Expression, Output = Expression>>,
}
#[derive(PartialOrd, PartialEq)]
pub enum Precedence {
    Lowest,
    Equals,
    LessGreater, // > or <
    Sum,         // +
    Product,     // *
    Prefix,      // -X or !X
    Call,
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
    pub fn peek_precedence(&self) -> Precedence {
        self.peek_token.precedence()
    }
    pub fn curr_precedence(&self) -> Precedence {
        self.curr_token.precedence()
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
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
        // Err(ParseError::ParsingFailed)
    }
    pub fn parse_let_statement(&mut self) -> ParseResult<Statement> {
        let name = self.expect_ident()?;
        let value;
        self.next_token();
        self.expect_peek(Token::Assign)?;
        while !self.is_curr_token(Token::Semicolon) {
            self.next_token();
        }
        value = Expression::Something;
        Ok(Statement::Let(LetStatement { name, value }))
    }
    pub fn parse_return_statement(&mut self) -> ParseResult<Statement> {
        let expression = Expression::Something;
        self.next_token();
        while &self.curr_token != &Token::Semicolon {
            self.next_token();
        }
        Ok(Statement::Return(ReturnStatement { value: expression }))
    }
    pub fn parse_expression_statement(&mut self) -> ParseResult<Statement> {
        let exp = ExpressionStatement {
            expression: self.parse_expression(Precedence::Lowest)?,
        };
        if self.peek_token == Token::Semicolon {
            self.next_token();
        }
        Ok(Statement::Expression(exp))
    }
    pub fn parse_expression(&mut self, precedence: Precedence) -> ParseResult<Expression> {
        // Get the corresponding prefix expression parser
        let prefix = self.prefix_fn();
        if prefix.is_none() {
            Err(ParseError::ParsingFailed(
                "Prefix operator was not found".to_string(),
            ))?
        }
        let mut left = prefix.unwrap()(self)?;
        while !self.is_peek_token(Token::Semicolon) && precedence < self.peek_precedence() {
            // Get the corresponding infix expression parser
            if let Some(infix) = self.infix_fn() {
                self.next_token();
                left = infix(self, left)?;
            } else {
                return Ok(left);
            }
        }
        Ok(left)
    }
    pub fn parse_identifier(parser: &mut Parser) -> ParseResult<Expression> {
        if let Token::Ident(ident) = &parser.curr_token {
            return Ok(Expression::Identifier(ident.clone()));
        }
        Err(ParseError::InvalidIdentifier(parser.curr_token.to_string()))?
    }
    pub fn prefix_fn(&self) -> Option<PrefixFn> {
        match &self.curr_token {
            Token::Ident(_) => Some(Parser::parse_identifier),
            Token::Int(_) => Some(Parser::parse_integer),
            Token::Bang | Token::Minus => Some(Parser::parse_prefix_expression),
            _ => None,
        }
    }
    pub fn infix_fn(&self) -> Option<InfixFn> {
        match &self.peek_token {
            Token::Plus
            | Token::Minus
            | Token::Slash
            | Token::Asterisk
            | Token::Eq
            | Token::NotEq
            | Token::Lt
            | Token::Gt => Some(Parser::parse_infix_expression),
            _ => None,
        }
    }
    // Currently not being used
    pub fn parse_integer(parser: &mut Parser) -> ParseResult<Expression> {
        if let Token::Int(number) = &parser.curr_token {
            return Ok(Expression::Integer(*number));
        }
        Err(ParseError::IntegerParsingFailed(
            parser.curr_token.to_string(),
        ))?
    }
    pub fn parse_prefix_expression(parser: &mut Parser) -> ParseResult<Expression> {
        let operator = parser.curr_token.to_string();
        parser.next_token();
        let right_exp = parser.parse_expression(Precedence::Prefix)?;
        Ok(Expression::Prefix(Box::new(PrefixExpression {
            operator,
            right: Box::new(right_exp),
        })))
    }
    /**
    Finds the current token precedence and parses the infix expression it accordingly

    */
    pub fn parse_infix_expression(
        parser: &mut Parser,
        left: Expression,
    ) -> ParseResult<Expression> {
        let operator = parser.curr_token.to_string();
        let precedence = parser.curr_precedence();
        parser.next_token();
        let right = parser.parse_expression(precedence)?;
        Ok(Expression::Infix(Box::new(InfixExpression {
            operator,
            left: Box::new(left),
            right: Box::new(right),
        })))
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
        Err(ParseError::LetSyntaxError(
            "Assignment operator not found in the let statement".to_string(),
        ))
    }
    fn expect_ident(&mut self) -> Result<String, ParseError> {
        match &self.peek_token {
            Token::Ident(name) => Ok(name.clone()),
            _ => Err(ParseError::LetSyntaxError(format!(
                "Expected next token to be identifier but found {:?}",
                &self.peek_token
            ))),
        }
    }
}

mod tests {
    use crate::ast::{
        Expression, ExpressionStatement, InfixExpression, LetStatement, PrefixExpression,
        ReturnStatement, Statement,
    };
    use crate::lexer::Lexer;
    use crate::parser::{ParseError, Parser};
    use crate::token::Token;
    use std::ops::Add;

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
                value: Expression::Something,
            }),
            Statement::Let(LetStatement {
                name: String::from("y"),
                value: Expression::Something,
            }),
            Statement::Let(LetStatement {
                name: String::from("foobar"),
                value: Expression::Something,
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
        assert_eq!(
            program,
            Err(ParseError::LetSyntaxError(format!(
                "Expected next token to be identifier but found {:?}",
                Token::Assign
            )))
        );
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
    #[test]
    fn test_prefix_parsing() {
        struct Test<'a> {
            input: &'a str,
            operator: &'a str,
            value: i64,
        }
        let tests = vec![
            Test {
                input: "!5",
                operator: "!",
                value: 5,
            },
            Test {
                input: "-15",
                operator: "-",
                value: 15,
            },
        ];
        for test in tests {
            let input = &test.input.clone();
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            assert!(program.is_ok());
            let statements = program.unwrap().statements;
            assert!(!statements.is_empty());
            let expected = Statement::Expression(ExpressionStatement {
                expression: Expression::Prefix(Box::from(PrefixExpression {
                    operator: test.operator.to_string(),
                    right: Box::new(Expression::Integer(test.value)),
                })),
            });
            assert_eq!(expected, statements[0])
        }
    }
    #[test]
    fn test_infix_parsing() {
        struct Test<'a> {
            input: &'a str,
            left_value: i64,
            operator: &'a str,
            right_value: i64,
        }
        let tests = vec![
            Test {
                input: "5 + 10;",
                operator: "+",
                left_value: 5,
                right_value: 10,
            },
            Test {
                input: "5 - 10;",
                operator: "-",
                left_value: 5,
                right_value: 10,
            },
            Test {
                input: "5 * 10;",
                operator: "*",
                left_value: 5,
                right_value: 10,
            },
            Test {
                input: "5 / 10;",
                operator: "/",
                left_value: 5,
                right_value: 10,
            },
            Test {
                input: "5 < 10;",
                operator: "<",
                left_value: 5,
                right_value: 10,
            },
            Test {
                input: "5 > 10;",
                operator: ">",
                left_value: 5,
                right_value: 10,
            },
            Test {
                input: "5 == 10;",
                operator: "==",
                left_value: 5,
                right_value: 10,
            },
            Test {
                input: "5 != 10;",
                operator: "!=",
                left_value: 5,
                right_value: 10,
            },
        ];
        for test in tests {
            let input = &test.input.clone();
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            assert!(program.is_ok());
            let statements = program.unwrap().statements;
            assert!(!statements.is_empty());
            let expected = Statement::Expression(ExpressionStatement {
                expression: Expression::Infix(Box::new(InfixExpression {
                    left: Box::from(Expression::Integer(test.left_value)),
                    operator: test.operator.to_string(),
                    right: Box::from(Expression::Integer(test.right_value)),
                })),
            });
            println!("{}", statements[0]);
            assert_eq!(expected, statements[0])
        }
    }
    #[test]
    fn test_infix_parsing_2() {
        struct Test<'a> {
            input: &'a str,
            expected: &'a str,
        }
        impl Test<'_> {
            fn new<'a>(input: &'a str, expected: &'a str) -> Test<'a> {
                Test { input, expected }
            }
        }
        let tests = vec![
            Test::new("-a * b", "((- a) * b)"),
            Test::new("!-a", "(! (- a))"),
            Test::new("a + b + c", "((a + b) + c)"),
            Test::new("a + b - c", "((a + b) - c)"),
            Test::new("a * b * c", "((a * b) * c)"),
            Test::new("a * b / c", "((a * b) / c)"),
            Test::new("a + b / c", "(a + (b / c))"),
            Test::new("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            Test::new("3 + 4; -5 * 5", "(3 + 4)((- 5) * 5)"),
            Test::new("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            Test::new(
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            Test::new(
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
        ];
        for test in tests {
            let input = &test.input.clone();
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            assert!(program.is_ok());
            let statements = program.unwrap().statements;
            assert!(!statements.is_empty());
            // println!("{}", statements.concat());
            let mut joined_string = String::new();
            for statement in &statements {
                joined_string = joined_string.add(&*statement.to_string());
            }
            assert_eq!(joined_string, test.expected);
        }
    }
}
