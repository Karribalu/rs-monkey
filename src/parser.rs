use crate::ast::{
    BlockStatement, CallExpression, Expression, ExpressionStatement, FunctionLiteral,
    IdentifierExpression, IfExpression, InfixExpression, LetStatement, PrefixExpression, Program,
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
    #[error("Expected identifier but found {0}")]
    InvalidIdentifier(String),
    #[error(transparent)]
    InvalidIntegerParse(#[from] ParseIntError),
    #[error("Expected integer but found {0}")]
    IntegerParsingFailed(String),
    #[error("Expected boolean but found {0}")]
    InvalidBoolean(String),
    #[error("Error while parsing: {0}")]
    ParsingFailed(String),
}
#[derive(Debug)]
pub struct Parser<'a> {
    pub lexer: Lexer<'a>,
    pub curr_token: Token,
    pub peek_token: Token,
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
            // The last token for block `}` will be skipped here
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
    }
    pub fn parse_let_statement(&mut self) -> ParseResult<Statement> {
        let name = self.expect_ident()?;
        let value;
        self.next_token();
        self.expect_peek(Token::Assign)?;
        self.next_token();
        value = self.parse_expression(Precedence::Lowest)?;
        while !self.is_curr_token(Token::Semicolon) {
            self.next_token();
        }
        Ok(Statement::Let(LetStatement { name, value }))
    }
    pub fn parse_return_statement(&mut self) -> ParseResult<Statement> {
        self.next_token();
        let expression = self.parse_expression(Precedence::Lowest)?;
        if self.is_peek_token(Token::Semicolon) {
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
            Token::True | Token::False => Some(Parser::parse_boolean),
            Token::LParen => Some(Parser::parse_grouped_expressions),
            Token::If => Some(Parser::parse_if_expression),
            Token::Function => Some(Parser::parse_fn_statement),
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
            Token::LParen => Some(Parser::parse_call_expression),
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

    pub fn parse_boolean(parser: &mut Parser) -> ParseResult<Expression> {
        if parser.curr_token == Token::True || parser.curr_token == Token::False {
            return Ok(Expression::Boolean(parser.curr_token == Token::True));
        }
        Err(ParseError::InvalidBoolean(parser.curr_token.to_string()))
    }
    pub fn parse_grouped_expressions(parser: &mut Parser) -> ParseResult<Expression> {
        if parser.curr_token != Token::LParen {
            Err(ParseError::ParsingFailed("L Paren not found".to_string()))?
        }
        parser.next_token();
        let exp = parser.parse_expression(Precedence::Lowest)?;
        parser.expect_peek(Token::RParen)?;
        Ok(exp)
    }
    pub fn parse_if_expression(parser: &mut Parser) -> ParseResult<Expression> {
        parser.expect_peek(Token::LParen)?;
        parser.next_token();

        let condition = parser.parse_expression(Precedence::Lowest)?;

        parser.expect_peek(Token::RParen)?;
        parser.expect_peek(Token::LBrace)?;

        let consequence = parser.parse_block_statement()?;
        let mut alternate = None;
        // The cursor is at `}`
        if parser.is_peek_token(Token::Else) {
            parser.next_token();
            parser.expect_peek(Token::LBrace)?;
            alternate = Some(parser.parse_block_statement()?);
        }
        Ok(Expression::If(Box::new(IfExpression {
            condition,
            consequence,
            alternative: alternate,
        })))
    }
    pub fn parse_block_statement(&mut self) -> ParseResult<BlockStatement> {
        let mut statements: Vec<Statement> = vec![];
        // We will be at before { i.e., at condition end at if or at else part,
        // Advance to move the cursor to the `{`
        self.next_token();

        while !self.is_curr_token(Token::RBrace) && !self.is_curr_token(Token::Eof) {
            let stmt = self.parse_statement()?;
            statements.push(stmt);
            self.next_token();
        }
        Ok(BlockStatement { statements })
    }
    pub fn parse_fn_statement(parser: &mut Parser) -> ParseResult<Expression> {
        parser.expect_peek(Token::LParen)?;
        parser.next_token();
        let mut parameters = vec![];
        while !parser.is_curr_token(Token::RParen) && !parser.is_curr_token(Token::Eof) {
            if parser.is_curr_token(Token::Comma) {
                parser.next_token();
                continue;
            }
            parameters.push(IdentifierExpression {
                value: parser.curr_token.clone().to_string(),
            });
            parser.next_token();
        }
        if parser.is_curr_token(Token::Eof) {
            // Function ended abruptly;
            Err(ParseError::ParsingFailed(
                "Expected the function block".to_string(),
            ))?
        }
        parser.next_token();
        let body = parser.parse_block_statement()?;
        Ok(Expression::Function(FunctionLiteral {
            parameters,
            body: Box::new(body),
        }))
    }
    fn parse_call_expression(parser: &mut Parser, function: Expression) -> ParseResult<Expression> {
        let arguments = parser.parse_call_arguments()?;
        Ok(Expression::Call(CallExpression {
            arguments,
            function: Box::new(function),
        }))
    }
    fn parse_call_arguments(&mut self) -> ParseResult<Vec<Box<Expression>>> {
        let mut arguments = vec![];
        if self.is_peek_token(Token::RParen) {
            self.next_token();
            return Ok(arguments);
        }
        self.next_token();
        arguments.push(Box::new(self.parse_expression(Precedence::Lowest)?));
        while self.is_peek_token(Token::Comma) {
            self.next_token();
            self.next_token();
            arguments.push(Box::new(self.parse_expression(Precedence::Lowest)?));
        }
        self.expect_peek(Token::RParen)?;

        Ok(arguments)
    }
    fn is_curr_token(&self, token: Token) -> bool {
        self.curr_token == token
    }
    fn is_peek_token(&self, token: Token) -> bool {
        self.peek_token == token
    }
    fn expect_peek(&mut self, token: Token) -> ParseResult<()> {
        if self.is_peek_token(token.clone()) {
            self.next_token();
            return Ok(());
        }
        Err(ParseError::ParsingFailed(format!(
            "Expected {} but found {}",
            &token, self.peek_token
        )))
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
    use crate::ast::Expression::Identifier;
    use crate::ast::{
        BlockStatement, CallExpression, Expression, ExpressionStatement, FunctionLiteral,
        IdentifierExpression, IfExpression, InfixExpression, LetStatement, PrefixExpression,
        Program, ReturnStatement, Statement,
    };
    use crate::lexer::Lexer;
    use crate::parser::{ParseError, ParseResult, Parser};
    use crate::token::Token;
    use std::ops::Add;

    struct Test<'a> {
        input: &'a str,
        expected: &'a str,
    }
    impl Test<'_> {
        fn new<'a>(input: &'a str, expected: &'a str) -> Test<'a> {
            Test { input, expected }
        }
    }
    fn preload(inp: &str) -> ParseResult<Program> {
        let lexer = Lexer::new(inp);
        let mut parser = Parser::new(lexer);
        parser.parse_program()
    }
    #[test]
    fn test_let_parser() {
        let input = "let x = 5;
            let y = true;
            let foobar = 838383;";
        let program = preload(input);

        assert!(program.is_ok());
        let statements = program.unwrap().statements;
        assert_eq!(statements.len(), 3);
        let expected = vec![
            Statement::Let(LetStatement {
                name: String::from("x"),
                value: Expression::Integer(5),
            }),
            Statement::Let(LetStatement {
                name: String::from("y"),
                value: Expression::Boolean(true),
            }),
            Statement::Let(LetStatement {
                name: String::from("foobar"),
                value: Expression::Integer(838383),
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
        let program = preload(input);
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
            return true;
            return add(x,y);";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        assert!(program.is_ok());
        let statements = program.unwrap().statements;
        let expected = vec![
            Statement::Return(ReturnStatement {
                value: Expression::Integer(10),
            }),
            Statement::Return(ReturnStatement {
                value: Expression::Boolean(true),
            }),
            Statement::Return(ReturnStatement {
                value: Expression::Call(CallExpression {
                    function: Box::from(Identifier("add".to_string())),
                    arguments: vec![
                        Box::new(Expression::Identifier("x".to_string())),
                        Box::new(Expression::Identifier("y".to_string())),
                    ],
                }),
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
            let program = preload(&test.input);
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
            let program = preload(&test.input);
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
            let program = preload(&test.input);
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
    #[test]
    fn test_boolean_parsing() {
        let tests = vec![
            Test::new("true;", "true"),
            Test::new("false;", "false"),
            Test::new("10 > 15 == false", "((10 > 15) == false)"),
            Test::new("10 < 12 == true", "((10 < 12) == true)"),
        ];
        let statement_tests = vec![
            vec![Statement::Expression(ExpressionStatement {
                expression: Expression::Boolean(true),
            })],
            vec![Statement::Expression(ExpressionStatement {
                expression: Expression::Boolean(false),
            })],
            vec![Statement::Expression(ExpressionStatement {
                expression: Expression::Infix(Box::from(InfixExpression {
                    left: Box::from(Expression::Infix(Box::from(InfixExpression {
                        left: Box::from(Expression::Integer(10)),
                        operator: ">".to_string(),
                        right: Box::from(Expression::Integer(15)),
                    }))),
                    operator: "==".to_string(),
                    right: Box::new(Expression::Boolean(false)),
                })),
            })],
            vec![Statement::Expression(ExpressionStatement {
                expression: Expression::Infix(Box::from(InfixExpression {
                    left: Box::from(Expression::Infix(Box::from(InfixExpression {
                        left: Box::from(Expression::Integer(10)),
                        operator: "<".to_string(),
                        right: Box::from(Expression::Integer(12)),
                    }))),
                    operator: "==".to_string(),
                    right: Box::new(Expression::Boolean(true)),
                })),
            })],
        ];
        for (idx, test) in tests.iter().enumerate() {
            let program = preload(&test.input);
            assert!(program.is_ok());
            let statements = program.unwrap().statements;
            assert!(!statements.is_empty());
            assert_eq!(statements[0].to_string(), test.expected);
            assert_eq!(statements, statement_tests[idx])
        }
    }
    #[test]
    fn test_operator_precedence_parsing() {
        let tests = vec![
            Test::new("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            Test::new("(5 + 5) * 2", "((5 + 5) * 2)"),
            Test::new("2 / (5 + 5)", "(2 / (5 + 5))"),
            Test::new("-(5 + 5)", "(- (5 + 5))"),
            Test::new("!(true == true)", "(! (true == true))"),
        ];
        for (_, test) in tests.iter().enumerate() {
            let program = preload(&test.input);
            assert!(program.is_ok());
            let statements = program.unwrap().statements;
            assert!(!statements.is_empty());
            assert_eq!(statements[0].to_string(), test.expected);
        }
    }
    #[test]
    fn test_if_expression() {
        let input = "if (x < y) {x}";
        let program = preload(input);
        assert!(program.is_ok());
        let statements = &program.clone().unwrap().statements;
        assert!(!statements.is_empty());
        let expected = vec![Statement::Expression(ExpressionStatement {
            expression: Expression::If(Box::new(IfExpression {
                condition: Expression::Infix(Box::new(InfixExpression {
                    left: Box::from(Identifier("x".to_string())),
                    operator: "<".to_string(),
                    right: Box::from(Identifier("y".to_string())),
                })),
                consequence: BlockStatement {
                    statements: vec![Statement::Expression(ExpressionStatement {
                        expression: Identifier("x".to_string()),
                    })],
                },
                alternative: None,
            })),
        })];
        for i in 0..expected.len() {
            assert_eq!(statements[i], expected[i]);
        }
    }
    #[test]
    fn test_if_else_expression() {
        let input = "if (x < y)\
        { x } else {\
            y
        }";
        let program = preload(input);
        assert!(program.is_ok());
        let statements = &program.clone().unwrap().statements;
        assert!(!statements.is_empty());
        let expected = vec![Statement::Expression(ExpressionStatement {
            expression: Expression::If(Box::new(IfExpression {
                condition: Expression::Infix(Box::new(InfixExpression {
                    left: Box::from(Identifier("x".to_string())),
                    operator: "<".to_string(),
                    right: Box::from(Identifier("y".to_string())),
                })),
                consequence: BlockStatement {
                    statements: vec![Statement::Expression(ExpressionStatement {
                        expression: Identifier("x".to_string()),
                    })],
                },
                alternative: Some(BlockStatement {
                    statements: vec![Statement::Expression(ExpressionStatement {
                        expression: Identifier("y".to_string()),
                    })],
                }),
            })),
        })];
        for i in 0..expected.len() {
            assert_eq!(statements[i], expected[i]);
        }
    }
    #[test]
    fn test_fn_literal_parsing() {
        let input = "fn(x, y) { x + y; }";
        let program = preload(input);
        assert!(program.is_ok());
        let statements = &program.clone().unwrap().statements;
        assert!(!statements.is_empty());
        let expected = vec![Statement::Expression(ExpressionStatement {
            expression: Expression::Function(FunctionLiteral {
                parameters: vec![
                    IdentifierExpression {
                        value: "x".to_string(),
                    },
                    IdentifierExpression {
                        value: "y".to_string(),
                    },
                ],
                body: Box::new(BlockStatement {
                    statements: vec![Statement::Expression(ExpressionStatement {
                        expression: Expression::Infix(Box::from(InfixExpression {
                            left: Box::new(Identifier("x".to_string())),
                            operator: "+".to_string(),
                            right: Box::new(Identifier("y".to_string())),
                        })),
                    })],
                }),
            }),
        })];
        for i in 0..expected.len() {
            assert_eq!(statements[i], expected[i]);
        }
    }
    #[test]
    fn test_more_fn_literal_parsing() {
        let input = "fn(x, y) { return a + b;}";
        let program = preload(input);
        assert!(program.is_ok());
        let statements = &program.clone().unwrap().statements;
        assert!(!statements.is_empty());
    }
    #[test]
    fn test_call_expression_parsing() {
        let input = "add(1, 2, 3*5)";
        let program = preload(input);
        let expected = vec![Statement::Expression(ExpressionStatement {
            expression: Expression::Call(CallExpression {
                function: Box::new(Identifier("add".to_string())),
                arguments: vec![
                    Box::new(Expression::Integer(1)),
                    Box::new(Expression::Integer(2)),
                    Box::new(Expression::Infix(Box::new(InfixExpression {
                        left: Box::from(Expression::Integer(3)),
                        operator: "*".to_string(),
                        right: Box::new(Expression::Integer(5)),
                    }))),
                ],
            }),
        })];
        assert!(program.is_ok());
        let statements = &program.clone().unwrap().statements;
        for i in 0..statements.len() {
            assert_eq!(statements[i], expected[i])
        }
    }
    #[test]
    fn test_more_call_expressions() {
        let tests = vec![
            Test::new("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            Test::new(
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            Test::new(
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
        ];
        for test in tests {
            let program = preload(&test.input);
            assert!(program.is_ok());
            let statements = program.unwrap().statements;
            let joined_statements = statements
                .iter()
                .map(|item| item.to_string())
                .collect::<Vec<String>>()
                .join("");
            assert!(!statements.is_empty());
            assert_eq!(joined_statements, test.expected);
        }
    }
}
