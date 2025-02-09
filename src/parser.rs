use thiserror::Error;
use crate::ast::{Expression, Identifier, LetStatement, Program, Statement};
use crate::lexer::Lexer;
use crate::token::Token;
type ParseResult<T> = Result<T, ParseError>;
#[derive(Error, Debug)]
pub enum ParseError {
    #[error("Error while parsing the let statement: {0}")]
    LetSyntaxError(String),
    #[error("Error while parsing")]
    ParsingFailed,
}
#[derive(Debug)]
pub struct Parser<'a> {
    pub lexer: Lexer<'a>,
    pub curr_token: Token,
    pub peek_token: Token,
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

    pub fn parse_program(&mut self) -> ParseResult<Program> {
        print!("Called");
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
            Token::Illegal => {}
            Token::Eof => {}
            Token::Ident(_) => {}
            Token::Int(_) => {}
            Token::Assign => {}
            Token::Plus => {}
            Token::Minus => {}
            Token::Bang => {}
            Token::Asterisk => {}
            Token::Slash => {}
            Token::Eq => {}
            Token::NotEq => {}
            Token::Lt => {}
            Token::Gt => {}
            Token::Comma => {}
            Token::Semicolon => {}
            Token::LParen => {}
            Token::RParen => {}
            Token::LBrace => {}
            Token::RBrace => {}
            Token::Function => {}
            Token::Let => {
                return self.parse_let_statement();
            }
            Token::True => {}
            Token::False => {}
            Token::If => {}
            Token::Else => {}
            Token::Return => {}
        }
        Err(ParseError::ParsingFailed)
    }
    pub fn parse_let_statement(&mut self) -> ParseResult<Statement> {
        let mut stmt = LetStatement {
            name: Identifier { value: "".to_string() },
            value: Expression::Something,
        };
        let identifier = self.expect_ident()?;
        stmt.name = Identifier {
            value: identifier
        };
        self.next_token();
        self.expect_peek(Token::Assign)?;
        while !self.is_curr_token(Token::Semicolon) {
            self.next_token();
        }
        Ok(Statement::Let(stmt))
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
                Err(ParseError::LetSyntaxError("Identifier not found for let statement".to_string()))
            }
        }
    }
}


mod tests {
    use crate::ast::{Expression, Identifier, LetStatement, Statement};
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    #[test]
    fn basic_test() {
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
                name: Identifier {
                    value: String::from("x")
                },
                value: Expression::Something
            }),
            Statement::Let(LetStatement {
                name: Identifier {
                    value: String::from("y")
                },
                value: Expression::Something
            }),
            Statement::Let(LetStatement {
                name: Identifier {
                    value: String::from("foobar")
                },
                value: Expression::Something
            }),
        ];
        for (idx, expected_statement) in expected.iter().enumerate() {
            let actual_statement = &statements[idx];
            assert_eq!(expected_statement, actual_statement)
        }
    }
}

