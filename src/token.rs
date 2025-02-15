use std::fmt::{Display, Formatter};

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Token {
    Illegal,
    Eof,

    // Identifiers + literals
    Ident(String),
    Int(i64),

    // Operators
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    Eq,
    NotEq,

    Lt,
    Gt,

    // Delimiters
    Comma,
    Semicolon,

    LParen,
    RParen,
    LBrace,
    RBrace,

    // Keywords
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
}

impl Token {
    pub fn lookup_ident(ident: String) -> Token {
        match ident.as_str() {
            "fn" => Token::Function,
            "let" => Token::Let,
            "true" => Token::True,
            "false" => Token::False,
            "if" => Token::If,
            "else" => Token::Else,
            "return" => Token::Return,
            _ => Token::Ident(ident.to_string())
        }
    }
}
impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let string = match self {
            Token::Illegal => { "Illegal" }
            Token::Eof => { "Eof" }
            Token::Ident(ident) => { &*format!("ident: {}", ident) }
            Token::Int(value) => { &*format!("int: {}", value) }
            Token::Assign => { "assign" }
            Token::Plus => { "plus" }
            Token::Minus => { "minus" }
            Token::Bang => { "bang!" }
            Token::Asterisk => { "Asterisk *" }
            Token::Slash => { "Slash /" }
            Token::Eq => { "Equal" }
            Token::NotEq => { "Not Equal" }
            Token::Lt => { "Less than" }
            Token::Gt => { "Greater Than" }
            Token::Comma => { "Comma" }
            Token::Semicolon => { "Semicolon" }
            Token::LParen => { "LParen (" }
            Token::RParen => { "RParen )" }
            Token::LBrace => { "LBrace {" }
            Token::RBrace => { "RBrace }" }
            Token::Function => { "fn" }
            Token::Let => { "let" }
            Token::True => { "true" }
            Token::False => { "false" }
            Token::If => { "if" }
            Token::Else => { "else" }
            Token::Return => { "return" }
        };
        write!(f, "{string}")
    }
}