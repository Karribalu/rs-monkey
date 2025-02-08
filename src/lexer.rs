use crate::token::Token;
use std::iter::Peekable;
use std::str::Chars;
#[derive(Debug)]
struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            input: input.chars().peekable(),
        }
    }
    fn read_char(&mut self) -> Option<char> {
        self.input.next()
    }
    fn read_identifier(&mut self) -> String {
        let mut ident = String::new();
        while self.input.peek().is_some() && is_letter(self.input.peek().unwrap()) {
            ident.push(self.read_char().unwrap());
        }
        ident
    }

    fn read_number(&mut self) -> i64 {
        let mut number = 0;
        // println!("hello  {:?}", self.input.peek());
        while self.input.peek().is_some() && self.input.peek().unwrap().is_numeric() {
            number = (number * 10) + self.read_char().unwrap().to_digit(10).unwrap() as i64;
        }
        number
    }
    fn eat_white_space(&mut self) {
        while self.input.peek().is_some() && self.input.peek().unwrap().is_whitespace() {
            self.read_char();
        }
    }
    fn next_token(&mut self) -> Token {
        self.eat_white_space();
        let token = self.input.peek();
        if let Some(token) = token {
            let token = match token {
                '=' => {
                    // Handle assignment and equal cases
                    self.read_char();
                    if let Some(next_char) = self.input.peek() {
                        if next_char == &'=' {
                            self.read_char();
                            return Token::Eq;
                        }
                    }
                    return Token::Assign;
                }
                ';' => Token::Semicolon,
                '(' => Token::LParen,
                ')' => Token::RParen,
                ',' => Token::Comma,
                '+' => Token::Plus,
                '{' => Token::LBrace,
                '}' => Token::RBrace,
                '!' => {
                    // Handle bang and not equals
                    self.read_char();
                    if let Some(next_char) = self.input.peek() {
                        if next_char == &'=' {
                            self.read_char();
                            return Token::NotEq;
                        }
                    }
                    return Token::Bang;
                }
                '-' => Token::Minus,
                '/' => Token::Slash,
                '*' => Token::Asterisk,
                '<' => Token::Lt,
                '>' => Token::Gt,
                _ => {
                    if is_letter(&token) {
                        let ident = self.read_identifier();
                        // We are returning as we don't want to read an extra character after parsing the number
                        return Token::lookup_ident(ident);
                    } else if token.is_numeric() {
                        let ident = self.read_number();
                        // We are returning as we don't want to read an extra character after parsing the number
                        return Token::Int(ident);
                    } else {
                        Token::Illegal
                    }
                }
            };
            self.read_char();
            return token;
        }
        Token::Eof
    }
}
fn is_letter(character: &char) -> bool {
    character.is_alphabetic() || character == &'_'
}

mod tests {
    use crate::lexer::Lexer;
    use crate::token::Token;

    #[test]
    fn test_next_token() {
        let input = "let five = 5;
            let ten = 10;
            let add = fn(x, y) {
                x + y;
            };
            let result = add(five, ten);
            !-/*5;
            5 < 10 > 5;
            if (5 < 10){
                return true;
            } else {
                return false;
            }

            10 == 10;
            10 != 9;
            ";
        let mut lexer = Lexer::new(input);
        let expected = vec![
            Token::Let,
            Token::Ident(String::from("five")),
            Token::Assign,
            Token::Int(5),
            Token::Semicolon,
            Token::Let,
            Token::Ident(String::from("ten")),
            Token::Assign,
            Token::Int(10),
            Token::Semicolon,
            Token::Let,
            Token::Ident(String::from("add")),
            Token::Assign,
            Token::Function,
            Token::LParen,
            Token::Ident(String::from("x")),
            Token::Comma,
            Token::Ident(String::from("y")),
            Token::RParen,
            Token::LBrace,
            Token::Ident(String::from("x")),
            Token::Plus,
            Token::Ident(String::from("y")),
            Token::Semicolon,
            Token::RBrace,
            Token::Semicolon,
            Token::Let,
            Token::Ident(String::from("result")),
            Token::Assign,
            Token::Ident(String::from("add")),
            Token::LParen,
            Token::Ident(String::from("five")),
            Token::Comma,
            Token::Ident(String::from("ten")),
            Token::RParen,
            Token::Semicolon,
            Token::Bang,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::Int(5),
            Token::Semicolon,
            Token::Int(5),
            Token::Lt,
            Token::Int(10),
            Token::Gt,
            Token::Int(5),
            Token::Semicolon,
            // IF Block starts
            Token::If,
            Token::LParen,
            Token::Int(5),
            Token::Lt,
            Token::Int(10),
            Token::RParen,
            Token::LBrace,
            Token::Return,
            Token::True,
            Token::Semicolon,
            Token::RBrace,
            Token::Else,
            Token::LBrace,
            Token::Return,
            Token::False,
            Token::Semicolon,
            Token::RBrace,

            Token::Int(10),
            Token::Eq,
            Token::Int(10),
            Token::Semicolon,
            Token::Int(10),
            Token::NotEq,
            Token::Int(9),
            Token::Semicolon
        ];
        for i in 0..expected.len() {
            let actual = lexer.next_token();
            println!("Current : {:?} actual: {:?}", expected[i], actual);
            assert_eq!(expected[i], actual);
        }
    }
    #[test]
    fn some_test() {
        let other = 'a';

        // assert!(some.is_alphanumeric());
        assert!(other.is_alphanumeric());
    }
}
