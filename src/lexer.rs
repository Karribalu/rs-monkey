use crate::token::Token;
use std::iter::Peekable;
use std::ops::Add;
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
                '=' => Token::ASSIGN,
                ';' => Token::SEMICOLON,
                '(' => Token::LPAREN,
                ')' => Token::RPAREN,
                ',' => Token::COMMA,
                '+' => Token::PLUS,
                '{' => Token::LBRACE,
                '}' => Token::RBRACE,
                '!' => Token::BANG,
                '-' => Token::MINUS,
                '/' => Token::SLASH,
                '*' => Token::ASTERISK,
                '<' => Token::LT,
                '>' => Token::GT,
                _ => {
                    if is_letter(&token) {
                        let ident = self.read_identifier();
                        // We are returning as we don't want to read an extra character after parsing the number
                        return Token::lookup_ident(ident);
                    } else if token.is_numeric() {
                        let ident = self.read_number();
                        // We are returning as we don't want to read an extra character after parsing the number
                        return Token::INT(ident);
                    } else {
                        Token::ILLEGAL
                    }
                }
            };
            self.read_char();
            return token;
        }
        Token::EOF
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
            ";
        let mut lexer = Lexer::new(input);
        let expected = vec![
            Token::LET,
            Token::IDENT(String::from("five")),
            Token::ASSIGN,
            Token::INT(5),
            Token::SEMICOLON,
            Token::LET,
            Token::IDENT(String::from("ten")),
            Token::ASSIGN,
            Token::INT(10),
            Token::SEMICOLON,
            Token::LET,
            Token::IDENT(String::from("add")),
            Token::ASSIGN,
            Token::FUNCTION,
            Token::LPAREN,
            Token::IDENT(String::from("x")),
            Token::COMMA,
            Token::IDENT(String::from("y")),
            Token::RPAREN,
            Token::LBRACE,
            Token::IDENT(String::from("x")),
            Token::PLUS,
            Token::IDENT(String::from("y")),
            Token::SEMICOLON,
            Token::RBRACE,
            Token::SEMICOLON,
            Token::LET,
            Token::IDENT(String::from("result")),
            Token::ASSIGN,
            Token::IDENT(String::from("add")),
            Token::LPAREN,
            Token::IDENT(String::from("five")),
            Token::COMMA,
            Token::IDENT(String::from("ten")),
            Token::RPAREN,
            Token::SEMICOLON,
            Token::BANG,
            Token::MINUS,
            Token::SLASH,
            Token::ASTERISK,
            INT(5),
            Token::SEMICOLON,
            INT(5),
            Token::LT,
            INT(10),
            Token::GT,
            INT(5),
            Token::SEMICOLON
        ];

        for i in 0..expected.len() {
            let actual = lexer.next_token();
            println!("Current : {:?} actual: {:?}", expected[i], actual);
            assert_eq!(expected[i], actual);
        }
    }
    #[test]
    fn some_test() {
        let some = '_';
        let other = 'a';

        // assert!(some.is_alphanumeric());
        assert!(other.is_alphanumeric());
    }
}
