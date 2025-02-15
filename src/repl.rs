use crate::lexer::Lexer;
use crate::token::Token;
use std::io;

pub fn start<R: io::BufRead, W: io::Write>(mut reader: R, mut writer: W) -> io::Result<()> {
    println!("Hello I am bala, Feel free to type any text or commands");

    loop {
        writer.write(b">> ")?;
        writer.flush()?;
        let mut line = String::new();
        reader.read_line(&mut line)?;
        // Get the tokens from given line of code
        let mut lexer = Lexer::new(line.as_str());
        let mut token = lexer.next_token();
        while token != Token::Eof {
            println!("{:?}", token);
            token = lexer.next_token();
        }
    }
}
