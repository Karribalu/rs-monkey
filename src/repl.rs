use crate::lexer::Lexer;
use crate::parser::Parser;
use std::io;
use crate::evaluator::eval_program;

const MONKEY_ART: &str = r#"
            __,__
   .--.  .-"     "-.  .--.
  / .. \/  .-. .-.  \/ .. \
 | |  '|  /   Y   \  |'  | |
 | \   \  \ 0 | 0 /  /   / |
  \ '- ,\.-"`` ``"-./, -' /
   `'-' /_   ^ ^   _\ '-'`
       |  \._   _./  |
       \   \ `~` /   /
        '._ '-=-' _.'
           '~---~'"#;
pub fn start<R: io::BufRead, W: io::Write>(mut reader: R, mut writer: W) -> io::Result<()> {
    println!("Hello I am bala, Feel free to type any text or commands");
    loop {
        writer.write(b">> ")?;
        writer.flush()?;
        let mut line = String::new();
        reader.read_line(&mut line)?;
        // Get the tokens from given line of code
        let lexer = Lexer::new(line.as_str());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        match &program {
            Ok(result) => {
                let evaluated = eval_program(result);
                println!("Evaluated result: {}", evaluated);
                print!("{}", result);
            }
            Err(error) => {
                println!("An error occurred {:?}\n {}", error, MONKEY_ART);
            }
        }
    }
}
