use crate::ast::Node;
use crate::evaluator::eval;
use crate::lexer::Lexer;
use crate::parser::Parser;
use std::io;
use crate::object::Environment;

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
    let mut env = Environment::new();
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
                let evaluated = eval(&Node::Program(Box::new(result.clone())), &mut env);
                println!("Evaluated result: {:?}", evaluated);
                print!("{}", result);
            }
            Err(error) => {
                println!("An error occurred {:?}\n {}", error, MONKEY_ART);
            }
        }
    }
}
