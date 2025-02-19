use crate::ast::{Expression, Node, Program, Statement};
use crate::object::Object;

pub fn eval(program: &Node) -> Object {
    match program {
        Node::Program(program) => eval_program(*&program),
        Node::Expression(expression) => eval_expression(expression),
        Node::Statement(statement) => eval_statement(statement),
    }
}
pub fn eval_program(program: &Program) -> Object {
    let mut res = Object::Null;

    for statement in &program.statements {
        res = eval_statement(statement);
    }
    res
}
fn eval_expression(expression: &Expression) -> Object {
    match expression {
        Expression::Integer(integer) => Object::Integer(*integer),
        _ => Object::Null,
    }
}
fn eval_statement(statement: &Statement) -> Object {
    match statement {
        Statement::Let(_) => {}
        Statement::Return(_) => {}
        Statement::Expression(expression) => {
            return eval_expression(&expression.expression);
        }
    }
    Object::Null
}
mod tests {
    use crate::evaluator::eval;
    use crate::object::Object;
    use crate::parser::parse;

    struct Test<'a> {
        input: &'a str,
        expected: i64,
    }
    fn test_eval(input: &str) -> Object {
        let parse_result = parse(input);
        match parse_result {
            Ok(node) => eval(&node),
            Err(error) => {
                println!("Program parsing failed");
                Object::Null
            }
        }
    }
    #[test]
    fn test_eval_integer() {
        let tests = vec![
            Test {
                input: "5",
                expected: 5,
            },
            Test {
                input: "10",
                expected: 10,
            },
        ];
        for test in tests {
            let evaluated = test_eval(test.input);
            assert_eq!(evaluated, Object::Integer(test.expected))
        }
    }
}
