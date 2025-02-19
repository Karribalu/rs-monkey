use crate::ast::{Expression, Node, PrefixExpression, Program, Statement};
use crate::object::Object;
use thiserror::Error;

type EvalResult<T> = Result<T, EvalError>;
#[derive(Error, Debug)]
enum EvalError {
    #[error("Error occurred while evaluating prefix {0}")]
    PrefixEvalFailed(String),
    #[error("Error occurred while evaluating bang {0}")]
    BangEvalFailed(String),
}
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
        Expression::Boolean(boolean) => Object::Boolean(*boolean),
        Expression::Prefix(prefix_expression) => {
            let right = eval_expression(&*prefix_expression.right);
            eval_prefix_expression(&*prefix_expression, right)
        }
        _ => Object::Null,
    }
}
fn eval_prefix_expression(expression: &PrefixExpression, right: Object) -> Object {
    match expression.operator.as_str() {
        "!" => eval_bang_operator(right),
        _ => Object::Null,
    }
}
fn eval_bang_operator(object: Object) -> Object {
    match object {
        Object::Integer(_) => Object::Boolean(false),
        Object::Boolean(boolean) => Object::Boolean(!boolean),
        Object::Null => Object::Boolean(true),
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
    #[derive(Debug)]
    struct Test<'a, I: ?Sized, E> {
        input: &'a I,
        expected: E,
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
    #[test]
    fn test_eval_boolean() {
        let tests = vec![
            Test {
                input: "true",
                expected: true,
            },
            Test {
                input: "false",
                expected: false,
            },
        ];
        for test in tests {
            let evaluated = test_eval(test.input);
            assert_eq!(evaluated, Object::Boolean(test.expected))
        }
    }
    #[test]
    fn test_bang_operator() {
        let tests = vec![
            Test {
                input: "!true",
                expected: false,
            },
            Test {
                input: "!false",
                expected: true,
            },
            Test {
                input: "!!false",
                expected: false,
            },
            Test {
                input: "!!true",
                expected: true,
            },
            Test {
                input: "!5",
                expected: false,
            },
            Test {
                input: "!!5",
                expected: true,
            },
        ];
        for test in tests {
            let evaluated = test_eval(test.input);
            assert_eq!(evaluated, Object::Boolean(test.expected))
        }
    }
}
