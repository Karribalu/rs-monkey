use crate::ast::{
    BlockStatement, Expression, IfExpression, Node, PrefixExpression, Program, ReturnStatement,
    Statement,
};
use crate::object::Object;
use thiserror::Error;

pub type EvalResult<T> = Result<T, EvalError>;
#[derive(Error, Debug, Eq, PartialEq)]
pub enum EvalError {
    #[error("type mismatch: {0}")]
    TypeMismatch(String),
    #[error("unknown operator: {0}")]
    UnknownOperator(String),
}
pub fn eval(program: &Node) -> EvalResult<Object> {
    match program {
        Node::Program(program) => eval_program(&program),
        Node::Expression(expression) => eval_expression(expression),
        Node::Statement(statement) => eval_statement(statement),
    }
}
pub fn eval_program(program: &Program) -> EvalResult<Object> {
    let mut res = Object::Null;

    for statement in &program.statements {
        match statement {
            Statement::Let(_) => {}
            Statement::Return(_) => {
                // Return the value if one of the statements is a return statement
                let Object::Return(object) = eval_statement(statement)? else {
                    todo!()
                };
                return Ok(*object);
            }
            Statement::Expression(_) => {}
        }
        res = eval_statement(statement)?;
        if let Object::Return(return_statement) = res {
            return Ok(*return_statement);
        }
    }
    Ok(res)
}
fn eval_expression(expression: &Expression) -> EvalResult<Object> {
    match expression {
        Expression::Integer(integer) => Ok(Object::Integer(*integer)),
        Expression::Boolean(boolean) => Ok(Object::Boolean(*boolean)),
        Expression::Prefix(prefix_expression) => {
            let right = eval_expression(&*prefix_expression.right)?;
            eval_prefix_expression(&*prefix_expression, right)
        }
        Expression::Infix(infix) => {
            let left_res = eval_expression(&infix.left)?;
            let right_res = eval_expression(&infix.right)?;
            eval_infix_expression(&infix.operator, left_res, right_res)
        }
        Expression::If(if_expression) => eval_if_expression(&*if_expression),
        _ => Ok(Object::Null),
    }
}
fn eval_if_expression(if_expression: &IfExpression) -> EvalResult<Object> {
    // Evaluate the condition
    let condition_res = eval_expression(&if_expression.condition)?;
    if is_truthy(&condition_res) {
        eval_block_statement(&if_expression.consequence)
    } else if if_expression.alternative.is_some() {
        eval_block_statement(&if_expression.clone().alternative.unwrap())
    } else {
        Ok(Object::Null)
    }
}
fn is_truthy(condition: &Object) -> bool {
    match condition {
        Object::Integer(_) => true,
        Object::Boolean(boolean_res) => *boolean_res,
        Object::Null => false,
        Object::Return(return_object) => is_truthy(return_object),
    }
}
fn eval_prefix_expression(expression: &PrefixExpression, right: Object) -> EvalResult<Object> {
    match expression.operator.as_str() {
        "!" => Ok(eval_bang_operator(right)),
        "-" => eval_minus_prefix_operator(right),
        _ => Err(EvalError::UnknownOperator(format!(
            "{}{}",
            expression.operator,
            right.get_type()
        ))),
    }
}
fn eval_minus_prefix_operator(object: Object) -> EvalResult<Object> {
    match object {
        Object::Integer(integer) => Ok(Object::Integer(-1 * integer)),
        _ => Err(EvalError::UnknownOperator(format!(
            "-{}",
            object.get_type()
        ))),
    }
}
fn eval_bang_operator(object: Object) -> Object {
    match object {
        Object::Integer(_) => Object::Boolean(false),
        Object::Boolean(boolean) => Object::Boolean(!boolean),
        Object::Null => Object::Boolean(true),
        Object::Return(return_object) => eval_bang_operator(*return_object),
    }
}
fn eval_statement(statement: &Statement) -> EvalResult<Object> {
    match statement {
        Statement::Let(_) => {}
        Statement::Return(return_statement) => {
            return Ok(eval_return_statement(return_statement)?);
        }
        Statement::Expression(expression) => {
            return Ok(eval_expression(&expression.expression)?);
        }
    }
    Ok(Object::Null)
}
fn eval_return_statement(return_statement: &ReturnStatement) -> EvalResult<Object> {
    Ok(Object::Return(Box::new(eval_expression(
        &return_statement.value,
    )?)))
}

fn eval_infix_expression(operator: &String, left: Object, right: Object) -> EvalResult<Object> {
    if left.get_type() != right.get_type() {
        Err(EvalError::TypeMismatch(format!(
            "{} {} {}",
            left.get_type(),
            operator,
            right.get_type()
        )))?
    }
    match (&left, &right) {
        (Object::Integer(l), Object::Integer(r)) => eval_integer_infix_expression(operator, l, r),
        _ => Err(EvalError::UnknownOperator(format!(
            "{} {} {}",
            left.get_type(),
            operator,
            right.get_type()
        ))),
    }
}
fn eval_integer_infix_expression(
    operator: &String,
    left_val: &i64,
    right_val: &i64,
) -> EvalResult<Object> {
    match operator.as_str() {
        "+" => Ok(Object::Integer(left_val + right_val)),
        "-" => Ok(Object::Integer(left_val - right_val)),
        "*" => Ok(Object::Integer(left_val * right_val)),
        "/" => Ok(Object::Integer(left_val / right_val)),
        "<" => Ok(Object::Boolean(left_val < right_val)),
        ">" => Ok(Object::Boolean(left_val > right_val)),
        "<=" => Ok(Object::Boolean(left_val <= right_val)),
        ">=" => Ok(Object::Boolean(left_val >= right_val)),
        "!=" => Ok(Object::Boolean(left_val != right_val)),
        "==" => Ok(Object::Boolean(left_val == right_val)),
        _ => Err(EvalError::UnknownOperator(format!(
            "{} {} {}",
            left_val, operator, right_val
        ))),
    }
}
fn eval_block_statement(block: &BlockStatement) -> EvalResult<Object> {
    let mut result = Object::Null;
    for statement in &block.statements {
        result = eval_statement(statement)?;
        if result.get_type() == "RETURN" {
            return Ok(result);
        }
    }
    Ok(result)
}
mod tests {
    use crate::evaluator::{eval, EvalResult};
    use crate::object::Object;
    use crate::parser::parse;
    #[derive(Debug)]
    struct Test<'a, I: ?Sized, E> {
        input: &'a I,
        expected: E,
    }
    fn test_eval(input: &str) -> EvalResult<Object> {
        let parse_result = parse(input);
        match parse_result {
            Ok(node) => eval(&node),
            Err(_) => {
                println!("Program parsing failed");
                Ok(Object::Null)
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
            let evaluated = test_eval(test.input).unwrap();
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
            let evaluated = test_eval(test.input).unwrap();
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
            let evaluated = test_eval(test.input).unwrap();
            assert_eq!(evaluated, Object::Boolean(test.expected))
        }
    }
    #[test]
    fn test_eval_integer_prefix() {
        let tests = vec![
            Test {
                input: "5",
                expected: 5,
            },
            Test {
                input: "-5",
                expected: -5,
            },
            Test {
                input: "-10",
                expected: -10,
            },
            Test {
                input: "10",
                expected: 10,
            },
        ];
        for test in tests {
            let evaluated = test_eval(test.input).unwrap();
            assert_eq!(evaluated, Object::Integer(test.expected))
        }
    }
    #[test]
    fn test_eval_integer_expression() {
        let tests = vec![
            Test {
                input: "5 + 5 + 5 + 5 - 10",
                expected: 10,
            },
            Test {
                input: "2 * 2 * 2 * 2 * 2",
                expected: 32,
            },
            Test {
                input: "-50 + 100 + -50",
                expected: 0,
            },
            Test {
                input: "5 * 2 + 10",
                expected: 20,
            },
            Test {
                input: "5 + 2 * 10",
                expected: 25,
            },
            Test {
                input: "20 + 2 * -10",
                expected: 0,
            },
            Test {
                input: "50 / 2 * 2 + 10",
                expected: 60,
            },
            Test {
                input: "2 * (5 + 10)",
                expected: 30,
            },
            Test {
                input: "3 * 3 * 3 + 10",
                expected: 37,
            },
            Test {
                input: "3 * (3 * 3) + 10",
                expected: 37,
            },
            Test {
                input: "(5 + 10 * 2 + 15 / 3) * 2 + -10",
                expected: 50,
            },
        ];
        for test in tests {
            let evaluated = test_eval(test.input).unwrap();
            assert_eq!(evaluated, Object::Integer(test.expected))
        }
    }
    #[test]
    fn test_eval_integer_conditional_expression() {
        let tests = vec![
            Test {
                input: "1 < 2",
                expected: true,
            },
            Test {
                input: "2 <= 2",
                expected: true,
            },
            Test {
                input: "3 <= 2",
                expected: false,
            },
            Test {
                input: "2 > 3",
                expected: false,
            },
            Test {
                input: "5 > 3",
                expected: true,
            },
            Test {
                input: "5 >= 5",
                expected: true,
            },
            Test {
                input: "5 == 5",
                expected: true,
            },
            Test {
                input: "6 != 5",
                expected: true,
            },
        ];
        for test in tests {
            let evaluated = test_eval(test.input).unwrap();
            assert_eq!(evaluated, Object::Boolean(test.expected))
        }
    }
    #[test]
    fn test_if_else_expressions() {
        let tests = vec![
            Test {
                input: "if (true) {10}",
                expected: 10,
            },
            Test {
                input: "if (false) {10}",
                expected: -1,
            },
            Test {
                input: "if (1) { 10 }",
                expected: 10,
            },
            Test {
                input: "if (1 < 2) { 10 }",
                expected: 10,
            },
            Test {
                input: "if (1 < 2) { 10 }",
                expected: 10,
            },
            Test {
                input: "if (1 > 2) { 10 }",
                expected: -1,
            },
            Test {
                input: "if (2 <= 2) { 10 }",
                expected: 10,
            },
        ];
        for test in tests {
            let evaluated = test_eval(test.input).unwrap();
            println!("{:?}", evaluated);
            if test.expected == -1 {
                assert_eq!(evaluated, Object::Null);
            } else {
                assert_eq!(evaluated, Object::Integer(test.expected));
            }
        }
    }
    #[test]
    fn test_return_statements() {
        let tests = vec![
            Test {
                input: "return 10;",
                expected: 10,
            },
            Test {
                input: "return 2 * 5; 9;",
                expected: 10,
            },
            Test {
                input: "9; return 2 * 20; 9;",
                expected: 40,
            },
            Test {
                input: "if (10 < 20) { if (10 > 1) { return 10; } return 1;}",
                expected: 10,
            },
        ];
        for test in tests {
            let evaluated = test_eval(test.input).unwrap();
            assert_eq!(evaluated, Object::Integer(test.expected));
        }
    }
    #[test]
    fn test_error_handling() {
        let tests = vec![
            Test {
                input: "5 + true;",
                expected: "type mismatch: INTEGER + BOOLEAN",
            },
            Test {
                input: "5 + true; 5;",
                expected: "type mismatch: INTEGER + BOOLEAN",
            },
            Test {
                input: "-true",
                expected: "unknown operator: -BOOLEAN",
            },
            Test {
                input: "true + false;",
                expected: "unknown operator: BOOLEAN + BOOLEAN",
            },
            Test {
                input: "5; true + false; 5",
                expected: "unknown operator: BOOLEAN + BOOLEAN",
            },
            Test {
                input: "if (10 > 1) { true + false; }",
                expected: "unknown operator: BOOLEAN + BOOLEAN",
            },
            Test {
                input: "if (10 > 1) {
                if (10 > 1) {
                    return true + false;
                }
                    return 1;
                }",
                expected: "unknown operator: BOOLEAN + BOOLEAN",
            },
        ];
        for test in tests {
            let evaluated = test_eval(test.input);
            assert!(evaluated.is_err());
            assert_eq!(evaluated.err().unwrap().to_string().as_str(), test.expected);
        }
    }
}
