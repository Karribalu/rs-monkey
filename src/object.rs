use std::fmt::{Display, Formatter};
#[derive(Debug, Eq, PartialEq)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
}
impl Display for Object {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Integer(integer) => {
                write!(f, "{}", integer)
            }
            Object::Boolean(boolean) => {
                write!(f, "{}", boolean.to_string())
            }
            Object::Null => {
                write!(f, "null")
            }
        }
    }
}

impl Object {
    pub fn get_type(&self) -> String {
        match self {
            Object::Integer(_) => "INTEGER".to_string(),
            Object::Boolean(_) => "BOOLEAN".to_string(),
            Object::Null => "NULL".to_string(),
        }
    }
}
