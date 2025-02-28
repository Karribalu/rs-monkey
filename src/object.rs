use std::collections::HashMap;
use std::fmt::{Display, Formatter};
#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Return(Box<Object>),
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
            Object::Return(return_statement) => write!(f, "{}", return_statement),
        }
    }
}

impl Object {
    pub fn get_type(&self) -> String {
        match self {
            Object::Integer(_) => "INTEGER".to_string(),
            Object::Boolean(_) => "BOOLEAN".to_string(),
            Object::Null => "NULL".to_string(),
            Object::Return(_) => "RETURN".to_string(),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Environment {
    pub store: HashMap<String, Object>,
}
impl Environment {
    pub fn new() -> Self {
        Environment {
            store: Default::default(),
        }
    }
    pub fn get(&self, name: &str) -> Option<&Object> {
        self.store.get(name)
    }
    pub fn set(&mut self, name: String, object: Object) -> Object {
        self.store.insert(name, object.clone());
        object
    }
}
