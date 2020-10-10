use super::expression::*;
use super::interpreter::{RuntimeError, Value};
use super::statement::*;
use super::token::{Token, TokenType};

use std::collections::HashMap;

pub struct Environment {
    values: HashMap<String, u64>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            values: HashMap::default(),
        }
    }

    pub fn get(&self, token: &Token) -> Option<u64> {
        let key = &token.lexeme;
        self.get_name(key)
    }

    pub fn get_name(&self, key: &str) -> Option<u64> {
        if self.values.contains_key(key) {
            Some(self.values[key].clone())
        } else {
            None
        }
    }

    pub fn declare(&mut self, name: &str, value: u64) {
        self.values.insert(String::from(name), value);
    }

    pub fn add(&mut self, token: &Token, value: u64) {
        self.values.insert(token.lexeme.clone(), value);
    }

    pub fn assign(&mut self, token: &Token, value: u64) -> Option<u64> {
        let key = &token.lexeme;
        if self.values.contains_key(key) {
            self.values.insert(key.clone(), value);
            self.get_name(key)
        } else {
            None
        }
    }
}
