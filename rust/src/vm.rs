use core::fmt;
use std::error;

pub fn translate(_input: &str) -> Result<String, TranslateError> {
    todo!()
}

#[derive(Debug)]
pub enum TranslateError {}

impl fmt::Display for TranslateError {
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl error::Error for TranslateError {}
