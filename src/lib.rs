use logos::Lexer;
use miette::Diagnostic;

use crate::{ast::Visitor, eval::Value, lexer::TokenIter, parser::Parser};

pub mod ast;
pub mod deserialize;
pub mod eval;
pub mod lexer;
pub mod parser;

pub use deserialize::from_str;

/// run kisu program with pretty printed errors
pub fn run(source: &str) -> Result<Value, miette::Error> {
    let source = String::from(source);
    eval(&source).map_err(|error| miette::Error::new(error).with_source_code(source))
}

/// run kisu program with internal error type
pub fn eval(source: &str) -> Result<Value, Error> {
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(TokenIter::from(lexer), source);
    let expr = parser.parse()?;
    let mut walker = eval::TreeWalker::default();
    walker.visit_expr(&expr)?;
    Ok(walker.consume()?)
}

#[derive(thiserror::Error, Diagnostic, Debug)]
pub enum Error {
    #[error(transparent)]
    #[diagnostic(transparent)]
    Parser(#[from] parser::Error),
    #[error(transparent)]
    #[diagnostic(transparent)]
    Runtime(#[from] eval::Error),
    #[error(transparent)]
    #[diagnostic(transparent)]
    Deserialize(#[from] deserialize::Error),
}
