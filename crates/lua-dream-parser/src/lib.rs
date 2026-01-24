pub mod ast;
use lua_dream_lexer::token::Token;

use crate::ast::Block;

#[derive(Clone, Debug)]
pub struct Parser<'a> {
    _tokens: &'a [Token],
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Self { _tokens: tokens }
    }

    pub fn parse(&mut self) -> Block {
        Block {
            stats: Vec::new(),
            ctrl_stmt: None,
        }
    }
}
