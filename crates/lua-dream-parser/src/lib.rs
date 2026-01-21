pub mod ast;
use lua_dream_lexer::token::Token;

use crate::ast::Block;

#[derive(Clone, Debug)]
pub struct Parser<'a> {
    tokens: &'a [Token],
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Self { tokens }
    }

    pub fn parse(&mut self) -> Block {
        Block {
            stats: Vec::new(),
            res_stmt: None,
        }
    }
}
