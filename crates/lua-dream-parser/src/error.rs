use lua_dream_lexer::token::Token;
use thiserror::Error;

use crate::ast::Expression;

#[derive(Error, Debug)]
pub enum Error {
    #[error("Unexpected EOF: {:?}", .0)]
    UnexpectedEof(Token),
    #[error("Unexpected token: {:?}", .0)]
    UnexpectedToken(Token),
    #[error("Unexpected attribute: {:?}", .0)]
    UnexpectedAttribute(Token),
    #[error("Not a statement: {:?}, {:?}", 0, 1)]
    NotAStatement(Token, Expression),
    #[error("Not a statement: {:?}, {:?}", 0, 1)]
    InvalidAssignmentTarget(Token),
}

impl Error {
    pub fn into_token(self) -> Option<Token> {
        let res = match self {
            Error::UnexpectedEof(token) => token,
            Error::UnexpectedToken(token) => token,
            Error::UnexpectedAttribute(token) => token,
            Error::NotAStatement(token, _) => token,
            Error::InvalidAssignmentTarget(token) => token,
        };
        Some(res)
    }
}
