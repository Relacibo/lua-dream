use lua_dream_lexer::token::Token;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum Error {
    #[error("Unexpected EOF")]
    UnexpectedEof,
    #[error("Expected EOF")]
    Eof,
    #[error("Unexpected token: {:?}", .0)]
    UnexpectedToken(Token),
    #[error("Unexpected attribute: {:?}", .0)]
    UnexpectedAttribute(String),
}
