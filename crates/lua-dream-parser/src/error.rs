use lua_dream_lexer::token::Token;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum Error {
    #[error("Unexpected EOF: {:?}", .0)]
    UnexpectedEof(Token),
    #[error("Unexpected token: {:?}", .0)]
    UnexpectedToken(Token),
    #[error("Unexpected attribute: {:?}", .0)]
    UnexpectedAttribute(Token),
    #[error("Expected expression. Got: {:?}", .0)]
    ExpectedExpression(Token),
    #[error("Expected Binary Operation. Got: {:?}", .0)]
    ExpectedBinaryOperation(Token),
}
