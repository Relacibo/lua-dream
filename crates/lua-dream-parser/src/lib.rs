pub mod ast;
pub mod error;
use std::{ops::Not, str::FromStr};

use lua_dream_lexer::token::{Token, TokenKind, TokenKindDiscriminants};

use crate::{
    ast::{Attribute, Block, ControlStatement, Expression, Statement},
    error::Error,
};

#[derive(Clone, Debug)]
pub struct Parser<'a> {
    tokens: &'a [Token],
    next_token_index: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Self {
            tokens,
            next_token_index: 0,
        }
    }

    pub fn next_token(&mut self) -> Result<&Token, Error> {
        let res = self
            .tokens
            .get(self.next_token_index)
            .ok_or(Error::UnexpectedEof)?;
        self.next_token_index += 1;
        Ok(res)
    }

    pub fn peek_token(&mut self) -> Result<&Token, Error> {
        self.tokens
            .get(self.next_token_index)
            .ok_or(Error::UnexpectedEof)
    }

    pub fn skip_token(&mut self) {
        self.next_token_index += 1;
    }

    pub fn expect_token_discriminant(
        &mut self,
        token_discriminant: TokenKindDiscriminants,
    ) -> Result<&Token, Error> {
        let token = self.next_token()?;
        if token_discriminant == TokenKindDiscriminants::from(&token.kind) {
            Ok(token)
        } else {
            Err(Error::UnexpectedToken(token.clone()))
        }
    }

    pub fn parse_block(&mut self) -> Result<Block, Error> {
        let mut statements = Vec::new();
        loop {
            let stmt = match self.parse_statement() {
                Ok(stmt) => stmt,
                Err(Error::Eof) => break,
                Err(err) => return Err(err),
            };
            match stmt {
                StatementResult::Statement(statement) => {
                    statements.push(statement);
                }
                StatementResult::ControlStatement(_control_statement) => {
                    todo!("Parse to end of block and return.")
                }
            }
        }
        Ok(Block {
            statements,
            control_statement: None,
        })
    }

    pub fn parse_statement(&mut self) -> Result<StatementResult, Error> {
        let token = self.next_token()?;

        let Token { kind, line, column } = token;

        let res = match kind {
            TokenKind::Eof => return Err(Error::Eof),
            TokenKind::KeywordLocal => {
                let TokenKind::Identifier(name) = self
                    .expect_token_discriminant(TokenKindDiscriminants::Identifier)?
                    .kind
                    .clone()
                else {
                    unreachable!();
                };

                let token = self.next_token()?;
                let attribute = match token.kind {
                    TokenKind::Lt => {
                        let TokenKind::Identifier(attr) = self
                            .expect_token_discriminant(TokenKindDiscriminants::Identifier)?
                            .kind
                            .clone()
                        else {
                            unreachable!();
                        };
                        self.expect_token_discriminant(TokenKindDiscriminants::Gt)?;
                        self.expect_token_discriminant(TokenKindDiscriminants::Assign)?;
                        let attr = Attribute::from_str(&attr)
                            .map_err(|_| Error::UnexpectedAttribute(attr))?;
                        Some(attr)
                    }
                    TokenKind::Assign => None,
                    _ => return Err(Error::UnexpectedToken(token.clone())),
                };

                let expression = self.parse_expression()?;

                StatementResult::Statement(Statement::AssignLocal {
                    name,
                    expression,
                    attribute,
                })
            }
            _ => unimplemented!(
                r#"Token not supported in this position: "{kind:?}" at {line}:{column}. \nExpecting Statement"#
            ),
        };
        dbg!(&res);
        Ok(res)
    }

    pub fn parse_expression(&mut self) -> Result<Expression, Error> {
        loop {
            let token = self.next_token()?;
            let Token { kind, line, column } = token;
            match kind {
                TokenKind::Identifier(_) => {
                    todo!(r#"Not yet supported: "Identifier" at {line}:{column}"#)
                }
                TokenKind::LiteralString(_) => {
                    todo!(r#"Not yet supported: "LiteralString" at {line}:{column}"#)
                }
                TokenKind::LiteralFloat(_) => {
                    todo!(r#"Not yet supported: "LiteralFloat" at {line}:{column}"#)
                }
                TokenKind::LiteralInt(_) => {
                    todo!(r#"Not yet supported: "LiteralInt" at {line}:{column}"#)
                }
                TokenKind::LiteralBoolean(_) => {
                    todo!(r#"Not yet supported: "LiteralBoolean" at {line}:{column}"#)
                }
                TokenKind::LiteralNil => {
                    todo!(r#"Not yet supported: "LiteralNil " at {line}:{column}"#)
                }
                TokenKind::Assign => {
                    todo!(r#"Not yet supported: "Assign"" at {line}:{column}"#)
                }
                TokenKind::Plus => {
                    todo!(r#"Not yet supported: "Plus" at {line}:{column}"#)
                }
                TokenKind::Minus => {
                    todo!(r#"Not yet supported: "Minus" at {line}:{column}"#)
                }
                TokenKind::Mul => {
                    todo!(r#"Not yet supported: "Mul" at {line}:{column}"#)
                }
                TokenKind::Div => {
                    todo!(r#"Not yet supported: "Div" at {line}:{column}"#)
                }
                TokenKind::FloorDiv => {
                    todo!(r#"Not yet supported: "FloorDiv" at {line}:{column}"#)
                }
                TokenKind::Mod => {
                    todo!(r#"Not yet supported: "Mod" at {line}:{column}"#)
                }
                TokenKind::Pow => {
                    todo!(r#"Not yet supported: "Pow" at {line}:{column}"#)
                }
                TokenKind::Eq => {
                    todo!(r#"Not yet supported: "Eq" at {line}:{column}"#)
                }
                TokenKind::Neq => {
                    todo!(r#"Not yet supported: "Neq" at {line}:{column}"#)
                }
                TokenKind::Leq => {
                    todo!(r#"Not yet supported: "Leq" at {line}:{column}"#)
                }
                TokenKind::Geq => {
                    todo!(r#"Not yet supported: "Geq" at {line}:{column}"#)
                }
                TokenKind::Lt => {
                    todo!(r#"Not yet supported: "Lt" at {line}:{column}"#)
                }
                TokenKind::Gt => {
                    todo!(r#"Not yet supported: "Gt" at {line}:{column}"#)
                }
                TokenKind::And => {
                    todo!(r#"Not yet supported: "And" at {line}:{column}"#)
                }
                TokenKind::Or => {
                    todo!(r#"Not yet supported: "Or" at {line}:{column}"#)
                }
                TokenKind::Not => {
                    todo!(r#"Not yet supported: "Not" at {line}:{column}"#)
                }
                TokenKind::Len => {
                    todo!(r#"Not yet supported: "Len" at {line}:{column}"#)
                }
                TokenKind::Concat => {
                    todo!(r#"Not yet supported: "Concat" at {line}:{column}"#)
                }
                TokenKind::BitAnd => {
                    todo!(r#"Not yet supported: "BitAnd" at {line}:{column}"#)
                }
                TokenKind::BitOr => {
                    todo!(r#"Not yet supported: "BitOr" at {line}:{column}"#)
                }
                TokenKind::BitXor => {
                    todo!(r#"Not yet supported: "BitXor" at {line}:{column}"#)
                }
                TokenKind::Shl => {
                    todo!(r#"Not yet supported: "Shl" at {line}:{column}"#)
                }
                TokenKind::Shr => {
                    todo!(r#"Not yet supported: "Shr" at {line}:{column}"#)
                }
                TokenKind::Comma => {
                    todo!(r#"Not yet supported: "Comma" at {line}:{column}"#)
                }
                TokenKind::Semicolon => {
                    todo!(r#"Not yet supported: "Semicolon" at {line}:{column}"#)
                }
                TokenKind::Varargs => {
                    todo!(r#"Not yet supported: "Varargs" at {line}:{column}"#)
                }
                TokenKind::Colon => {
                    todo!(r#"Not yet supported: "Colon" at {line}:{column}"#)
                }
                TokenKind::DoubleColon => {
                    todo!(r#"Not yet supported: "DoubleColon" at {line}:{column}"#)
                }
                TokenKind::ParenOpen => {
                    todo!(r#"Not yet supported: "ParenOpen" at {line}:{column}"#)
                }
                TokenKind::ParenClose => {
                    todo!(r#"Not yet supported: "ParenClose" at {line}:{column}"#)
                }
                TokenKind::BracketsOpen => {
                    todo!(r#"Not yet supported: "BracketsOpen" at {line}:{column}"#)
                }
                TokenKind::BracketsClose => {
                    todo!(r#"Not yet supported: "BracketsClose" at {line}:{column}"#)
                }
                TokenKind::CurlyBracesOpen => {
                    todo!(r#"Not yet supported: "CurlyBracesOpen" at {line}:{column}"#)
                }
                TokenKind::CurlyBracesClose => {
                    todo!(r#"Not yet supported: "CurlyBracesClose" at {line}:{column}"#)
                }
                TokenKind::KeywordFunction => {
                    todo!(r#"Not yet supported: "KeywordFunction" at {line}:{column}"#)
                }
                TokenKind::KeywordIf => {
                    todo!(r#"Not yet supported: "KeywordIf" at {line}:{column}"#)
                }
                TokenKind::KeywordThen => {
                    todo!(r#"Not yet supported: "KeywordThen" at {line}:{column}"#)
                }
                TokenKind::KeywordElseIf => {
                    todo!(r#"Not yet supported: "KeywordElseIf" at {line}:{column}"#)
                }
                TokenKind::KeywordElse => {
                    todo!(r#"Not yet supported: "KeywordElse" at {line}:{column}"#)
                }
                TokenKind::KeywordEnd => {
                    todo!(r#"Not yet supported: "KeywordEnd" at {line}:{column}"#)
                }
                TokenKind::KeywordReturn => {
                    todo!(r#"Not yet supported: "KeywordReturn" at {line}:{column}"#)
                }
                TokenKind::KeywordWhile => {
                    todo!(r#"Not yet supported: "KeywordWhile" at {line}:{column}"#)
                }
                TokenKind::KeywordDo => {
                    todo!(r#"Not yet supported: "KeywordDo" at {line}:{column}"#)
                }
                TokenKind::KeywordBreak => {
                    todo!(r#"Not yet supported: "KeywordBreak" at {line}:{column}"#)
                }
                TokenKind::KeywordRepeat => {
                    todo!(r#"Not yet supported: "KeywordRepeat" at {line}:{column}"#)
                }
                TokenKind::KeywordUntil => {
                    todo!(r#"Not yet supported: "KeywordUntil" at {line}:{column}"#)
                }
                TokenKind::KeywordGoto => {
                    todo!(r#"Not yet supported: "KeywordGoto" at {line}:{column}"#)
                }
                TokenKind::KeywordFor => {
                    todo!(r#"Not yet supported: "KeywordFor" at {line}:{column}"#)
                }
                TokenKind::Eof => return Err(Error::Eof),
                _ => unimplemented!(
                    r#"Token not supported in this position: "{kind:?}" at {line}:{column}. \nExpecting Statement"#
                ),
            }
        }
    }
}

#[derive(Debug, Clone)]
enum StatementResult {
    Statement(Statement),
    ControlStatement(ControlStatement),
}
