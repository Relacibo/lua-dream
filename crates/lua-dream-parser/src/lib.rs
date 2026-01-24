pub mod ast;
pub mod error;
use std::str::FromStr;

use lua_dream_lexer::token::{Token, TokenKind, TokenKindDiscriminants};

use crate::{
    ast::{
        Attribute, Block, ControlStatement, ElseBranch, Expression, ExpressionDiscriminants,
        Statement,
    },
    error::Error,
};

#[derive(Clone, Debug)]
pub struct Parser<'a> {
    tokens: &'a [Token],
    next_token_index: usize,
    current_depth: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Self {
            tokens,
            next_token_index: 0,
            current_depth: 0,
        }
    }

    pub fn next_token(&mut self) -> &Token {
        let res = self
            .tokens
            .get(self.next_token_index)
            .expect("Unexpected end of file");
        self.next_token_index += 1;
        res
    }

    pub fn peek_token(&mut self) -> &Token {
        self.tokens
            .get(self.next_token_index)
            .expect("Unexpected end of file")
    }

    pub fn skip_token(&mut self) {
        self.next_token_index += 1;
    }

    pub fn expect_token_discriminant(
        &mut self,
        token_discriminant: TokenKindDiscriminants,
    ) -> Result<&Token, Error> {
        let token = self.next_token();
        if token_discriminant == TokenKindDiscriminants::from(&token.kind) {
            Ok(token)
        } else {
            Err(Error::UnexpectedToken(token.clone()))
        }
    }

    pub fn next_token_if_discriminant(
        &mut self,
        token_discriminant: TokenKindDiscriminants,
    ) -> Option<&Token> {
        let token = self.peek_token();
        if token_discriminant == TokenKindDiscriminants::from(&token.kind) {
            Some(self.next_token())
        } else {
            None
        }
    }

    pub fn parse_block(&mut self) -> Result<Block, Error> {
        self.current_depth += 1;
        let mut statements = Vec::new();
        loop {
            let stmt = self.parse_statement()?;
            match stmt {
                StatementResult::Statement(statement) => {
                    statements.push(statement);
                }
                StatementResult::ControlStatement(_control_statement) => {
                    todo!("Parse to end of block and return.")
                }
                StatementResult::End => break,
                StatementResult::Eof if self.current_depth == 1 => break,
                StatementResult::Eof => {
                    return Err(Error::UnexpectedEof(self.next_token().clone()))
                }
            }
        }
        self.current_depth -= 1;
        Ok(Block {
            statements,
            control_statement: None,
        })
    }

    fn parse_statement(&mut self) -> Result<StatementResult, Error> {
        let token = self.next_token();

        let Token {
            kind: statement_kind,
            line,
            column,
        } = token;

        let res = match statement_kind {
            TokenKind::Eof => StatementResult::Eof,
            TokenKind::KeywordEnd => StatementResult::End,
            TokenKind::KeywordLocal | TokenKind::KeywordGlobal => {
                let statement_kind_discriminant = TokenKindDiscriminants::from(statement_kind);
                let TokenKind::Identifier(name) = self
                    .expect_token_discriminant(TokenKindDiscriminants::Identifier)?
                    .kind
                    .clone()
                else {
                    unreachable!();
                };

                let token = self.peek_token();
                let attribute =
                    if TokenKindDiscriminants::from(&token.kind) == TokenKindDiscriminants::Lt {
                        self.next_token();
                        let iden_token =
                            self.expect_token_discriminant(TokenKindDiscriminants::Identifier)?;
                        let TokenKind::Identifier(attr) = iden_token.kind.clone() else {
                            unreachable!();
                        };
                        let attr = Attribute::from_str(&attr)
                            .map_err(|_| Error::UnexpectedAttribute(iden_token.clone()))?;
                        self.expect_token_discriminant(TokenKindDiscriminants::Gt)?;
                        Some(attr)
                    } else {
                        None
                    };

                let assign = if self
                    .next_token_if_discriminant(TokenKindDiscriminants::Assign)
                    .is_some()
                {
                    Some(self.expect_expression()?)
                } else {
                    None
                };

                let stmt = if statement_kind_discriminant == TokenKindDiscriminants::KeywordLocal {
                    Statement::DeclareLocal {
                        name,
                        assign,
                        attribute,
                    }
                } else {
                    Statement::DeclareGlobal {
                        name,
                        assign,
                        attribute,
                    }
                };
                StatementResult::Statement(stmt)
            }
            TokenKind::Identifier(name) => {
                let name = name.clone();
                self.expect_token_discriminant(TokenKindDiscriminants::Assign)?;
                let expression = self.parse_expression()?;
                StatementResult::Statement(Statement::Assign { name, expression })
            }
            TokenKind::KeywordIf => {
                let condition = self.expect_expression()?;
                self.expect_token_discriminant(TokenKindDiscriminants::KeywordThen)?;
                let then_block = self.parse_block()?;

                let else_branch = self.parse_else_branch()?;
                StatementResult::Statement(Statement::If {
                    condition,
                    then_block,
                    else_branch,
                })
            }
            TokenKind::KeywordReturn => {
                let res = self.parse_expression()?;
                StatementResult::ControlStatement(ControlStatement::Return(res))
            }
            TokenKind::KeywordBreak => StatementResult::ControlStatement(ControlStatement::Break),
            TokenKind::KeywordWhile => {
                let condition = self.expect_expression()?;
                self.expect_token_discriminant(TokenKindDiscriminants::KeywordDo)?;
                let do_block = self.parse_block()?;
                StatementResult::Statement(Statement::While {
                    condition,
                    do_block,
                })
            }
            TokenKind::KeywordRepeat => {
                let repeat_block = self.parse_block()?;
                self.expect_token_discriminant(TokenKindDiscriminants::KeywordUntil)?;
                let until = self.expect_expression()?;
                StatementResult::Statement(Statement::Repeat {
                    repeat_block,
                    until,
                })
            }
            TokenKind::KeywordGoto => {
                let TokenKind::Identifier(name) = self
                    .expect_token_discriminant(TokenKindDiscriminants::Identifier)?
                    .kind
                    .clone()
                else {
                    unreachable!();
                };
                StatementResult::Statement(Statement::Goto(name.clone()))
            }
            TokenKind::KeywordFor => {
                // TODO: generic loops
                let from = self.expect_expression()?;
                self.expect_token_discriminant(TokenKindDiscriminants::Comma)?;
                let to = self.expect_expression()?;
                self.expect_token_discriminant(TokenKindDiscriminants::Comma)?;
                let increment = self.expect_expression()?;
                self.expect_token_discriminant(TokenKindDiscriminants::KeywordDo)?;
                let do_block = self.parse_block()?;
                StatementResult::Statement(Statement::For {
                    from,
                    to,
                    increment,
                    do_block,
                })
            }
            TokenKind::DoubleColon => {
                let TokenKind::Identifier(name) = self
                    .expect_token_discriminant(TokenKindDiscriminants::Identifier)?
                    .kind
                    .clone()
                else {
                    unreachable!();
                };
                self.expect_token_discriminant(TokenKindDiscriminants::DoubleColon)?;
                StatementResult::Statement(Statement::Label(name))
            }
            TokenKind::KeywordFunction => {
                let TokenKind::Identifier(name) = self
                    .expect_token_discriminant(TokenKindDiscriminants::Identifier)?
                    .kind
                    .clone()
                else {
                    unreachable!();
                };
                self.expect_token_discriminant(TokenKindDiscriminants::ParenOpen)?;
                let mut args = Vec::new();
                let mut has_varargs = false;
                if self
                    .next_token_if_discriminant(TokenKindDiscriminants::ParenClose)
                    .is_none()
                {
                    loop {
                        let expression = self.expect_expression()?;
                        if ExpressionDiscriminants::from(&expression)
                            == ExpressionDiscriminants::Varargs
                        {
                            has_varargs = true;
                            self.expect_token_discriminant(TokenKindDiscriminants::ParenClose)?;
                            break;
                        }
                        args.push(expression);
                        let token = self.next_token();
                        match TokenKindDiscriminants::from(&token.kind) {
                            TokenKindDiscriminants::ParenClose => {
                                break;
                            }
                            TokenKindDiscriminants::Comma => {}
                            _ => return Err(Error::UnexpectedToken(token.clone())),
                        }
                    }
                }
                let block = self.parse_block()?;
                StatementResult::Statement(Statement::Function {
                    name,
                    args,
                    has_varargs,
                    block,
                })
            }
            _ => unimplemented!(
                r#"Token not supported in this position: "{statement_kind:?}" at {line}:{column}. \nExpecting Statement"#
            ),
        };
        dbg!(&res);
        Ok(res)
    }

    fn expect_expression(&mut self) -> Result<Expression, Error> {
        self.parse_expression()?
            .ok_or_else(|| Error::ExpectedExpression(self.next_token().clone()))
    }

    fn parse_expression(&mut self) -> Result<Option<Expression>, Error> {
        self.parse_expression_helper(None)
    }

    fn parse_expression_helper(
        &mut self,
        acc: Option<Expression>,
    ) -> Result<Option<Expression>, Error> {
        loop {
            let token = self.peek_token();
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
                _ => return Ok(acc),
            }
        }
    }

    fn parse_else_branch(&mut self) -> Result<Option<ElseBranch>, Error> {
        let token = self.peek_token();
        let else_branch = match TokenKindDiscriminants::from(&token.kind) {
            TokenKindDiscriminants::KeywordElseIf => {
                self.next_token();
                let condition = self.expect_expression()?;
                self.expect_token_discriminant(TokenKindDiscriminants::KeywordThen)?;
                let then_block = self.parse_block()?;
                let else_branch = self.parse_else_branch()?;
                ElseBranch::ElseIf {
                    condition,
                    then_block,
                    else_branch: else_branch.map(Box::new),
                }
            }
            TokenKindDiscriminants::KeywordElse => {
                self.next_token();
                let block = self.parse_block()?;
                ElseBranch::Else(block)
            }
            _ => return Ok(None),
        };
        Ok(Some(else_branch))
    }
}

#[derive(Debug, Clone)]
enum StatementResult {
    Statement(Statement),
    ControlStatement(ControlStatement),
    End,
    Eof,
}
