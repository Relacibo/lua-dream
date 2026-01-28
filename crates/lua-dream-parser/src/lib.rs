pub mod ast;
pub mod error;
use std::str::FromStr;

use lua_dream_lexer::token::{Token, TokenKind, TokenKindDiscriminants};

use crate::{
    ast::{
        Attribute, BinaryOp, Block, Branch, ControlStatement, ElseBranch, Expression,
        ExpressionDiscriminants, Statement, TableRow, UnaryOp,
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
        if token_discriminant == token.kind.discriminant() {
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
        if token_discriminant == token.kind.discriminant() {
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
            dbg!(&stmt);

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
                    return Err(Error::UnexpectedEof(self.next_token().clone()));
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
                    Some(self.parse_expression()?)
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
                if self
                    .next_token_if_discriminant(TokenKindDiscriminants::Assign)
                    .is_some()
                {
                    let expression = self.parse_expression().ok();
                    StatementResult::Statement(Statement::Assign { name, expression })
                } else {
                    let token =  self.next_token();
                    if self.next_token_if_discriminant(TokenKindDiscriminants::Colon).is_some() {
                        let TokenKind::Identifier(iden) = self.expect_token_discriminant(TokenKindDiscriminants::Identifier)?.kind else {
                        unreachable!();
                        };

                    }
                    match token.kind.discriminant(){
                        TokenKindDiscriminants::Colon => {
                            
                        }
                        TokenKindDiscriminants::ParenOpen
                    }
                    // self.parse_prefix_expression()
                    todo!("parse function call statements")
                }
            }
            TokenKind::KeywordIf => {
                let condition = self.parse_expression()?;
                self.expect_token_discriminant(TokenKindDiscriminants::KeywordThen)?;
                let then_block = self.parse_block()?;

                let else_branch = self.parse_else_branch()?.map(Box::new);
                StatementResult::Statement(Statement::If(Branch {
                    condition,
                    then_block,
                    else_branch,
                }))
            }
            TokenKind::KeywordReturn => {
                let res = self.parse_expression().ok();
                StatementResult::ControlStatement(ControlStatement::Return(res))
            }
            TokenKind::KeywordBreak => StatementResult::ControlStatement(ControlStatement::Break),
            TokenKind::KeywordWhile => {
                let condition = self.parse_expression()?;
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
                let until = self.parse_expression()?;
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
                let TokenKind::Identifier(variable_name) = self
                    .expect_token_discriminant(TokenKindDiscriminants::Identifier)?
                    .kind
                    .clone()
                else {
                    unreachable!();
                };

                let token = self.next_token();
                match TokenKindDiscriminants::from(&token.kind) {
                    TokenKindDiscriminants::Assign => {
                        let from = self.parse_expression()?;
                        self.expect_token_discriminant(TokenKindDiscriminants::Comma)?;
                        let to = self.parse_expression()?;
                        self.expect_token_discriminant(TokenKindDiscriminants::Comma)?;
                        let increment = self.parse_expression()?;
                        self.expect_token_discriminant(TokenKindDiscriminants::KeywordDo)?;
                        let do_block = self.parse_block()?;
                        StatementResult::Statement(Statement::For {
                            variable_name,
                            from,
                            to,
                            increment,
                            do_block,
                        })
                    }
                    token_kind @ (TokenKindDiscriminants::Comma
                    | TokenKindDiscriminants::KeywordIn) => {
                        let mut variable_names = vec![variable_name];
                        if token_kind == TokenKindDiscriminants::Comma {
                            loop {
                                let TokenKind::Identifier(variable_name) = self
                                    .expect_token_discriminant(TokenKindDiscriminants::Identifier)?
                                    .kind
                                    .clone()
                                else {
                                    unreachable!();
                                };
                                variable_names.push(variable_name);
                                let token = self.next_token();
                                match token.kind.discriminant() {
                                    TokenKindDiscriminants::Comma => {}
                                    TokenKindDiscriminants::KeywordIn => {
                                        break;
                                    }
                                    _ => return Err(Error::UnexpectedToken(token.clone())),
                                }
                            }
                        }
                        let mut iterators = Vec::new();
                        loop {
                            let iterator = self.parse_expression()?;
                            iterators.push(iterator);
                            let token = self.next_token();
                            match TokenKindDiscriminants::from(&token.kind) {
                                TokenKindDiscriminants::Comma => {}
                                TokenKindDiscriminants::KeywordDo => {
                                    break;
                                }
                                _ => return Err(Error::UnexpectedToken(token.clone())),
                            }
                        }
                        let do_block = self.parse_block()?;
                        StatementResult::Statement(Statement::ForGeneric {
                            variable_names,
                            iterators,
                            do_block,
                        })
                    }
                    _ => return Err(Error::UnexpectedToken(token.clone())),
                }
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
                        let expression = self.parse_expression()?;
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
        Ok(res)
    }

    fn parse_primary(&mut self) -> Result<Expression, Error> {
        let token = self.peek_token().clone();
        let res = match token.kind {
            TokenKind::LiteralInteger(n) => {
                self.skip_token();
                Expression::Integer(n)
            }
            TokenKind::LiteralDouble(n) => {
                self.skip_token();
                Expression::Double(n)
            }
            TokenKind::LiteralString(s) => {
                self.skip_token();
                Expression::String(s)
            }
            TokenKind::LiteralBoolean(b) => {
                self.skip_token();
                Expression::Boolean(b)
            }
            TokenKind::LiteralNil => {
                self.skip_token();
                Expression::Nil
            }

            TokenKind::ParenOpen => {
                self.skip_token();
                let expr = self.parse_expression()?;
                self.expect_token_discriminant(TokenKindDiscriminants::ParenClose)?;
                expr
            }

            TokenKind::Minus | TokenKind::Not | TokenKind::Len | TokenKind::BitXor => {
                self.skip_token();
                let op = UnaryOp::try_from(token.kind.discriminant()).unwrap();
                let val = self.parse_expression_min_presendence(10)?;
                Expression::UnaryOp {
                    op,
                    val: Box::new(val),
                }
            }

            TokenKind::CurlyBracesOpen => {
                self.skip_token();
                let mut elems = Vec::new();
                loop {
                    let in_brackets = self
                        .next_token_if_discriminant(TokenKindDiscriminants::BracketsOpen)
                        .is_some();
                    let key = if in_brackets {
                        let res = self.parse_expression()?;
                        self.expect_token_discriminant(TokenKindDiscriminants::BracketsClose)?;
                        res
                    } else {
                        let token = self.next_token();
                        let key = match &token.kind {
                            TokenKind::Identifier(key) => key.clone(),
                            TokenKind::CurlyBracesClose => break,
                            _ => return Err(Error::UnexpectedToken(token.clone())),
                        };
                        Expression::String(key)
                    };
                    self.expect_token_discriminant(TokenKindDiscriminants::Assign)?;
                    let value = self.parse_expression()?;
                    elems.push(TableRow { key, value });
                    let token = self.next_token();
                    match token.kind.discriminant() {
                        TokenKindDiscriminants::Comma | TokenKindDiscriminants::Semicolon => {}
                        TokenKindDiscriminants::CurlyBracesClose => break,
                        _ => return Err(Error::UnexpectedToken(token.clone())),
                    }
                }
                Expression::Table(elems)
            }

            TokenKind::Identifier(_) => {
                // self.parse_prefix_expression()
                dbg!(&token);
                todo!()
            }

            _ => return Err(Error::UnexpectedToken(token)),
        };
        Ok(res)
    }

    fn parse_expression(&mut self) -> Result<Expression, Error> {
        self.parse_expression_min_presendence(0)
    }

    fn parse_expression_min_presendence(
        &mut self,
        min_precedence: u8,
    ) -> Result<Expression, Error> {
        let mut left = self.parse_primary()?;
        loop {
            let Ok(op) = BinaryOp::try_from(self.peek_token().kind.discriminant()) else {
                return Ok(left);
            };
            let precedence = op.get_precedence();

            if precedence < min_precedence {
                break;
            }

            self.skip_token();
            let right = self.parse_expression_min_presendence(precedence + 1)?;

            left = Expression::BinaryOp {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_else_branch(&mut self) -> Result<Option<ElseBranch>, Error> {
        let token = self.peek_token();
        let else_branch = match &token.kind.discriminant() {
            TokenKindDiscriminants::KeywordElseIf => {
                self.next_token();
                let condition = self.parse_expression()?;
                self.expect_token_discriminant(TokenKindDiscriminants::KeywordThen)?;
                let then_block = self.parse_block()?;
                let else_branch = self.parse_else_branch()?;
                ElseBranch::ElseIf(Branch {
                    condition,
                    then_block,
                    else_branch: else_branch.map(Box::new),
                })
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
