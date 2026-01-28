pub mod ast;
pub mod error;
use std::str::FromStr;

use lua_dream_lexer::token::{Token, TokenKind, TokenKindDiscriminants};

use crate::{
    ast::{
        Attribute, BinaryOp, Block, Branch, ControlStatement, ElseBranch, Expression,
        ForGenericLoop, ForLoop, FunctionCall, FunctionDecl, MethodCall, RepeatUntilLoop,
        Statement, TableRow, UnaryOp, WhileLoop,
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

    pub fn peek_token(&self) -> &Token {
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
        let mut control_statement = None;

        loop {
            let stmt = self.parse_statement()?;

            match stmt {
                StatementResult::Statement(statement) => {
                    statements.push(statement);
                }
                StatementResult::ControlStatement(control) => {
                    control_statement = Some(control);

                    let next = self.parse_statement()?;
                    match next {
                        StatementResult::End => break,
                        StatementResult::Eof if self.current_depth == 1 => break,
                        _ => {
                            return Err(Error::UnexpectedToken(self.peek_token().clone()));
                        }
                    }
                }
                StatementResult::End => break,
                StatementResult::Eof if self.current_depth == 1 => break,
                StatementResult::Eof => {
                    return Err(Error::UnexpectedEof(self.peek_token().clone()));
                }
            }
        }

        self.current_depth -= 1;
        Ok(Block {
            statements,
            control_statement,
        })
    }

    fn parse_statement(&mut self) -> Result<StatementResult, Error> {
        let token = self.peek_token();
        let statement_kind = token.kind.clone();

        let res = match statement_kind.discriminant() {
            TokenKindDiscriminants::Eof => {
                self.skip_token();
                StatementResult::Eof
            }
            TokenKindDiscriminants::KeywordEnd
            | TokenKindDiscriminants::KeywordElse
            | TokenKindDiscriminants::KeywordElseIf
            | TokenKindDiscriminants::KeywordUntil => {
                // Don't consume the token but leave it to the calling function
                StatementResult::End
            }
            TokenKindDiscriminants::KeywordLocal | TokenKindDiscriminants::KeywordGlobal => {
                self.skip_token();
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
                        self.skip_token();
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

            TokenKindDiscriminants::Identifier | TokenKindDiscriminants::ParenOpen => {
                let token = token.clone();
                let expr = self.parse_primary()?;

                if self
                    .next_token_if_discriminant(TokenKindDiscriminants::Assign)
                    .is_some()
                {
                    let value = self.parse_expression()?;
                    let name = match expr {
                        Expression::Identifier(n) => n,
                        _ => return Err(Error::InvalidAssignmentTarget(token)),
                    };
                    StatementResult::Statement(Statement::Assign {
                        name,
                        expression: Some(value),
                    })
                } else {
                    match expr {
                        Expression::FunctionCall(call) => {
                            StatementResult::Statement(Statement::FunctionCall(call))
                        }
                        Expression::MethodCall(call) => {
                            StatementResult::Statement(Statement::MethodCall(call))
                        }
                        _ => return Err(Error::NotAStatement(token, expr)),
                    }
                }
            }

            TokenKindDiscriminants::KeywordIf => {
                self.skip_token();
                let condition = self.parse_expression()?;
                self.expect_token_discriminant(TokenKindDiscriminants::KeywordThen)?;
                let then_block = self.parse_block()?;
                let else_branch = self.parse_else_branch()?.map(Box::new);
                StatementResult::Statement(Statement::If(Box::new(Branch {
                    condition,
                    then_block,
                    else_branch,
                })))
            }

            TokenKindDiscriminants::KeywordReturn => {
                self.skip_token();
                let res = self.parse_expression().ok();
                StatementResult::ControlStatement(ControlStatement::Return(res))
            }

            TokenKindDiscriminants::KeywordBreak => {
                self.skip_token();
                StatementResult::ControlStatement(ControlStatement::Break)
            }

            TokenKindDiscriminants::KeywordWhile => {
                self.skip_token();
                let condition = self.parse_expression()?;
                self.expect_token_discriminant(TokenKindDiscriminants::KeywordDo)?;
                let do_block = self.parse_block()?;
                StatementResult::Statement(Statement::While(Box::new(WhileLoop {
                    condition,
                    do_block,
                })))
            }

            TokenKindDiscriminants::KeywordRepeat => {
                self.skip_token();
                let repeat_block = self.parse_block()?;
                self.expect_token_discriminant(TokenKindDiscriminants::KeywordUntil)?;
                let until = self.parse_expression()?;
                StatementResult::Statement(Statement::Repeat(Box::new(RepeatUntilLoop {
                    repeat_block,
                    until,
                })))
            }

            TokenKindDiscriminants::KeywordGoto => {
                self.skip_token();
                let TokenKind::Identifier(name) = self
                    .expect_token_discriminant(TokenKindDiscriminants::Identifier)?
                    .kind
                    .clone()
                else {
                    unreachable!();
                };
                StatementResult::Statement(Statement::Goto(name))
            }

            TokenKindDiscriminants::KeywordFor => {
                self.skip_token();
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
                        StatementResult::Statement(Statement::For(Box::new(ForLoop {
                            variable_name,
                            from,
                            to,
                            increment,
                            do_block,
                        })))
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
                        StatementResult::Statement(Statement::ForGeneric(Box::new(
                            ForGenericLoop {
                                variable_names,
                                iterators,
                                do_block,
                            },
                        )))
                    }
                    _ => return Err(Error::UnexpectedToken(token.clone())),
                }
            }

            TokenKindDiscriminants::DoubleColon => {
                self.skip_token();
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

            TokenKindDiscriminants::KeywordFunction => {
                self.skip_token();
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
                        let token = self.next_token();
                        let name = match &token.kind {
                            TokenKind::Varargs => {
                                has_varargs = true;
                                self.expect_token_discriminant(TokenKindDiscriminants::ParenClose)?;
                                break;
                            }
                            TokenKind::Identifier(name) => name.clone(),
                            _ => return Err(Error::UnexpectedToken(token.clone())),
                        };
                        args.push(name);
                        let t = self.next_token();
                        match t.kind.discriminant() {
                            TokenKindDiscriminants::ParenClose => break,
                            TokenKindDiscriminants::Comma => {}
                            _ => return Err(Error::UnexpectedToken(t.clone())),
                        }
                    }
                }
                let block = self.parse_block()?;
                StatementResult::Statement(Statement::Function(Box::new(FunctionDecl {
                    name,
                    args,
                    has_varargs,
                    block,
                })))
            }

            _ => return Err(Error::UnexpectedToken(self.next_token().clone())),
        };
        // dbg!(&res);
        Ok(res)
    }

    fn parse_table_constructor(&mut self) -> Result<Expression, Error> {
        self.expect_token_discriminant(TokenKindDiscriminants::CurlyBracesOpen)?;

        let mut fields = Vec::new();

        loop {
            if self.peek_token().kind.discriminant() == TokenKindDiscriminants::CurlyBracesClose {
                break;
            }

            let field = match &self.peek_token().kind {
                // [expression] = value
                TokenKind::BracketsOpen => {
                    self.skip_token();
                    let key = self.parse_expression()?;
                    self.expect_token_discriminant(TokenKindDiscriminants::BracketsClose)?;
                    self.expect_token_discriminant(TokenKindDiscriminants::Assign)?;
                    let value = self.parse_expression()?;
                    TableRow::KeyValue { key, value }
                }
                // identifier = value
                TokenKind::Identifier(name)
                    if self
                        .tokens
                        .get(self.next_token_index + 1)
                        .unwrap()
                        .kind
                        .discriminant()
                        == TokenKindDiscriminants::Assign =>
                {
                    let name = name.clone();
                    // It is `id = value`
                    self.skip_token(); // Identifier
                    self.skip_token(); // '='
                    let value = self.parse_expression()?;
                    TableRow::KeyValue {
                        key: Expression::String(name),
                        value,
                    }
                }
                // List elements
                _ => {
                    let value = self.parse_expression()?;
                    TableRow::ListElem(value)
                }
            };

            fields.push(field);

            // Trenner konsumieren: , oder ;
            let next_kind = self.peek_token().kind.discriminant();
            if next_kind == TokenKindDiscriminants::Comma
                || next_kind == TokenKindDiscriminants::Semicolon
            {
                self.skip_token();
            } else if next_kind != TokenKindDiscriminants::CurlyBracesClose {
                // In Lua ist ein fehlender Trenner nur vor der schließenden Klammer erlaubt
                break;
            }
        }

        self.expect_token_discriminant(TokenKindDiscriminants::CurlyBracesClose)?;
        Ok(Expression::Table(fields))
    }

    fn parse_primary(&mut self) -> Result<Expression, Error> {
        let token = self.peek_token().clone();

        let mut expr = match token.kind {
            TokenKind::LiteralInteger(n) => {
                self.skip_token();
                return Ok(Expression::Integer(n));
            }
            TokenKind::LiteralDouble(n) => {
                self.skip_token();
                return Ok(Expression::Double(n));
            }
            TokenKind::LiteralString(s) => {
                self.skip_token();
                return Ok(Expression::String(s));
            }
            TokenKind::LiteralBoolean(b) => {
                self.skip_token();
                return Ok(Expression::Boolean(b));
            }
            TokenKind::LiteralNil => {
                self.skip_token();
                return Ok(Expression::Nil);
            }
            TokenKind::Varargs => {
                self.skip_token();
                return Ok(Expression::Varargs);
            }

            TokenKind::Identifier(name) => {
                self.skip_token();
                Expression::Identifier(name)
            }
            TokenKind::ParenOpen => {
                self.skip_token();
                let inner = self.parse_expression_min_presendence(0)?;
                self.expect_token_discriminant(TokenKindDiscriminants::ParenClose)?;
                inner
            }
            TokenKind::CurlyBracesOpen => return self.parse_table_constructor(),

            TokenKind::Minus | TokenKind::Not | TokenKind::Len | TokenKind::BitXor => {
                self.skip_token();
                let op = UnaryOp::try_from(token.kind.discriminant()).unwrap();
                let val = self.parse_expression_min_presendence(10)?;
                return Ok(Expression::UnaryOp {
                    op,
                    val: Box::new(val),
                });
            }
            _ => return Err(Error::UnexpectedToken(token)),
        };

        loop {
            match self.peek_token().kind.discriminant() {
                TokenKindDiscriminants::ParenOpen => {
                    self.skip_token();
                    expr = Expression::FunctionCall(Box::new(FunctionCall {
                        prefix: expr,
                        arguments: self.parse_call_arguments()?,
                    }));
                }

                TokenKindDiscriminants::Colon => {
                    self.skip_token();
                    let method_name = match self
                        .expect_token_discriminant(TokenKindDiscriminants::Identifier)?
                        .kind
                        .clone()
                    {
                        TokenKind::Identifier(s) => s,
                        _ => unreachable!(),
                    };

                    let arguments = match self.peek_token().kind.discriminant() {
                        TokenKindDiscriminants::ParenOpen => {
                            self.skip_token();
                            self.parse_call_arguments()?
                        }
                        TokenKindDiscriminants::LiteralString => {
                            let TokenKind::LiteralString(s) = self.next_token().kind.clone() else {
                                unreachable!()
                            };
                            vec![Expression::String(s)]
                        }
                        TokenKindDiscriminants::CurlyBracesOpen => {
                            vec![self.parse_table_constructor()?]
                        }
                        _ => return Err(Error::UnexpectedToken(self.peek_token().clone())),
                    };

                    expr = Expression::MethodCall(Box::new(MethodCall {
                        prefix: expr,
                        method_name,
                        arguments,
                    }));
                }

                TokenKindDiscriminants::Dot => {
                    self.skip_token();
                    let TokenKind::Identifier(key_name) = self
                        .expect_token_discriminant(TokenKindDiscriminants::Identifier)?
                        .kind
                        .clone()
                    else {
                        unreachable!()
                    };
                    expr = Expression::Index {
                        table: Box::new(expr),
                        key: Box::new(Expression::String(key_name)),
                    };
                }

                TokenKindDiscriminants::BracketsOpen => {
                    self.skip_token();
                    let key_expr = self.parse_expression_min_presendence(0)?;
                    self.expect_token_discriminant(TokenKindDiscriminants::BracketsClose)?;
                    expr = Expression::Index {
                        table: Box::new(expr),
                        key: Box::new(key_expr),
                    };
                }

                _ => break,
            }
        }

        Ok(expr)
    }

    fn parse_call_arguments(&mut self) -> Result<Vec<Expression>, Error> {
        let mut args = Vec::new();

        if self.peek_token().kind.discriminant() != TokenKindDiscriminants::ParenClose {
            loop {
                let expr = self.parse_expression_min_presendence(0)?;
                args.push(expr);

                if self.peek_token().kind.discriminant() == TokenKindDiscriminants::Comma {
                    self.skip_token();
                } else {
                    break;
                }
            }
        }

        self.expect_token_discriminant(TokenKindDiscriminants::ParenClose)?;

        Ok(args)
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
        // Wir schauen, welches Token parse_block zum Abbruch gebracht hat
        let token = self.peek_token();

        match token.kind.discriminant() {
            TokenKindDiscriminants::KeywordElseIf => {
                self.skip_token(); // Jetzt erst 'elseif' konsumieren
                let condition = self.parse_expression()?;
                self.expect_token_discriminant(TokenKindDiscriminants::KeywordThen)?;
                let then_block = self.parse_block()?;
                let else_branch = self.parse_else_branch()?.map(Box::new);

                Ok(Some(ElseBranch::ElseIf(Branch {
                    condition,
                    then_block,
                    else_branch,
                })))
            }
            TokenKindDiscriminants::KeywordElse => {
                self.skip_token(); // Jetzt erst 'else' konsumieren
                let block = self.parse_block()?;
                // Nach einem 'else' MUSS ein 'end' kommen
                self.expect_token_discriminant(TokenKindDiscriminants::KeywordEnd)?;
                Ok(Some(ElseBranch::Else(block)))
            }
            TokenKindDiscriminants::KeywordEnd => {
                self.skip_token(); // Das finale 'end' der if-Struktur
                Ok(None)
            }
            _ => Err(Error::UnexpectedToken(token.clone())),
        }
    }
}

#[derive(Debug, Clone)]
enum StatementResult {
    Statement(Statement),
    ControlStatement(ControlStatement),
    End,
    Eof,
}
