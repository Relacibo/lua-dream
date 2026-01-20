pub mod ast;
pub mod error;
pub mod slicable_queue;
pub mod token;

use std::{io::BufRead, path::Path};

use crate::{error::LexerError, slicable_queue::SlicableQueue, token::Token};

use utf8_chars::BufReadCharsExt;

#[derive(Debug, Clone, Copy)]
struct Cursor {
    line: usize,
    column: usize,
}

struct Lexer<'a, T: BufRead + ?Sized> {
    file_path: &'a Path,
    chars: utf8_chars::Chars<'a, T>,
    cursor: Cursor,
    peek_buf: SlicableQueue<char>,
    new_line_next: bool,
}

impl<'a, T: BufRead + ?Sized> Lexer<'a, T> {
    pub(crate) fn new(file_path: &'a Path, reader: &'a mut T) -> Self {
        Self {
            file_path,
            chars: reader.chars(),
            cursor: Cursor { line: 1, column: 0 },
            peek_buf: SlicableQueue::default(),
            new_line_next: false,
        }
    }

    pub(crate) fn cursor(&self) -> &Cursor {
        &self.cursor
    }

    pub(crate) fn peek(&mut self, num: usize) -> std::io::Result<&[char]> {
        let Self {
            chars, peek_buf, ..
        } = self;

        while peek_buf.len() < num {
            let Some(res) = chars.next() else {
                return Ok(peek_buf.as_slice());
            };

            match res {
                Ok(c) => {
                    peek_buf.push_back(c);
                }
                Err(err) => return Err(err),
            }
        }

        Ok(&peek_buf[..num])
    }

    pub(crate) fn peeking_take_while(
        &mut self,
        buf: &mut String,
        mut f: impl FnMut(&char) -> bool,
    ) -> std::io::Result<Option<char>> {
        loop {
            let [c] = self.peek(1)? else {
                return Ok(None);
            };
            if !f(c) {
                return Ok(Some(*c));
            }
            let Ok(Some(c)) = self.next() else {
                unreachable!()
            };
            buf.push(c);
        }
    }

    pub(crate) fn take_while(
        &mut self,
        buf: &mut String,
        mut f: impl FnMut(char) -> bool,
    ) -> std::io::Result<Option<char>> {
        loop {
            let Some(c) = self.next()? else {
                return Ok(None);
            };
            if !f(c) {
                return Ok(Some(c));
            }
            buf.push(c);
        }
    }

    pub(crate) fn skip_while(
        &mut self,
        mut f: impl FnMut(char) -> bool,
    ) -> std::io::Result<Option<char>> {
        loop {
            let Some(c) = self.next()? else {
                return Ok(None);
            };
            if !f(c) {
                return Ok(Some(c));
            }
        }
    }

    fn next(&mut self) -> std::io::Result<Option<char>> {
        let Self {
            chars,
            cursor,
            peek_buf,
            new_line_next,
            ..
        } = self;

        let Cursor { line: row, column } = cursor;
        if *new_line_next {
            *new_line_next = false;
            *row += 1;
            *column = 0;
        }

        let c = if let Some(c) = peek_buf.pop_front() {
            c
        } else {
            let Some(res) = chars.next() else {
                return Ok(None);
            };
            res?
        };

        if c == '\n' {
            *new_line_next = true;
        }
        *column += 1;
        Ok(Some(c))
    }

    fn skip(&mut self, num: usize) -> std::io::Result<()> {
        for _ in 0..num {
            // TODO: maybe better error handling, if it would ever be needed to recover
            self.next()?;
        }
        Ok(())
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>, LexerError> {
        let mut tokens = Vec::new();
        let mut buf = String::new();
        loop {
            let Some(next_char) = self.next()? else {
                break;
            };

            let token = match next_char {
                c if c.is_ascii_whitespace() => continue,
                '=' if self.peek(1)? == ['='] => {
                    let _ = self.next();
                    Token::OpEq
                }
                '=' => Token::OpAssign,
                '~' if self.peek(1)? == ['='] => {
                    let _ = self.next();
                    Token::OpNotEq
                }
                '<' if self.peek(1)? == ['='] => {
                    let _ = self.next();
                    Token::OpLessEq
                }
                '<' if self.peek(1)? == ['<'] => {
                    let _ = self.skip(1);
                    Token::OpShiftLeft
                }
                '<' => Token::OpLess,
                '>' if self.peek(1)? == ['='] => {
                    let _ = self.next();
                    Token::OpGreaterEq
                }
                '>' if self.peek(1)? == ['>'] => {
                    let _ = self.skip(1);
                    Token::OpShiftRight
                }
                '>' => Token::OpGreater,
                '-' if self.peek(1)? == ['-'] => {
                    let peeked = self.peek(3)?;
                    match peeked {
                        [_, '[', '['] => {
                            let _ = self.skip(3);
                            let mut last_char = ' ';
                            self.skip_while(|c| {
                                if last_char == ']' && c == ']' {
                                    return false;
                                }
                                last_char = c;
                                true
                            })?;
                        }
                        _ => {
                            let _ = self.skip(1);
                            self.skip_while(|c| c != '\n')?;
                        }
                    };
                    continue;
                }
                '+' => Token::OpPlus,
                '-' => Token::OpMinus,
                '/' => Token::OpDiv,
                '*' => Token::OpMul,
                '^' => Token::OpXor,
                '%' => Token::OpMod,
                '&' => Token::OpBitwiseAnd,
                '|' => Token::OpBitwiseOr,
                '~' => Token::OpBitwiseNot,
                '(' => Token::ParenOpen,
                ')' => Token::ParenClose,
                '[' if self.peek(1)? == ['['] => {
                    let _ = self.skip(1);
                    let mut last_char = ' ';
                    let Some(_) = self.take_while(&mut buf, |c| {
                        if last_char == ']' && c == ']' {
                            return false;
                        }
                        last_char = c;
                        true
                    })?
                    else {
                        self.print_state();
                        todo!("Error: parsing string failed");
                    };
                    buf.pop();
                    let mut buf2 = String::new();
                    std::mem::swap(&mut buf, &mut buf2);
                    Token::LiteralString(buf2)
                }
                '[' => Token::BracketsOpen,
                ']' => Token::BracketsClose,
                '{' => Token::CurlyBracesOpen,
                '}' => Token::CurlyBracesClose,
                ',' => Token::Comma,
                ';' => Token::SemiColon,
                ':' => Token::Colon,
                '.' if self.peek(1)? == ['.'] => {
                    let _ = self.skip(1);
                    if self.peek(1)? == ['.'] {
                        let _ = self.skip(1);
                        Token::Varargs
                    } else {
                        Token::OpConcat
                    }
                }
                '#' => Token::OpLength,
                '"' | '\'' => {
                    let Some(res) = self.take_while(&mut buf, |c| c != next_char && c != '\n')?
                    else {
                        self.print_state();
                        todo!("Error: parsing string failed");
                    };
                    if res == '\n' {
                        self.print_state();
                        todo!("Error: parsing string failed");
                    }
                    let mut buf2 = String::new();
                    std::mem::swap(&mut buf, &mut buf2);
                    Token::LiteralString(buf2)
                }
                n if n.is_numeric() => {
                    buf.push(n);
                    let mut dot_appeared = false;
                    self.peeking_take_while(&mut buf, |c| {
                        if !dot_appeared && *c == '.' {
                            dot_appeared = true;
                            return true;
                        }
                        c.is_numeric()
                    })?;

                    let Ok(num) = buf.parse() else {
                        self.print_state();
                        todo!("Error: parsing number failed")
                    };
                    buf.clear();
                    Token::LiteralNumber(num)
                }
                a if a.is_alphabetic() || a == '_' => {
                    buf.push(a);
                    self.peeking_take_while(&mut buf, |c| c.is_alphanumeric() || *c == '_')?;
                    let token = match buf.as_str() {
                        "if" => Token::KeywordIf,
                        "then" => Token::KeywordThen,
                        "else" => Token::KeywordElse,
                        "elseif" => Token::KeywordElseIf,
                        "end" => Token::KeywordEnd,
                        "function" => Token::KeywordFunction,
                        "local" => Token::KeywordLocal,
                        "return" => Token::KeywordReturn,
                        "and" => Token::KeywordAnd,
                        "or" => Token::KeywordOr,
                        "not" => Token::KeywordNot,
                        "true" => Token::LiteralBoolean(true),
                        "false" => Token::LiteralBoolean(false),
                        _ => {
                            let mut buf2 = String::new();
                            std::mem::swap(&mut buf, &mut buf2);
                            Token::Identifier(buf2)
                        }
                    };
                    buf.clear();
                    token
                }
                _ => {
                    self.print_state();
                    unimplemented!("{next_char}")
                }
            };
            tokens.push(token);
        }
        tokens.push(Token::Eof);
        Ok(tokens)
    }

    pub(crate) fn print_state(&self) {
        println!("State:");
        println!(
            "  {}:{}:{}",
            self.file_path.to_string_lossy(),
            self.cursor.line,
            self.cursor.column
        );
    }
}

mod tests {
    use std::{fs::File, io::BufReader, path::Path};

    use crate::Lexer;

    const EXAMPLE_LUA_FILE_PATH: &str =
        concat!(env!("CARGO_MANIFEST_DIR"), "/resources/example.lua");

    #[test]
    fn test_tokenize_example() {
        let path = Path::new(EXAMPLE_LUA_FILE_PATH);
        let file = File::open(path).unwrap();
        let mut reader = BufReader::new(file);
        let mut lexer = Lexer::new(path, &mut reader);
        let tokens = lexer.tokenize().unwrap();
        dbg!(tokens);
    }
}
