pub mod error;
pub mod slicable_queue;
pub mod token;

use std::{collections::VecDeque, io::BufRead, path::Path};

use crate::{
    error::LexerError,
    slicable_queue::SlicableQueue,
    token::{Token, TokenKind},
};

use utf8_chars::BufReadCharsExt;

#[derive(Debug, Clone, Copy)]
struct Cursor {
    line: usize,
    column: usize,
}

pub struct Lexer<'a, T: BufRead + ?Sized> {
    file_path: Option<&'a Path>,
    chars: utf8_chars::Chars<'a, T>,
    cursor: Cursor,
    peek_buf: SlicableQueue<char>,
    new_line_next: bool,
}

impl<'a, T: BufRead + ?Sized> Lexer<'a, T> {
    pub fn new(file_path: Option<&'a Path>, reader: &'a mut T) -> Self {
        Self {
            file_path,
            chars: reader.chars(),
            cursor: Cursor { line: 1, column: 0 },
            peek_buf: SlicableQueue::default(),
            new_line_next: false,
        }
    }

    fn cursor(&self) -> &Cursor {
        &self.cursor
    }

    fn peek(&mut self, num: usize) -> std::io::Result<&[char]> {
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

    fn peek_while(&mut self, mut f: impl FnMut(&char) -> bool) -> std::io::Result<Option<&char>> {
        let mut i = 0;
        loop {
            let c = if let Some(c) = &mut self.peek_buf.get(i) {
                c
            } else {
                let Some(res) = self.chars.next() else {
                    return Ok(None);
                };
                match res {
                    Ok(c) => {
                        self.peek_buf.push_back(c);
                    }
                    Err(err) => return Err(err),
                }
                &self.peek_buf[i]
            };
            if !f(c) {
                break;
            }
            i += 1;
        }
        Ok(Some(&self.peek_buf[i]))
    }

    fn peeking_take_while(
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

    fn take_while(
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

    fn skip_while(&mut self, mut f: impl FnMut(char) -> bool) -> std::io::Result<Option<char>> {
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

        // skip Shebang
        if self.peek(2)? == ['#', '!'] {
            let _ = self.skip(2);
            self.skip_while(|c| c != '\n')?;
        }

        loop {
            let Some(next_char) = self.next()? else {
                break;
            };

            let column_start = self.cursor.column;
            let line_start = self.cursor.line;

            let kind = match next_char {
                c if c.is_ascii_whitespace() => continue,
                '=' if self.peek(1)? == ['='] => {
                    let _ = self.next();
                    TokenKind::Eq
                }
                '=' => TokenKind::Assign,
                '~' if self.peek(1)? == ['='] => {
                    let _ = self.next();
                    TokenKind::Neq
                }
                '<' if self.peek(1)? == ['='] => {
                    let _ = self.next();
                    TokenKind::Leq
                }
                '<' if self.peek(1)? == ['<'] => {
                    let _ = self.skip(1);
                    TokenKind::Shl
                }
                '<' => TokenKind::Lt,
                '>' if self.peek(1)? == ['='] => {
                    let _ = self.next();
                    TokenKind::Geq
                }
                '>' if self.peek(1)? == ['>'] => {
                    let _ = self.skip(1);
                    TokenKind::Shr
                }
                '>' => TokenKind::Gt,
                '-' if self.peek(1)? == ['-'] => {
                    let _ = self.skip(1);
                    let mut eq_count = 0;
                    if self.peek_while(|c| {
                        if *c == '=' {
                            eq_count += 1;
                            true
                        } else {
                            false
                        }
                    })? == Some(&'[')
                    {
                        let _ = self.skip(eq_count + 1);
                        let mut curr_eq_count = None;
                        let Some(_) = self.skip_while(|c| {
                            should_continue_multiline(eq_count, &mut curr_eq_count, c)
                        })?
                        else {
                            self.print_state();
                            todo!("Error: parsing string failed");
                        };
                    } else {
                        self.skip_while(|c| c != '\n')?;
                    }
                    // match peeked {
                    //     ['[', '['] => {
                    //         let _ = self.skip(2);
                    //         let mut last_char = ' ';
                    //         self.skip_while(|c| {
                    //             if last_char == ']' && c == ']' {
                    //                 return false;
                    //             }
                    //             last_char = c;
                    //             true
                    //         })?;
                    //     }
                    //     _ => {
                    //         self.skip_while(|c| c != '\n')?;
                    //     }
                    // };
                    continue;
                }
                '+' => TokenKind::Plus,
                '-' => TokenKind::Minus,
                '/' if self.peek(1)? == ['/'] => {
                    let _ = self.skip(1);
                    TokenKind::FloorDiv
                }
                '/' => TokenKind::Div,
                '*' => TokenKind::Mul,
                '^' => TokenKind::Pow,
                '%' => TokenKind::Mod,
                '&' => TokenKind::BitAnd,
                '|' => TokenKind::BitOr,
                '~' => TokenKind::BitXor,
                '(' => TokenKind::ParenOpen,
                ')' => TokenKind::ParenClose,
                '[' => {
                    let mut eq_count = 0;
                    if self.peek_while(|c| {
                        if *c == '=' {
                            eq_count += 1;
                            true
                        } else {
                            false
                        }
                    })? == Some(&'[')
                    {
                        let _ = self.skip(eq_count + 1);
                        let mut curr_eq_count = None;
                        let Some(_) = self.take_while(&mut buf, |c| {
                            should_continue_multiline(eq_count, &mut curr_eq_count, c)
                        })?
                        else {
                            self.print_state();
                            todo!("Error: parsing string failed");
                        };

                        buf.truncate(buf.len() - 1 - eq_count);
                        let mut buf2 = String::new();
                        std::mem::swap(&mut buf, &mut buf2);
                        TokenKind::LiteralString(buf2)
                    } else {
                        TokenKind::BracketsOpen
                    }
                }
                ']' => TokenKind::BracketsClose,
                '{' => TokenKind::CurlyBracesOpen,
                '}' => TokenKind::CurlyBracesClose,
                ',' => TokenKind::Comma,
                ';' => TokenKind::Semicolon,
                ':' if self.peek(1)? == [':'] => {
                    let _ = self.skip(1);
                    TokenKind::DoubleColon
                }
                ':' => TokenKind::Colon,
                '.' if self.peek(1)? == ['.'] => {
                    let _ = self.skip(1);
                    if self.peek(1)? == ['.'] {
                        let _ = self.skip(1);
                        TokenKind::Varargs
                    } else {
                        TokenKind::Concat
                    }
                }
                '#' => TokenKind::Len,
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
                    TokenKind::LiteralString(buf2)
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

                    if dot_appeared {
                        let Ok(num) = buf.parse() else {
                            self.print_state();
                            todo!("Error: parsing number failed")
                        };
                        buf.clear();
                        TokenKind::LiteralFloat(num)
                    } else {
                        let Ok(num) = buf.parse() else {
                            self.print_state();
                            todo!("Error: parsing number failed")
                        };
                        buf.clear();
                        TokenKind::LiteralInt(num)
                    }
                }
                a if a.is_alphabetic() || a == '_' => {
                    buf.push(a);
                    self.peeking_take_while(&mut buf, |c| c.is_alphanumeric() || *c == '_')?;
                    let token = match buf.as_str() {
                        "if" => TokenKind::KeywordIf,
                        "then" => TokenKind::KeywordThen,
                        "else" => TokenKind::KeywordElse,
                        "elseif" => TokenKind::KeywordElseIf,
                        "end" => TokenKind::KeywordEnd,
                        "function" => TokenKind::KeywordFunction,
                        "local" => TokenKind::KeywordLocal,
                        "return" => TokenKind::KeywordReturn,
                        "and" => TokenKind::And,
                        "or" => TokenKind::Or,
                        "not" => TokenKind::Not,
                        "true" => TokenKind::LiteralBoolean(true),
                        "false" => TokenKind::LiteralBoolean(false),
                        "while" => TokenKind::KeywordWhile,
                        "do" => TokenKind::KeywordDo,
                        "break" => TokenKind::KeywordBreak,
                        "repeat" => TokenKind::KeywordRepeat,
                        "until" => TokenKind::KeywordUntil,
                        "goto" => TokenKind::KeywordGoto,
                        "for" => TokenKind::KeywordFor,
                        "nil" => TokenKind::LiteralNil,
                        _ => {
                            let mut buf2 = String::new();
                            std::mem::swap(&mut buf, &mut buf2);
                            TokenKind::Identifier(buf2)
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
            tokens.push(Token {
                kind,
                line: line_start,
                column: column_start,
            });
        }
        tokens.push(Token {
            kind: TokenKind::Eof,
            line: self.cursor.line,
            column: 1,
        });
        Ok(tokens)
    }

    fn print_state(&self) {
        println!("State:");
        if let Some(fp) = self.file_path {
            print!("{}:", fp.to_string_lossy());
        }
        println!("  {}:{}", self.cursor.line, self.cursor.column);
    }
}

fn should_continue_multiline(
    target_eq_count: usize,
    eq_count: &mut Option<usize>,
    c: char,
) -> bool {
    if let Some(cec) = eq_count {
        if *cec >= target_eq_count {
            if c == ']' {
                return false;
            }
            *eq_count = None;
        } else if c == '=' {
            *cec += 1;
        }
    } else if c == ']' {
        *eq_count = Some(0);
    }
    true
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
