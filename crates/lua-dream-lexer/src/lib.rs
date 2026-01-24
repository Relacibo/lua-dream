pub mod error;
pub mod slicable_queue;
pub mod token;

use std::{io::BufRead, path::Path};

use crate::{
    error::Error,
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

    fn peeking_skip_while(
        &mut self,
        mut f: impl FnMut(&char) -> bool,
    ) -> std::io::Result<Option<char>> {
        loop {
            let [c] = self.peek(1)? else {
                return Ok(None);
            };
            if !f(c) {
                return Ok(Some(*c));
            }
            let Ok(Some(_)) = self.next() else {
                unreachable!()
            };
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

    pub fn tokenize(&mut self) -> Result<Vec<Token>, Error> {
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
                    self.skip(1)?;

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
                        self.skip(eq_count + 1)?;
                        let seq_len = eq_count + 2;

                        loop {
                            if is_end_sequence(self.peek(seq_len)?, eq_count) {
                                let _ = self.skip(seq_len);
                                break;
                            }

                            if self.next()?.is_none() {
                                break;
                            }
                        }
                    } else {
                        self.skip_while(|c| c != '\n')?;
                    }
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
                        let seq_len = eq_count + 2;

                        let mut string_content = String::new();
                        loop {
                            if is_end_sequence(self.peek(seq_len)?, eq_count) {
                                let _ = self.skip(seq_len);
                                break;
                            }

                            match self.next()? {
                                Some(c) => string_content.push(c),
                                None => todo!("Error: Unexpected file end"),
                            }
                        }
                        TokenKind::LiteralString(string_content)
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
                    let quote_char = next_char;
                    let mut string_builder = String::new();

                    loop {
                        match self.next()? {
                            Some('\\') => {
                                match self.next()? {
                                    Some('n') => string_builder.push('\n'),
                                    Some('t') => string_builder.push('\t'),
                                    Some('r') => string_builder.push('\r'),
                                    Some('\\') => string_builder.push('\\'),
                                    Some('\n') => string_builder.push('\n'),
                                    Some('z') => {
                                        self.peeking_skip_while(|c| c.is_whitespace())?;
                                    }
                                    Some('x') => {
                                        let hex_slice = self.peek(2)?;
                                        let Ok(code) = u8::from_str_radix(
                                            &hex_slice.iter().collect::<String>(),
                                            16,
                                        ) else {
                                            todo!("Invalid hex escape")
                                        };
                                        let _ = self.skip(2);
                                        string_builder.push(code as char);
                                    }
                                    Some('u') => {
                                        let Some(c) = self.next()? else {
                                            todo!("Error: unexpected eof")
                                        };

                                        if c != '{' {
                                            todo!("Error: unexpected symbol");
                                        }

                                        let c = self.take_while(&mut buf, |c| c.is_ascii_hexdigit())?;

                                        match c {
                                            Some('}') => {}
                                            None => todo!("Error: unexpected eof"), 
                                            _ => todo!("Error: unexpected symbol"),
                                        }
                                        let Ok(t) = u32::from_str_radix(&buf, 16) else {
                                            buf.clear();
                                            todo!("Error: convert hex to u32 failed");
                                        };
                                        buf.clear();
                                        let Some(t) = char::from_u32(t) else{
                                            todo!("Error: convert hex to u32 failed");
                                        };
                                        string_builder.push(t);
                                            
                                    }
                                    Some(c) if c.is_ascii_digit() => {
                                        let mut val = c.to_digit(10).unwrap();

                                        if let [d2] =
                                            self.peek(1)? && d2.is_ascii_digit()
                                        {
                                            val = val * 10 + d2.to_digit(10).unwrap();
                                            let _ = self.skip(1);

                                            if let [d3] =
                                                self.peek(1)? && d3.is_ascii_digit()
                                            {
                                                let val3 = val * 10 + d3.to_digit(10).unwrap();
                                                if val3 <= 255 {
                                                    val = val3;
                                                    let _ = self.skip(1);
                                                }
                                            }
                                        }
                                        string_builder.push(val as u8 as char);
                                    }
                                    Some(c @ ('"' | '\'')) => string_builder.push(c),
                                    Some(c) => {
                                        eprintln!("Unknown escape character: {c}");
                                        string_builder.push(c);
                                    }
                                    None => todo!("Error: Unfinished escape sequence"),
                                }
                            }
                            Some(c) if c == quote_char => break, // String zu Ende
                            Some('\n') | None => todo!("Error: Unfinished string literal"),
                            Some(c) => string_builder.push(c),
                        }
                    }
                    TokenKind::LiteralString(string_builder)
                }
                n if n.is_ascii_digit() || n == '.' => {
                    let mut buf = String::new();
                    let mut has_exp = false;

                    let (is_hex, mut has_dot, exp_char, mut last_char) =
                        if n == '0' && self.peek(1)? == ['x'] {
                            self.skip(1)?;
                            (true, false, 'p', 'x')
                        } else {
                            buf.push(n);
                            (false, n == '.', 'e', n)
                        };

                    // Generously take everything, that could be part of a number
                    self.peeking_take_while(&mut buf, |c| {
                        match c {
                            '.' if !has_dot => {
                                has_dot = true;
                            }
                            'a'..='f' | 'A'..='F' if is_hex && !has_exp => {}
                            '+' | '-' if last_char.eq_ignore_ascii_case(&exp_char) => {}
                            c if c.eq_ignore_ascii_case(&exp_char) => {
                                has_exp = true;
                            }
                            c if is_hex && c.is_ascii_hexdigit() => {}
                            c if c.is_ascii_digit() => {}
                            _ => {
                                return false;
                            }
                        }
                        last_char = *c;
                        true
                    })?;

                    if is_hex {
                        if has_dot || has_exp {
                            let val = parse_lua_hex_float(&buf);
                            TokenKind::LiteralFloat(val)
                        } else {
                            let val = i64::from_str_radix(&buf, 16).expect("Invalid hex int");
                            TokenKind::LiteralInt(val)
                        }
                    } else if has_dot || has_exp {
                        let val: f64 = buf.parse().expect("Invalid float");
                        TokenKind::LiteralFloat(val)
                    } else {
                        let val: i64 = buf.parse().expect("Invalid int");
                        TokenKind::LiteralInt(val)
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
                        "global" => TokenKind::KeywordGlobal,
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
                        "in" => TokenKind::KeywordIn,
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

fn is_end_sequence(seq: &[char], eq_count: usize) -> bool {
    let mut iter = seq.iter();
    if iter.next() != Some(&']') {
        return false;
    }

    if iter.by_ref().take(eq_count).any(|c| c != &'=') {
        return false;
    }

    iter.next() == Some(&']')
}

fn parse_lua_hex_float(s: &str) -> f64 {
    let parts: Vec<&str> = s.split(['p', 'P']).collect();
    let mantissa_part = parts[0];

    let mut val: f64 = 0.0;
    if let Some(dot_pos) = mantissa_part.find('.') {
        let int_str = &mantissa_part[..dot_pos];
        let frac_str = &mantissa_part[dot_pos + 1..];

        val += i64::from_str_radix(if int_str.is_empty() { "0" } else { int_str }, 16).unwrap_or(0)
            as f64;
        for (i, c) in frac_str.chars().enumerate() {
            if let Some(digit) = c.to_digit(16) {
                val += (digit as f64) / 16.0_f64.powi(i as i32 + 1);
            }
        }
    } else {
        val = i64::from_str_radix(mantissa_part, 16).unwrap_or(0) as f64;
    }

    if parts.len() > 1 {
        let exp: i32 = parts[1].parse().unwrap_or(0);
        val * 2.0_f64.powi(exp)
    } else {
        val
    }
}

#[cfg(test)]
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
        let mut lexer = Lexer::new(Some(path), &mut reader);
        let tokens = lexer.tokenize().unwrap();
        dbg!(tokens);
    }
}
