//! Module responsible for the lexing of the literals in the source file,
//! like integer, float, string and char literals

use crate::{
    errors::{Diag, DiagStream, PartialResult},
    Span,
};

use super::tokens::{RawToken, RawTokenType};

impl<'r> super::Lexer<'r> {
    pub fn lex_int(&mut self, num: String) -> PartialResult<RawToken> {
        match self.make_int(&num, 10) {
            Ok(lit) => PartialResult::Good(RawToken {
                tt: RawTokenType::Int(lit),
                loc: self.current_span(),
            }),
            Err(diag) => PartialResult::new_fail(diag),
        }
    }

    pub fn make_int(&mut self, num: &str, radix: u8) -> Result<u64, Diag> {
        match parse_u64(num, radix) {
            Ok(number) => Ok(number),
            Err(ParseUIntError::IntegerOverflow) => Err(self
                .dcx
                .struct_err("integer literal is too large", self.current_span())),
            Err(ParseUIntError::DigitOutOfRange(loc)) => {
                let i = self.idx - num.len().into() + loc.lo;
                Err(self.dcx.struct_err(
                    format!(
                        "digit out of radix {:?}",
                        &num[loc.clone().range_usize()].chars().next().unwrap()
                    ),
                    Span::new(i, i + 1.into()),
                ))
            }
            Err(ParseUIntError::InvalidCharacter(loc)) => {
                let i = self.idx - num.len().into() + loc.lo;
                Err(self.dcx.struct_err(
                    format!(
                        "invalid character in literal, {:?}",
                        &num[loc.clone().range_usize()].chars().next().unwrap()
                    ),
                    Span::new(i, i + 1.into()),
                ))
            }
            Err(ParseUIntError::InvalidRadix) => {
                Err(self.dcx.struct_err("invalid radix", self.current_span()))
            }
        }
    }

    /// Lexes a string literal
    pub fn lex_str(&mut self) -> PartialResult<RawToken> {
        let mut str = String::new();
        let mut diags = DiagStream::new();

        loop {
            match self.peek() {
                Some('"') => {
                    self.expect('"');
                    break;
                }
                Some('\\') => {
                    self.expect('\\');

                    let es = match self.pop() {
                        Some(es) => es,
                        None => continue,
                    };

                    if es == '"' {
                        str.push(es);
                        continue;
                    }

                    match self.make_escape_sequence(es) {
                        Ok(res) => str.push(res),
                        Err(diag) => diags.push(diag),
                    }
                }
                Some(c) => {
                    str.push(c);
                    self.expect(c);
                }
                _ => {
                    return PartialResult::new_fail(
                        self.dcx
                            .struct_err("unterminated string literal", self.current_span_end()),
                    )
                }
            }
        }

        let tok = RawToken {
            tt: RawTokenType::Str(str),
            loc: self.current_span(),
        };
        if diags.is_empty() {
            PartialResult::Good(tok)
        } else {
            PartialResult::Fuzzy(tok, diags)
        }
    }

    pub fn make_escape_sequence(&mut self, es: char) -> Result<char, Diag> {
        Ok(match es {
            '0' => '\0',
            'n' => '\n',
            'r' => '\r',
            't' => '\t',
            'x' => self.make_hex_es()?,
            'u' => {
                // TODO: implement the lexing of unicode es
                return Err(self.dcx.struct_err(
                    "unicode escape sequence are not yet supported",
                    Span::new(self.idx - 2.into(), self.idx),
                ));
            }
            _ => {
                return Err(self.dcx.struct_err(
                    format!("unknown escape sequence: '\\{es}'"),
                    Span::new(self.idx - 2.into(), self.idx),
                ))
            }
        })
    }

    pub fn make_hex_es(&mut self) -> Result<char, Diag> {
        let mut str = String::with_capacity(2);
        for _ in 0..2 {
            str.push(self.pop().ok_or_else(|| {
                self.dcx
                    .struct_err("unterminated string literal", self.current_span())
            })?);
        }

        Ok(self.make_int(&str, 16)? as u8 as char)
    }

    /// Lexes a char literal
    pub fn lex_char(&mut self) -> PartialResult<RawToken> {
        let char;
        let mut diags = DiagStream::new();

        // We are using the `Default::default()` as a fallback if we encounter
        // an error, that allows us to continue lexing and parsing even when we
        // encounter an error.
        match self.peek() {
            Some('\\') => {
                self.expect('\\');

                char = match self.peek() {
                    Some(es) => 'here: {
                        self.expect(es);

                        if es == '\'' {
                            break 'here es;
                        }

                        match self.make_escape_sequence(es) {
                            Ok(res) => res,
                            Err(diag) => {
                                diags.push(diag);
                                Default::default()
                            }
                        }
                    }
                    None => {
                        diags.push(
                            self.dcx
                                .struct_err("unterminated char literal", self.current_span()),
                        );
                        Default::default()
                    }
                };
            }
            Some('\'') => {
                self.expect('\'');
                diags.push(if let Some('\'') = self.peek() {
                    self.dcx.struct_err(
                        "char literal must be escaped `'`",
                        Span::new(self.idx - 1.into(), self.idx),
                    )
                } else {
                    self.dcx
                        .struct_err("empty char literal", self.current_span())
                });
                char = Default::default();
            }
            Some(c) => {
                self.expect(c);
                char = c;
            }
            None => {
                return PartialResult::new_fail(self.dcx.struct_err(
                    "unexpected end of file",
                    Span::new(self.idx - 1.into(), self.idx),
                ))
            }
        }
        match self.peek() {
            Some('\'') => {
                self.pop();
            }
            _ => diags.push(
                self.dcx
                    .struct_err("unterminated char literal", self.current_span()),
            ),
        }
        let tok = RawToken {
            tt: RawTokenType::Char(char),
            loc: self.current_span(),
        };
        if diags.is_empty() {
            PartialResult::Good(tok)
        } else {
            PartialResult::Fuzzy(tok, diags)
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum ParseUIntError {
    InvalidRadix,
    InvalidCharacter(Span),
    DigitOutOfRange(Span),
    IntegerOverflow,
}

/// Parse a number passed as input into a u64 using the radix.
///
/// # Note
///
/// The radix is 'inclusive' if you want to parse a number as a decimal, then
/// `radix = 10` and if you want to parse a number as hexadecimal `radix = 16`
/// etc etc...
pub fn parse_u64(input: &str, radix: u8) -> Result<u64, ParseUIntError> {
    if !(2..=36).contains(&radix) {
        return Err(ParseUIntError::InvalidRadix);
    }

    let mut result: u64 = 0;

    for (i, c) in input.char_indices().peekable() {
        let digit = match c {
            '0'..='9' => (c as u8 - b'0') as u32,
            'a'..='z' => (c as u8 - b'a' + 10) as u32,
            'A'..='Z' => (c as u8 - b'A' + 10) as u32,
            '_' => continue,
            _ => {
                return Err(ParseUIntError::InvalidCharacter(Span::new(i, i + 1)));
            }
        };

        if digit >= radix.into() {
            return Err(ParseUIntError::DigitOutOfRange(Span::new(i, i + 1)));
        }

        result = match result
            .checked_mul(radix as u64)
            .and_then(|r| r.checked_add(digit as u64))
        {
            Some(val) => val,
            None => return Err(ParseUIntError::IntegerOverflow),
        };
    }

    Ok(result)
}
