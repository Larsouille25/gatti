//! Lexing of Gatti source code into RawTokens.

use std::str::{CharIndices, FromStr};
use std::{iter::Peekable, path::Path};

use crate::errors::{Diag, DiagCtxt, PartialResult};
use crate::{BytePos, Span};

use self::tokens::{
    RawToken,
    RawTokenType::{self, *},
};
use crate::toks::{Keyword, Punctuation};

pub mod literals;
pub mod tokens;

pub struct SourceFile<'r> {
    filepath: &'r Path,
    filetext: &'r str,
    /// Index of the last `pop`ed char, starting from 1.
    idx: BytePos,

    iter: Peekable<CharIndices<'r>>,
}

impl<'r> SourceFile<'r> {
    pub fn new(filepath: &'r Path, filetext: &'r str) -> SourceFile<'r> {
        SourceFile {
            filepath,
            filetext,
            idx: 0.into(),
            iter: filetext.char_indices().peekable(),
        }
    }

    pub fn pop(&mut self) -> Option<char> {
        let (i, ch) = self.iter.next()?;
        self.idx = i.into();
        Some(ch)
    }

    pub fn peek(&mut self) -> Option<char> {
        Some(self.iter.peek()?.1)
    }

    pub fn filepath(&self) -> &'r Path {
        self.filepath
    }

    pub fn filetext(&self) -> &'r str {
        self.filetext
    }

    /// NOTE: This function can slow the lexing, it shouldn't be called too
    /// often.
    pub fn reset(&mut self) {
        self.iter = self.filetext.char_indices().peekable();
        self.idx = BytePos::ZERO;
    }

    /// Returns the true length, the count of how many Unicode characters there is
    /// in the source code file.
    pub fn length(&self) -> usize {
        // NOTE: This function is slow because it creates a new iterator each
        // time it's called, if it's called to much time, we should consider
        // storing the lenght of the file in a field and compute it only once.
        self.filetext.chars().count()
    }

    /// Resets the iterator and put the iterator to the new index. The index
    /// starts from 1.
    ///
    /// NOTE: This function can slow the lexing, it shouldn't be called too
    /// often.
    pub fn reset_to(&mut self, new_idx: usize) -> Option<()> {
        if new_idx > self.length() {
            return None;
        }
        self.reset();
        // TODO: use `advance_by` method on the iterator when it will be
        // stabilized
        for _ in 0..new_idx {
            if let Some((i, _)) = self.iter.next() {
                self.idx = i.into();
            } else {
                unreachable!("Should've been caught before.")
            }
        }

        Some(())
    }

    pub fn relative_reset(&mut self, offset: isize) -> Option<()> {
        let new_idx = ((<BytePos as Into<usize>>::into(self.idx) as isize + offset).try_into()
            as Result<usize, _>)
            .ok()?;
        self.reset_to(new_idx)
    }
}

pub struct Lexer<'r> {
    file: SourceFile<'r>,
    prev_idx: BytePos,
    idx: BytePos,
    dcx: &'r DiagCtxt<'r>,
}

impl<'r> Lexer<'r> {
    pub fn new(filepath: &'r Path, filetext: &'r str, dcx: &'r DiagCtxt<'r>) -> Lexer<'r> {
        Lexer {
            file: SourceFile::new(filepath, filetext),
            prev_idx: 0.into(),
            idx: 0.into(),
            dcx,
        }
    }

    /// Advance the iterator and the index (self.idx)
    pub fn pop(&mut self) -> Option<char> {
        self.idx += 1;
        self.file.pop()
    }

    pub fn expect(&mut self, expected: char) {
        let popped = self.pop().unwrap();
        assert_eq!(popped, expected, "Expected to be the same")
    }

    #[inline]
    pub fn peek(&mut self) -> Option<char> {
        self.file.peek()
    }

    /// Current location
    pub fn current_span(&self) -> Span {
        Span {
            lo: self.prev_idx,
            hi: self.idx,
        }
    }

    /// Current location but used when we know we are at the end of file.
    pub fn current_span_end(&self) -> Span {
        Span {
            lo: self.prev_idx,
            hi: self.idx - 1.into(),
        }
    }

    pub fn lex(&mut self) -> PartialResult<Vec<RawToken>> {
        let mut tokens = Vec::new();
        let mut diags: Vec<Diag> = Vec::new();

        loop {
            match self.make_token() {
                PartialResult::Good(tok @ RawToken { tt: EOF, .. }) => {
                    tokens.push(tok);
                    break;
                }
                PartialResult::Good(tok) => tokens.push(tok),
                PartialResult::Fuzzy(tok, dgs) => {
                    tokens.push(tok);
                    diags.extend(dgs);
                }
                PartialResult::Fail(dgs) => {
                    diags.extend(dgs);
                    return PartialResult::Fail(diags);
                }
            }
        }

        if diags.is_empty() {
            PartialResult::Good(tokens)
        } else {
            PartialResult::Fuzzy(tokens, diags)
        }
    }

    pub fn make_token(&mut self) -> PartialResult<RawToken> {
        self.prev_idx = self.idx;

        let tt = 'm: {
            match self.pop() {
                Some(c @ ('A'..='Z' | 'a'..='z' | '_' | '0'..='9')) => {
                    return self.lex_word(c);
                }
                Some('"') => return self.lex_str(),
                Some('\'') => return self.lex_char(),
                Some('\n') => NewLine,
                Some(w) if w.is_whitespace() => WhiteSpace,
                Some('/') => match self.peek() {
                    Some('/') => {
                        // skip the second slash
                        self.pop();

                        let mut comment = String::new();
                        while self.peek() != Some('\n') {
                            comment.push(self.pop().unwrap());
                        }
                        Comment(comment)
                    }
                    Some('*') => {
                        // TODO: Add support for nested multiline comments
                        // skip the asterisk
                        self.pop();

                        let mut comment = String::new();
                        loop {
                            match self.pop() {
                                Some('*') if self.peek() == Some('/') => break,
                                Some(c) => comment.push(c),
                                None => {
                                    return PartialResult::new_fail(self.dcx.struct_err(
                                        "unterminated multiple line comment",
                                        self.current_span_end(),
                                    ))
                                }
                            }
                        }

                        // skip the end of the multiline comment, `*/`
                        self.pop();
                        self.pop();
                        Comment(comment)
                    }
                    _ => Punct(Punctuation::Slash),
                },
                Some(c) => {
                    if let Some(punct) = self.could_make_punct(c) {
                        // pop the lenght of the punctuation.
                        for _ in 0..punct.size() - 1 {
                            self.pop();
                        }
                        break 'm RawTokenType::Punct(punct);
                    }
                    let err = self
                        .dcx
                        .struct_err(format!("unknown start of token {c:?}"), self.current_span());
                    return PartialResult::new_fail(err);
                }
                None => {
                    let len = self.file.length();
                    return PartialResult::Good(RawToken {
                        tt: EOF,
                        loc: Span::new(len - 1, len),
                    });
                }
            }
        };

        PartialResult::Good(RawToken {
            tt,
            loc: self.current_span(),
        })
    }

    pub fn make_word(&mut self, c: char) -> (String, bool) {
        let mut word = String::from(c);
        let mut numeric = c.is_numeric();

        while let Some(c) = self.peek() {
            match c {
                'A'..='Z' | 'a'..='z' => {
                    word.push(c);
                    numeric = false;
                }
                '0'..='9' | '_' => {
                    word.push(c);
                }
                _ => break,
            }
            self.pop();
        }

        (word, numeric)
    }

    pub fn lex_word(&mut self, c: char) -> PartialResult<RawToken> {
        let (word, numeric) = self.make_word(c);

        let tt = if numeric {
            return self.lex_int(word);
        } else {
            self.lex_keyword(word)
        };

        PartialResult::Good(RawToken {
            tt,
            loc: self.current_span(),
        })
    }

    pub fn lex_keyword(&self, word: String) -> RawTokenType {
        if let Ok(kw) = Keyword::from_str(&word) {
            RawTokenType::KW(kw)
        } else {
            RawTokenType::Ident(word)
        }
    }

    // Try to make punctuation, expect the `Slash` punctuation that is handled
    // somewhere else
    pub fn could_make_punct(&mut self, c: char) -> Option<Punctuation> {
        use Punctuation::*;
        Some(match c {
            // single char punctuation
            '(' => LParen,
            ')' => RParen,
            '[' => LBracket,
            ']' => RBracket,
            '{' => LBrace,
            '}' => RBrace,
            ':' => Colon,
            ';' => Semi,
            ',' => Comma,
            '@' => At,
            '*' => Asterisk,
            '^' => Caret,
            '.' => Dot,
            '%' => Percent,
            '+' => Plus,

            // ambigious
            '!' => match self.peek() {
                Some('=') => ExclamationmarkEqual,
                _ => Exclamationmark,
            },
            '=' => match self.peek() {
                Some('=') => Equal2,
                _ => Equal,
            },
            '<' => match self.peek() {
                Some('<') => LArrow2,
                Some('=') => LArrowEqual,
                _ => LArrow,
            },
            '>' => match self.peek() {
                Some('>') => RArrow2,
                Some('=') => RArrowEqual,
                _ => RArrow,
            },
            '-' => match self.peek() {
                Some('>') => ThinRArrow,
                _ => Minus,
            },

            _ => return None,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const UNIT_TEST_PATH: &str = "<unit test>";
    const TEXT1: &str = "Gatti 😸";

    macro_rules! unit_test_path {
        ($path_str:ident) => {
            Path::new($path_str.into())
        };
        () => {
            unit_test_path!(UNIT_TEST_PATH)
        };
    }

    #[test]
    fn lexr_file_peek_pop() {
        let mut lfile = SourceFile::new(unit_test_path!(), TEXT1);
        assert_eq!(lfile.peek(), Some('G'));
        assert_eq!(lfile.pop(), Some('G'));

        assert_eq!(lfile.peek(), Some('a'));
        assert_eq!(lfile.pop(), Some('a'));

        assert_eq!(lfile.peek(), Some('t'));
        assert_eq!(lfile.pop(), Some('t'));

        assert_eq!(lfile.peek(), Some('t'));
        assert_eq!(lfile.pop(), Some('t'));

        assert_eq!(lfile.peek(), Some('i'));
        assert_eq!(lfile.pop(), Some('i'));

        assert_eq!(lfile.peek(), Some(' '));
        assert_eq!(lfile.pop(), Some(' '));

        assert_eq!(lfile.peek(), Some('😸'));
        assert_eq!(lfile.pop(), Some('😸'));

        assert_eq!(lfile.pop(), None);
        assert_eq!(lfile.pop(), None);
    }

    #[test]
    fn lexr_file_reset() {
        let mut lfile = SourceFile::new(unit_test_path!(), TEXT1);
        assert_eq!(lfile.pop(), Some('G'));
        assert_eq!(lfile.pop(), Some('a'));
        assert_eq!(lfile.pop(), Some('t'));
        assert_eq!(lfile.pop(), Some('t'));
        assert_eq!(lfile.pop(), Some('i'));
        lfile.reset_to(3);
        assert_eq!(lfile.peek(), Some('t'));
        assert_eq!(lfile.pop(), Some('t'));

        lfile.reset_to(7);
        assert_eq!(lfile.pop(), None);
        assert_eq!(lfile.pop(), None);
    }

    #[test]
    fn lexer_peek_pop() {
        let dcx = DiagCtxt::new(TEXT1, unit_test_path!());
        let mut lexer = Lexer::new(unit_test_path!(), TEXT1, &dcx);

        assert_eq!(lexer.pop(), Some('G'));

        assert_eq!(lexer.peek(), Some('a'));
        assert_eq!(lexer.peek(), Some('a'));
        assert_eq!(lexer.pop(), Some('a'));

        assert_eq!(lexer.pop(), Some('t'));

        assert_eq!(lexer.pop(), Some('t'));

        assert_eq!(lexer.current_span(), Span::new(0, 4));

        assert_eq!(lexer.pop(), Some('i'));
        assert_eq!(lexer.pop(), Some(' '));
        assert_eq!(lexer.pop(), Some('😸'));
        assert_eq!(lexer.pop(), None);
    }

    #[test]
    fn lexer_identifier_and_keywords() {
        let text = "abc fun  return let mut type true false pub break continue";
        let dcx = DiagCtxt::new(text, unit_test_path!());
        let mut lexer = Lexer::new(unit_test_path!(), text, &dcx);

        let actual: Vec<_> = lexer
            .lex()
            .unwrap()
            .iter()
            .map(|rt| rt.tt.clone())
            .collect();

        let expected = vec![
            Ident("abc".to_string()),
            WhiteSpace,
            KW(Keyword::Fun),
            WhiteSpace,
            WhiteSpace,
            KW(Keyword::Return),
            WhiteSpace,
            KW(Keyword::Let),
            WhiteSpace,
            KW(Keyword::Mut),
            WhiteSpace,
            KW(Keyword::Type),
            WhiteSpace,
            KW(Keyword::True),
            WhiteSpace,
            KW(Keyword::False),
            WhiteSpace,
            KW(Keyword::Pub),
            WhiteSpace,
            KW(Keyword::Break),
            WhiteSpace,
            KW(Keyword::Continue),
            EOF,
        ];

        assert_eq!(actual, expected);
    }

    #[test]
    #[should_panic]
    fn lexer_too_large_int() {
        let text = u128::MAX.to_string();
        let dcx = DiagCtxt::new(&text, unit_test_path!());
        let mut lexer = Lexer::new(unit_test_path!(), &text, &dcx);
        // Should panic because we unwrap an Fuzzy::Err due to the
        // source code containing a number too large to fit in the int literal
        lexer.lex().unwrap();
    }
}
