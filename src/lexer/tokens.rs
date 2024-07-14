//! Gatti's raws tokens.

use std::fmt::Display;

use crate::{
    toks::{Keyword, Punctuation, Token, TokenType},
    Span,
};

/// Raw tokens are used between the lexer and the parser, replaced by simple
/// `Token` after the semicolons are inserted
#[derive(Debug, Clone)]
pub struct RawToken {
    pub tt: RawTokenType,
    pub loc: Span,
}

impl RawToken {
    /// Return the corresponding Token, or None if their is no corresponding
    /// token, like `WhiteSpace`, `NewLine`, or `Comment`.
    pub fn unraw(self) -> Option<Token> {
        // TODO: Make some tests
        Some(match self {
            RawToken {
                tt: RawTokenType::KW(keyword),
                loc,
            } => Token {
                tt: TokenType::KW(keyword),
                loc: Some(loc),
            },
            RawToken {
                tt: RawTokenType::Punct(punctuation),
                loc,
            } => Token {
                tt: TokenType::Punct(punctuation),
                loc: Some(loc),
            },
            RawToken {
                tt: RawTokenType::Int(i),
                loc,
            } => Token {
                tt: TokenType::Int(i),
                loc: Some(loc),
            },
            RawToken {
                tt: RawTokenType::Str(s),
                loc,
            } => Token {
                tt: TokenType::Str(s),
                loc: Some(loc),
            },
            RawToken {
                tt: RawTokenType::Char(c),
                loc,
            } => Token {
                tt: TokenType::Char(c),
                loc: Some(loc),
            },
            RawToken {
                tt: RawTokenType::Ident(id),
                loc,
            } => Token {
                tt: TokenType::Ident(id),
                loc: Some(loc),
            },
            RawToken {
                tt: RawTokenType::NewLine,
                ..
            } => {
                return None;
            }
            RawToken {
                tt: RawTokenType::Comment(_),
                ..
            } => {
                return None;
            }
            RawToken {
                tt: RawTokenType::WhiteSpace,
                ..
            } => {
                return None;
            }
            RawToken {
                tt: RawTokenType::EOF,
                loc,
            } => Token {
                tt: TokenType::EOF,
                loc: Some(loc),
            },
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RawTokenType {
    /// Keywords
    KW(Keyword),

    /// Operators and Punctuation
    Punct(Punctuation),

    // LITERALS
    /// Integer literal
    Int(u64),
    /// String literal
    Str(String),
    /// Char literal
    Char(char),

    /// Identifier
    Ident(String),

    // RAW TOKS SPECIAL
    /// Newline '\n'
    NewLine,
    /// Comment
    Comment(String),
    /// WhiteSpace
    WhiteSpace,

    /// End of file
    EOF,
}

impl Display for RawTokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::KW(kw) => write!(f, "keyword `{kw}`"),
            Self::Punct(punct) => write!(f, "`{punct}`"),
            Self::Int(i) => write!(f, "int `{i}`"),
            Self::Str(s) => write!(f, "string {s:?}"),
            Self::Char(c) => write!(f, "char {c:?}"),
            Self::Ident(id) => write!(f, "identifier `{id}`"),
            Self::NewLine => write!(f, "newline"),
            Self::WhiteSpace => write!(f, "whitespace"),
            Self::Comment(..) => write!(f, "comment"),
            Self::EOF => write!(f, "end of file"),
        }
    }
}
