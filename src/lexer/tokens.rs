//! Gatti's raws tokens.

use std::fmt::Display;

use crate::{
    toks::{Keyword, Punctuation},
    Span,
};

/// Raw tokens are used between the lexer and the parser, replaced by simple
/// `Token` after the semicolons are inserted
#[derive(Debug, Clone)]
pub struct RawToken {
    pub tt: RawTokenType,
    pub loc: Span,
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
