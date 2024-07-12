//! Gatti's raws tokens.

use std::{fmt::Display, str::FromStr};

use crate::Span;

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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Punctuation {
    // Delimiters:
    RParen,
    LParen,

    RBracket,
    LBracket,

    RBrace,
    LBrace,

    // Punctuation:
    Colon,
    Semi,
    Comma,
    At,
    /// '->'
    ThinRArrow,

    // Operators:
    Asterisk,
    Caret,
    Dot,
    Equal,
    Equal2,
    Exclamationmark,
    ExclamationmarkEqual,
    LArrow,
    LArrow2,
    LArrowEqual,
    Minus,
    Percent,
    Plus,
    RArrow,
    RArrow2,
    RArrowEqual,
    Slash,
}

impl Punctuation {
    pub fn size(&self) -> usize {
        use Punctuation::*;
        match self {
            RParen | LParen | RBracket | LBracket | RBrace | LBrace | Colon | Semi | Comma | At
            | Asterisk | Caret | Dot | Equal | Exclamationmark | LArrow | Minus | Percent
            | Plus | RArrow | Slash => 1,
            ThinRArrow | Equal2 | ExclamationmarkEqual | LArrow2 | LArrowEqual | RArrow2
            | RArrowEqual => 2,
        }
    }
}

impl Display for Punctuation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::RParen => "(",
                Self::LParen => ")",

                Self::RBracket => "[",
                Self::LBracket => "]",

                Self::RBrace => "{",
                Self::LBrace => "}",

                Self::Colon => ":",
                Self::Semi => ";",
                Self::Comma => ",",
                Self::At => "@",
                Self::ThinRArrow => "->",

                Self::Asterisk => "*",
                Self::Caret => "^",
                Self::Dot => ".",
                Self::Equal => "=",
                Self::Equal2 => "==",
                Self::Exclamationmark => "!",
                Self::ExclamationmarkEqual => "!=",
                Self::LArrow => "<",
                Self::LArrow2 => "<<",
                Self::LArrowEqual => "<=",
                Self::Minus => "-",
                Self::Percent => "%",
                Self::Plus => "+",
                Self::RArrow => ">",
                Self::RArrow2 => ">>",
                Self::RArrowEqual => ">=",
                Self::Slash => "/",
            }
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Keyword {
    Fun,
    Return,
    Let,
    Mut,
    Type,
    True,
    False,
    If,
    Else,
    Pub,
}

impl FromStr for Keyword {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "fun" => Keyword::Fun,
            "return" => Keyword::Return,
            "let" => Keyword::Let,
            "mut" => Keyword::Mut,
            "type" => Keyword::Type,
            "true" => Keyword::True,
            "false" => Keyword::False,
            "if" => Keyword::If,
            "else" => Keyword::Else,
            "pub" => Keyword::Pub,
            _ => return Err(()),
        })
    }
}

impl Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Fun => "fun",
                Self::Return => "return",
                Self::Let => "let",
                Self::Mut => "mut",
                Self::Type => "type",
                Self::True => "true",
                Self::False => "false",
                Self::If => "if",
                Self::Else => "else",
                Self::Pub => "pub",
            }
        )
    }
}
