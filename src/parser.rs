//! Parsing of Gatti's tokens to an Abstract Syntax Tree.
use std::fmt::{self, Display, Write};

use crate::{
    errors::{
        DiagCtxt, DiagStream,
        PartialResult::{self, *},
    },
    toks::{
        Keyword, Punctuation, Token, TokenStream,
        TokenType::{Punct, EOF},
    },
    Span,
};

use self::{decl::Declaration, precedence::PrecedenceValue};

pub mod block;
pub mod decl;
pub mod expr;
pub mod precedence;
pub mod stmt;
pub mod types;

/// The parser for the Gatti Programming language.
///
/// It is implemented with a [Recursive Descent Parser][rdp] for everything
/// expect the parsing for Binary Expression that uses a [Operator-precedence
/// parser][opp].
///
/// [rdp]: https://en.wikipedia.org/wiki/Recursive_descent_parser
/// [opp]: https://en.wikipedia.org/wiki/Operator-precedence_parser
pub struct Parser<'gi> {
    // /// the actual precedence value when parsing expressions
    // current_precedence: PrecedenceValue,
    //..
    /// Diagnostic context
    dcx: &'gi DiagCtxt<'gi>,
    /// The token stream containing the lexed tokens
    ts: TokenStream,
    /// Token index of the next Token that will be `pop`ed, in the TokenStream
    ti: usize,
    /// The current value of the precedence, used to parse binary and unary expressions
    current_precedence: PrecedenceValue,
}

impl<'gi> Parser<'gi> {
    /// Creates a new parser. :)
    pub fn new(dcx: &'gi DiagCtxt, ts: TokenStream) -> Parser<'gi> {
        Parser {
            dcx,
            ts,
            ti: 0,
            current_precedence: 0,
        }
    }

    /// Pops a tokens of the stream
    ///
    /// If there is no more tokens in the stream, it will not increment the
    /// `ti` field.
    #[inline]
    pub fn pop(&mut self) -> Option<Token> {
        let tok = self.peek_tok()?.clone();
        self.ti += 1;
        Some(tok)
    }

    /// Get the `nth` token ahead of the next to be popped
    #[inline]
    pub fn nth_tok(&self, idx: usize) -> Option<&Token> {
        self.ts.get(self.ti + idx)
    }

    /// Get the token that will be popped if you call `pop` after this call.
    #[inline]
    pub fn peek_tok(&self) -> Option<&Token> {
        self.nth_tok(0)
    }

    pub fn begin_parsing(&mut self) -> PartialResult<Vec<Declaration>> {
        let mut decls = Vec::new();
        let mut diags = DiagStream::new();

        loop {
            // If we reached the EOF, break of the loop
            if let Some(Token { tt: EOF, .. }) = self.peek_tok() {
                self.pop();
                break;
            }
            // Parse the declaration
            match Declaration::parse(self) {
                Good(decl) => decls.push(decl),
                Fuzzy(decl, dgs) => {
                    decls.push(decl);
                    diags.extend(dgs);
                }
                Fail(dgs) => {
                    diags.extend(dgs);
                    break;
                }
            }

            // Expect a semicolon after the decl
            if let Some(Token {
                tt: Punct(Punctuation::Semi),
                ..
            }) = self.peek_tok()
            {
                self.pop();
            } else {
                let end = decls
                    .last()
                    .map(|d| d.loc().hi.0.checked_sub(1).unwrap_or(0))
                    .unwrap_or(0);
                diags.push(self.dcx.struct_err(
                    "expected a semicolon after the declaration",
                    Span::from_inner((end as usize).into(), (end as usize + 1).into()),
                ))
            }
        }

        if diags.is_empty() {
            Good(decls)
        } else {
            Fuzzy(decls, diags)
        }
    }
}

pub trait AstNode: fmt::Debug {
    type Output: Location;

    fn parse(parser: &mut Parser<'_>) -> PartialResult<Self::Output>;
}

pub trait Location {
    fn loc(&self) -> Span;
}

#[macro_export]
macro_rules! derive_loc {
    ($t:ty $(where $( $tt:tt )* )? ) => {
        impl $( $( $tt )* )? $crate::parser::Location for $t {
            #[inline]
            fn loc(&self) -> crate::Span {
                self.loc.clone()
            }
        }
    };
}

pub enum FmtToken {
    KW(Keyword),

    Punct(Punctuation),

    IntLiteral,
    StrLiteral,
    CharLiteral,

    Identifier,
    NamedIdentifier(String),

    Indent,
    NewLine,

    EndOfFile,
}

impl Display for FmtToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::KW(kw) => write!(f, "{kw}"),
            Self::Punct(punct) => write!(f, "{punct}"),
            Self::IntLiteral => write!(f, "integer literal"),
            Self::StrLiteral => write!(f, "string literal"),
            Self::CharLiteral => write!(f, "char literal"),
            Self::Identifier => write!(f, "identifier"),
            Self::NamedIdentifier(name) => write!(f, "{name}"),
            Self::Indent => write!(f, "indendation"),
            Self::NewLine => write!(f, "new line"),
            Self::EndOfFile => write!(f, "end of file"),
        }
    }
}

pub enum AstPart {
    Expression,
    Statement,
    FunctionDef,
    Declaration,
    ImportDecl,
    UnaryOperator,
    Type,
}

impl Display for AstPart {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Expression => write!(f, "expression"),
            Self::Statement => write!(f, "statement"),
            Self::FunctionDef => write!(f, "function definition"),
            Self::Declaration => write!(f, "declaration"),
            Self::ImportDecl => write!(f, "import declaration"),
            Self::UnaryOperator => write!(f, "unary operator"),
            Self::Type => write!(f, "type"),
        }
    }
}

/// This macro is used to expect a token from the parser, one of the most
/// useful macro in the parser
///
/// # Note
///
/// If you use a value contained in the token, like the content of a string
/// literal, an integer literal, or an identifier, remember to either
/// dereference it, if it implements [`Copy`] or [clone][`Clone`] it if it
/// doesn't.
#[macro_export]
macro_rules! expect_token {
    ($parser:expr => [ $($token:pat, $result:expr $(,in $between:stmt)?);* ] else $unexpected:block) => (
        match $parser.peek_tok() {
            $(
                Some($crate::toks::Token { tt: $token, .. }) => {
                    $(
                        $between
                    )?
                    // we allow unreacheable code because the $between type may be `!`
                    #[allow(unreachable_code)]
                    ($result, $parser.pop().unwrap().loc)
                }
            )*
            _ => $unexpected
        }
    );

    ($parser:expr => [ $($token:pat, $result:expr $(,in $between:stmt)?);* $( ; )?], $expected:expr) => (
        match $parser.peek_tok() {
            $(
                // we allow unused variable in case of a $between that terminates.
                #[allow(unused_variables)]
                Some($crate::toks::Token {
                    tt: $token,
                    ..
                }) => {
                    $(
                        $between
                    )?
                    // we allow unreacheable code because the $between type may
                    // be `!` and we can use unwraps and we already know that
                    // there is a tokens with a location so it is sure we wont
                    // panic
                    #[allow(unreachable_code)]
                    ($result, $parser.pop().unwrap().loc.unwrap())
                }
            )*
            Some($crate::toks::Token { tt, loc: Some(loc) }) => {
                return $crate::errors::PartialResult::new_fail(
                    $parser
                    .dcx
                    .struct_err($crate::parser::expected_tok_msg(tt, $expected), loc.clone())
                )
            }
            // TODO: remove those panics and throw errors
            None => panic!("Tried to expect a token but the end of file has been reached"),
            _ => panic!("Expected a token but found a semicolon without location"),
        }
    );

    (@noloc $parser:expr => [ $($token:pat, $result:expr $(,in $between:stmt)?);* $( ; )?], $expected:expr) => (
        match $parser.peek_tok() {
            $(
                // we allow unused variable in case of a $between that terminates.
                #[allow(unused_variables)]
                Some($crate::toks::Token {
                    tt: $token,
                    ..
                }) => {
                    $(
                        $between
                    )?
                    // we allow unreacheable code because the $between type may
                    // be `!` and we can use unwraps and we already know that
                    // there is a tokens with a location so it is sure we wont
                    // panic
                    #[allow(unreachable_code)]
                    $result
                }
            )*
            Some($crate::toks::Token { tt, loc: Some(loc) }) => {
                return $crate::errors::PartialResult::new_fail(
                    $parser
                    .dcx
                    .struct_err($crate::parser::expected_tok_msg(tt, $expected), loc.clone())
                )
            }
            // TODO: remove those panics and throw errors
            None => panic!("Tried to expect a token but the end of file has been reached"),
            _ => panic!("Expected a token but found a semicolon without location"),
        }
    )
}

#[macro_export]
macro_rules! parse {
    ($parser:expr => $node:ty) => {
        parse!(@fn $parser => <$node as $crate::parser::AstNode>::parse)
    };
    (@fn $parser:expr => $parsing_fn:expr $(, $arg:expr)*) => (
        match $parsing_fn($parser $(, $arg)*) {
            $crate::errors::PartialResult::Good(ast) => ast,
            $crate::errors::PartialResult::Fuzzy(ast, dgs) => {
                $parser.dcx.emit_diags(dgs);
                ast
            }
            $crate::errors::PartialResult::Fail(err) =>
                return $crate::errors::PartialResult::Fail(err),
        }
    )
}

pub fn expected_tok_msg<const N: usize>(
    found: impl Display,
    expected: [impl Display; N],
) -> String {
    // TODO: when the token is a literal, don't use the lexeme instead so int
    // literals with `0x` will be rendered with the `0x` and the rest in hex.
    format!("expected {}, found {}", format_expected(expected), found)
}

fn format_expected<const N: usize>(exptd: [impl Display; N]) -> String {
    if exptd.len() == 1 {
        return format!("{}", exptd.first().unwrap());
    }
    let mut s = String::new();

    for (idx, token) in exptd.iter().enumerate() {
        if idx == exptd.len() - 2 {
            write!(s, "{token} ")
        } else if idx == exptd.len() - 1 {
            write!(s, "or {token}")
        } else {
            write!(s, "{token}, ")
        }
        .unwrap();
    }

    s
}
