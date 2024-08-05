use crate::{
    derive_loc,
    errors::PartialResult::{self, *},
    expect_token, parse,
    toks::{Keyword, Punctuation, Token, TokenType::*},
    Span,
};

use super::{expected_tok_msg, AstNode, AstPart, FmtToken, Parser};

#[derive(Debug, Clone)]
pub enum TypeInner {
    // unsigned integers
    UInt8,
    UInt16,
    UInt32,
    UInt64,
    UInt,
    // signed integers
    Int8,
    Int16,
    Int32,
    Int64,
    Int,

    Bool,
    Char,
    // String,

    // e.g: `fun (int, bool) -> int` is a fn ptr
    // like `fun ()` is also a fn ptr
    FnPtr {
        args: Vec<Type>,
        ret: Option<Box<Type>>,
    },
}

impl TypeInner {
    pub fn is_primitive_type(ty: &str) -> bool {
        matches!(
            ty,
            "uint8"
                | "uint16"
                | "uint32"
                | "uint64"
                | "uint"
                | "int8"
                | "int16"
                | "int32"
                | "int64"
                | "int"
                | "bool"
                | "char"
        )
    }
}

#[derive(Debug, Clone)]
pub struct Type {
    pub ty: TypeInner,
    pub loc: Span,
}

derive_loc!(Type);

impl AstNode for Type {
    type Output = Self;

    fn parse(parser: &mut Parser<'_>) -> PartialResult<Self::Output> {
        match parser.peek_tok() {
            Some(Token {
                tt: Ident(name), ..
            }) if TypeInner::is_primitive_type(name) => parse_primitive_type(parser),
            Some(Token {
                tt: KW(Keyword::Fun),
                ..
            }) => parse_fn_ptr_type(parser),
            Some(t) => {
                let tok = t.clone();
                PartialResult::new_fail(
                    parser
                        .dcx
                        .struct_err(expected_tok_msg(tok.tt, [AstPart::Type]), tok.loc),
                )
            }
            None => parser.reached_eof_diag(),
        }
    }
}

pub fn parse_primitive_type(parser: &mut Parser<'_>) -> PartialResult<Type> {
    let (ty_str, loc) =
        expect_token!(parser => [Ident(ty_str), ty_str.clone()], [FmtToken::Identifier]);

    let ty = match ty_str.as_str() {
        "uint8" => TypeInner::UInt8,
        "uint16" => TypeInner::UInt16,
        "uint32" => TypeInner::UInt32,
        "uint64" => TypeInner::UInt64,
        "uint" => TypeInner::UInt,

        "int8" => TypeInner::Int8,
        "int16" => TypeInner::Int16,
        "int32" => TypeInner::Int32,
        "int64" => TypeInner::Int64,
        "int" => TypeInner::Int,

        "bool" => TypeInner::Bool,
        "char" => TypeInner::Char,
        _ => {
            return PartialResult::new_fail(parser.dcx.struct_err(
                expected_tok_msg(FmtToken::NamedIdentifier(ty_str), ["primitive type"]),
                loc,
            ))
        }
    };

    Good(Type { ty, loc })
}

pub fn parse_fn_ptr_type(parser: &mut Parser<'_>) -> PartialResult<Type> {
    let ((), start) = expect_token!(parser => [KW(Keyword::Fun), ()], [FmtToken::KW(Keyword::Fun)]);

    expect_token!(parser => [Punct(Punctuation::LParen), ()], [FmtToken::Punct(Punctuation::LParen)]);
    let mut args = Vec::new();
    loop {
        let arg = parse!(parser => Type);
        args.push(arg);
        expect_token!(parser => [Punct(Punctuation::Colon), (); Punct(Punctuation::RParen), (), in break], [FmtToken::Punct(Punctuation::Colon), FmtToken::Punct(Punctuation::LParen)]);
    }
    let ((), paren_end) = expect_token!(parser => [Punct(Punctuation::RParen), ()], [FmtToken::Punct(Punctuation::LParen)]);

    let (end, ret) = if let Some(Token {
        tt: Punct(Punctuation::Arrow),
        ..
    }) = parser.peek_tok()
    {
        expect_token!(parser => [Punct(Punctuation::Arrow), ()], [FmtToken::Punct(Punctuation::Arrow)]);

        let ret = Box::new(parse!(parser => Type));

        (ret.loc.clone(), Some(ret))
    } else {
        (paren_end, None)
    };

    Good(Type {
        ty: TypeInner::FnPtr { args, ret },
        loc: Span::from_ends(start, end),
    })
}
