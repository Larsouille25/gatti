//! Module responsible for parsing Gatti's declarations, like functions, type
//! defs, imports

use crate::{derive_loc, expect_token, parse, FmtToken};

use gattic_errors::{
    spans::Span,
    DiagStream,
    PartialResult::{self, *},
};
use gattic_tokens::{Keyword, Punctuation, Token, TokenType::*};

use super::{block::Block, expected_tok_msg, types::Type, AstNode, AstPart, Location, Parser};

#[derive(Debug, Clone, PartialEq)]
pub enum Visibility {
    Public { loc: Span },
    Private,
}

impl Location for Visibility {
    fn loc(&self) -> Span {
        match self {
            Visibility::Public { loc } => loc.clone(),
            Visibility::Private => panic!("No location on private visibility"),
        }
    }
}

impl AstNode for Visibility {
    type Output = Self;

    fn parse(parser: &mut Parser<'_>) -> PartialResult<Self::Output> {
        let (is_pub, loc) =
            expect_token!(parser => [KW(Keyword::Pub), true] else { (false, None) });
        if is_pub {
            // Should not panic because only semicolons may not have a location
            // we know it's not a semicolon
            Good(Visibility::Public { loc: loc.unwrap() })
        } else {
            Good(Visibility::Private)
        }
    }
}

#[derive(Debug, Clone)]
pub struct Declaration {
    pub decl: DeclarationInner,
    pub loc: Span,
}

derive_loc!(Declaration);

impl AstNode for Declaration {
    type Output = Declaration;

    fn parse(parser: &mut Parser<'_>) -> PartialResult<Self::Output> {
        // TODO: add support for parsing visibilities
        match parser.peek_tok() {
            Some(Token {
                tt: KW(Keyword::Fun),
                ..
            }) => parse_fun_decl(parser),
            Some(t) => {
                let tok = t.clone();
                PartialResult::new_fail(
                    parser
                        .dcx
                        .struct_err(expected_tok_msg(tok.tt, [AstPart::Declaration]), tok.loc),
                )
            }
            None => parser.reached_eof_diag(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum DeclarationInner {
    Function {
        vis: Visibility,
        proto: Prototype,
        body: Block,
    },
}

#[derive(Debug, Clone)]
pub struct Prototype {
    pub name: String,
    pub args: Vec<Arg>,
    pub ret: Option<Type>,
    pub loc: Span,
}

derive_loc!(Prototype);

impl AstNode for Prototype {
    type Output = Self;

    fn parse(parser: &mut Parser<'_>) -> PartialResult<Self::Output> {
        // Name parsing
        let (name, start) =
            expect_token!(parser => [Ident(id), id.clone()], [FmtToken::Identifier]);

        // Arguments parsing
        expect_token!(parser => [Punct(Punctuation::LParen), ()], [FmtToken::Punct(Punctuation::LParen)]);
        let mut args = Vec::new();
        loop {
            if let Some(Token {
                tt: Punct(Punctuation::RParen),
                ..
            }) = parser.peek_tok()
            {
                break;
            }

            let arg = parse!(parser => Arg);
            args.push(arg);
            expect_token!(parser
                => [
                    Punct(Punctuation::Colon), ();
                    Punct(Punctuation::RParen), (), in break;
                ],
                [
                    FmtToken::Punct(Punctuation::Colon),
                    FmtToken::Punct(Punctuation::RParen),
                ]
            );
        }
        let ((), paren_end) = expect_token!(parser => [Punct(Punctuation::RParen), ()], [FmtToken::Punct(Punctuation::LParen)]);

        // Return type parsing
        let (end, ret) = if let Some(Token {
            tt: Punct(Punctuation::Arrow),
            ..
        }) = parser.peek_tok()
        {
            expect_token!(parser => [Punct(Punctuation::Arrow), ()], [FmtToken::Punct(Punctuation::Arrow)]);

            let ret = parse!(parser => Type);

            (ret.loc.clone(), Some(ret))
        } else {
            (paren_end, None)
        };

        Good(Prototype {
            name,
            args,
            ret,
            loc: Span::from_ends(start, end),
        })
    }
}

#[derive(Debug, Clone)]
pub struct Arg {
    pub name: String,
    pub ty: Type,
    pub loc: Span,
}

derive_loc!(Arg);

impl AstNode for Arg {
    type Output = Self;

    fn parse(parser: &mut Parser<'_>) -> PartialResult<Self::Output> {
        let (name, start) =
            expect_token!(parser => [Ident(name), name.clone()], [FmtToken::Identifier]);
        expect_token!(parser => [Punct(Punctuation::Colon), ()], [FmtToken::Punct(Punctuation::Colon)]);
        let ty = parse!(parser => Type);
        let end = ty.loc.clone();

        Good(Arg {
            name,
            ty,
            loc: Span::from_ends(start, end),
        })
    }
}

pub fn parse_fun_decl(parser: &mut Parser<'_>) -> PartialResult<Declaration> {
    let vis = parse!(parser => Visibility);

    let ((), fun_start) =
        expect_token!(parser => [KW(Keyword::Fun), ()], [FmtToken::KW(Keyword::Fun)]);

    let start = match vis {
        Visibility::Public { ref loc } => loc.clone(),
        Visibility::Private => fun_start,
    };

    let proto = parse!(parser => Prototype);

    let body = parse!(parser => Block);

    let end = body.loc.clone();
    Good(Declaration {
        decl: DeclarationInner::Function { vis, proto, body },
        loc: Span::from_ends(start, end),
    })
}

#[derive(Clone, Debug)]
pub struct DeclarationList {
    pub decls: Vec<Declaration>,
    pub loc: Span,
}

derive_loc!(DeclarationList);

impl AstNode for DeclarationList {
    type Output = Self;

    fn parse(parser: &mut Parser<'_>) -> PartialResult<Self::Output> {
        let mut decls = Vec::new();
        let mut diags = DiagStream::new();

        loop {
            // If we reached the EOF, break of the loop
            if let Some(Token { tt: EOF, .. }) = parser.peek_tok() {
                parser.pop();
                break;
            }
            // Parse the declaration
            match Declaration::parse(parser) {
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
            expect_token!(@noloc parser => [Punct(Punctuation::SemiColon), ()], [FmtToken::Punct(Punctuation::SemiColon)]);
        }

        let start = decls.first().map(|d| d.loc.clone()).unwrap_or_default();
        let mut end = start.clone();
        if let Some(Declaration { loc, .. }) = decls.last() {
            end = loc.clone();
        }

        let res = DeclarationList {
            decls,
            loc: Span::from_ends(start, end),
        };

        if diags.is_empty() {
            Good(res)
        } else {
            Fuzzy(res, diags)
        }
    }
}
