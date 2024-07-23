use crate::{
    derive_loc,
    errors::PartialResult::{self, *},
    expect_token, parse,
    parser::stmt::Statement,
    toks::{Punctuation, Token, TokenType::*},
    Span,
};

use super::{expr::Expression, stmt::StatementInner, AstNode, FmtToken, Parser};

#[derive(Debug, Clone)]
pub struct Block {
    pub stmts: Vec<Statement>,
    pub ret_expr: Option<Expression>,
    pub loc: Span,
}

derive_loc!(Block);

impl AstNode for Block {
    type Output = Self;

    fn parse(parser: &mut Parser<'_>) -> PartialResult<Self::Output> {
        let ((), start) = expect_token!(parser => [Punct(Punctuation::LBrace), ()], [FmtToken::Punct(Punctuation::LBrace)]);
        let mut stmts = Vec::new();
        loop {
            if let Some(Token {
                tt: Punct(Punctuation::RBrace),
                ..
            }) = parser.peek_tok()
            {
                break;
            }

            let stmt = parse!(parser => Statement);
            stmts.push(stmt);
            expect_token!(@noloc parser
                => [
                    Punct(Punctuation::SemiColon), ();
                    Punct(Punctuation::RBrace), (), in break;
                ],
                [
                    FmtToken::Punct(Punctuation::SemiColon),
                    FmtToken::Punct(Punctuation::RBrace),
                ]
            );
        }
        let ((), end) = expect_token!(parser => [Punct(Punctuation::RBrace), ()], [FmtToken::Punct(Punctuation::RBrace)]);

        let ret_expr = if let Some(Statement {
            stmt: StatementInner::ExprStmt(expr),
            ..
        }) = stmts.last()
        {
            let expr = expr.clone();
            stmts.pop();
            Some(expr)
        } else {
            None
        };

        Good(Block {
            stmts,
            ret_expr,
            loc: Span::from_ends(start, end),
        })
    }
}
