use crate::{derive_loc, parse};

use gattic_errors::{
    spans::Span,
    PartialResult::{self, *},
};

use super::{expr::Expression, AstNode, Parser};

#[derive(Debug, Clone)]
pub enum StatementInner {
    ExprStmt(Expression),
}

#[derive(Debug, Clone)]
pub struct Statement {
    pub stmt: StatementInner,
    pub loc: Span,
}

derive_loc!(Statement);

impl AstNode for Statement {
    type Output = Self;

    fn parse(parser: &mut Parser<'_>) -> PartialResult<Self::Output> {
        // TODO: add support for variable declarations
        match parser.peek_tok() {
            _ => parse_expr_stmt(parser),
        }
    }
}

pub fn parse_expr_stmt(parser: &mut Parser<'_>) -> PartialResult<Statement> {
    let expr = parse!(parser => Expression);
    Good(Statement {
        loc: expr.loc.clone(),
        stmt: StatementInner::ExprStmt(expr),
    })
}
