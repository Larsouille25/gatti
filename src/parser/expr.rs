//! Module containing parsing for Gatti's Expression using a Pratt Parser or
//! something close to it

use crate::{
    derive_loc,
    errors::PartialResult::{self, *},
    expect_token, parse,
    toks::{
        Keyword, Punctuation, Token,
        TokenType::{self, *},
    },
    Span,
};

use super::{block::Block, expected_tok_msg, AstNode, AstPart, FmtToken, Parser};

/// The associativity, is used to parse the binary operations
#[derive(Clone, Debug, PartialEq)]
pub enum Associativity {
    LeftToRight,
    RightToLeft,
    None,
}

/// The precedence table of the Gatti Programming Language
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Precedence {
    /// [`None`] is a special precedence value, it is used to exit out of the
    /// loop when a non expression token is after an expression. It is not the
    /// [`HIGHEST_PRECEDENCE`] for the same reason.
    #[doc(hidden)]
    __None__,
    // !!! If you change the highest precedence in this enumeration change the
    // HIGHEST_PRECEDENCE constant !!!
    //
    /// `"="`
    Assignement,
    // TODO: implement logical operators, `"and"` and `"or"`
    /// `"or"`
    LogicalOr,
    /// `"and"`
    LogicalAnd,
    // TODO: implement bitwise operators
    /// `"|"`
    BitOr,
    /// `"^"`
    BitXor,
    /// `"&"`
    BitAnd,
    /// `( "==" | "!=" )`
    Equality,
    /// `( "<" | ">" | "<=" | ">=" )`
    Comparison,
    /// `( "<<" | ">>" )`
    Shift,
    /// `( "+" | "-" )`
    Term,
    /// `( "*" | "/" | "%" )`
    Factor,
    /// `op expression`
    Unary,
    /// `expression "(" (expression),* ")"`
    Call,
    /// `( intlit | "true" | "false" | charlit | strlit | "(" expression ")" )`
    Primary,
    /// Like [`__None__`] it is a special variant of [`Precedence`] that should
    /// not be used unless you know exactly what you are doing
    ///
    /// [`__None__`]: Precedence::__None__
    #[doc(hidden)]
    __Last__,
}

impl Precedence {
    /// Returns the [`Precedence`] following the one passed as arg.
    pub fn next(self) -> Precedence {
        match self {
            Self::__None__ => Self::Assignement,
            Self::Assignement => Self::LogicalOr,
            Self::LogicalOr => Self::LogicalAnd,
            Self::LogicalAnd => Self::BitOr,
            Self::BitOr => Self::BitXor,
            Self::BitXor => Self::BitAnd,
            Self::BitAnd => Self::Equality,
            Self::Equality => Self::Comparison,
            Self::Comparison => Self::Shift,
            Self::Shift => Self::Term,
            Self::Term => Self::Factor,
            Self::Factor => Self::Unary,
            Self::Unary => Self::Call,
            Self::Call => Self::Primary,
            Self::Primary => Self::__Last__,
            Self::__Last__ => unreachable!(),
        }
    }

    pub fn associativity(&self) -> Associativity {
        match self {
            Self::Assignement => Associativity::RightToLeft,
            Self::LogicalOr => Associativity::LeftToRight,
            Self::LogicalAnd => Associativity::LeftToRight,
            Self::BitOr => Associativity::LeftToRight,
            Self::BitXor => Associativity::LeftToRight,
            Self::BitAnd => Associativity::LeftToRight,
            Self::Equality => Associativity::LeftToRight,
            Self::Comparison => Associativity::LeftToRight,
            Self::Shift => Associativity::LeftToRight,
            Self::Term => Associativity::LeftToRight,
            Self::Factor => Associativity::LeftToRight,
            Self::Unary => Associativity::RightToLeft,
            Self::Call => Associativity::LeftToRight,
            Self::Primary => Associativity::LeftToRight,
            Self::__Last__ | Self::__None__ => unreachable!(),
        }
    }
}

impl From<TokenType> for Precedence {
    fn from(value: TokenType) -> Self {
        match value {
            Punct(Punctuation::Equal) => Precedence::Assignement,
            Punct(Punctuation::Equal2 | Punctuation::BangEqual) => Precedence::Equality,
            Punct(
                Punctuation::LArrow
                | Punctuation::RArrow
                | Punctuation::LArrowEqual
                | Punctuation::RArrowEqual,
            ) => Precedence::Comparison,
            Punct(Punctuation::LArrow2 | Punctuation::RArrow2) => Precedence::Shift,
            Punct(Punctuation::Plus | Punctuation::Minus) => Precedence::Term,
            Punct(Punctuation::Asterisk | Punctuation::Slash | Punctuation::Percent) => {
                Precedence::Factor
            }
            Punct(Punctuation::LParen) => Precedence::Call,
            _ => Precedence::__None__,
        }
    }
}

/// The higest precedence of [`Precedence`]
pub const HIGHEST_PRECEDENCE: Precedence = Precedence::Assignement;

// TODO: unify the grammars in the docs of the parsing functions, like that:
//
// # Grammar
//
// ```
// test-expr := "test" expr
// ```
#[derive(Debug, Clone)]
pub struct Expression {
    pub expr: ExpressionInner,
    pub loc: Span,
}

derive_loc!(Expression);

/// Parses an expression given the following precedence.
pub fn parse_expr_precedence(
    parser: &mut Parser<'_>,
    precedence: Precedence,
) -> PartialResult<Expression> {
    let mut lhs = match parser.peek_tt() {
        Some(Int(_)) => parse!(@fn parser => parse_intlit_expr),
        Some(KW(Keyword::True | Keyword::False)) => parse!(@fn parser => parse_boollit_expr),
        Some(Char(_)) => parse!(@fn parser => parse_charlit_expr),
        Some(Str(_)) => parse!(@fn parser => parse_strlit_expr),
        Some(Punct(Punctuation::LParen)) => parse!(@fn parser => parse_grouping_expr),
        Some(Ident(_)) => parse!(@fn parser => parse_path_expr),
        Some(KW(Keyword::If)) => parse!(@fn parser => parse_if_expr),
        Some(Punct(maybe_pre_op)) if UnaryOp::from_punct(maybe_pre_op.clone()).is_some() => {
            parse!(@fn parser => parse_pre_unary_expr)
        }
        Some(_) => {
            // unwrap is safe because we already know the next has a token type
            let t = parser.peek_tok().unwrap().clone();
            return PartialResult::new_fail(
                parser
                    .dcx
                    .struct_err(expected_tok_msg(t.tt, [AstPart::Expression]), t.loc),
            );
        }
        None => return parser.reached_eof_diag(),
    };

    loop {
        let Some(tt) = parser.peek_tt().cloned() else {
            break;
        };
        if precedence > Precedence::from(tt) {
            break;
        }

        lhs = match parser.peek_tt() {
            // we match a token here, because, in the future there will be
            // binary operators that are Keyword, like Logical And.
            Some(Punct(Punctuation::LParen)) => {
                parse!(@fn parser => parse_call_expr, Box::new(lhs))
            }
            Some(maybe_bin_op) if BinaryOp::from_tt(maybe_bin_op.clone()).is_some() => {
                parse!(@fn parser => parse_binary_expr, lhs)
            }
            _ => break,
        }
    }

    Good(lhs)
}

#[derive(Debug, Clone)]
pub enum ExpressionInner {
    IntLiteral(u64),
    BoolLiteral(bool),
    CharLiteral(char),
    StrLiteral(String),
    Grouping(Box<Expression>),
    Unary {
        op: UnaryOp,
        expr: Box<Expression>,
    },
    Binary {
        lhs: Box<Expression>,
        op: BinaryOp,
        rhs: Box<Expression>,
    },
    Path(Vec<String>),
    Call {
        callee: Box<Expression>,
        args: Vec<Expression>,
    },
    If {
        predicate: Box<Expression>,
        true_branch: Box<Expression>,
        false_branch: Option<Box<Expression>>,
    },
    Block(Box<Block>),
}

impl AstNode for Expression {
    type Output = Self;

    #[inline]
    #[track_caller]
    fn parse(parser: &mut Parser<'_>) -> PartialResult<Self::Output> {
        parse_expr_precedence(parser, HIGHEST_PRECEDENCE)
    }
}

/// Parse integer literal expression, `intlit`
pub fn parse_intlit_expr(parser: &mut Parser<'_>) -> PartialResult<Expression> {
    let (i, loc) = expect_token!(parser => [Int(i), *i], [FmtToken::IntLiteral]);
    Good(Expression {
        expr: ExpressionInner::IntLiteral(i),
        loc,
    })
}

/// Parse boolean literal expression, `( "true" | "false" )`
pub fn parse_boollit_expr(parser: &mut Parser<'_>) -> PartialResult<Expression> {
    let (bool, loc) = expect_token!(
        parser => [
            KW(Keyword::True), true;
            KW(Keyword::False), false
        ],
        [
            FmtToken::KW(Keyword::True),
            FmtToken::KW(Keyword::False)
        ]
    );
    Good(Expression {
        expr: ExpressionInner::BoolLiteral(bool),
        loc,
    })
}

/// Parse char literal expression, `charlit`
pub fn parse_charlit_expr(parser: &mut Parser<'_>) -> PartialResult<Expression> {
    let (c, loc) = expect_token!(parser => [Char(c), *c], [FmtToken::IntLiteral]);
    Good(Expression {
        expr: ExpressionInner::CharLiteral(c),
        loc,
    })
}

/// Parse string literal expression, `strlit`
pub fn parse_strlit_expr(parser: &mut Parser<'_>) -> PartialResult<Expression> {
    let (s, loc) = expect_token!(parser => [Str(s), s.clone()], [FmtToken::IntLiteral]);
    Good(Expression {
        expr: ExpressionInner::StrLiteral(s),
        loc,
    })
}

/// Parse grouping expression, `"(" expression ")"`
pub fn parse_grouping_expr(parser: &mut Parser<'_>) -> PartialResult<Expression> {
    let ((), start) = expect_token!(parser => [Punct(Punctuation::LParen), ()], [FmtToken::Punct(Punctuation::LParen)]);
    let expr = Box::new(parse!(parser => Expression));
    let ((), end) = expect_token!(parser => [Punct(Punctuation::RParen), ()], [FmtToken::Punct(Punctuation::RParen)]);

    Good(Expression {
        expr: ExpressionInner::Grouping(expr),
        loc: Span::from_ends(start, end),
    })
}

/// Unary Operators
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum UnaryOp {
    // LEFT UNARY OPERATOR
    /// `"-" expression`
    Negation,
    /// `"!" expression`
    LogicalNot,
    // /// `"~" expression`
    // BitNot,
    //
    // RIGHT UNARY OPERATOR
    // /// `expression ".?"`
    // Unwrap,
}

impl UnaryOp {
    pub fn from_punct(punct: Punctuation) -> Option<UnaryOp> {
        use Punctuation as Punct;
        use UnaryOp as UOp;
        Some(match punct {
            Punct::Minus => UOp::Negation,
            Punct::Bang => UOp::LogicalNot,
            _ => return None,
        })
    }

    /// Is the unary operator on the left of the operand -> Pre Unary Operator
    pub fn is_pre(&self) -> bool {
        matches!(self, Self::Negation | Self::LogicalNot)
    }

    /// Is the unary operator on the right of the operand -> Post Unary Operator
    pub fn is_post(&self) -> bool {
        !self.is_pre()
    }
}

/// Parse pre-unary expression, `op expression`
pub fn parse_pre_unary_expr(parser: &mut Parser<'_>) -> PartialResult<Expression> {
    let (punct, start) =
        expect_token!(parser => [Punct(punct), punct.clone()], [AstPart::PreUnaryOperator]);

    let op = match UnaryOp::from_punct(punct.clone()) {
        Some(v) if v.is_pre() => v,
        _ => {
            return PartialResult::new_fail(
                parser
                    .dcx
                    .struct_err(expected_tok_msg(punct, [AstPart::PreUnaryOperator]), start),
            )
        }
    };

    let expr = Box::new(parse!(@fn parser => parse_expr_precedence, Precedence::Unary));

    Good(Expression {
        loc: Span::from_ends(start, expr.loc.clone()),
        expr: ExpressionInner::Unary { op, expr },
    })
}

/// Binary Operations
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BinaryOp {
    /// Multiplication
    Mul,
    /// Division
    Div,
    /// Remainder
    Rem,
    /// Addition
    Add,
    /// Substraction
    Sub,
    /// Right Shift
    RShift,
    /// Left Shift
    LShift,
    /// comparison Less Than
    CompLT,
    /// comparison Greater Than
    CompGT,
    /// comparison Less than or Equal
    CompLE,
    /// comparison Greater than or Equal
    CompGE,
    /// comparison EQual
    CompEq,
    /// comparison Not Equal
    CompNe,
    /// Assignement
    Assignement,
}

impl BinaryOp {
    pub fn from_punct(punct: Punctuation) -> Option<BinaryOp> {
        use BinaryOp as BOp;
        use Punctuation as Punct;
        Some(match punct {
            Punct::Asterisk => BOp::Mul,
            Punct::Slash => BOp::Div,
            Punct::Percent => BOp::Rem,
            Punct::Plus => BOp::Add,
            Punct::Minus => BOp::Sub,
            Punct::RArrow2 => BOp::RShift,
            Punct::LArrow2 => BOp::LShift,
            Punct::LArrow => BOp::CompLT,
            Punct::RArrow => BOp::CompGT,
            Punct::LArrowEqual => BOp::CompLE,
            Punct::RArrowEqual => BOp::CompGE,
            Punct::Equal2 => BOp::CompEq,
            Punct::BangEqual => BOp::CompNe,
            Punct::Equal => BOp::Assignement,
            _ => return None,
        })
    }

    pub fn from_tt(tt: TokenType) -> Option<BinaryOp> {
        match tt {
            Punct(p) => Self::from_punct(p),
            _ => None,
        }
    }
}

/// Parse binary expression, `expression op expression`
pub fn parse_binary_expr(parser: &mut Parser<'_>, lhs: Expression) -> PartialResult<Expression> {
    let (op, tok) = match parser.peek_tok() {
        // TODO: here we compute twice the binary op its a little dumb, find a solution to that problem.
        Some(Token { tt: op, .. }) if BinaryOp::from_tt(op.clone()).is_some() => {
            let op = op.clone();
            parser.pop();
            (BinaryOp::from_tt(op.clone()).unwrap(), op)
        }
        Some(tok) => {
            let t = tok.clone();
            return PartialResult::new_fail(
                parser
                    .dcx
                    .struct_err(expected_tok_msg(t.tt, [AstPart::BinaryOperator]), t.loc),
            );
        }
        None => return parser.reached_eof_diag(),
    };
    let mut pr = Precedence::from(tok.clone());

    if pr.associativity() == Associativity::LeftToRight {
        pr = pr.next();
    }

    let rhs = parse!(@fn parser => parse_expr_precedence, pr);
    let loc = Span::from_ends(lhs.loc.clone(), rhs.loc.clone());

    Good(Expression {
        expr: ExpressionInner::Binary {
            lhs: Box::new(lhs),
            op,
            rhs: Box::new(rhs),
        },
        loc,
    })
}

/// Parse binary expression, `ident ( ":" ident )*`
pub fn parse_path_expr(parser: &mut Parser<'_>) -> PartialResult<Expression> {
    let mut id = Vec::new();
    let (first_id, start) =
        expect_token!(parser => [Ident(id), id.clone()], [FmtToken::Identifier]);
    id.push(first_id);
    let mut end = start.clone();

    while let Some(Token {
        tt: Punct(Punctuation::Colon),
        ..
    }) = parser.peek_tok()
    {
        parser.pop();
        let bit = expect_token!(parser => [Ident(id), id.clone()], [FmtToken::Identifier]);
        end = bit.1;
        id.push(bit.0);
    }

    Good(Expression {
        expr: ExpressionInner::Path(id),
        loc: Span::from_ends(start, end),
    })
}

/// Parse call expression, `expression "(" expression? ( , expression )* ")"`
pub fn parse_call_expr(
    parser: &mut Parser<'_>,
    callee: Box<Expression>,
) -> PartialResult<Expression> {
    expect_token!(parser => [Punct(Punctuation::LParen), ()], [FmtToken::Punct(Punctuation::LParen)]);

    let mut args = Vec::new();
    loop {
        if let Some(Punct(Punctuation::RParen)) = parser.peek_tt() {
            break;
        }
        args.push(parse!(parser => Expression));
        expect_token!(parser => [Punct(Punctuation::Colon), (); Punct(Punctuation::RParen), (), in break], [FmtToken::Punct(Punctuation::Colon), FmtToken::Punct(Punctuation::LParen)]);
    }

    let ((), end) = expect_token!(parser => [Punct(Punctuation::RParen), ()], [FmtToken::Punct(Punctuation::RParen)]);

    let start = callee.loc.clone();

    Good(Expression {
        expr: ExpressionInner::Call { callee, args },
        loc: Span::from_ends(start, end),
    })
}

// TODO: make the doc for this fn
pub fn parse_block_expr(parser: &mut Parser<'_>) -> PartialResult<Expression> {
    let block = parse!(parser => Block);
    Good(Expression {
        loc: block.loc.clone(),
        expr: ExpressionInner::Block(Box::new(block)),
    })
}

/// Parse if expression, `"if" expression block-expr [ "else" ( block-expr | if-expr-block ) ]`
pub fn parse_if_expr(parser: &mut Parser<'_>) -> PartialResult<Expression> {
    let ((), start) = expect_token!(parser => [KW(Keyword::If), ()], [FmtToken::KW(Keyword::If)]);
    let predicate = Box::new(parse!(parser => Expression));

    let true_branch = Box::new(parse!(@fn parser => parse_block_expr));

    let mut end = true_branch.loc.clone();
    let false_branch = if let Some(KW(Keyword::Else)) = parser.peek_tt() {
        expect_token!(parser => [KW(Keyword::Else), ()], [FmtToken::KW(Keyword::Else)]);

        let res = match parser.peek_tt() {
            Some(KW(Keyword::If)) => Box::new(parse!(@fn parser => parse_if_expr)),
            Some(Punct(Punctuation::LBrace)) => Box::new(parse!(@fn parser => parse_block_expr)),
            Some(_) => {
                // unwrap is safe because we already know the next has a token type
                let t = parser.peek_tok().unwrap().clone();
                return PartialResult::new_fail(parser.dcx.struct_err(
                    expected_tok_msg(t.tt, [AstPart::IfExpression, AstPart::BlockExpression]),
                    t.loc,
                ));
            }
            None => return parser.reached_eof_diag(),
        };

        end = res.loc.clone();

        Some(res)
    } else {
        None
    };

    Good(Expression {
        expr: ExpressionInner::If {
            predicate,
            true_branch,
            false_branch,
        },
        loc: Span::from_ends(start, end),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn precedence_correct_direction() {
        assert!(Precedence::Call as isize > Precedence::Unary as isize);
        assert!(Precedence::Call > Precedence::Unary);
    }
}
