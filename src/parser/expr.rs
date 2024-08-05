use crate::{
    derive_loc,
    errors::PartialResult::{self, *},
    expect_token, parse,
    toks::{Keyword, Punctuation, Token, TokenType::*},
    Span,
};

use super::{
    expected_tok_msg,
    precedence::{operator_precedence, PrecedenceValue},
    AstNode, AstPart, FmtToken, Parser,
};

/// An operator, either a binary operator or a unary operator.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Operator {
    Binary(BinaryOp),
    Unary(UnaryOp),
}

impl From<BinaryOp> for Operator {
    fn from(value: BinaryOp) -> Self {
        Self::Binary(value)
    }
}

impl From<UnaryOp> for Operator {
    fn from(value: UnaryOp) -> Self {
        Self::Unary(value)
    }
}

/// Binary Operators
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
    /// Right shift
    RShift,
    /// Left shift
    LShift,
    /// Comparison Less Than
    CompLT,
    /// Comparison Greater Than
    CompGT,
    /// Comparison Less Than or Equal
    CompLTE,
    /// Comparison Greater Than or Equal
    CompGTE,
    /// Comparison Equal
    CompEq,
    /// Comparison Not Equal
    CompNe,
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
            Punct::LArrowEqual => BOp::CompLTE,
            Punct::RArrowEqual => BOp::CompGTE,
            Punct::Equal2 => BOp::CompEq,
            Punct::BangEqual => BOp::CompNe,
            _ => return None,
        })
    }
}

/// Unary Operators
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum UnaryOp {
    // LEFT UNARY OPERATOR
    /// -a
    Negation,
    /// !a
    Not,
    //
    // RIGHT UNARY OPERATOR
    // /// a.?
    // Unwrap,
}

impl UnaryOp {
    pub fn from_punct(punct: Punctuation) -> Option<UnaryOp> {
        use Punctuation as Punct;
        use UnaryOp as UOp;
        Some(match punct {
            Punct::Minus => UOp::Negation,
            Punct::Bang => UOp::Not,
            _ => return None,
        })
    }

    /// Is the unary operator on the left of the operand
    pub fn is_left(&self) -> bool {
        matches!(self, Self::Negation | Self::Not)
    }

    /// Is the unary operator on the right of the operand
    pub fn is_right(&self) -> bool {
        !self.is_left()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Associativity {
    LeftToRight,
    RightToLeft,
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub expr: ExpressionInner,
    pub loc: Span,
}

derive_loc!(Expression);

impl AstNode for Expression {
    type Output = Self;

    fn parse(parser: &mut Parser<'_>) -> PartialResult<Self::Output> {
        let mut lhs = parse!(parser => ExpressionInner);

        let mut binary_times: u8 = 0;
        loop {
            lhs = match &parser.peek_tok() {
                Some(Token { tt: Punct(p), .. })
                    if BinaryOp::from_punct(p.clone()).is_some() && binary_times != 1 =>
                {
                    binary_times += 1;
                    parse!(@fn parser => parse_binary_expr, parser.current_precedence, lhs)
                }
                _ => break,
            };
            if binary_times >= 2 {
                binary_times = 0;
            }
        }

        Good(lhs)
    }
}

#[derive(Debug, Clone)]
pub enum ExpressionInner {
    BinaryExpr {
        lhs: Box<Expression>,
        op: BinaryOp,
        rhs: Box<Expression>,
    },
    UnaryExpr {
        op: UnaryOp,
        operand: Box<Expression>,
    },

    // primary expression
    IntLiteral(u64),
    BoolLiteral(bool),
    CharLiteral(char),
    StrLiteral(String),
    PathExpr(Vec<String>),
}

impl AstNode for ExpressionInner {
    type Output = Expression;

    fn parse(parser: &mut Parser<'_>) -> PartialResult<Self::Output> {
        match parser.peek_tok() {
            Some(Token { tt: Int(_), .. }) => parse_intlit_expr(parser),
            Some(Token {
                tt: KW(Keyword::True | Keyword::False),
                ..
            }) => parse_boollit_expr(parser),
            Some(Token { tt: Char(_), .. }) => parse_charlit_expr(parser),
            Some(Token { tt: Str(_), .. }) => parse_strlit_expr(parser),
            Some(Token { tt: Ident(_), .. }) => parse_path_expr(parser),
            Some(Token {
                tt: Punct(punct), ..
            }) if UnaryOp::from_punct(punct.clone()).is_some_and(|op| op.is_left()) => {
                parse_left_unary_expr(parser)
            }
            Some(t) => {
                let t = t.clone();
                PartialResult::new_fail(
                    parser
                        .dcx
                        .struct_err(expected_tok_msg(t.tt, [AstPart::Expression]), t.loc),
                )
            }
            None => parser.reached_eof_diag(),
        }
    }
}

pub fn parse_intlit_expr(parser: &mut Parser<'_>) -> PartialResult<Expression> {
    let (i, loc) = expect_token!(parser => [Int(i), *i], [FmtToken::IntLiteral]);
    Good(Expression {
        expr: ExpressionInner::IntLiteral(i),
        loc,
    })
}

pub fn parse_binary_expr(
    parser: &mut Parser<'_>,
    min_precedence: PrecedenceValue,
    mut lhs: Expression,
) -> PartialResult<Expression> {
    while let Some(Token {
        tt: Punct(punct), ..
    }) = &parser.peek_tok()
    {
        // check if the punctuation is a binary operator
        let op = match BinaryOp::from_punct(punct.clone()) {
            Some(op) => op,
            None => break,
        };

        // get the precedence of the operator
        let (_, op_precede) = operator_precedence(op.clone());

        // check if the binary operator has more precedence than what's
        // required.
        if op_precede < min_precedence {
            break;
        }

        // consume the binary operator.
        parser.pop();

        // parse the right-hand side of the binary expression
        let mut rhs = parse!(parser => ExpressionInner);

        while let Some(Token {
            tt: Punct(lh_punct),
            ..
        }) = &parser.peek_tok()
        {
            // check if the lookahead punctuation is a binary operator
            let lh_op = match BinaryOp::from_punct(lh_punct.clone()) {
                Some(op) => op,
                None => break,
            };

            // get the precedence of the lookahead operator
            let (lh_assoc, lh_op_precede) = operator_precedence(lh_op);

            // break if the precendence of the lookahead operator is smaller
            // than the current operator's one. if associativity is LeftToRight
            // we also break if the precedences are equal.
            match lh_assoc {
                Associativity::LeftToRight if lh_op_precede <= op_precede => break,
                Associativity::RightToLeft if lh_op_precede < op_precede => break,
                _ => {}
            }
            rhs = parse!(@fn parser => parse_binary_expr, lh_op_precede, rhs);
        }
        let loc = Span::from_ends(lhs.loc.clone(), rhs.loc.clone());

        lhs = Expression {
            expr: ExpressionInner::BinaryExpr {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(rhs),
            },
            loc,
        };
    }

    Good(lhs)
}

pub fn parse_left_unary_expr(parser: &mut Parser<'_>) -> PartialResult<Expression> {
    let (punct, lhs) =
        expect_token!(parser => [Punct(punct), punct.clone()], [AstPart::UnaryOperator]);

    let op = match UnaryOp::from_punct(punct.clone()) {
        Some(v) if v.is_left() => v,
        _ => {
            return PartialResult::new_fail(
                parser
                    .dcx
                    .struct_err(expected_tok_msg(punct, ["left unary operator"]), lhs),
            )
        }
    };

    parser.current_precedence = operator_precedence(op.clone()).1;
    let operand = Box::new(parse!(parser => Expression));

    Good(Expression {
        loc: Span::from_ends(lhs, operand.loc.clone()),
        expr: ExpressionInner::UnaryExpr { op, operand },
    })
}

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

pub fn parse_charlit_expr(parser: &mut Parser<'_>) -> PartialResult<Expression> {
    let (c, loc) = expect_token!(parser => [Char(c), *c], [FmtToken::IntLiteral]);
    Good(Expression {
        expr: ExpressionInner::CharLiteral(c),
        loc,
    })
}

pub fn parse_strlit_expr(parser: &mut Parser<'_>) -> PartialResult<Expression> {
    let (s, loc) = expect_token!(parser => [Str(s), s.clone()], [FmtToken::IntLiteral]);
    Good(Expression {
        expr: ExpressionInner::StrLiteral(s),
        loc,
    })
}

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
        expr: ExpressionInner::PathExpr(id),
        loc: Span::from_ends(start, end),
    })
}
