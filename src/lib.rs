use std::{
    collections::HashMap,
    ops::{Add, AddAssign, Range, RangeInclusive, Sub},
};

pub mod errors;
pub mod interposer;
pub mod lexer;
pub mod parser;
pub mod toks;

type BytePosInner = u32;

pub const VERSION_AND_GIT_HASH: &str = env!("VERSION_AND_GIT_HASH");

/// A type used to store the offset in byte. It's an alias of u32 because,
/// there is a lot of them in the AST.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct BytePos(pub BytePosInner);

impl Add for BytePos {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        BytePos(self.0 + rhs.0)
    }
}

impl Sub for BytePos {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        BytePos(self.0 - rhs.0)
    }
}

impl AddAssign<Self> for BytePos {
    fn add_assign(&mut self, rhs: Self) {
        self.0 += rhs.0
    }
}

impl AddAssign<u32> for BytePos {
    fn add_assign(&mut self, rhs: u32) {
        self.0 += rhs
    }
}

impl From<BytePos> for usize {
    fn from(val: BytePos) -> Self {
        val.0 as Self
    }
}

impl From<usize> for BytePos {
    fn from(val: usize) -> Self {
        BytePos(val as u32)
    }
}

impl BytePos {
    pub const ZERO: BytePos = BytePos(0);
}

#[derive(Clone, Debug, PartialEq)]
pub struct Span {
    /// start index of the span, starting from zero.
    pub lo: BytePos,
    /// end index of the span, starting from zero.
    pub hi: BytePos,
}

impl Span {
    pub const ZERO: Span = Span::from_inner(BytePos::ZERO, BytePos::ZERO);

    pub fn new<I: Into<BytePos>>(lo: I, hi: I) -> Span {
        Span {
            lo: lo.into(),
            hi: hi.into(),
        }
    }

    pub const fn from_inner(lo: BytePos, hi: BytePos) -> Span {
        Span { lo, hi }
    }

    pub fn range(self) -> Range<BytePosInner> {
        self.lo.0..self.hi.0
    }

    pub fn range_usize(self) -> Range<usize> {
        self.lo.0 as usize..self.hi.0 as usize
    }

    pub fn offset<I: Into<BytePos> + Copy>(self, offset: I) -> Span {
        Span {
            lo: self.lo + offset.into(),
            hi: self.hi + offset.into(),
        }
    }

    /// Takes the low part of the left-hand side and the high part of the right
    /// -hand side and combine them into one Span.
    pub fn from_ends(lhs: Span, rhs: Span) -> Span {
        Span {
            lo: lhs.lo,
            hi: rhs.hi,
        }
    }
}

impl From<RangeInclusive<BytePosInner>> for Span {
    fn from(value: RangeInclusive<BytePosInner>) -> Self {
        Span::new(*value.start() as usize, *value.end() as usize)
    }
}

impl From<RangeInclusive<usize>> for Span {
    fn from(value: RangeInclusive<usize>) -> Self {
        Span::new(*value.start(), *value.end())
    }
}

impl Default for Span {
    fn default() -> Self {
        Span::ZERO
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct MultiSpan {
    pub(crate) primary_spans: Vec<Span>,
    // TODO: Implement this part of the MultiSpan type, for it's not one of the
    // most important thing to worry about.
    // pub(crate) span_labels: Vec<(DiagSpan, DiagMessage)>,
}

impl MultiSpan {
    pub fn from_spans(primary_spans: Vec<Span>) -> MultiSpan {
        MultiSpan { primary_spans }
    }

    pub fn primary(&self) -> &Span {
        self.primary_spans
            .first()
            .expect("There is no span in the primary spans vector.")
    }

    pub fn primaries(&self) -> &Vec<Span> {
        &self.primary_spans
    }
}

#[derive(Debug)]
pub struct LineCol {
    /// Line number, starting from one.
    pub line: u32,
    /// Column number, starting from one.
    pub col: u32,
}

pub type FullLinePos = Range<LineCol>;

#[derive(Debug, Default)]
pub struct LinesData {
    /// key = line number
    /// value = a list of spans inside that line.
    p: HashMap<u32, Vec<Range<u32>>>,
}

impl LinesData {
    pub fn new() -> LinesData {
        LinesData { p: HashMap::new() }
    }

    pub fn insert_inline(&mut self, line: u32, span: Range<u32>) -> Option<()> {
        let spans = self.p.get_mut(&line)?;
        spans.push(span);

        Some(())
    }

    pub fn push(&mut self, line: u32, poses: Vec<Range<u32>>) -> Option<()> {
        self.p.insert(line, poses)?;
        Some(())
    }

    pub fn lines(&self) -> Vec<u32> {
        let mut v: Vec<u32> = self.p.keys().copied().collect();
        v.sort();
        v
    }

    pub fn contains_line(&self, line: u32) -> bool {
        self.p.contains_key(&line)
    }

    pub fn get(&self, line: u32) -> Vec<Range<u32>> {
        let mut v: Vec<_> = self
            .p
            .get(&line)
            .expect("This line does not exist.")
            .clone();
        v.sort_by(|a, b| a.clone().cmp(b.clone()));
        v
    }

    pub fn push_or_append(&mut self, line: u32, r: Range<u32>) -> Option<()> {
        if self.contains_line(line) {
            self.insert_inline(line, r)?;
        } else {
            self.push(line, vec![r]);
        }
        Some(())
    }
}
