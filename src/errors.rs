//! Crate responsible for the error handling in Gatti.

use std::borrow::Cow;
use std::cell::RefCell;
use std::cmp::max;
use std::fmt::{self, Debug};
use std::io::{self, Write};
use std::ops::Range;
use std::path::Path;

use termcolor::{Color, ColorSpec, StandardStream, WriteColor};

use crate::{BytePos, FullLinePos, LineCol, LinesData, MultiSpan, Span};
use style::{SetStyle, Style};

pub mod style;

#[derive(Clone, Debug)]
pub enum Level {
    Error,
    Warning,
    Note,
    Help,
}

impl Level {
    pub fn color(&self) -> ColorSpec {
        let mut spec = ColorSpec::new();
        match self {
            Level::Error => {
                spec.set_fg(Some(Color::Red)).set_intense(true);
                spec.set_bold(true);
            }
            Level::Warning => {
                spec.set_fg(Some(Color::Yellow)).set_intense(true);
                spec.set_bold(true);
            }
            Level::Note => {
                spec.set_fg(Some(Color::Green)).set_intense(true);
            }
            Level::Help => {
                spec.set_fg(Some(Color::Cyan)).set_intense(true);
            }
        }
        spec
    }

    pub fn format(&self, s: &mut StandardStream) -> io::Result<()> {
        s.set_color(&self.color())?;
        match self {
            Level::Error => write!(s, "error"),
            Level::Warning => write!(s, "warning"),
            Level::Note => write!(s, "note"),
            Level::Help => write!(s, "help"),
        }?;
        s.set_no_style()?;
        Ok(())
    }
}

/// `Diag` for `Diagnostic`
#[derive(Clone, Debug)]
pub struct Diag {
    level: Level,
    msg: DiagMessage,
    span: MultiSpan,
}

impl Diag {
    pub fn format(&self, dcx: &DiagCtxt, s: &mut StandardStream) -> io::Result<()> {
        let prim_pos = self.primary_line_pos(dcx);
        let LineCol { line, col } = prim_pos[0].start;

        s.set_style(Style::PathLineCol, &self.level)?;
        write!(s, "{}:{}:{}: ", dcx.filepath.display(), line, col)?;
        s.set_no_style()?;

        self.level.format(s)?;
        write!(s, ": ")?;
        s.set_style(Style::HeaderMsg, &self.level)?;
        write!(s, "{}", self.msg)?;
        s.set_no_style()?;

        self.render_snippet(dcx, s, prim_pos)?;
        writeln!(s)?;
        s.flush()?;
        Ok(())
    }

    pub fn primary_line_pos(&self, dcx: &DiagCtxt) -> Vec<FullLinePos> {
        let mut lines = Vec::new();

        for span in self.span.primaries() {
            let lo = dcx.line_col(span.lo);
            let mut hi = dcx.line_col(span.hi);
            let c = dcx.filetext.get(span.lo.0 as usize..span.hi.0 as usize);

            // handle the case where the diag wants to point to a new line,
            // it's kinda a hacky fix but it is what it is..
            if let Some("\n") = c {
                hi = LineCol {
                    line: lo.line,
                    col: lo.col + 1,
                };
            }
            lines.push(lo..hi);
        }

        lines
    }

    /// Returns true if the diagnostic is an error.
    pub fn is_error(&self) -> bool {
        matches!(&self.level, Level::Error)
    }

    fn render_snippet(
        &self,
        dcx: &DiagCtxt,
        s: &mut StandardStream,
        prim_pos: Vec<FullLinePos>,
    ) -> io::Result<()> {
        // TODO: remove this unwrap and put something else.
        let lines_data = self.build_lines_data(dcx, prim_pos).unwrap();
        writeln!(s)?;

        let lines = lines_data.lines();
        let mut line_no_width = 3;

        for no in &lines {
            let width = no.to_string().len();
            line_no_width = max(width, line_no_width);
        }

        let mut previous_line_no = lines[0];
        for &line in &lines {
            if previous_line_no + 3 <= line {
                s.set_style(Style::LineNumber, &self.level)?;
                writeln!(s, "...")?;
                s.set_no_style()?;
            }
            self.print_line(dcx, s, line, line_no_width, lines_data.get(line))?;
            previous_line_no = line;
        }

        Ok(())
    }

    fn build_lines_data(&self, dcx: &DiagCtxt, prim_pos: Vec<FullLinePos>) -> Option<LinesData> {
        let mut data = LinesData::new();

        for prim in prim_pos {
            if prim.start.line != prim.end.line {
                // Mark the bit of code at the start
                data.push_or_append(
                    prim.start.line,
                    // plus one at the end because starts from one.
                    prim.start.col..dcx.get_line_width(prim.start.line).unwrap() as u32 + 1,
                );

                // Mark the lines in between the start and the end
                let diff = prim.end.line - prim.start.line;
                if diff == 2 {
                    let l = prim.start.line + 1;
                    // plus one at the end of the range because the range is offseted by one.
                    data.push_or_append(l, 1..dcx.get_line_width(l)? as u32 + 1)?;
                }

                // Mark the end of the span
                data.push_or_append(prim.end.line, 1..prim.end.col + 1)?;
            }
            let line = prim.start.line;
            data.push_or_append(line, prim.start.col..prim.end.col)?;
        }

        Some(data)
    }

    /// When calling this function, curs is assumed to be sorted
    fn print_line(
        &self,
        dcx: &DiagCtxt,
        s: &mut StandardStream,
        line: u32,
        width: usize,
        curs: Vec<Range<u32>>,
    ) -> io::Result<()> {
        s.set_style(Style::LineNumber, &self.level)?;
        write!(s, "{:^width$}| ", line)?;
        s.set_no_style()?;
        writeln!(s, "{}", dcx.get_line(line).unwrap())?;

        s.set_style(Style::LineNumber, &self.level)?;
        write!(s, "{:width$}| ", "")?;

        s.set_style(Style::Level(self.level.clone()), &self.level)?;

        // Find the maximum end point among all cursors
        let max = curs.iter().map(|r| r.end).max().unwrap_or(0);
        let mut in_range = vec![false; max as usize];

        // Mark the indices within each Range.
        for cur in curs {
            for i in cur {
                // minus one in the index because the cursors starts at 1.
                in_range[i as usize - 1] = true;
            }
        }

        for in_r in in_range {
            write!(s, "{}", if in_r { '^' } else { ' ' })?;
        }

        s.set_no_style()?;
        writeln!(s)?;
        s.flush()?;
        Ok(())
    }
}

pub type DiagMessage = Cow<'static, str>;

#[derive(Debug)]
pub struct DiagCtxt<'r> {
    filetext: &'r str,
    filepath: &'r Path,

    diags: RefCell<Vec<Diag>>,
}

impl<'r> DiagCtxt<'r> {
    pub fn new(filetext: &'r str, filepath: &'r Path) -> Self {
        DiagCtxt {
            filetext,
            filepath,
            diags: RefCell::new(Vec::new()),
        }
    }

    pub fn diag(
        &'r self,
        level: Level,
        msg: impl Into<DiagMessage>,
        primary_spans: Vec<Span>,
    ) -> Diag {
        Diag {
            level,
            msg: msg.into(),
            span: MultiSpan::from_spans(primary_spans),
        }
    }

    pub fn struct_err(&'r self, msg: impl Into<DiagMessage>, primary_span: Span) -> Diag {
        self.struct_spans_err(msg, vec![primary_span])
    }

    pub fn struct_warn(&'r self, msg: impl Into<DiagMessage>, primary_span: Span) -> Diag {
        self.struct_spans_warn(msg, vec![primary_span])
    }

    pub fn struct_spans_err(
        &'r self,
        msg: impl Into<DiagMessage>,
        primary_spans: Vec<Span>,
    ) -> Diag {
        self.diag(Level::Error, msg, primary_spans)
    }

    pub fn struct_spans_warn(
        &'r self,
        msg: impl Into<DiagMessage>,
        primary_spans: Vec<Span>,
    ) -> Diag {
        self.diag(Level::Warning, msg, primary_spans)
    }

    pub fn line_col(&self, idx: BytePos) -> LineCol {
        let mut line = 1;
        let mut col = 1;

        for (i, ch) in self.filetext.char_indices() {
            if i == idx.into() {
                break;
            }
            match ch {
                '\n' => {
                    col = 1;
                    line += 1;
                }
                _ => col += 1,
            }
        }

        LineCol { line, col }
    }

    pub fn render_all(&self, s: &mut StandardStream) {
        for d in self.diags.borrow().iter() {
            d.format(self, s).unwrap();
        }
    }

    pub fn emit_diag(&self, diag: Diag) {
        self.diags.borrow_mut().push(diag);
    }

    pub fn emit_diags(&self, diags: impl IntoIterator<Item = Diag>) {
        for diag in diags {
            self.emit_diag(diag);
        }
    }

    pub fn failed(&self) -> bool {
        for diag in self.diags.borrow().iter() {
            if diag.is_error() {
                return true;
            }
        }
        false
    }

    /// Returns the content of the source file at the `line`
    ///
    /// The line number argument starts from one.
    fn get_line(&self, line: u32) -> Option<&str> {
        // NOTE: This is slow because we are creating a new iterator every time
        // want top get the content of one line it may be faster if we store
        // the byte offset of the start and end of each line in a vector.
        self.filetext.lines().nth(line as usize - 1)
    }

    /// Returns the length, in bytes (not utf8 codepoints or something like
    /// that..) of the `line` in the source file.
    fn get_line_width(&self, line: u32) -> Option<usize> {
        let width = self.get_line(line).map(|s| s.len());
        width
    }

    pub fn filetext(&self) -> &str {
        self.filetext
    }

    pub fn filepath(&self) -> &Path {
        self.filepath
    }
}

/// Partial result, when
pub struct PartialResult<T, D = Diag> {
    value: Option<T>,
    diags: Vec<D>,
}

impl<T, D> PartialResult<T, D> {
    /// New successful result
    pub fn ok(value: T) -> Self {
        Self {
            value: Some(value),
            diags: Vec::new(),
        }
    }

    /// New result but encounter problems
    pub fn fuzzy(value: T, diags: Vec<D>) -> Self {
        Self {
            value: Some(value),
            diags,
        }
    }

    /// New failed result with multiple errors
    pub fn fails(diags: Vec<D>) -> Self {
        Self { value: None, diags }
    }

    /// New failed result with one error
    pub fn fail(diag: D) -> Self {
        Self {
            value: None,
            diags: vec![diag],
        }
    }

    /// Get the contained value if any
    #[inline]
    pub fn value(self) -> Option<T> {
        self.value
    }

    #[inline]
    pub fn value_ref(&self) -> Option<&T> {
        self.value.as_ref()
    }

    /// Unwraps the value.
    ///
    /// # Panic
    ///
    /// Panic if there is no value
    #[inline]
    pub fn unwrap(&self) -> &T {
        // we don't check `diags` because if there is no value, it means there
        // is a diag containing an error and if there is a value, there is no
        // diag with an error
        self.value.as_ref().unwrap()
    }

    /// Get the diags
    #[inline]
    pub fn diags(self) -> Vec<D> {
        self.diags
    }
}

impl<T> PartialResult<T, Diag> {
    /// Did the result is a success?
    pub fn success(&self) -> bool {
        for diag in &self.diags {
            if diag.is_error() {
                return false;
            }
        }
        true
    }

    /// Did the result is a failure?
    pub fn failure(&self) -> bool {
        !self.success()
    }
}

impl<T: Debug, D: Debug> Debug for PartialResult<T, D> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut res = f.debug_struct("PartialResult");
        if let Some(val) = &self.value {
            res.field("value", val);
        }
        if !self.diags.is_empty() {
            res.field("diags", &self.diags);
        }
        res.finish()
    }
}

pub enum VerboseResult<T, D> {
    Ok(T),
    Fuzzy(T, Vec<D>),
    Fail(Vec<D>),
}

impl<T, D> From<PartialResult<T, D>> for VerboseResult<T, D> {
    fn from(value: PartialResult<T, D>) -> Self {
        if value.diags.is_empty() {
            if let Some(val) = value.value() {
                return VerboseResult::Ok(val);
            } else {
                unreachable!()
            }
        } else {
            if let Some(val) = value.value {
                return VerboseResult::Fuzzy(val, value.diags);
            } else {
                return VerboseResult::Fail(value.diags);
            }
        }
    }
}
