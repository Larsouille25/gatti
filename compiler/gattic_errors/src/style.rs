use std::io;

use crate::Level;
use termcolor::{Color, ColorSpec, StandardStream, WriteColor};

pub enum Style {
    HeaderMsg,
    Level(Level),
    PathLineCol,
    LineNumber,
    UnderlinePrimary,
    UnderlineSecondary,
    LabelPrimary,
    LabelSecondary,
    NoStyle,
}

impl Style {
    pub fn color_spec(&self, level: Level) -> ColorSpec {
        let mut spec = ColorSpec::new();
        match self {
            Style::HeaderMsg => {
                spec.set_bold(true);
            }
            Style::Level(lvl) => {
                spec = lvl.color();
                spec.set_bold(true);
            }
            Style::PathLineCol => {
                spec.set_bold(true);
            }
            Style::UnderlinePrimary | Style::LabelPrimary => {
                spec = level.color();
                spec.set_bold(true);
            }
            Style::UnderlineSecondary | Style::LabelSecondary => {
                spec.set_bold(true).set_intense(true);
                spec.set_fg(Some(Color::Blue));
            }
            Style::LineNumber => {
                spec.set_bold(true).set_intense(true);
                spec.set_fg(Some(Color::Blue));
            }
            Style::NoStyle => {}
        }
        spec
    }
}

pub(crate) trait SetStyle {
    fn set_style(&mut self, style: Style, lvl: &Level) -> io::Result<()>;

    fn set_no_style(&mut self) -> io::Result<()>;
}

impl SetStyle for StandardStream {
    fn set_style(&mut self, style: Style, lvl: &Level) -> io::Result<()> {
        self.set_color(&style.color_spec(lvl.clone()))
    }

    fn set_no_style(&mut self) -> io::Result<()> {
        self.set_style(Style::NoStyle, &Level::Note)
        // here the level doesn't matter we can put anything because the
        // `NoStyle` depends on nothing.
    }
}
