//! Module responsible for the interposing of the semicolons inside the token stream
use crate::{
    lexer::tokens::{RawToken, RawTokenType},
    toks::{Keyword, Punctuation, Token, TokenStream, TokenType},
};

/// The interposer is a stage between lexing and parsing. It removes useless
/// tokens like `WhiteSpace`, `NewLine` and `Comment`.
///
/// 1. When the input in broken into tokens, a semicolon is interposed into the
///    token stream immediatly after a line's final token if that token is:
///     * an identifier
///     * an integer, floating-point, char, or string literal
///     * one of the keywords 'break', 'continue', 'return'
///     * one of the punctuations ')', ']', '}'
/// 2. A semicolon is omited if it will be followed by a `}` or a `)`
pub struct Interposer {
    rawtoks: Vec<RawToken>,
    idx: usize,
}

impl Interposer {
    #[inline]
    pub fn new(rawtoks: Vec<RawToken>) -> Interposer {
        Interposer { rawtoks, idx: 0 }
    }

    #[inline]
    #[must_use]
    pub fn current(&self) -> Option<RawToken> {
        self.peek_nth(0).cloned()
    }

    #[inline]
    #[must_use]
    pub fn peek_nth(&self, nth: usize) -> Option<&RawToken> {
        self.rawtoks.get(self.idx + nth)
    }

    #[inline]
    #[must_use]
    pub fn peek(&self) -> Option<&RawToken> {
        self.peek_nth(1)
    }

    #[inline]
    pub fn advance(&mut self) {
        self.idx += 1;
    }

    pub fn run(&mut self) -> TokenStream {
        // TODO: Make tests to check if the interposer correctly insert
        // semicolons where the rule says to interpose one.

        // TODO: Allow expression on multiple lines, like
        //
        // let x = 12
        //    * someFun(4)
        //
        // let str = "test"
        //    .iter()
        //    .filter(..)
        //    .collect()
        //
        // maybe add a new point to the 2nd rule, `or if a punctuation that is
        // a binary operator or a dot follows the newline`
        let mut token_stream = TokenStream::new();

        loop {
            let Some(current) = self.current() else { break };
            let next = self.peek();
            let two_ahead = self.peek_nth(2);

            let Some(tok) = current.clone().unraw() else {
                self.advance();
                continue;
            };

            token_stream.push(tok);

            match (next, two_ahead) {
                (
                    Some(RawToken {
                        tt: RawTokenType::NewLine,
                        ..
                    }),
                    Some(RawToken {
                        tt: RawTokenType::Punct(Punctuation::RParen | Punctuation::RBrace),
                        ..
                    }),
                ) => {
                    // Rule n°2
                }
                (
                    Some(RawToken {
                        tt: RawTokenType::NewLine,
                        ..
                    }),
                    _,
                ) => match current.tt {
                    RawTokenType::Ident(_)
                    | RawTokenType::Int(_)
                    | RawTokenType::Char(_)
                    | RawTokenType::Str(_)
                    | RawTokenType::KW(Keyword::Break | Keyword::Continue | Keyword::Return)
                    | RawTokenType::Punct(
                        Punctuation::RParen | Punctuation::RBracket | Punctuation::RBrace,
                    ) => token_stream.push(Token {
                        tt: TokenType::Punct(Punctuation::Semi),
                        loc: None,
                    }),
                    _ => {}
                },
                _ => {}
            }

            self.advance();
        }

        token_stream
    }
}
