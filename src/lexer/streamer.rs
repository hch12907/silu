use crate::lexer::*;

pub trait TokenStreamer {
    fn current_span(&self) -> &Span;

    fn next_token(&mut self) -> Option<(Span, Token)>;
}

impl<'a> TokenStreamer for Lexer<'a> {
    fn current_span(&self) -> &Span {
        &self.span
    }

    fn next_token(&mut self) -> Option<(Span, Token)> {
        self.next()
    }
}
