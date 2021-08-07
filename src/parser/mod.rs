#[macro_use] mod macros;
mod ast;
mod declaration;
mod definition;
mod expression;
mod statement;
mod symbol;

use std::collections::VecDeque;
use std::fmt::Debug;
use std::num::ParseIntError;
use crate::lexer::{Span, Token, TokenStreamer};

pub use ast::*;
pub use declaration::*;
pub use expression::*;
pub use statement::*;
pub use symbol::*;

#[derive(Clone, Debug, PartialEq)]
pub enum ParseErr {
    InvalidFloat,
    InvalidInt(ParseIntError),
    InvalidStringPrefix(String),
    InvalidDirectAbstractDeclarator,
    InvalidFunctionSignature,
    InvalidTypedef,
    UnexpectedToken(Token),
    UnexpectedEnd,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ParseResult<T: Clone + Debug + PartialEq> {
    Parsed(Span, T),
    Error(Span, ParseErr),

    Unhandled,
}

impl<T: Clone + Debug + PartialEq> From<ParseErr> for ParseResult<T> {
    fn from(e: ParseErr) -> Self {
        Self::Error(Span::default(), e)
    }
}

impl<T: Clone + Debug + PartialEq> From<(Span, ParseErr)> for ParseResult<T> {
    fn from((span, e): (Span, ParseErr)) -> Self {
        Self::Error(span, e)
    }
}

pub enum TokenResult {
    Obtained(Span, Token),
    EndOfStream(Span),
    Unwanted,
}

impl TokenResult {
    pub fn span(self) -> Option<Span> {
        match self {
            TokenResult::Obtained(s, _) => Some(s),
            _ => None
        }
    }

    pub fn is_obtained(&self) -> bool {
        matches!(self, TokenResult::Obtained(_, _))
    }
    
    pub fn unwrap(self) -> (Span, Token) {
        match self {
            Self::Obtained(span, tok) => (span, tok),
            Self::EndOfStream(_) => panic!("unwrapped on an EndOfStream result"),
            Self::Unwanted => panic!("unwrapped on an Unwanted result"),
        }
    }
}

pub struct Parser<T: TokenStreamer> {
    token_stream: T,

    // We need infinite lookahead, use VecDeque instead of FixedFifo
    backlog: VecDeque<(Span, Token)>,
    symbol_table: SymbolTable,
    current_scope: Scope,
}

impl<T: TokenStreamer> Parser<T> {
    pub fn new(token_stream: T) -> Self {
        Self {
            token_stream,
            backlog: VecDeque::new(),
            symbol_table: SymbolTable::new(),
            current_scope: Scope::File { filename: std::path::PathBuf::new() }
        }
    }

    /*fn peek_parser(&mut self) -> Parser<&mut Self> {
        Parser::<&mut Self>::new(self)
    }*/

    fn peek(&mut self) -> Option<&(Span, Token)> {
        if self.backlog.front().is_some() {
            self.backlog.front()
        } else {
            self.peek_next()
        }
    }

    fn peek_nth(&mut self, index: usize) -> Option<&(Span, Token)> {
        if index >= self.backlog.len() {
            let range = index + 1 - self.backlog.len();
            for _ in 0..range {
                self.peek_next();
            }
        }

        self.backlog.get(index)
    }

    fn peek_next(&mut self) -> Option<&(Span, Token)> {
        let c = self.token_stream.next_token()?;
        self.backlog.push_back(c);
        self.backlog.back()
    }

    fn eat(&mut self) -> TokenResult {
        match self.backlog.pop_front().or_else(|| self.token_stream.next_token()) {
            Some((span, tok)) => TokenResult::Obtained(span, tok),
            None => TokenResult::EndOfStream(self.token_stream.current_span().clone())
        }
    }

    fn eat_if<F>(&mut self, cond: F) -> TokenResult 
        where F: Fn(&(Span, Token)) -> bool
    {
        let peeked = if self.backlog.is_empty() {
            match self.peek_next() {
                Some(x) => x,
                None => return TokenResult::EndOfStream(self.token_stream.current_span().clone()),
            }
        } else {
            self.backlog.front().unwrap()
        };

        if cond(peeked) {
            self.eat()
        } else {
            TokenResult::Unwanted
        }
    }

    pub fn into_token_tree() -> ParseResult<CommaExpression> {
        todo!()
    }
}

/*impl<T: TokenStreamer> TokenStreamer for &mut Parser<T> {
    fn current_span(&self) -> &Span {
        self.backlog.back()
            .map(|(s, _)| s)
            .unwrap_or_else(|| self.token_stream.current_span())
    }

    fn next_token(&mut self) -> Option<(Span, Token)> {
        self.backlog_index = 0;
        self.peek().cloned()
    }
}*/
