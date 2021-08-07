mod streamer;
mod token;

use std::str::{Chars, FromStr};

pub use crate::lexer::streamer::*;
pub use crate::lexer::token::*;
use crate::utils::{eq, neq, eq_any, FixedFifo};

const BACKLOG_SIZE: usize = 2;

#[derive(Clone, Debug, Default, PartialEq, PartialOrd)]
pub struct Span {
    pub line: usize,
    pub column: usize,
    pub length: usize,
}

pub struct Lexer<'a> {
    input: Chars<'a>,
    
    backlog: FixedFifo<char, BACKLOG_SIZE>,
    span: Span,
    offset: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input: input.chars(),
            backlog: FixedFifo::new(),
            span: Span::default(),
            offset: 0,
        }
    }

    pub fn span(&self) -> Span {
        self.span.clone()
    }

    fn peek(&mut self) -> Option<char> {
        if let x @ Some(_) = self.last_peek() {
            x
        } else {
            self.peek_next()
        }
    }

    fn peek_next(&mut self) -> Option<char> {
        let c = self.input.next()?;
        self.backlog.push(c).unwrap();
        Some(c)
    }

    fn last_peek(&self) -> Option<char> {
        self.backlog.last().copied()
    }

    fn eat(&mut self) -> Option<char> {
        let c = self.backlog.pop().or_else(|| self.input.next());

        if c.is_some() {
            self.span.column += 1;
            self.offset += 1;
        }

        if c == Some('\n') {
            self.span.line += 1;
            self.span.column = 0;
        }

        c
    }

    fn eat_if<F>(&mut self, cond: F) -> Option<char> 
        where F: Fn(char) -> bool
    {
        let peeked = if self.backlog.is_empty() {
            self.peek_next()?
        } else {
            *self.backlog.first().unwrap()
        };

        if cond(peeked) {
            self.eat()
        } else {
            None
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = (Span, Token);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            while self.eat_if(|x| x.is_ascii_whitespace()).is_some() {}

            let begin_pos = self.span.clone();
            let begin_offset = self.offset;

            macro_rules! finish {
                ($tok:expr) => {
                    return Some((Span {
                        line: begin_pos.line,
                        column: begin_pos.column,
                        length: self.offset - begin_offset,
                    }, $tok))
                };
            }

            macro_rules! symbol_scan {
                (
                    ($first:expr, $first_tok:expr), 
                    [$(($next:expr, $next_tok:expr, 
                        [$(($third:expr, $third_tok:expr)),*]
                    )),*] 
                ) => {
                    // First level
                    if self.eat_if(eq($first)).is_some() {
                        // Second level
                        $(
                            if self.eat_if(eq($next)).is_some() {
                                // Third level
                                $(
                                    if self.eat_if(eq($third)).is_some() {
                                        finish!($third_tok);
                                    }
                                )*
                                finish!($next_tok);
                            }
                        )*
                        finish!($first_tok);
                    }
                };
            }

            symbol_scan!(
                ('+', Token::Plus),
                [('+', Token::Increment, []),
                 ('=', Token::PlusAssign, [])]);

            symbol_scan!(
                ('-', Token::Minus),
                [('-', Token::Decrement, []),
                 ('=', Token::MinusAssign, []),
                 ('>', Token::Arrow, [])]
            );

            symbol_scan!(
                ('*', Token::Multiply),
                [('=', Token::MultiplyAssign, []),
                 ('/', Token::MultiCommentEnd, [])]
            );

            symbol_scan!(
                ('/', Token::Divide),
                [('/', Token::CommentStarter, [('/', Token::DocStarter)]),
                 ('=', Token::DivideAssign, []),
                 ('*', Token::MultiCommentStarter, [])]
            );

            symbol_scan!(
                ('%', Token::Remainder),
                [('=', Token::RemainderAssign, [])]
            );

            symbol_scan!(
                ('<', Token::LessThan),
                [('<', Token::ShiftLeft,
                  [('=', Token::ShiftLeftAssign)]),
                 ('=', Token::LessEqual, [])]
            );

            symbol_scan!(
                ('>', Token::GreaterThan),
                [('>', Token::ShiftRight,
                [('=', Token::ShiftRightAssign)]),
                ('=', Token::GreaterEqual, [])]
            );

            symbol_scan!(
                ('=', Token::Equal),
                [('=', Token::LogicEqual, [])]
            );

            symbol_scan!(
                ('&', Token::BitAnd),
                [('&', Token::LogicAnd, []),
                ('=', Token::BitAndAssign, [])]
            );

            symbol_scan!(
                ('|', Token::BitOr),
                [('|', Token::LogicOr, []),
                ('=', Token::BitOrAssign, [])]
            );

            symbol_scan!(
                ('^', Token::BitXor),
                [('=', Token::BitXorAssign, [])]
            );

            symbol_scan!(
                ('~', Token::BitNot),
                [('=', Token::BitNotAssign, [])]
            );

            symbol_scan!(
                ('!', Token::LogicNot),
                [('=', Token::LogicNotEqual, [])]
            );

            symbol_scan!(('(', Token::ParenOpen), []);
            symbol_scan!(('[', Token::BracketOpen), []);
            symbol_scan!(('{', Token::BraceOpen), []);
            symbol_scan!((')', Token::ParenClose), []);
            symbol_scan!((']', Token::BracketClose), []);
            symbol_scan!(('}', Token::BraceClose), []);

            symbol_scan!((',', Token::Comma), []);
            symbol_scan!((';', Token::Semicolon), []);
            symbol_scan!(('?', Token::Question), []);
            symbol_scan!((':', Token::Colon), []);
            symbol_scan!(
                ('#', Token::Preprocessor),
                [('#', Token::Preprocessor2, [])]
            );

            // Helper to check for the escapes
            let escape_handle = |lexer: &mut Self, str: &mut String, c| {
                if c == '\\' {
                    match lexer.eat() {
                        Some('\\') => str.push('\\'),
                        Some('\r') => lexer.eat_if(eq('\n')).map(|_| ()).unwrap(),
                        Some('\n') => {},
                        Some('n') => str.push('\n'),
                        Some('r') => str.push('\r'),
                        Some('t') => str.push('\t'),
                        Some('"') => str.push('"'),
                        Some('\'') => str.push('\''),

                        Some(x) => return Err(TokenError::BadEscape(x)),
                        None => return Err(TokenError::UnexpectedEof),
                    };
                } else if c == '\r' || c == '\n' {
                    return Err(TokenError::UnexpectedNewline);
                } else {
                    str.push(c);
                }

                Ok(())
            };

            // lex escape sequences
            if self.eat_if(eq('\\')).is_some() {
                let mut escape_seq = String::with_capacity(1);
                match escape_handle(self, &mut escape_seq, '\\') {
                    Ok(()) if escape_seq.len() > 0 && escape_seq.chars().nth(0).unwrap().is_alphabetic() => 
                        finish!(Token::Ident(escape_seq)),

                    Ok(()) if escape_seq.len() > 0 =>
                        finish!(Token::Unknown(escape_seq.chars().nth(0).unwrap())),

                    Ok(()) if escape_seq.len() == 0 => {},

                    Err(why) => finish!(Token::Error(why)),

                    _ => unreachable!(),
                }
            }

            // lex a dot
            let mut number_starts_with_dot = false;
            if let Some(_) = self.eat_if(eq('.')) {
                match self.peek() {
                    Some(x) if x.is_ascii_digit() => {
                        number_starts_with_dot = true;
                    },

                    _ => finish!(Token::Dot),
                }
            }

            // lex numbers
            if let Some(x) = self.eat_if(|x| x.is_ascii_digit()) {
                let mut num = String::new();

                struct LexerNumberState {
                    prefix: NumberType,
                    num_type: NumberType,
                    // float specific
                    after_exp: bool,
                    after_exp_sign: bool,
                }

                let mut state = LexerNumberState {
                    prefix: NumberType::Dec,
                    num_type: NumberType::Dec,
                    after_exp: false,
                    after_exp_sign: false
                };

                if number_starts_with_dot {
                    num.push_str("0.");
                    state.num_type = NumberType::Float;
                }

                match x {
                    '0'..='9' if number_starts_with_dot => {
                        num.push(x);
                    },

                    '0' => match self.eat_if(eq_any(&['x', 'b', 'X', 'B'])) {
                        Some('x' | 'X') => state.num_type = NumberType::Hex,
                        Some('b' | 'B') => state.num_type = NumberType::Bin,
                        None => {
                            num.push(x);
                            state.num_type = NumberType::Oct
                        },
                        _ => unreachable!(),
                    },

                    _ => {
                        num.push(x);
                        state.num_type = NumberType::Dec;
                    },
                }

                state.prefix = state.num_type.clone();

                let valid_digit = |x: char, state: &LexerNumberState| match x {
                    '.' => match state.num_type {
                        NumberType::Float => false,
                        _ => true,
                    },

                    _ => match state.num_type {
                        NumberType::Bin => x.is_digit(2),
                        NumberType::Dec => x.is_digit(10),
                        NumberType::Hex => x.is_digit(16),
                        NumberType::Oct => x.is_digit(8),
                        NumberType::Float | NumberType::HexFloat if state.after_exp_sign => 
                            x.is_digit(10),
                        NumberType::Float | NumberType::HexFloat if state.after_exp =>
                            x.is_digit(10) || eq_any(&['+', '-'])(x),
                        NumberType::Float => x.is_digit(10) ||
                            eq_any(&['e', 'E'])(x),
                        NumberType::HexFloat => x.is_digit(16) ||
                            eq_any(&['p', 'P'])(x),
                    },
                };

                while let Some(x) = self.eat_if(|x| valid_digit(x, &state)) {
                    match x {
                        '.' => state.num_type = match state.num_type {
                            NumberType::Hex => NumberType::HexFloat,
                            _ => NumberType::Float,
                        },

                        'e' | 'E' if state.num_type == NumberType::Float =>
                            state.after_exp = true,

                        'p' | 'P' if state.num_type == NumberType::HexFloat => 
                            state.after_exp = true,

                        '+' | '-' => state.after_exp_sign = true,
                        _ => (),
                    }

                    num.push(x);
                }

                let mut suffix = String::new();
                while let Some(x) = self.eat_if(eq_any(&['u', 'U', 'l', 'L', 'f', 'F'])) {
                    suffix.push(x);
                }

                if state.num_type == NumberType::Float
                   && !(state.prefix == NumberType::Oct ||
                        state.prefix == NumberType::Dec ||
                        state.prefix == NumberType::Float)
                {
                    finish!(Token::Error(TokenError::InvalidFloatPrefix(state.prefix, num, suffix)))
                }
                    
                if state.num_type == NumberType::HexFloat
                    && !state.after_exp
                {
                    finish!(Token::Error(TokenError::InvalidHexFloat))
                }

                if num.is_empty() && state.num_type == NumberType::Oct {
                    finish!(Token::Number(NumberType::Dec, String::from("0"), String::new()));
                } else {
                    finish!(Token::Number(state.num_type, num, suffix));
                }
            }

            // lex chars
            if self.eat_if(eq('\'')).is_some() {
                let mut chars = String::with_capacity(1);
                let mut err = None;
                
                while let Some(x) = self.eat_if(neq('\'')) {
                    match escape_handle(self, &mut chars, x) {
                        Ok(_) => continue,
                        Err(why) => err = Some(Token::Error(why)),
                    };
                }

                assert_eq!(self.eat().unwrap(), '\'');

                if err.is_some() {
                    finish!(err.unwrap())
                }

                if chars.len() > 1 {
                    finish!(Token::Error(TokenError::BadChar(chars)))
                } else if chars.len() == 1 {
                    finish!(Token::Char(chars.chars().nth(0).unwrap()))
                } else {
                    finish!(Token::Error(TokenError::EmptyChar))
                }
            }

            // lex strings
            if self.eat_if(eq('"')).is_some() {
                let mut str = String::new();
                let mut err = None;

                while let Some(x) = self.eat_if(neq('"')) {
                    match escape_handle(self, &mut str, x) {
                        Ok(()) => continue,
                        Err(why) => err = Some(Token::Error(why)),
                    };
                }

                assert_eq!(self.eat_if(eq('"')).unwrap(), '"');

                if err.is_some() {
                    finish!(err.unwrap())
                } else {
                    finish!(Token::Str(str));
                }
            }

            // lex identifiers + keywords
            // TODO: support \uNNNN identifiers
            if let Some(x) = self.eat_if(|x| x.is_alphabetic() || x == '_') {
                let mut ident = x.to_string();
                while let Some(x) = self.eat_if(|x| x.is_alphabetic()
                    || x.is_ascii_digit()
                    || x == '_')
                {
                    ident.push(x);
                }

                match Keyword::from_str(&ident) {
                    Ok(x) => finish!(Token::Keyword(x)),
                    Err(()) => finish!(Token::Ident(ident)),
                }
            }

            // if we see a weird token, eat it
            if self.span.column == begin_pos.column && self.span.line == begin_pos.line {
                finish!(Token::Unknown(self.eat()?))
            } else if self.span.line != begin_pos.line {
                continue
            }

            if !(self.last_peek().is_some() || self.peek_next().is_some()) {
                break
            }
        }

        None
    }
}
