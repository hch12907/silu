use crate::lexer::{Keyword, NumberType, Span, Token, TokenStreamer};
use crate::parser::{Parser, ParseResult, ParseErr, TokenResult, TypeName};
use TokenResult::*;

#[derive(Clone, Debug, PartialEq)]
pub enum StrPrefix {
    Narrow,
    Utf8,
    Utf16,
    Utf32,
    Wide,
    Unknown(String),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    Char(char),
    Float(f64, String),
    Integer(u64, String),
    String(StrPrefix, String),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Identifier(pub String);

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Def(Identifier),
    Struct(Identifier),
    Union(Identifier),
    Int,
    Long,
    Short,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ArgumentExpr(pub(crate) Vec<AssignmentExpr>);

#[derive(Clone, PartialEq)]
pub enum PrimaryExpr {
    Identifier(Identifier),
    Literal(Literal),
    Grouped(Box<CommaExpression>),
}

impl std::fmt::Debug for PrimaryExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use self::Identifier as Ident;
        use self::Literal as Lit;
        use PrimaryExpr::*;

        match self {
            Identifier(Ident(ident)) => write!(f, "ident:{}", ident),
            Literal(Lit::Char(c)) => write!(f, "char:{}", c),
            Literal(Lit::Float(flt, suf)) =>  write!(f, "flt:{}{}", flt, suf),
            Literal(Lit::Integer(i, suf)) => write!(f, "int:{}{}", i, suf),
            Literal(Lit::String(pre, s)) => write!(f, "str:{:?}\"{}\"", pre, s),
            Grouped(expr) => expr.fmt(f),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum PostfixExpr {
    Nothing(PrimaryExpr),              
    PostIncrement(Box<Self>),          
    PostDecrement(Box<Self>),          
    Access(Box<Self>, Identifier),     
    AccessDeref(Box<Self>, Identifier),
    Call(Box<Self>, ArgumentExpr),     
    Index(Box<Self>, Box<CommaExpression>), 
}

#[derive(Clone, Debug, PartialEq)]
pub enum PrefixExpr {
    Nothing(PostfixExpr),   
    PreIncrement(Box<Self>),
    PreDecrement(Box<Self>),
    Positive(Box<Self>),    
    Negative(Box<Self>),    
    LogicNot(Box<Self>),    
    BitNot(Box<Self>),      
    Cast(TypeName, Box<Self>),  
    Deref(Box<Self>),       
    Reference(Box<Self>),   
    Sizeof(Box<Self>),
    SizeofType(TypeName),
}

#[derive(Clone, Debug, PartialEq)]
pub enum BinaryOperator {
    Mul,
    Div,
    Rem,
    Add,
    Sub,
    Shl,
    Shr,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Equal,
    Unequal,
    BitAnd,
    BitXor,
    BitOr,
    And,
    Or,

    None,
}

impl BinaryOperator {
    fn from_token(tok: &Token) -> Option<Self> {
        match tok {
            Token::Multiply => Some(BinaryOperator::Mul),
            Token::Divide => Some(BinaryOperator::Div),
            Token::Remainder => Some(BinaryOperator::Rem),
            Token::Plus => Some(BinaryOperator::Add),
            Token::Minus => Some(BinaryOperator::Sub),
            Token::ShiftLeft => Some(BinaryOperator::Shl),
            Token::ShiftRight => Some(BinaryOperator::Shr),
            Token::LessThan => Some(BinaryOperator::Less),
            Token::LessEqual => Some(BinaryOperator::LessEqual),
            Token::GreaterThan => Some(BinaryOperator::Greater),
            Token::GreaterEqual => Some(BinaryOperator::GreaterEqual),
            Token::LogicEqual => Some(BinaryOperator::Equal),
            Token::LogicNotEqual => Some(BinaryOperator::Unequal),
            Token::BitAnd => Some(BinaryOperator::BitAnd),
            Token::BitXor => Some(BinaryOperator::BitXor),
            Token::BitOr => Some(BinaryOperator::BitOr),
            Token::LogicAnd => Some(BinaryOperator::And),
            Token::LogicOr => Some(BinaryOperator::Or),
            _ => None,
        }
    }
}

impl PartialOrd for BinaryOperator {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        fn precedence(op: &BinaryOperator) -> u32 {
            match op {
                BinaryOperator::Mul => 10,
                BinaryOperator::Div => 10,
                BinaryOperator::Rem => 10,
                BinaryOperator::Add => 9,
                BinaryOperator::Sub => 9,
                BinaryOperator::Shl => 8,
                BinaryOperator::Shr => 8,
                BinaryOperator::Less => 7,
                BinaryOperator::LessEqual => 7,
                BinaryOperator::Greater => 7,
                BinaryOperator::GreaterEqual => 7,
                BinaryOperator::Equal => 6,
                BinaryOperator::Unequal => 6,
                BinaryOperator::BitAnd => 5,
                BinaryOperator::BitXor => 4,
                BinaryOperator::BitOr => 3,
                BinaryOperator::And => 2,
                BinaryOperator::Or => 1,
                BinaryOperator::None => 0,
            }
        }

        precedence(self).partial_cmp(&precedence(other))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum BinaryExpr {
    Nothing(PrefixExpr),
    Binary(BinaryOperator, Box<Self>, Box<Self>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum ConditionalExpr {
    Nothing(BinaryExpr),
    Cond(BinaryExpr, Box<CommaExpression>, Box<Self>) // cond, then, else
}

#[derive(Clone, Debug, PartialEq)]
pub enum AssignOperator {
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    RemAssign,
    ShlAssign,
    ShrAssign,
    BitAndAssign,
    BitXorAssign,
    BitOrAssign,
}

#[derive(Clone, Debug, PartialEq)]
pub enum AssignmentExpr {
    Nothing(ConditionalExpr),
    Assign(PrefixExpr, Box<Self>),
    OpAssign(AssignOperator, PrefixExpr, Box<Self>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum CommaExpression {
    Nothing(AssignmentExpr),
    Comma(Box<Self>, AssignmentExpr),
}

impl<T: TokenStreamer> Parser<T> {
    pub(super) fn parse_number(&mut self) -> ParseResult<Literal> {
        let (span, tok) = match self.eat_if(|(_, tok)| matches!(tok, Token::Number(_, _, _))) {
            EndOfStream(_) | Unwanted =>
                return ParseResult::Unhandled,
            Obtained(s, t) => (s, t),
        };

        let (num_type, number, suffix) = force_unwrap!(tok, Token::Number(t, n, s) => (t, n, s));

        let radix = |num_type| match num_type {
            NumberType::Bin => 2,
            NumberType::Dec => 10,
            NumberType::Hex => 16,
            NumberType::Oct => 8,
            NumberType::Float => 10,
            NumberType::HexFloat => 16,
        };

        let literal = match num_type {
            x @ (NumberType::Bin | NumberType::Dec | NumberType::Hex | NumberType::Oct) => {
                let number = u64::from_str_radix(&number, radix(x))
                    .map_err(|e| ParseErr::InvalidInt(e));

                let number = match number {
                    Ok(x) => x,
                    Err(why) => return (span, why).into(),
                };
                
                Literal::Integer(number, suffix)
            },

            NumberType::Float => {
                let number = number.parse::<f64>().map_err(|_| ParseErr::InvalidFloat);

                let number = match number {
                    Ok(x) => x,
                    Err(why) => return (span, why).into(),
                };

                Literal::Float(number, suffix)
            },

            NumberType::HexFloat => todo!(),
        };

        ParseResult::Parsed(span, literal)
    }

    pub(super) fn parse_char(&mut self) -> ParseResult<Literal> {
        let (span, tok) = match self.eat_if(|(_span, tok)| matches!(tok, Token::Char(_))) {
            EndOfStream(_) | Unwanted =>
                return ParseResult::Unhandled,
            Obtained(s, t) => (s, t),
        };

        let c = force_unwrap!(tok, Token::Char(c) => c);
        ParseResult::Parsed(span, Literal::Char(c))
    }

    pub(super) fn parse_string(&mut self) -> ParseResult<Literal> {
        let mut prefix = StrPrefix::Narrow;
        let mut prefix_len = 0;

        if let Some((pfx_span, Token::Ident(_))) = self.peek() {
            let pfx_span = pfx_span.clone();

            if let Some((str_span, Token::Str(_))) = self.peek_next() {
                if pfx_span.line == str_span.line
                    && pfx_span.column + pfx_span.length == str_span.column
                {
                    let pfx_tok = self.eat().unwrap();
                    let pfx = force_unwrap!(pfx_tok, (_, Token::Ident(pfx)) => pfx);
                    
                    prefix_len = pfx.len();
                    prefix = match pfx.as_str() {
                        "L" => StrPrefix::Wide,
                        "u8" => StrPrefix::Utf8,
                        "u" => StrPrefix::Utf16,
                        "U" => StrPrefix::Utf32,
                        _ => StrPrefix::Unknown(pfx),
                    }
                }
            }
        }

        let (mut span, tok) = match self.eat_if(|(_span, tok)| matches!(tok, Token::Str(_))) {
            EndOfStream(_) | Unwanted =>
                return ParseResult::Unhandled,
            Obtained(s, t) => (s, t),
        };

        span.column -= prefix_len;
        span.length += prefix_len;

        let str = force_unwrap!(tok, Token::Str(x) => x);

        ParseResult::Parsed(span, Literal::String(prefix, str))
    }

    pub(super) fn parse_primary_expression(&mut self) -> ParseResult<PrimaryExpr> {
        let funcs = [
            Self::parse_char,
            Self::parse_number,
            Self::parse_string,
        ];

        for func in funcs {
            let result = func(self);
            if result != ParseResult::Unhandled {
                let result = force_unwrap!(result, ParseResult::Parsed(span, lit) => (span, lit));
                return ParseResult::Parsed(result.0, PrimaryExpr::Literal(result.1))
            }
        }

        if let Obtained(span, tok) = self.eat_if(|(_span, tok)| matches!(tok, Token::Ident(_))) {
            if tok != Token::EndOfStream {
                let ident = Identifier(force_unwrap!(tok, Token::Ident(i) => i));
                return ParseResult::Parsed(span, PrimaryExpr::Identifier(ident))
            } else {
                return ParseResult::Unhandled
            }
        }

        if let Obtained(_, _) = self.eat_if(|(_span, tok)| matches!(tok, Token::ParenOpen)) {
            let (expr_span, expr) = try_parse!(self.parse_expression());
            expect_token!(self, Token::ParenClose);
            return ParseResult::Parsed(expr_span, PrimaryExpr::Grouped(Box::new(expr)))
        };

        ParseResult::Unhandled
    }

    pub(super) fn parse_postfix_expression(&mut self) -> ParseResult<PostfixExpr> {
        let (span, left) = try_parse!(
            self.parse_primary_expression(),
            Parsed(s, t) => (s, PostfixExpr::Nothing(t))
        );

        self.parse_postfix_expression_right((span, left))
    }

    fn parse_postfix_expression_right(&mut self, (span, left): (Span, PostfixExpr))
        -> ParseResult<PostfixExpr> 
    {
        let (span, tok) = match self.eat_if(|(_, tok)| matches!(tok,
            Token::Increment
            | Token::Decrement
            | Token::Dot
            | Token::Arrow
            | Token::ParenOpen
            | Token::BracketOpen)) 
        {
            EndOfStream(_) | Unwanted =>
                return ParseResult::Parsed(span, left),
            Obtained(span, tok) => (span, tok),
        };

        match tok {
            Token::Increment => {
                let left = PostfixExpr::PostIncrement(Box::new(left));
                self.parse_postfix_expression_right((span, left))
            },
            Token::Decrement => {
                let left = PostfixExpr::PostDecrement(Box::new(left));
                self.parse_postfix_expression_right((span, left))
            },
            tok_prev @ (Token::Dot | Token::Arrow) => {
                let (span, tok) = match self.eat() {
                    Obtained(span, tok) => (span, tok),
                    _ => return ParseResult::Error(span, ParseErr::UnexpectedEnd),
                };

                let id = match tok {
                    Token::Ident(id) => Identifier(id),
                    x => return ParseResult::Error(span, ParseErr::UnexpectedToken(x))
                };

                let left = match tok_prev {
                    Token::Dot => PostfixExpr::Access(Box::new(left), id),
                    Token::Arrow => PostfixExpr::AccessDeref(Box::new(left), id),
                    _ => unreachable!()
                };

                self.parse_postfix_expression_right((span, left))
            },
            Token::ParenOpen => {
                //let args = self.parse_argument_expression();
                fn parse_argument_expression<T: TokenStreamer>(
                    parser: &mut Parser<T>,
                    span: Span,
                    mut args: Vec<AssignmentExpr>
                )
                    -> ParseResult<ArgumentExpr> 
                {
                    let (span, _) = try_parse!(parser.parse_assignment_expression(),
                        Parsed(s, p) => (s, args.push(p)),
                        Unhandled => return ParseResult::Parsed(span, ArgumentExpr(args))
                    );

                    parse_argument_expression_right(parser, span, args)
                    // P => term
                    // P => P , term
                    // P => term P'
                    // P' => , term P'
                }

                fn parse_argument_expression_right<T: TokenStreamer>(parser: &mut Parser<T>, span: Span, mut args: Vec<AssignmentExpr>)
                    -> ParseResult<ArgumentExpr> 
                {
                    match parser.eat_if(|(_, tok)| matches!(tok, Token::Comma)) {
                        EndOfStream(_) | Unwanted =>
                            return ParseResult::Parsed(span, ArgumentExpr(args)),
                        Obtained(_, _) => (),
                    };

                    let (span, _) = try_parse!(parser.parse_assignment_expression(),
                        Parsed(s, p) => (s, args.push(p)),
                        Unhandled => {
                            let (span, tok) = parser.eat().unwrap();
                            return ParseResult::Error(span, ParseErr::UnexpectedToken(tok))
                        }
                    );

                    parse_argument_expression_right(parser, span, args)
                }
                let (span, args) = try_parse!(parse_argument_expression(self, span, Vec::new()));
                expect_token!(self, Token::ParenClose);
                ParseResult::Parsed(span, PostfixExpr::Call(Box::new(left), args))
            },
            Token::BracketOpen => {
                let (span, expr) = try_parse!(self.parse_expression());
                expect_token!(self, Token::BracketClose);
                ParseResult::Parsed(span, PostfixExpr::Index(Box::new(left), Box::new(expr)))
            }
            _ => unreachable!()
        }
    }

    pub(super) fn parse_prefix_expression(&mut self) -> ParseResult<PrefixExpr> {
        if self.peek().map(|x| &x.1) != Some(&Token::ParenOpen) {
            try_parse!(
                self.parse_postfix_expression(), 
                Parsed(s, t) => return ParseResult::Parsed(s, PrefixExpr::Nothing(t)),
                Unhandled => ()
            );
        }

        let (l_span, tok) = match self.eat_if(|(_, tok)| matches!(tok,
            Token::Increment
            | Token::Decrement
            | Token::Plus
            | Token::Minus
            | Token::BitNot
            | Token::LogicNot
            | Token::ParenOpen
            | Token::Multiply
            | Token::BitAnd
            | Token::Keyword(Keyword::Sizeof))) 
        {
            EndOfStream(_) | Unwanted => return ParseResult::Unhandled,
            Obtained(span, tok) => (span, tok),
        };

        match tok {
            tok @ (Token::Increment
            | Token::Decrement
            | Token::Plus
            | Token::Minus
            | Token::BitNot
            | Token::LogicNot
            | Token::Multiply
            | Token::BitAnd) => {
                let (r_span, right) = try_parse!(self.parse_prefix_expression());

                let span = Span {
                    line: l_span.line,
                    column: l_span.column,
                    length: l_span.length + r_span.length,
                };

                let result = match tok {
                    Token::Increment => PrefixExpr::PreIncrement(Box::new(right)),
                    Token::Decrement => PrefixExpr::PreDecrement(Box::new(right)),
                    Token::Plus => PrefixExpr::Positive(Box::new(right)),
                    Token::Minus => PrefixExpr::Negative(Box::new(right)),
                    Token::BitNot => PrefixExpr::BitNot(Box::new(right)),
                    Token::LogicNot => PrefixExpr::LogicNot(Box::new(right)),
                    Token::Multiply => PrefixExpr::Deref(Box::new(right)),
                    Token::BitAnd => PrefixExpr::Reference(Box::new(right)),
                    _ => unreachable!(),
                };

                ParseResult::Parsed(span, result)
            },

            Token::ParenOpen => {
                let type_name = try_parse!(
                    self.parse_type_name(),
                    Parsed(s, p) => Some((s, p)),
                    Unhandled => None
                );

                if let Some((span, type_name)) = type_name {
                    expect_token!(self, Token::ParenClose);
                    let (_span, prefix) = try_parse!(self.parse_prefix_expression());

                    let result = PrefixExpr::Cast(type_name, Box::new(prefix));
                    ParseResult::Parsed(span, result)
                } else {
                    self.backlog.push_front((l_span, Token::ParenOpen));
                    let (span, postfix) = try_parse!(self.parse_postfix_expression());
                    ParseResult::Parsed(span, PrefixExpr::Nothing(postfix))
                }
            },

            Token::Keyword(Keyword::Sizeof) => {
                if let Obtained(l_span, _) = expect_token_opt!(self, Token::ParenOpen) {
                    let type_name = try_parse!(
                        self.parse_type_name(),
                        Parsed(s, p) => Some((s, p)),
                        Unhandled => None
                    );

                    if let Some((span, type_name)) = type_name {
                        expect_token!(self, Token::ParenClose);
                        let result = PrefixExpr::SizeofType(type_name);
                        ParseResult::Parsed(span, result)
                    } else {
                        self.backlog.push_front((l_span, Token::ParenOpen));
                        let (span, prefix) = try_parse!(self.parse_prefix_expression());
                        let result = PrefixExpr::Sizeof(Box::new(prefix));
                        ParseResult::Parsed(span, result)
                    }
                } else {
                    let (span, prefix) = try_parse!(self.parse_prefix_expression());
                    let result = PrefixExpr::Sizeof(Box::new(prefix));
                    ParseResult::Parsed(span, result)
                }
            },

            _ => unreachable!(),
        }
    }

    pub(super) fn parse_binary_expression(&mut self) -> ParseResult<BinaryExpr> {
        let (span, left) = try_parse!(
            self.parse_prefix_expression(),
            Parsed(s, t) => (s, BinaryExpr::Nothing(t))
        );

        // implement binary expressions with precedence climbing
        fn inner<T: TokenStreamer>(
            parser: &mut Parser<T>,
            (mut span, mut left): (Span, BinaryExpr),
            min_op: BinaryOperator
        )
            -> ParseResult<BinaryExpr> 
        {
            loop {
                let (_, op) = match parser.peek() {//lex.eat_if(|(_span, tok)| BinaryOperator::from_token(tok).is_some()) {
                    None =>
                        return ParseResult::Parsed(span, left),
                    Some((_, tok)) if BinaryOperator::from_token(&tok).is_none() =>
                        return ParseResult::Parsed(span, left),
                    Some((s, tok)) =>
                        (s, BinaryOperator::from_token(tok).unwrap()),
                };

                let (l_span, op) = if op < min_op { 
                    return ParseResult::Parsed(span, left) 
                } else {
                    let (span, tok) = parser.eat().unwrap();
                    (span, BinaryOperator::from_token(&tok).unwrap())
                };

                let (mut r_span, mut right) = try_parse!(
                    parser.parse_prefix_expression(),
                    Parsed(s, p) => (s, BinaryExpr::Nothing(p))
                );

                loop {
                    let op_new = match parser.peek() {
                        None => break,
                        Some((_, tok)) if BinaryOperator::from_token(tok).is_none() => break,
                        Some((_, tok)) => BinaryOperator::from_token(tok).unwrap(),
                    };

                    if op_new > op {
                        let (s, r) = try_parse!(inner(parser, (r_span, right), op_new));

                        r_span = s;
                        right = r;
                    } else {
                        break
                    }
                }

                span = l_span;
                left = BinaryExpr::Binary(op, Box::new(left), Box::new(right))
            }
        }

        inner(self, (span, left), BinaryOperator::None)
    }

    pub(super) fn parse_conditional_expression(&mut self) -> ParseResult<ConditionalExpr> {
        // P => term ? E : P
        let (cond_span, cond) = try_parse!(self.parse_binary_expression());

        match self.eat_if(|(_, tok)| matches!(tok, Token::Question)) {
            EndOfStream(_) | Unwanted =>
                return ParseResult::Parsed(cond_span, ConditionalExpr::Nothing(cond)),
            _ => (),
        };

        let (expr_span, expr) = try_parse!(self.parse_expression());

        expect_token!(self, Token::Colon);

        let (_expr_else_span, expr_else) = try_parse!(self.parse_conditional_expression());

        // TODO: proper span
        ParseResult::Parsed(expr_span, ConditionalExpr::Cond(cond, Box::new(expr), Box::new(expr_else)))
    }

    pub(super) fn parse_assignment_expression(&mut self) -> ParseResult<AssignmentExpr> {
        let (l_span, left) = try_parse!(
            self.parse_conditional_expression(),
            Parsed(s, p) => (s, p)
        );

        let left = match left {
            ConditionalExpr::Nothing(BinaryExpr::Nothing(x)) => x,
            x => return ParseResult::Parsed(l_span, AssignmentExpr::Nothing(x)),
        };

        let (_span, tok) = match self.peek()
        {
            Some((_, Token::Equal
                | Token::PlusAssign
                | Token::MinusAssign
                | Token::MultiplyAssign
                | Token::DivideAssign
                | Token::RemainderAssign
                | Token::ShiftLeftAssign
                | Token::ShiftRightAssign
                | Token::BitAndAssign
                | Token::BitXorAssign
                | Token::BitOrAssign
            )) => {
                let (span, tok) = self.eat().unwrap();
                (span, tok)
            },

            _ => {
                return ParseResult::Parsed(l_span,
                    AssignmentExpr::Nothing(ConditionalExpr::Nothing(BinaryExpr::Nothing(left)))
                )
            }
        };

        let (r_span, right) = try_parse!(self.parse_assignment_expression());

        let result = match tok {
            Token::Equal => AssignmentExpr::Assign(left, Box::new(right)),
            Token::PlusAssign => AssignmentExpr::OpAssign(AssignOperator::AddAssign, left, Box::new(right)),
            Token::MinusAssign => AssignmentExpr::OpAssign(AssignOperator::SubAssign, left, Box::new(right)),
            Token::MultiplyAssign => AssignmentExpr::OpAssign(AssignOperator::MulAssign, left, Box::new(right)),
            Token::DivideAssign => AssignmentExpr::OpAssign(AssignOperator::DivAssign, left, Box::new(right)),
            Token::RemainderAssign => AssignmentExpr::OpAssign(AssignOperator::RemAssign, left, Box::new(right)),
            Token::ShiftLeftAssign => AssignmentExpr::OpAssign(AssignOperator::ShlAssign, left, Box::new(right)),
            Token::ShiftRightAssign => AssignmentExpr::OpAssign(AssignOperator::ShrAssign, left, Box::new(right)),
            Token::BitAndAssign => AssignmentExpr::OpAssign(AssignOperator::BitAndAssign, left, Box::new(right)),
            Token::BitXorAssign => AssignmentExpr::OpAssign(AssignOperator::BitXorAssign, left, Box::new(right)),
            Token::BitOrAssign => AssignmentExpr::OpAssign(AssignOperator::BitOrAssign, left, Box::new(right)),
            _ => unreachable!(),
        };

        // TODO: fix span
        ParseResult::Parsed(r_span, result)
    }

    pub fn parse_expression(&mut self) -> ParseResult<CommaExpression> {
        let (l_span, left) = try_parse!(self.parse_assignment_expression());
        self.parse_expression_right((l_span, CommaExpression::Nothing(left)))
    }

    fn parse_expression_right(&mut self, (l_span, left): (Span, CommaExpression))
        -> ParseResult<CommaExpression>
    {
        match self.eat_if(|(_, tok)| matches!(tok, Token::Comma)) {
            EndOfStream(_) | Unwanted =>
                return ParseResult::Parsed(l_span, left),
            Obtained(_, _) => (),
        };

        let (r_span, right) = try_parse!(self.parse_assignment_expression());

        let new_left = CommaExpression::Comma(Box::new(left), right);

        self.parse_expression_right((r_span, new_left))
    }
}
