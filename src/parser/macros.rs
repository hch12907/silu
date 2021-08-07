#[macro_export]
macro_rules! wrap {
    (postfix $p:expr) => {
        wrap!(prefix PrefixExpr::Nothing($p))
    };

    (prefix $p:expr) => {
        wrap!(binary BinaryExpr::Nothing($p))
    };

    (binary $p:expr) => {
        wrap!(conditional ConditionalExpr::Nothing($p))
    };

    (conditional $p:expr) => {
        wrap!(assignment AssignmentExpr::Nothing($p))
    };

    (assignment $p:expr) => {
        wrap!(expression CommaExpression::Nothing($p))
    };

    (expression $p:expr) => {
        $p
    };
}

#[macro_export]
macro_rules! force_unwrap {
    ($value:expr, $match:pat => $eval:expr) => {
        if let $match = $value {
            $eval
        } else {
            unreachable!()
        }
    };
}

#[macro_export]
macro_rules! expect_token {
    ($parser:expr, $token:pat $(if $cond:expr)?) => {
        match $parser.eat_if(|(_, tok)| matches!(tok, $token $(if $cond)?)) {
            TokenResult::EndOfStream(span) => 
                return ParseResult::Error(span, ParseErr::UnexpectedEnd),
            TokenResult::Obtained(span, tok) => (span, tok),
            TokenResult::Unwanted => {
                let (span, tok) = $parser.eat().unwrap();
                return ParseResult::Error(span, ParseErr::UnexpectedToken(tok))
            }
        }
    };
}

#[macro_export]
macro_rules! expect_token_opt {
    ($parser:expr, $token:pat $(if $cond:expr)?) => {
        $parser.eat_if(|(_, tok)| matches!(tok, $token $(if $cond)?))
    };
}

macro_rules! try_parse {
    ($result:expr) => {
        try_parse!($result, Parsed(s, p) => (s, p), Unhandled => return ParseResult::Unhandled)
    };

    ($result:expr, Parsed($s:ident, $p:ident) => $ret:expr) => {
        try_parse!($result, Parsed($s, $p) => $ret, Unhandled => return ParseResult::Unhandled)
    };

    ($result:expr, Parsed($s:ident, $p:ident) => $ret:expr, Unhandled => $ret2:expr) => {
        match $result {
            ParseResult::Parsed($s, $p) => $ret,
            ParseResult::Error(span, err) => return ParseResult::Error(span, err),
            ParseResult::Unhandled => $ret2,
        }
    };
}

macro_rules! try_parse_opt {
    ($result:expr) => {
        try_parse!($result, Parsed(s, p) => Some((s, p)), Unhandled => None)
    };
}
