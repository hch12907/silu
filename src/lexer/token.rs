use std::str::FromStr;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Keyword {
    Auto,
    Bool,
    Break,
    Case,
    Char,
    Const,
    Continue,
    Default,
    Do,
    Double,
    Else,
    Enum,
    Extern,
    Float,
    For,
    Goto,
    If,
    Inline,
    Int,
    Long,
    Register,
    Restrict,
    Return,
    Short,
    Signed,
    Sizeof,
    Static,
    Struct,
    Switch,
    Typedef,
    Union,
    Unsigned,
    Void,
    Volatile,
    While,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum NumberType {
    Bin,
    Dec,
    Hex,
    Oct,
    Float,
    HexFloat,
}

impl FromStr for Keyword {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let result = match s {
            "auto" => Some(Keyword::Auto),
            "_Bool" => Some(Keyword::Bool),
            "break" => Some(Keyword::Break),
            "case" => Some(Keyword::Case),
            "const" => Some(Keyword::Const),
            "continue" => Some(Keyword::Continue),
            "char" => Some(Keyword::Char),
            "default" => Some(Keyword::Default),
            "do" => Some(Keyword::Do),
            "double" => Some(Keyword::Double),
            "else" => Some(Keyword::Else),
            "enum" => Some(Keyword::Enum),
            "extern" => Some(Keyword::Extern),
            "float" => Some(Keyword::Float),
            "for" => Some(Keyword::For),
            "goto" => Some(Keyword::Goto),
            "if" => Some(Keyword::If),
            "inline" => Some(Keyword::Inline),
            "int" => Some(Keyword::Int),
            "long" => Some(Keyword::Long),
            "register" => Some(Keyword::Register),
            "restrict" => Some(Keyword::Restrict),
            "return" => Some(Keyword::Return),
            "signed" => Some(Keyword::Signed),
            "sizeof" => Some(Keyword::Sizeof),
            "short" => Some(Keyword::Short),
            "static" => Some(Keyword::Static),
            "struct" => Some(Keyword::Struct),
            "switch" => Some(Keyword::Switch),
            "typedef" => Some(Keyword::Typedef),
            "union" => Some(Keyword::Union),
            "unsigned" => Some(Keyword::Unsigned),
            "void" => Some(Keyword::Void),
            "volatile" => Some(Keyword::Volatile),
            "while" => Some(Keyword::While),
            _ => None,
        };

        result.ok_or(())
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TokenError {
    BadChar(String),
    BadEscape(char),
    EmptyChar,
    InvalidFloatPrefix(NumberType, String, String),
    InvalidHexFloat, // no exponent
    UnexpectedNewline,
    UnexpectedEof,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
    // Single character tokens
    Plus,
    Minus,
    Multiply,
    Divide,
    Remainder,
    Equal,
    GreaterThan,
    LessThan,
    ParenOpen,
    ParenClose,
    BracketOpen,
    BracketClose,
    BraceOpen,
    BraceClose,
    Comma,
    Dot,
    Semicolon,
    BitAnd,
    BitOr,
    BitXor,
    BitNot,
    LogicNot,
    Question,
    Colon,
    Preprocessor,
    Preprocessor2,

    // Double character tokens
    PlusAssign,
    Increment,
    MinusAssign,
    Decrement,
    MultiplyAssign,
    MultiCommentEnd,
    DivideAssign,
    RemainderAssign,
    CommentStarter,
    MultiCommentStarter,
    LogicEqual,
    ShiftLeft,
    ShiftRight,
    GreaterEqual,
    LessEqual,
    LogicAnd,
    BitAndAssign,
    LogicOr,
    BitOrAssign,
    BitXorAssign,
    BitNotAssign,
    LogicNotEqual,
    Arrow,

    // Triple character tokens
    DocStarter,
    ShiftLeftAssign,
    ShiftRightAssign,

    Keyword(Keyword),

    Char(char),
    Ident(String),
    Number(NumberType, String, String),
    Str(String),
    
    Error(TokenError),
    Unknown(char),

    EndOfStream,
}
