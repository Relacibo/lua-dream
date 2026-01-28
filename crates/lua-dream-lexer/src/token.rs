use strum::EnumDiscriminants;

#[derive(Clone, Debug, EnumDiscriminants)]
pub enum TokenKind {
    Identifier(String),
    LiteralString(String),
    LiteralDouble(f64),
    LiteralInteger(i64),
    LiteralBoolean(bool),
    LiteralNil,
    Assign,
    Plus,
    Minus,
    Mul,
    Div,
    FloorDiv,
    Mod,
    Pow,
    Eq,
    Neq,
    Leq,
    Geq,
    Lt,
    Gt,
    And,
    Or,
    Not,
    Len,
    Concat,
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
    Comma,
    Semicolon,
    Varargs,
    Dot,
    Colon,
    DoubleColon,
    ParenOpen,
    ParenClose,
    BracketsOpen,
    BracketsClose,
    CurlyBracesOpen,
    CurlyBracesClose,
    KeywordLocal,
    KeywordGlobal,
    KeywordFunction,
    KeywordIf,
    KeywordThen,
    KeywordElseIf,
    KeywordElse,
    KeywordEnd,
    KeywordReturn,
    KeywordWhile,
    KeywordDo,
    KeywordBreak,
    KeywordRepeat,
    KeywordUntil,
    KeywordGoto,
    KeywordFor,
    KeywordIn,
    Eof,
}

impl TokenKind {
    pub fn discriminant(&self) -> TokenKindDiscriminants {
        TokenKindDiscriminants::from(self)
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub line: usize,
    pub column: usize,
}
