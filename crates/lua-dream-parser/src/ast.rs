use lua_dream_lexer::token::TokenKindDiscriminants;

#[derive(Clone, Debug)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub control_statement: Option<ControlStatement>,
}

#[derive(Clone, Debug)]
pub enum ControlStatement {
    Return(Option<Expression>),
    Break,
}

#[derive(Clone, Debug, strum::EnumDiscriminants)]
pub enum Expression {
    Double(f64),
    Integer(i64),
    String(String),
    Boolean(bool),
    Identifier(String),
    Nil,
    Varargs,
    UnaryOp {
        op: UnaryOp,
        val: Box<Expression>,
    },
    BinaryOp {
        op: BinaryOp,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Table(Vec<TableRow>),
    FunctionCall(Box<FunctionCall>),
    Index {
        table: Box<Expression>,
        key: Box<Expression>,
    },
    MethodCall(Box<MethodCall>),
}

#[derive(Clone, Debug)]
pub enum FnArg {
    Name(String),
    Varargs,
}

#[derive(Clone, Debug)]
pub enum Statement {
    DeclareLocal {
        name: String,
        assign: Option<Expression>,
        attribute: Option<Attribute>,
    },
    DeclareGlobal {
        name: String,
        assign: Option<Expression>,
        attribute: Option<Attribute>,
    },
    Assign {
        name: String,
        expression: Option<Expression>,
    },
    If(Box<Branch>),
    For(Box<ForLoop>),
    ForGeneric(Box<ForGenericLoop>),
    While(Box<WhileLoop>),
    Repeat(Box<RepeatUntilLoop>),
    Label(String),
    Goto(String),
    Function(Box<FunctionDecl>),
    FunctionCall(Box<FunctionCall>),
    MethodCall(Box<MethodCall>),
}

#[derive(Clone, Debug, strum::EnumString)]
#[strum(serialize_all = "snake_case")]
pub enum Attribute {
    Const,
    Close,
}

#[derive(Clone, Debug)]
pub enum TableRow {
    KeyValue { key: Expression, value: Expression },
    ListElem(Expression),
}

#[derive(Clone, Debug)]
pub enum BinaryOp {
    Add,
    Sub,
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
    Concat,
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
}

impl BinaryOp {
    pub fn get_precedence(&self) -> u8 {
        use BinaryOp::*;
        match self {
            Or => 1,
            And => 2,

            Eq | Neq | Lt | Gt | Leq | Geq => 3,

            BitOr | BitXor => 4,
            BitAnd => 5,
            Shl | Shr => 6,

            Concat => 7,

            Add | Sub => 8,

            Mul | Div | FloorDiv | Mod => 9,

            Pow => 11,
        }
    }

    pub fn is_right_associative(&self) -> bool {
        matches!(self, BinaryOp::Pow | BinaryOp::Concat)
    }
}
impl TryFrom<TokenKindDiscriminants> for BinaryOp {
    type Error = ();

    fn try_from(value: TokenKindDiscriminants) -> Result<Self, Self::Error> {
        use BinaryOp::*;
        let res = match value {
            TokenKindDiscriminants::Plus => Add,
            TokenKindDiscriminants::Minus => Sub,
            TokenKindDiscriminants::Mul => Mul,
            TokenKindDiscriminants::Div => Div,
            TokenKindDiscriminants::FloorDiv => FloorDiv,
            TokenKindDiscriminants::Mod => Mod,
            TokenKindDiscriminants::Pow => Pow,
            TokenKindDiscriminants::Eq => Eq,
            TokenKindDiscriminants::Neq => Neq,
            TokenKindDiscriminants::Leq => Leq,
            TokenKindDiscriminants::Geq => Geq,
            TokenKindDiscriminants::Lt => Lt,
            TokenKindDiscriminants::Gt => Gt,
            TokenKindDiscriminants::And => And,
            TokenKindDiscriminants::Or => Or,
            TokenKindDiscriminants::Concat => Concat,
            TokenKindDiscriminants::BitAnd => BitAnd,
            TokenKindDiscriminants::BitOr => BitOr,
            TokenKindDiscriminants::BitXor => BitXor,
            TokenKindDiscriminants::Shl => Shl,
            TokenKindDiscriminants::Shr => Shr,
            _ => return Err(()),
        };
        Ok(res)
    }
}
#[derive(Clone, Debug)]
pub enum UnaryOp {
    BitNot,
    Not,
    Len,
    Neg,
}

impl TryFrom<TokenKindDiscriminants> for UnaryOp {
    type Error = ();
    fn try_from(value: TokenKindDiscriminants) -> Result<Self, Self::Error> {
        let res = match value {
            TokenKindDiscriminants::BitXor => UnaryOp::BitNot,
            TokenKindDiscriminants::Not => UnaryOp::Not,
            TokenKindDiscriminants::Len => UnaryOp::Len,
            TokenKindDiscriminants::Minus => UnaryOp::Neg,
            _ => return Err(()),
        };
        Ok(res)
    }
}

#[derive(Clone, Debug)]
pub struct Branch {
    pub condition: Expression,
    pub then_block: Block,
    pub else_branch: Option<Box<ElseBranch>>,
}

#[derive(Clone, Debug)]
pub enum ElseBranch {
    Else(Block),
    ElseIf(Branch),
}

#[derive(Clone, Debug)]
pub struct FunctionCall {
    pub prefix: Expression,
    pub arguments: Vec<Expression>,
}

#[derive(Clone, Debug)]
pub struct MethodCall {
    pub prefix: Expression,
    pub method_name: String,
    pub arguments: Vec<Expression>,
}

#[derive(Clone, Debug)]
pub struct ForLoop {
    pub variable_name: String,
    pub from: Expression,
    pub to: Expression,
    pub increment: Expression,
    pub do_block: Block,
}

#[derive(Clone, Debug)]
pub struct FunctionDecl {
    pub name: String,
    pub args: Vec<String>,
    pub has_varargs: bool,
    pub block: Block,
}

#[derive(Clone, Debug)]
pub struct ForGenericLoop {
    pub variable_names: Vec<String>,
    pub iterators: Vec<Expression>,
    pub do_block: Block,
}

#[derive(Clone, Debug)]
pub struct WhileLoop {
    pub condition: Expression,
    pub do_block: Block,
}

#[derive(Clone, Debug)]
pub struct RepeatUntilLoop {
    pub repeat_block: Block,
    pub until: Expression,
}
