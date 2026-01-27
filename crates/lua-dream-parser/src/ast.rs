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
    Call(FunctionCall),
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
    If(Branch),
    For {
        variable_name: String,
        from: Expression,
        to: Expression,
        increment: Expression,
        do_block: Block,
    },
    ForGeneric {
        variable_names: Vec<String>,
        iterators: Vec<Expression>,
        do_block: Block,
    },
    While {
        condition: Expression,
        do_block: Block,
    },
    Repeat {
        repeat_block: Block,
        until: Expression,
    },
    Label(String),
    Goto(String),
    Function {
        name: String,
        args: Vec<Expression>,
        has_varargs: bool,
        block: Block,
    },
    Call(FunctionCall),
}

#[derive(Clone, Debug, strum::EnumString)]
#[strum(serialize_all = "snake_case")]
pub enum Attribute {
    Const,
    Close,
}

#[derive(Clone, Debug)]
pub struct TableRow {
    pub key: Expression,
    pub value: Expression,
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
    pub prefix: Box<Expression>,
    pub arguments: Vec<Expression>,
}
