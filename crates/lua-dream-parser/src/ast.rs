use lua_dream_lexer::token::TokenKind;

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
    pub expr: Expression,
}

#[derive(Clone, Debug)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    DivFloor,
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

#[derive(Clone, Debug)]
pub enum UnaryOp {
    BitNot,
    Not,
    Len,
    Neg,
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
