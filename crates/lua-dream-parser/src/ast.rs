#[derive(Clone, Debug)]
pub struct Block {
    pub stats: Vec<Stmt>,
    pub ctrl_stmt: Option<CtrlStmt>,
}

#[derive(Clone, Debug)]
pub enum CtrlStmt {
    Return(Option<Expr>),
    Break,
}

#[derive(Clone, Debug)]
pub enum Expr {
    Number(f64),
    String(String),
    Boolean(bool),
    UnaryOp {
        op: UnaryOp,
        val: Box<Expr>,
    },
    BinaryOp {
        op: BinaryOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Goto(String),
    Table(Vec<TableRow>),
}

#[derive(Clone, Debug)]
pub enum FnArg {
    Name(String),
    Varargs,
}

#[derive(Clone, Debug)]
pub enum Stmt {
    AssignLocal {
        name: String,
        val: Expr,
        attr: Option<Attr>,
    },
    AssignGlobal {
        name: String,
        val: Expr,
        attr: Option<Attr>,
    },
    Label(String),
    Expr(Expr),
}

#[derive(Clone, Debug)]
pub enum Attr {
    Const,
    Close,
}

#[derive(Clone, Debug)]
pub struct TableRow {
    pub key: Expr,
    pub val: Expr,
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
