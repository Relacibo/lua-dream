#[derive(Clone, Debug)]
pub struct Block {
    pub stats: Vec<Stmt>,
    pub res_stmt: Option<ResStmt>,
}

#[derive(Clone, Debug)]
pub enum ResStmt {
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
    Table(Vec<TableRow>),
}

#[derive(Clone, Debug)]
pub enum FnArg {
    Name(String),
    Varargs,
}

#[derive(Clone, Debug)]
pub enum Stmt {
    AssignLocal { name: String, val: Expr },
    AssignGlobal { name: String, val: Expr },
    Expr(Expr),
}

#[derive(Clone, Debug)]
pub struct TableRow {
    pub key: Expr,
    pub value: Expr,
}

#[derive(Clone, Debug)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
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
