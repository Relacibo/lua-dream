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

#[derive(Clone, Debug)]
pub enum Expression {
    Double(f64),
    Integer(i64),
    String(String),
    Boolean(bool),
    UnaryOp {
        op: UnaryOp,
        val: Box<Expression>,
    },
    BinaryOp {
        op: BinaryOp,
        left: Box<Expression>,
        right: Box<Expression>,
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
pub enum Statement {
    AssignLocal {
        name: String,
        expression: Expression,
        attribute: Option<Attribute>,
    },
    AssignGlobal {
        name: String,
        expression: Expression,
        attribute: Option<Attribute>,
    },
    Label(String),
    Expression(Expression),
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
