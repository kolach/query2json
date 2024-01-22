use core::fmt;

use byte_unit::ByteUnit;
use serde::{Deserialize, Serialize};

#[derive(Debug, Copy, Clone, PartialEq, Serialize, Deserialize)]
pub enum TimeUnit {
    S, // second
    M, // minute
    H, // hour
    D, // day
}

impl fmt::Display for TimeUnit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                &TimeUnit::S => "s",
                &TimeUnit::M => "m",
                &TimeUnit::H => "h",
                &TimeUnit::D => "d",
            }
        )
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(tag = "t", content = "c")]
pub enum ExprValue<'ast> {
    Word(&'ast str),
    Str(&'ast str),
    // SemVer { major: i64, minor: i64, patch: i64, pre: Options<&'ast str> }, // v0.0.1-patch.1
    // RelVer { major: i64, beta: Option<i64>, kernel: Option<&'ast str> }, // 123(-beta123)(-x86_kiosk)
    Int(i64),
    Bool(bool),
    Byte { v: f64, u: ByteUnit },
    Duration { v: f64, u: TimeUnit },
    Null,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum BoolOp {
    Ne, // !, example: `!foo` which equals to `foo = false`
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ExprBool<'ast> {
    pub op: Option<BoolOp>,
    pub field: &'ast str,
}

impl<'ast> From<ExprBool<'ast>> for Box<Expr<'ast>> {
    fn from(expr: ExprBool<'ast>) -> Self {
        Box::new(Expr::Bool(expr))
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum RelOp {
    Eq, // equal
    Ne, // not equal
    Lt, // less than
    Gt, // greater than
    Le, // less than or equal
    Ge, // greater than or equal
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ExprRel<'ast> {
    pub op: RelOp,
    pub field: &'ast str,
    pub value: ExprValue<'ast>,
}

impl<'ast> From<ExprRel<'ast>> for Box<Expr<'ast>> {
    fn from(expr: ExprRel<'ast>) -> Self {
        Box::new(Expr::Rel(expr))
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ExprInArray<'ast> {
    pub field: &'ast str,
    pub values: Vec<ExprValue<'ast>>,
}

impl<'ast> From<ExprInArray<'ast>> for Box<Expr<'ast>> {
    fn from(expr: ExprInArray<'ast>) -> Self {
        Box::new(Expr::InArray(expr))
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ExprReg<'ast> {
    pub field: &'ast str,
    pub value: &'ast str,
}

impl<'ast> From<ExprReg<'ast>> for Box<Expr<'ast>> {
    fn from(expr: ExprReg<'ast>) -> Self {
        Box::new(Expr::Reg(expr))
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ExprInRange<'ast> {
    pub field: &'ast str,
    pub lower: ExprValue<'ast>,
    pub upper: ExprValue<'ast>,
}

impl<'ast> From<ExprInRange<'ast>> for Box<Expr<'ast>> {
    fn from(expr: ExprInRange<'ast>) -> Self {
        Box::new(Expr::InRange(expr))
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum LogicOp {
    And, // and, &&
    Or,  // or, ||
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ExprLogic<'ast> {
    pub op: LogicOp,
    #[serde(borrow)]
    pub exprs: Vec<Box<Expr<'ast>>>,
}

impl<'ast> From<ExprLogic<'ast>> for Box<Expr<'ast>> {
    fn from(expr: ExprLogic<'ast>) -> Self {
        Box::new(Expr::Logic(expr))
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(tag = "t")]
pub enum Expr<'ast> {
    #[serde(borrow)]
    Value(ExprValue<'ast>),
    Bool(ExprBool<'ast>),
    Rel(ExprRel<'ast>),
    InArray(ExprInArray<'ast>),
    InRange(ExprInRange<'ast>),
    Reg(ExprReg<'ast>),
    Logic(ExprLogic<'ast>),
    Empty,
}
