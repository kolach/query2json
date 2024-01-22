use std::{collections::HashMap, fmt::Write};

use serde::Serialize;

use crate::parser::{
    syn::{
        BoolOp, Expr, ExprBool, ExprInArray, ExprInRange, ExprLogic, ExprReg, ExprRel, ExprValue,
        LogicOp, RelOp, TimeUnit,
    },
    visit::{visit, Visit},
};

struct BuildQuery<'a> {
    out: &'a mut dyn Write,
}

impl<'ast> Visit<'ast> for BuildQuery<'ast> {
    type Result = Result<(), std::fmt::Error>;

    fn visit_expr_value(&mut self, _node: &'ast ExprValue) -> Self::Result {
        match *_node {
            ExprValue::Word(v) => self.out.write_str(v),
            ExprValue::Str(v) => write!(self.out, "'{}'", v),
            ExprValue::Int(v) => write!(self.out, "{}", v),
            ExprValue::Bool(v) => write!(self.out, "{}", v),
            ExprValue::Byte { v, u } => write!(self.out, "{}{}", v, u),
            ExprValue::Duration { v, u } => write!(self.out, "{}{}", v, u),
            ExprValue::Null => write!(self.out, "null"),
        }
    }

    fn visit_expr_bool(&mut self, _node: &'ast ExprBool) -> Self::Result {
        if let Some(op) = &_node.op {
            match op {
                BoolOp::Ne => self.out.write_char('!')?,
            }
        }
        self.out.write_str(_node.field)
    }

    fn visit_expr_in_array(&mut self, _node: &'ast ExprInArray) -> Self::Result {
        self.out.write_str(_node.field)?;
        self.out.write_str(" in [")?;
        let mut it = _node.values.iter().peekable();
        while let Some(expr) = it.next() {
            self.visit_expr_value(&*expr)?;
            if !it.peek().is_none() {
                self.out.write_str(", ")?;
            }
        }
        self.out.write_char(']')
    }

    fn visit_expr_in_range(&mut self, _node: &'ast ExprInRange) -> Self::Result {
        self.out.write_str(_node.field)?;
        self.out.write_str(" in ")?;
        self.visit_expr_value(&_node.lower)?;
        self.out.write_str("..")?;
        self.visit_expr_value(&_node.upper)
    }

    fn visit_expr_rel(&mut self, _node: &'ast ExprRel) -> Self::Result {
        self.out.write_str(_node.field)?;
        self.out.write_str(match _node.op {
            RelOp::Eq => " = ",
            RelOp::Ne => " != ",
            RelOp::Gt => " > ",
            RelOp::Ge => " >= ",
            RelOp::Lt => " < ",
            RelOp::Le => " <= ",
        })?;
        self.visit_expr_value(&_node.value)
    }

    fn visit_expr_reg(&mut self, _node: &'ast ExprReg) -> Self::Result {
        write!(self.out, "{} ~= /{}/", _node.field, _node.value)
    }

    fn visit_expr_logic(&mut self, _node: &'ast ExprLogic) -> Self::Result {
        // skip writing expression if it's empty
        if _node.exprs.is_empty() {
            return Ok(());
        }

        // no need to group expressions if we have only 1
        if _node.exprs.len() == 1 {
            return visit(&_node.exprs[0], self);
        }

        // enclose expressions in parens
        self.out.write_char('(')?;

        let mut it = _node.exprs.iter().peekable();
        while let Some(expr) = it.next() {
            visit(&*expr, self)?;
            if !it.peek().is_none() {
                self.out.write_str(match _node.op {
                    LogicOp::And => " and ",
                    LogicOp::Or => " or ",
                })?;
            }
        }

        self.out.write_char(')')
    }

    fn visit_expr_empty(&mut self) -> Self::Result {
        Ok(())
    }
}

/// Build query.
pub fn build_query<'a>(expr: &Expr<'a>) -> Result<String, std::fmt::Error> {
    let mut out = String::new();
    let mut vis = BuildQuery { out: &mut out };
    visit(expr, &mut vis)?;
    Ok(out)
}

#[derive(Debug, Serialize)]
#[serde(untagged)]
pub enum WhereFilter<'a> {
    Str(&'a str),
    Int(i64),
    Bool(bool),
    Array(Vec<WhereFilter<'a>>),
    Obj(HashMap<&'a str, WhereFilter<'a>>),
    Null,
}

impl<'a> From<(&'a str, WhereFilter<'a>)> for WhereFilter<'a> {
    fn from(pair: (&'a str, WhereFilter<'a>)) -> Self {
        Self::Obj(HashMap::from([pair]))
    }
}

impl<'a> From<Vec<WhereFilter<'a>>> for WhereFilter<'a> {
    fn from(vec: Vec<WhereFilter<'a>>) -> Self {
        Self::Array(vec)
    }
}

struct BuildWhereFilter {}

impl<'a> Visit<'a> for BuildWhereFilter {
    type Result = WhereFilter<'a>;

    fn visit_expr_empty(&mut self) -> Self::Result {
        WhereFilter::Obj(HashMap::new())
    }

    fn visit_expr_value(&mut self, v: &ExprValue<'a>) -> Self::Result {
        match *v {
            ExprValue::Word(w) => WhereFilter::Str(w),
            ExprValue::Str(s) => WhereFilter::Str(s),
            ExprValue::Int(i) => WhereFilter::Int(i),
            ExprValue::Bool(b) => WhereFilter::Bool(b),
            ExprValue::Byte { v, u } => {
                // TODO: think about using TryFrom and handle possible error here
                let bytes = byte_unit::Byte::from_str(format!("{}{}", v, u)).unwrap();
                WhereFilter::Int(bytes.get_bytes() as i64)
            }
            ExprValue::Duration { v, u } => {
                let ms = match u {
                    TimeUnit::S => (v * 1000.0) as i64,
                    TimeUnit::M => (v * 1000.0 * 60.0) as i64,
                    TimeUnit::H => (v * 1000.0 * 60.0 * 60.0) as i64,
                    TimeUnit::D => (v * 1000.0 * 60.0 * 60.0 * 24.0) as i64,
                };
                WhereFilter::Int(ms)
            }
            ExprValue::Null => WhereFilter::Null,
        }
    }

    fn visit_expr_bool(&mut self, v: &ExprBool<'a>) -> Self::Result {
        let value = match v.op {
            Some(BoolOp::Ne) => false,
            None => true,
        };
        let value = WhereFilter::Bool(value);
        (v.field, value).into()
    }

    fn visit_expr_rel(&mut self, v: &'a ExprRel) -> Self::Result {
        let value = self.visit_expr_value(&v.value);
        let inner = match v.op {
            RelOp::Eq => value,
            RelOp::Ne => ("neq", value).into(),
            RelOp::Gt => ("gt", value).into(),
            RelOp::Ge => ("gte", value).into(),
            RelOp::Lt => ("lt", value).into(),
            RelOp::Le => ("lte", value).into(),
        };
        (v.field, inner).into()
    }

    fn visit_expr_in_array(&mut self, v: &'a ExprInArray) -> Self::Result {
        let values: Vec<WhereFilter> = v.values.iter().map(|x| self.visit_expr_value(x)).collect();
        let values = values.into();
        let inner = ("inq", values).into();
        (v.field, inner).into()
    }

    fn visit_expr_in_range(&mut self, v: &'a ExprInRange) -> Self::Result {
        let lower = self.visit_expr_value(&v.lower);
        let upper = self.visit_expr_value(&v.upper);
        let values = vec![lower, upper];
        let inner = ("between", values.into()).into();
        (v.field, inner).into()
    }

    fn visit_expr_reg(&mut self, v: &'a ExprReg) -> Self::Result {
        let value = WhereFilter::Str(v.value);
        let inner = ("regexp", value).into();
        (v.field, inner).into()
    }

    fn visit_expr_logic(&mut self, v: &'a ExprLogic) -> Self::Result {
        let values: Vec<WhereFilter> = v
            .exprs
            .iter()
            .map(|expr| visit(expr, &mut BuildWhereFilter {}))
            .collect();

        let op = match v.op {
            LogicOp::Or => "or",
            LogicOp::And => "and",
        };

        (op, values.into()).into()
    }
}

pub fn build_where<'a>(expr: &'a Expr) -> WhereFilter<'a> {
    visit(expr, &mut BuildWhereFilter {})
}

#[cfg(test)]
mod tests {
    use crate::parser::{
        build_query,
        syn::{
            BoolOp, Expr, ExprBool, ExprInArray, ExprInRange, ExprLogic, ExprReg, ExprRel,
            ExprValue, LogicOp, RelOp,
        },
    };

    #[test]
    fn test_compose_query() {
        let exp = Expr::Logic(ExprLogic {
            op: LogicOp::Or,
            exprs: vec![
                ExprRel {
                    op: RelOp::Eq,
                    field: "foo",
                    value: ExprValue::Int(5),
                }
                .into(),
                ExprInRange {
                    field: "baz",
                    lower: ExprValue::Int(1),
                    upper: ExprValue::Int(100),
                }
                .into(),
                ExprLogic {
                    op: LogicOp::And,
                    exprs: vec![
                        ExprInArray {
                            field: "baz",
                            values: vec![ExprValue::Int(1), ExprValue::Int(2), ExprValue::Int(3)],
                        }
                        .into(),
                        ExprReg {
                            field: "moo",
                            value: "^head",
                        }
                        .into(),
                        ExprBool {
                            op: Some(BoolOp::Ne),
                            field: "online",
                        }
                        .into(),
                    ],
                }
                .into(),
                ExprRel {
                    op: RelOp::Ne,
                    field: "bar",
                    value: ExprValue::Str("foo bar"),
                }
                .into(),
            ],
        });

        assert_eq!(
            build_query(&exp).unwrap(),
            "(foo = 5 or baz in 1..100 or (baz in [1, 2, 3] and moo ~= /^head/ and !online) or bar != 'foo bar')"
        );
    }
}
