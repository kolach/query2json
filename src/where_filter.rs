use crate::parser::{
    syn::{
        BoolOp, Expr, ExprBool, ExprInArray, ExprInRange, ExprLogic, ExprReg, ExprRel, ExprValue,
        LogicOp, RelOp, TimeUnit,
    },
    visit::{visit, Visit},
};

use crate::json::{self, Writer};

/// write ExprValue
impl json::Value for &ExprValue<'_> {
    fn write(&self, out: &mut dyn std::fmt::Write) -> json::Result {
        match self {
            ExprValue::Word(v) => write!(out, "\"{}\"", v),
            ExprValue::Str(v) => write!(out, "\"{}\"", v),
            ExprValue::Int(v) => write!(out, "{}", v),
            ExprValue::Bool(v) => write!(out, "{}", v),
            ExprValue::Byte { v, u } => {
                let bytes = byte_unit::Byte::from_unit(*v, *u).unwrap();
                write!(out, "{}", bytes)
            }
            ExprValue::Duration { v, u } => {
                let ms = match u {
                    TimeUnit::S => (v * 1000.0) as i64,
                    TimeUnit::M => (v * 1000.0 * 60.0) as i64,
                    TimeUnit::H => (v * 1000.0 * 60.0 * 60.0) as i64,
                    TimeUnit::D => (v * 1000.0 * 60.0 * 60.0 * 24.0) as i64,
                };
                write!(out, "{}", ms)
            }
            ExprValue::Null => write!(out, "null"),
        }
    }
}

/// Write arbitrary expression using visiter
impl json::Value for &Expr<'_> {
    fn write(&self, out: &mut dyn std::fmt::Write) -> json::Result {
        let mut writer = Writer::new(out);
        let mut visitor = SerializeAsWhere::new(&mut writer);
        visit(self, &mut visitor)
    }
}

/// Convers relational operator to string
impl json::Key for &RelOp {
    fn as_str(&self) -> &str {
        match self {
            RelOp::Eq => "eq",
            RelOp::Ne => "neq",
            RelOp::Gt => "gt",
            RelOp::Ge => "gte",
            RelOp::Lt => "lt",
            RelOp::Le => "lte",
        }
    }
}

/// Convers logical operator to string
impl json::Key for &LogicOp {
    fn as_str(&self) -> &str {
        match self {
            LogicOp::And => "and",
            LogicOp::Or => "or",
        }
    }
}

struct SerializeAsWhere<'a> {
    write: &'a mut Writer<'a>,
}

impl<'a> SerializeAsWhere<'a> {
    fn new(writer: &'a mut Writer<'a>) -> Self {
        Self { write: writer }
    }
}

impl<'ast> Visit<'ast> for SerializeAsWhere<'ast> {
    type Result = json::Result;

    fn visit_expr_value(&mut self, n: &'ast ExprValue) -> Self::Result {
        self.write.value(n)
    }

    /// -> {"foo": true}
    /// -> {"foo": false}
    fn visit_expr_bool(&mut self, n: &'ast ExprBool) -> Self::Result {
        let value = match n.op {
            Some(BoolOp::Ne) => false,
            _ => true,
        };
        self.write.object(|write| write.field(n.field, value))
    }

    /// -> {"foo": {"inq": ["a", "b", "c"]}}
    fn visit_expr_in_array(&mut self, n: &'ast ExprInArray) -> Self::Result {
        self.write.object(|write| {
            write.object(n.field, |write| {
                write.array("inq", |write| {
                    for v in &n.values {
                        write.elem(v)?;
                    }
                    Ok(())
                })
            })
        })
    }

    /// {"foo": {"between": [1, 10]}}
    fn visit_expr_in_range(&mut self, n: &'ast ExprInRange) -> Self::Result {
        self.write.object(|write| {
            write.object(n.field, |write| {
                // -> [lower, upper]
                write.array("between", |write| {
                    write.elem(&n.lower)?;
                    write.elem(&n.upper)
                })
            })
        })
    }

    // -> {"foo": "bar"}
    // -> {"foo": {"gt": 1}}
    fn visit_expr_rel(&mut self, n: &'ast ExprRel) -> Self::Result {
        self.write.object(|write| {
            if n.op == RelOp::Eq {
                // {<n.field>: <n.value>}
                return write.field(n.field, &n.value);
            }

            // otherwise
            // {<n.field>: {<n.op>: <n.value>}}
            write.object(n.field, |write| {
                // nested part for all cases but RelOp::Eq
                write.field(&n.op, &n.value)
            })
        })
    }

    // -> {"foo": {"regexp": value}}
    fn visit_expr_reg(&mut self, n: &'ast ExprReg) -> Self::Result {
        // {<n.field>: {"regexp": <n.value>}}
        self.write
            .object(|write| write.object(n.field, |write| write.field("regexp", n.value)))
    }

    // -> {"and": [{"foo": "bar"}, {"size": {"gt": 100}}]}
    fn visit_expr_logic(&mut self, n: &'ast ExprLogic) -> Self::Result {
        if n.exprs.is_empty() {
            // skip writing expression if it's empty
            return Ok(());
        }

        if n.exprs.len() == 1 {
            // no need making complex expression for a single expr in array
            return visit(&n.exprs[0], self);
        }

        // {<n.op>: [{<n.exprs[0]>}, {<n.exprs[1]>}, ... {<n.exprs[N]>}]}
        self.write.object(|write| {
            write.array(&n.op, |write| {
                for expr in &n.exprs {
                    write.elem(&**expr)?;
                }
                Ok(())
            })
        })
    }

    // {} # just return an empty object
    fn visit_expr_empty(&mut self) -> Self::Result {
        self.write.object(|_| Ok(()))
    }
}

/// Build where filter as JSON string
pub fn serialize_as_where<'a>(expr: &Expr<'a>) -> Result<String, std::fmt::Error> {
    let mut out = String::new();
    let mut writer = Writer::new(&mut out);
    let mut visitor = SerializeAsWhere::new(&mut writer);
    visit(expr, &mut visitor)?;
    Ok(out)
}

#[cfg(test)]
mod tests {
    use crate::parser::parse_query;
    use crate::where_filter::serialize_as_where;

    #[test]
    fn test_where_filter() {
        let test_cases = vec![
            // <query>, <where filter JSON string>
            ("a = 1", r#"{"a":1}"#),
            ("a == 1", r#"{"a":1}"#),
            ("a > 1", r#"{"a":{"gt":1}}"#),
            ("a < 1", r#"{"a":{"lt":1}}"#),
            ("a >= 1", r#"{"a":{"gte":1}}"#),
            ("a <= 1", r#"{"a":{"lte":1}}"#),
            ("a != 1", r#"{"a":{"neq":1}}"#),
            ("foo", r#"{"foo":true}"#),
            ("!foo", r#"{"foo":false}"#),
            ("a in 1..2", r#"{"a":{"between":[1,2]}}"#),
            (
                "a in [foo, bar, 'a b']",
                r#"{"a":{"inq":["foo","bar","a b"]}}"#,
            ),
            ("a ~= /^test$/", r#"{"a":{"regexp":"^test$"}}"#),
            (
                "a > 1 or b < 2",
                r#"{"or":[{"a":{"gt":1}},{"b":{"lt":2}}]}"#,
            ),
            (
                "(a > 1 or b < 2) and !online",
                r#"{"and":[{"or":[{"a":{"gt":1}},{"b":{"lt":2}}]},{"online":false}]}"#,
            ),
            ("a = null", r#"{"a":null}"#),
        ];

        for tc in &test_cases {
            let ast = parse_query(tc.0).unwrap();
            let filter = serialize_as_where(&ast).unwrap();
            assert_eq!(filter, tc.1);
        }
    }
}
