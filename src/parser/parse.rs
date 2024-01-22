use byte_unit::ByteUnit;
use nom::{
    branch::alt,
    bytes::complete::{escaped, tag},
    character::complete::{alpha1, alphanumeric1, digit1, multispace0, none_of},
    combinator::{all_consuming, cut, eof, map, map_res, opt, recognize, value},
    error::{context, convert_error, ParseError, VerboseError},
    multi::{many0, separated_list1},
    sequence::{delimited, pair, separated_pair, tuple},
    Parser,
};

use crate::parser::syn::{
    BoolOp, Expr, ExprBool, ExprInArray, ExprInRange, ExprLogic, ExprReg, ExprRel, ExprValue,
    LogicOp, RelOp, TimeUnit,
};

use std::{ops::Deref, str::FromStr};

pub type IResult<I, O> = nom::IResult<I, O, VerboseError<I>>;

/// Identifier
///
/// ```rust
/// use query2json::parser::parse::ident;
///
/// assert_eq!(ident("foo;"), Ok((";", "foo")));
/// assert_eq!(ident("foo123"), Ok(("", "foo123")));
/// assert_eq!(ident("_foo"), Ok(("", "_foo")));
/// ```
pub fn ident<'a>(input: &'a str) -> IResult<&'a str, &'a str> {
    context(
        "identifier",
        recognize(pair(
            alt((alpha1, tag("_"))),
            many0(alt((alphanumeric1, tag("_")))),
        )),
    )(input)
}

/// Dot-separated path to object property
///
/// ```rust
/// use query2json::parser::parse::ident_path;
///
/// assert_eq!(ident_path("foo;"), Ok((";", "foo")));
/// assert_eq!(ident_path("foo.bar"), Ok(("", "foo.bar")));
/// ```
pub fn ident_path<'a>(input: &'a str) -> IResult<&'a str, &'a str> {
    context(
        "identifier path",
        recognize(separated_list1(tag("."), ident)),
    )(input)
}

/// Single-quoted string
///
/// ```rust
/// use query2json::parser::parse::string;
/// use nom::error::VerboseError;
///
/// assert_eq!(string("'hello, world';"), Ok((";", "hello, world")));
/// ```
pub fn string<'a>(input: &'a str) -> IResult<&'a str, &'a str> {
    let esc = escaped(none_of("\\\'"), '\\', tag("'"));
    let esc_or_empty = alt((esc, tag("")));
    let begin = tag("'");
    let end = context("closing '", tag("'"));
    context("string", delimited(begin, esc_or_empty, end))(input)
}

/// regexp string
///
/// ```rust
/// use query2json::parser::parse::regexp_str;
/// use nom::error::VerboseError;
///
/// assert_eq!(regexp_str("/^(foo)$/;"), Ok((";", "^(foo)$")));
/// ```
pub fn regexp_str<'a>(input: &'a str) -> IResult<&'a str, &'a str> {
    let esc = escaped(none_of("\\/"), '\\', tag("/"));
    let esc_or_empty = alt((esc, tag("")));
    let begin = tag("/");
    let end = context("closing /", tag("/"));
    context("regexp string", delimited(begin, esc_or_empty, end))(input)
}

/// Positive integer (i64)
///
/// ```rust
/// use query2json::parser::parse::int;
///
/// assert_eq!(int("41;"), Ok((";", 41)));
/// ```
pub fn int<'a>(input: &'a str) -> IResult<&'a str, i64> {
    context(
        "integer number",
        map_res(digit1, |digits: &str| i64::from_str_radix(digits, 10)),
    )(input)
}

/// Naive decimal number (f64)
///
/// ```rust
/// use query2json::parser::parse::decimal;
///
/// assert_eq!(decimal("41.5;"), Ok((";", 41.5)));
/// assert_eq!(decimal("0.7;"), Ok((";", 0.7)));
/// ```
pub fn decimal<'a>(input: &'a str) -> IResult<&'a str, f64> {
    context(
        "decimal number",
        map_res(
            recognize(separated_pair(digit1, tag("."), digit1)),
            |digits: &str| f64::from_str(digits),
        ),
    )(input)
}

/// Time units
///
/// ```rust
/// use query2json::parser::parse::time_unit;
/// use query2json::parser::syn::TimeUnit;
///
/// assert_eq!(time_unit("s;"), Ok((";", TimeUnit::S)));
/// assert_eq!(time_unit("m;"), Ok((";", TimeUnit::M)));
/// assert_eq!(time_unit("h;"), Ok((";", TimeUnit::H)));
/// assert_eq!(time_unit("d;"), Ok((";", TimeUnit::D)));
/// ```
pub fn time_unit<'a>(input: &'a str) -> IResult<&'a str, TimeUnit> {
    context(
        "time unit",
        alt((
            value(TimeUnit::S, tag("s")),
            value(TimeUnit::M, tag("m")),
            value(TimeUnit::H, tag("h")),
            value(TimeUnit::D, tag("d")),
        )),
    )(input)
}

/// Duration
///
/// ```rust
/// use query2json::parser::parse::duration;
/// use query2json::parser::syn::TimeUnit;
///
/// assert_eq!(duration("100s;"), Ok((";", (100.0, TimeUnit::S))));
/// assert_eq!(duration("60m;"), Ok((";", (60.0, TimeUnit::M))));
/// assert_eq!(duration("1.5h;"), Ok((";", (1.5, TimeUnit::H))));
/// assert_eq!(duration("2d;"), Ok((";", (2.0, TimeUnit::D))));
pub fn duration<'a>(input: &'a str) -> IResult<&'a str, (f64, TimeUnit)> {
    context(
        "duration",
        pair(alt((decimal, map(int, |i| i as f64))), time_unit),
    )(input)
}

/// Byte units
///
/// ```rust
/// use query2json::parser::parse::byte_unit;
/// use byte_unit::ByteUnit;
///
/// assert_eq!(byte_unit("B;"), Ok((";", ByteUnit::B)));
/// assert_eq!(byte_unit("MB;"), Ok((";", ByteUnit::MB)));
/// assert_eq!(byte_unit("MiB;"), Ok((";", ByteUnit::MiB)));
/// assert_eq!(byte_unit("MiB;"), Ok((";", ByteUnit::MiB)));
/// assert_eq!(byte_unit("GB;"), Ok((";", ByteUnit::GB)));
/// assert_eq!(byte_unit("TB;"), Ok((";", ByteUnit::TB)));
/// assert_eq!(byte_unit("GiB;"), Ok((";", ByteUnit::GiB)));
/// assert_eq!(byte_unit("TiB;"), Ok((";", ByteUnit::TiB)));
/// assert_eq!(byte_unit("PB;"), Ok((";", ByteUnit::PB)));
/// assert_eq!(byte_unit("PiB;"), Ok((";", ByteUnit::PiB)));
/// assert_eq!(byte_unit("EB;"), Ok((";", ByteUnit::EB)));
/// assert_eq!(byte_unit("EiB;"), Ok((";", ByteUnit::EiB)));
/// assert_eq!(byte_unit("ZB;"), Ok((";", ByteUnit::ZB)));
/// assert_eq!(byte_unit("ZiB;"), Ok((";", ByteUnit::ZiB)));
/// ```
pub fn byte_unit<'a>(input: &'a str) -> IResult<&'a str, ByteUnit> {
    context(
        "byte unit",
        alt((
            value(ByteUnit::B, tag("B")),
            value(ByteUnit::KB, tag("KB")),
            value(ByteUnit::KiB, tag("KiB")),
            value(ByteUnit::MB, tag("MB")),
            value(ByteUnit::MiB, tag("MiB")),
            value(ByteUnit::GB, tag("GB")),
            value(ByteUnit::GiB, tag("GiB")),
            value(ByteUnit::TB, tag("TB")),
            value(ByteUnit::TiB, tag("TiB")),
            value(ByteUnit::PB, tag("PB")),
            value(ByteUnit::PiB, tag("PiB")),
            value(ByteUnit::EB, tag("EB")),
            value(ByteUnit::EiB, tag("EiB")),
            value(ByteUnit::ZB, tag("ZB")),
            value(ByteUnit::ZiB, tag("ZiB")),
        )),
    )(input)
}

/// Bytes
///
/// ```rust
/// use query2json::parser::parse::bytes;
/// use byte_unit::{ByteUnit, Byte};
///
/// assert_eq!(bytes("100B;"), Ok((";", (100.0, ByteUnit::B))));
/// assert_eq!(bytes("123KB;"), Ok((";", (123.0, ByteUnit::KB))));
/// assert_eq!(bytes("321KiB;"), Ok((";", (321.0, ByteUnit::KiB))));
/// assert_eq!(bytes("0.5GB;"), Ok((";", (0.5, ByteUnit::GB))));
pub fn bytes<'a>(input: &'a str) -> IResult<&'a str, (f64, ByteUnit)> {
    context(
        "number of bytes",
        map_res(
            pair(alt((decimal, map(int, |i| i as f64))), byte_unit),
            |(value, unit)| -> std::result::Result<(f64, ByteUnit), byte_unit::ByteError> {
                // this is a guard to ensure bytes value can be really constructed
                byte_unit::Byte::from_unit(value, unit)?;
                Ok((value, unit))
            },
        ),
    )(input)
}

/// Boolean
///
/// ```rust
/// use query2json::parser::parse::boolean;
///
/// assert_eq!(boolean("true;"), Ok((";", true)));
/// assert_eq!(boolean("false;"), Ok((";", false)));
/// ```
pub fn boolean<'a>(input: &'a str) -> IResult<&'a str, bool> {
    context(
        "boolean",
        map(alt((tag("true"), tag("false"))), |v: &str| match v {
            "true" => true,
            _ => false,
        }),
    )(input)
}

/// Positive integer (i64)
///
/// ```rust
/// use query2json::parser::parse::null;
///
/// assert_eq!(null("null;"), Ok((";", ())));
/// ```
pub fn null<'a>(input: &'a str) -> IResult<&'a str, ()> {
    context("null", map(tag("null"), |_| ()))(input)
}

/// A combinator that takes a parser `inner` and produces a parser that also consumes both
/// leading and trailing whitespace, returning the output of `inner`.
///
/// ```rust
/// use query2json::parser::parse::ws;
/// use query2json::parser::parse::ident;
///
/// let mut p = ws(ident);
///
/// assert_eq!(p(" foo;"), Ok((";", "foo")));
/// assert_eq!(p("foo  ;"), Ok((";", "foo")));
/// assert_eq!(p(" foo  ;"), Ok((";", "foo")));
/// ```
pub fn ws<'a, O, E: ParseError<&'a str>, F>(
    inner: F,
) -> impl FnMut(&'a str) -> nom::IResult<&'a str, O, E>
where
    F: Parser<&'a str, O, E>,
{
    delimited(multispace0, inner, multispace0)
}

/// value parser
///
/// ```rust
/// use query2json::parser::syn::{ExprValue, TimeUnit};
/// use query2json::parser::parse::value_expr;
/// use byte_unit::{Byte, ByteUnit};
///
/// assert_eq!(value_expr("foo;"), Ok((";", ExprValue::Word("foo"))));
/// assert_eq!(value_expr("true;"), Ok((";", ExprValue::Bool(true))));
/// assert_eq!(value_expr("31;"), Ok((";", ExprValue::Int(31))));
/// assert_eq!(value_expr("'foo bar';"), Ok((";", ExprValue::Str("foo bar"))));
/// assert_eq!(value_expr("32KB;"), Ok((";", ExprValue::Byte{v: 32.0, u: ByteUnit::KB})));
/// assert_eq!(value_expr("1.5h;"), Ok((";", ExprValue::Duration{v: 1.5, u: TimeUnit::H})));
/// assert_eq!(value_expr("null;"), Ok((";", ExprValue::Null)));
/// ```
pub fn value_expr<'a>(input: &'a str) -> IResult<&'a str, ExprValue<'a>> {
    alt((
        map(null, |_| ExprValue::Null),
        map(bytes, |v| ExprValue::Byte { v: v.0, u: v.1 }),
        map(duration, |v| ExprValue::Duration { v: v.0, u: v.1 }),
        map(int, |v| ExprValue::Int(v)),
        map(boolean, |v| ExprValue::Bool(v)),
        map(ident, |v| ExprValue::Word(v)),
        map(string, |v| ExprValue::Str(v)),
    ))(input)
}

/// Vector of values parser
///
/// ```rust
/// use query2json::parser::syn::ExprValue;
/// use query2json::parser::parse::value_vec;
///
/// assert_eq!(value_vec("[foo, 1, true, 'bar bar'];"), Ok((";", vec![
///   ExprValue::Word("foo"),
///   ExprValue::Int(1),
///   ExprValue::Bool(true),
///   ExprValue::Str("bar bar"),
/// ])));
/// ```
pub fn value_vec<'a>(input: &'a str) -> IResult<&'a str, Vec<ExprValue<'a>>> {
    context(
        "array of values",
        delimited(
            ws(tag("[")),
            separated_list1(tag(","), ws(value_expr)),
            ws(context("closing bracket", tag("]"))),
        ),
    )(input)
}

/// Comparison operator parser
///
/// ```rust
/// use query2json::parser::syn::RelOp;
/// use query2json::parser::parse::rel_op;
///
/// assert_eq!(rel_op("=;"), Ok((";", RelOp::Eq)));
/// assert_eq!(rel_op("==;"), Ok((";", RelOp::Eq)));
/// assert_eq!(rel_op("!=;"), Ok((";", RelOp::Ne)));
/// assert_eq!(rel_op(">=;"), Ok((";", RelOp::Ge)));
/// assert_eq!(rel_op("<=;"), Ok((";", RelOp::Le)));
/// assert_eq!(rel_op(">;"), Ok((";", RelOp::Gt)));
/// assert_eq!(rel_op("<;"), Ok((";", RelOp::Lt)));
/// ```
pub fn rel_op(input: &str) -> IResult<&str, RelOp> {
    context(
        "relational operator",
        alt((
            value(RelOp::Eq, tag("==")),
            value(RelOp::Ge, tag(">=")),
            value(RelOp::Le, tag("<=")),
            value(RelOp::Ne, tag("!=")),
            value(RelOp::Eq, tag("=")),
            value(RelOp::Gt, tag(">")),
            value(RelOp::Lt, tag("<")),
        )),
    )(input)
}

/// Compare/relational expression parser
///
/// ```rust
/// use query2json::parser::syn::{RelOp, ExprRel, ExprValue};
/// use query2json::parser::parse::rel_expr;
///
/// assert_eq!(rel_expr("a > 5;"), Ok((";", ExprRel{ op: RelOp::Gt, field: "a", value: ExprValue::Int(5) }.into())));
/// assert_eq!(rel_expr("a != 'foo';"), Ok((";", ExprRel{ op: RelOp::Ne, field: "a", value: ExprValue::Str("foo") }.into())));
/// assert_eq!(rel_expr("foo.bar != 'baz';"), Ok((";", ExprRel{ op: RelOp::Ne, field: "foo.bar", value: ExprValue::Str("baz") }.into())));
/// ```
pub fn rel_expr<'a>(input: &'a str) -> IResult<&'a str, Box<Expr<'a>>> {
    context(
        "relational expression",
        map(
            tuple((ws(ident_path), rel_op, cut(ws(value_expr)))),
            |(field, op, value)| ExprRel { op, field, value }.into(),
        ),
    )(input)
}

/// Includes expression parser
///
/// ```rust
/// use query2json::parser::syn::{ExprInArray, ExprValue};
/// use query2json::parser::parse::in_array_expr;
///
/// assert_eq!(in_array_expr("foo in [bar, 1, false, 'baz'];"), Ok((";", ExprInArray{
///   field: "foo",
///   values: vec![
///     ExprValue::Word("bar"),
///     ExprValue::Int(1),
///     ExprValue::Bool(false),
///     ExprValue::Str("baz"),
///   ],
/// }.into())));
/// ```
pub fn in_array_expr<'a>(input: &'a str) -> IResult<&'a str, Box<Expr<'a>>> {
    context(
        "include expression",
        map(
            separated_pair(ws(ident_path), tag("in"), cut(value_vec)),
            |(field, values)| ExprInArray { field, values }.into(),
        ),
    )(input)
}

/// Includes expression parser
///
/// ```rust
/// use query2json::parser::syn::ExprValue;
/// use query2json::parser::parse::value_range;
///
/// assert_eq!(value_range("1..10;"), Ok((";", (ExprValue::Int(1), ExprValue::Int(10)))));
/// assert_eq!(value_range("'2021-10-01'..'2021-12-01';"), Ok((";", (ExprValue::Str("2021-10-01"), ExprValue::Str("2021-12-01")))));
/// ```
pub fn value_range<'a>(input: &'a str) -> IResult<&'a str, (ExprValue<'a>, ExprValue<'a>)> {
    context(
        "range",
        separated_pair(value_expr, tag(".."), cut(value_expr)),
    )(input)
}

/// In range expression parser
///
/// ```rust
/// use query2json::parser::syn::{ExprInRange, ExprValue};
/// use query2json::parser::parse::in_range_expr;
///
/// assert_eq!(in_range_expr("foo in 1..10;"), Ok((";", ExprInRange { field: "foo", lower: ExprValue::Int(1), upper: ExprValue::Int(10) }.into())));
/// assert_eq!(in_range_expr("foo in '2021-01-01'..'2022-01-01';"), Ok((";", ExprInRange {
///     field: "foo",
///     lower: ExprValue::Str("2021-01-01"),
///     upper: ExprValue::Str("2022-01-01"),
/// }.into())));
/// ```
pub fn in_range_expr<'a>(input: &'a str) -> IResult<&'a str, Box<Expr<'a>>> {
    context(
        "in range expression",
        map(
            separated_pair(ws(ident_path), tag("in"), ws(value_range)),
            |(field, (lower, upper))| {
                ExprInRange {
                    field,
                    lower,
                    upper,
                }
                .into()
            },
        ),
    )(input)
}

/// ```rust
/// use query2json::parser::syn::ExprReg;
/// use query2json::parser::parse::reg_expr;
///
/// assert_eq!(reg_expr("foo ~= /^bar$/;"), Ok((";", ExprReg{ field: "foo", value: "^bar$" }.into())));
/// ```
pub fn reg_expr<'a>(input: &'a str) -> IResult<&'a str, Box<Expr<'a>>> {
    context(
        "regular expression",
        map(
            separated_pair(ws(ident_path), tag("~="), cut(ws(regexp_str))),
            |(field, value)| ExprReg { field, value }.into(),
        ),
    )(input)
}

/// Bool operator parser
///
/// ```rust
/// use query2json::parser::syn::BoolOp;
/// use query2json::parser::parse::bool_op;
///
/// assert_eq!(bool_op("!;"), Ok((";", BoolOp::Ne)));
/// ```
pub fn bool_op(input: &str) -> IResult<&str, BoolOp> {
    value(BoolOp::Ne, tag("!"))(input)
}

/// Boolean unary expression parser
///
/// ```rust
/// use query2json::parser::syn::{ExprBool, BoolOp};
/// use query2json::parser::parse::bool_expr;
///
/// assert_eq!(bool_expr("!foo;"), Ok((";", ExprBool{ op: Some(BoolOp::Ne), field: "foo" }.into())));
/// assert_eq!(bool_expr("  !foo ;"), Ok((";", ExprBool{ op: Some(BoolOp::Ne), field: "foo" }.into())));
/// assert_eq!(bool_expr("foo;"), Ok((";", ExprBool{ op: None, field: "foo"}.into())));
/// ```
pub fn bool_expr<'a>(input: &'a str) -> IResult<&'a str, Box<Expr<'a>>> {
    context(
        "boolean expression",
        ws(map(tuple((opt(bool_op), ident_path)), |(op, field)| {
            ExprBool { op, field }.into()
        })),
    )(input)
}

/// ```rust
/// use query2json::parser::syn::{ExprRel, RelOp, ExprBool, BoolOp, ExprInArray, ExprReg, ExprValue};
/// use query2json::parser::parse::expr;
///
///
/// assert_eq!(expr("a > 5;"), Ok((";", ExprRel{ op: RelOp::Gt, field: "a", value: ExprValue::Int(5) }.into())));
/// assert_eq!(expr("a != 'foo';"), Ok((";", ExprRel{ op: RelOp::Ne, field: "a", value: ExprValue::Str("foo") }.into())));
/// assert_eq!(expr("foo.bar != 'baz';"), Ok((";", ExprRel{ op: RelOp::Ne, field: "foo.bar", value: ExprValue::Str("baz") }.into())));
/// assert_eq!(expr("!foo;"), Ok((";", ExprBool{ op: Some(BoolOp::Ne), field: "foo" }.into())));
/// assert_eq!(expr("  !foo ;"), Ok((";", ExprBool{ op: Some(BoolOp::Ne), field: "foo" }.into())));
/// assert_eq!(expr("foo;"), Ok((";", ExprBool{ op: None, field: "foo"}.into())));
/// assert_eq!(expr("foo ~= /^bar$/;"), Ok((";", ExprReg{ field: "foo", value: "^bar$" }.into())));
/// assert_eq!(expr("foo in [bar, 1, false, 'baz'];"), Ok((";", ExprInArray{
///   field: "foo",
///   values: vec![
///     ExprValue::Word("bar"),
///     ExprValue::Int(1),
///     ExprValue::Bool(false),
///     ExprValue::Str("baz"),
///   ],
/// }.into())));
/// ```
pub fn expr<'a>(input: &'a str) -> IResult<&'a str, Box<Expr<'a>>> {
    alt((rel_expr, in_range_expr, in_array_expr, reg_expr, bool_expr))(input)
}

/// Logic operator (OR and AND) parser
///
/// ```rust
/// use query2json::parser::syn::LogicOp;
/// use query2json::parser::parse::logic_op;
///
/// assert_eq!(logic_op("and;"), Ok((";", LogicOp::And)));
/// assert_eq!(logic_op("or;"), Ok((";", LogicOp::Or)));
/// assert_eq!(logic_op("&&;"), Ok((";", LogicOp::And)));
/// assert_eq!(logic_op("||;"), Ok((";", LogicOp::Or)));
/// ```
pub fn logic_op(input: &str) -> IResult<&str, LogicOp> {
    context(
        "logic operator",
        alt((
            value(LogicOp::And, tag("&&")),
            value(LogicOp::Or, tag("||")),
            value(LogicOp::And, tag("and")),
            value(LogicOp::Or, tag("or")),
        )),
    )(input)
}

/// Result parser without left recursion.
/// See https://github.com/glebec/left-recursion for details
///
/// filter = filter_start, filter_end
/// filter_start = group | expr
/// filter_end = (and | or, filter) | nothing
///
/// ```rust
/// use query2json::parser::syn::{ExprRel, RelOp, ExprBool, BoolOp, ExprInArray, ExprReg, ExprLogic, LogicOp,
/// ExprValue};
/// use query2json::parser::parse::logic_expr;
///
/// assert_eq!(logic_expr("a > 5 and (a <= 3);"), Ok((";", ExprLogic{
///     op: LogicOp::And,
///     exprs: vec![
///         ExprRel{ op: RelOp::Gt, field: "a", value: ExprValue::Int(5)}.into(),
///         ExprRel{ op: RelOp::Le, field: "a", value: ExprValue::Int(3)}.into(),
///     ],
/// }.into())));
///
/// assert_eq!(logic_expr("(a > 5 and a <= 3) or c != foo;"), Ok((";", ExprLogic{
///     op: LogicOp::Or,
///     exprs: vec![
///         ExprLogic{
///             op: LogicOp::And,
///             exprs: vec![
///                 ExprRel{ op: RelOp::Gt, field: "a", value: ExprValue::Int(5) }.into(),
///                 ExprRel{ op: RelOp::Le, field: "a", value: ExprValue::Int(3) }.into(),
///             ],
///         }.into(),
///         ExprRel{ op: RelOp::Ne, field: "c", value: ExprValue::Word("foo") }.into(),
///     ],
/// }.into())));
///
/// assert_eq!(logic_expr("a > 5 and a <= 3 and c != foo and !b;"), Ok((";", ExprLogic{
///     op: LogicOp::And,
///     exprs: vec![
///         ExprRel{ op: RelOp::Gt, field: "a", value: ExprValue::Int(5) }.into(),
///         ExprRel{ op: RelOp::Le, field: "a", value: ExprValue::Int(3) }.into(),
///         ExprRel{ op: RelOp::Ne, field: "c", value: ExprValue::Word("foo") }.into(),
///         ExprBool{ op: Some(BoolOp::Ne), field: "b" }.into(),
///     ],
/// }.into())));
/// ```
pub fn logic_expr<'a>(input: &'a str) -> IResult<&'a str, Box<Expr<'a>>> {
    context(
        "logic expression",
        map(
            tuple((logic_expr_start, logic_expr_end)),
            |(lft, op_rht)| {
                match op_rht {
                    // op is either `and' or `or'
                    //
                    // very simple implementation that do not aggregate and/or branches
                    // Some((op, b)) => ExprLogic{op, exprs: vec![a, b]}.into(),
                    //
                    // more advanced version
                    Some((op, rht)) => {
                        match *lft {
                            Expr::Logic(mut logic_expr) if logic_expr.op == op => {
                                // if `lft` is logic expression and `rht` is joined to it with the same operator
                                // do not create a new logic expression. Just add `rht` to condition list
                                // of `lft`.
                                //
                                // Example: (a > 1 && x != 'foo') && !c => { and: [(a > 1), (x != 'foo'), (!c)] }
                                logic_expr.exprs.push(rht);
                                logic_expr.into()
                            }
                            _ => {
                                match *rht {
                                    // if `rht` is a logic expression and `lft` is joined with it  with the same operator
                                    // do not create a new logic exxpression. Just add `lft` to conditions
                                    // list of `rght`.
                                    //
                                    // Example: a > 1 && (x != 'foo' && !c) => { and: [(a > 1), (x != 'foo'), (!c)] }
                                    Expr::Logic(mut logic_expr) if logic_expr.op == op => {
                                        logic_expr.exprs.insert(0, lft);
                                        logic_expr.into()
                                    }
                                    // otherwise create a new logic expression
                                    _ => ExprLogic {
                                        op,
                                        exprs: vec![lft, rht],
                                    }
                                    .into(),
                                }
                            }
                        }
                    }
                    None => lft,
                }
            },
        ),
    )(input)
}

fn logic_expr_start<'a>(input: &'a str) -> IResult<&'a str, Box<Expr<'a>>> {
    alt((expr_group, expr))(input)
}

fn logic_expr_end<'a>(input: &'a str) -> IResult<&'a str, Option<(LogicOp, Box<Expr<'a>>)>> {
    opt(tuple((logic_op, logic_expr)))(input)
}

fn expr_group<'a>(input: &'a str) -> IResult<&'a str, Box<Expr<'a>>> {
    context(
        "expression group",
        alt((
            expr,
            delimited(
                ws(tag("(")),
                logic_expr,
                context("closing paren", ws(tag(")"))),
            ),
        )),
    )(input)
}

/// ```rust
/// use query2json::parser::syn::Expr;
/// use query2json::parser::parse::expr_empty;
///
/// assert_eq!(expr_empty(""), Ok(("",  Box::new(Expr::Empty))));
/// assert_eq!(expr_empty("  "), Ok(("",  Box::new(Expr::Empty))));
pub fn expr_empty<'a>(input: &'a str) -> IResult<&'a str, Box<Expr<'a>>> {
    value(Box::new(Expr::Empty), ws(eof))(input)
}

/// Parse query string and return expression syntax tree.
///
/// ```rust
/// use query2json::parser::syn::{Expr, ExprLogic, LogicOp, ExprRel, RelOp, ExprBool, BoolOp, ExprValue};
/// use query2json::parser::parse::query;
///
/// assert_eq!(query(""), Ok(("", Box::new(Expr::Empty))));
/// assert_eq!(query("  "), Ok(("", Box::new(Expr::Empty))));
/// assert_eq!(query(" a > 1 and !c "), Ok(("", ExprLogic{
///     op: LogicOp::And,
///     exprs: vec![
///         ExprRel{op: RelOp::Gt, field: "a", value: ExprValue::Int(1)}.into(),
///         ExprBool{op: Some(BoolOp::Ne), field: "c"}.into(),
///     ]
/// }.into())));
pub fn query<'a>(input: &'a str) -> IResult<&'a str, Box<Expr<'a>>> {
    context("query", all_consuming(ws(alt((logic_expr, expr_empty)))))(input)
}

/// Input-aware error.
#[derive(Debug, Clone, PartialEq)]
pub struct Error<I> {
    pub input: I,
    pub cause: nom::Err<nom::error::VerboseError<I>>,
}

/// To convert input aware error into printable stack trace.
impl<I> From<Error<I>> for String
where
    I: Deref<Target = str>,
{
    fn from(e: Error<I>) -> Self {
        match e.cause {
            nom::Err::Error(err) | nom::Err::Failure(err) => convert_error(e.input, err),
            nom::Err::Incomplete(needed) => {
                format!("incomplete, needed: {:?}", needed)
            }
        }
    }
}

impl<I> Error<I>
where
    I: Deref<Target = str>,
{
    pub fn into_stack_trace(self) -> String {
        String::from(self)
    }
}

/// Parses query.
pub fn parse_query<'a>(input: &'a str) -> Result<Expr<'a>, Error<&'a str>> {
    match query(input) {
        Ok((_, expr)) => Ok(*expr),
        Err(cause) => Err(Error { input, cause }),
    }
}
