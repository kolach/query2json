//! Converts humand readable query text into filter JSON understanded by backend microservices.
//! Here is a few examples of query text and corresponding JSON filter:
//!
//! - `a = 1` -> `{"a":1}`
//! - `a > 1` -> `{"a":{"gt":1}}`
//! - `a in [1, 2, 3]` -> `{"a":{"inq":[1,2,3]}}`
//! - `a in 1..2` -> `{"a":{"between":[1,2]}}`
//! - `a ~= /^test$/` -> `{"a":{"regexp":"^test$"}}`
//! - `a > 1 or b < 2` -> `{"or":[{"a":{"gt":1}},{"b":{"lt":2}}]}`
//! - `(a > 1 or b < 2) and !online` -> `{"and":[{"or":[{"a":{"gt":1}},{"b":{"lt":2}}]},{"online":false}]}`
//! - `a > 10KB` -> `{"a":{"gt":10000}}`
//! - `a in '2019-01-01'..'2019-12-31'` -> `{"a":{"between":["2019-01-01","2019-12-31"]}}`

pub mod error;
pub mod json;
pub mod parser;
pub mod where_filter;

/// Convert query text to JSON string
///
/// ```rust
/// use query2json::query2json;
///
/// assert_eq!(query2json("a = 1"), Ok(r#"{"a":1}"#.to_string()));
/// assert_eq!(query2json("a > 1"), Ok(r#"{"a":{"gt":1}}"#.to_string()));
/// assert_eq!(query2json("size > 10KB"), Ok(r#"{"size":{"gt":10000}}"#.to_string()));
/// assert_eq!(query2json("t < 1h"), Ok(r#"{"t":{"lt":3600000}}"#.to_string()));
/// assert_eq!(query2json("t in 5m..10m"), Ok(r#"{"t":{"between":[300000,600000]}}"#.to_string()));
/// assert_eq!(query2json("(a > 1 or b < 2) and !online"), Ok(r#"{"and":[{"or":[{"a":{"gt":1}},{"b":{"lt":2}}]},{"online":false}]}"#.to_string()));
/// ```
pub fn query2json(query: &str) -> Result<String, error::Error> {
    // make AST from query text
    let ast = parser::parse_query(query)?;
    // convert query AST to where filter format and serialize as JSON string
    let out = where_filter::serialize_as_where(&ast)?;
    Ok(out)
}
