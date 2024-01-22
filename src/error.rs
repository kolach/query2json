use thiserror::Error;

#[derive(Error, Debug, PartialEq)]
pub enum Error {
    #[error("{0}")]
    Parse(String),

    #[error("failed to write query: `{0}`")]
    Format(#[from] std::fmt::Error),
}

impl<'a> From<crate::parser::Error<&'a str>> for Error {
    fn from(e: crate::parser::Error<&'a str>) -> Self {
        Self::Parse(e.into_stack_trace())
    }
}
