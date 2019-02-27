use lalrpop_util::lalrpop_mod;

mod ast;

lalrpop_mod! {
    #[clippy::all]
    pub grammar
}

/// Custom parsing errors
#[derive(Debug, PartialEq)]
pub enum Error {}

pub type TErrorRecovery<'input> =
    lalrpop_util::ErrorRecovery<usize, crate::token::Token<'input>, Error>;
pub type TParseError<'input> = lalrpop_util::ParseError<usize, crate::token::Token<'input>, Error>;

#[derive(Debug, Clone, PartialEq)]
pub enum Number {
    Integer(i64),
    Float(f64),
}

impl From<i64> for Number {
    fn from(x: i64) -> Number {
        Number::Integer(x)
    }
}

impl From<f64> for Number {
    fn from(x: f64) -> Number {
        Number::Float(x)
    }
}

// #[derive(Debug, Clone, PartialEq)]
// pub struct Param {
//     name: String,
//     ty: Option<String>,
//     keyword: bool,
// }
