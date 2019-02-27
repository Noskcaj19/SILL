use pest::{error::Error as PestError, Parser};
use std::str::FromStr;

#[derive(pest_derive::Parser)]
#[grammar = "token/tokens.pest"]
struct Tokenize;

#[derive(Debug)]
pub enum Error<'input> {
    PestErr(PestError<Rule>),
    InvalidInteger,
    InvalidHex,
    InvalidBinary,
    InvalidFloat,
    InvalidEscape,
    UnknownKeyword,
    UnknownGlyph,
    UnknownSymbol(&'input str),

    UnknownErr,
}

#[derive(Debug, Clone)]
pub enum Token<'input> {
    Ident(&'input str),
    StringLit(&'input str),
    Char(char),
    Integer(i64),
    Float(f64),

    Let,
    Func,
    Struct,
    Enum,
    Type,

    // Glyphs
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    LAngleBracket,
    RAngleBracket,

    Arrow,
    DoubleColon,
    Colon,
    Semicolon,
    Comma,
    Equal,
    Question,
    Bang,
    Tilde,
    At,
    Hash,
    Dollar,
    Percent,
    Carrot,
    Ampersand,
    Star,
    Plus,
    Minus,
    Slash,
    BackSlash,
    Pipe,
    Underscore,
}

/// A span holding the start and end of a token
#[derive(Clone, Copy)]
pub struct Span {
    pub start: u16,
    pub end: u16,
}

impl std::fmt::Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}-{}", self.start, self.end)
    }
}

impl<'a> From<pest::Span<'a>> for Span {
    fn from(pest_span: pest::Span) -> Self {
        Span {
            start: pest_span.start() as u16,
            end: pest_span.end() as u16,
        }
    }
}

#[derive(Debug, Clone)]
pub struct SpannedToken<'a>(pub Token<'a>, pub Span);

impl<'a> SpannedToken<'a> {
    pub fn new<S: Into<Span>>(token: Token<'a>, span: S) -> Self {
        SpannedToken(token, span.into())
    }
}

#[derive(Debug)]
pub struct SpannedError<'a>(pub Error<'a>, pub Option<Span>);

impl<'a> SpannedError<'a> {
    pub fn spanned<S: Into<Span>>(err: Error, span: S) -> SpannedError {
        SpannedError(err, Some(span.into()))
    }

    pub fn spanless(err: Error) -> SpannedError {
        SpannedError(err, None)
    }
}

/// Tokenizes a string into a stream of tokens
///
/// On failure, a tuple with the error and all successfully parsed tokens are
/// returned
pub fn tokenize(input: &str) -> Result<Vec<SpannedToken>, (SpannedError, Vec<SpannedToken>)> {
    let mut token_list = match Tokenize::parse(Rule::token_list, input) {
        Ok(token_list) => token_list,
        Err(e) => return Err((SpannedError::spanless(Error::PestErr(e)), vec![])),
    };
    let tokens = match token_list.next() {
        Some(tokens) => tokens.into_inner(),
        None => return Err((SpannedError::spanless(Error::UnknownErr), vec![])),
    };

    let mut output_tokens = Vec::new();

    for token in tokens {
        let tok = match token.as_rule() {
            Rule::ident => SpannedToken::new(Token::Ident(token.as_str()), token.as_span()),
            Rule::string => {
                let str = token.as_str();

                SpannedToken::new(Token::StringLit(&str[1..str.len() - 1]), token.as_span())
            }
            Rule::char => {
                let str = token.as_str();
                let str = &str[1..str.len() - 1];

                let final_char = match str {
                    r"\n" => '\n',
                    r"\\" => '\\',
                    _ if str.len() == 1 => str.chars().next().unwrap(),
                    _ => {
                        return Err((
                            SpannedError::spanned(Error::InvalidEscape, token.as_span()),
                            output_tokens,
                        ));
                    }
                };

                SpannedToken::new(Token::Char(final_char), token.as_span())
            }
            Rule::integer => {
                let stripped_int = token.as_str().replace('_', "");
                let int_token = match token.into_inner().next() {
                    Some(tok) => tok,
                    None => panic!("Seems to be an invalid state!"),
                };

                let int = match int_token.as_rule() {
                    Rule::decimal_int => match stripped_int.parse() {
                        Ok(i) => i,
                        Err(_) => {
                            println!("{:#?}", stripped_int);
                            return Err((
                                SpannedError::spanned(Error::InvalidInteger, int_token.as_span()),
                                output_tokens,
                            ));
                        }
                    },
                    Rule::hex_int => {
                        let stripped_int = if let Some('x') = stripped_int.chars().skip(1).next() {
                            &stripped_int[2..]
                        } else {
                            &stripped_int[..stripped_int.len() - 1]
                        };
                        match i64::from_str_radix(stripped_int, 16) {
                            Ok(int) => int,
                            Err(_) => {
                                return Err((
                                    SpannedError::spanned(Error::InvalidHex, int_token.as_span()),
                                    output_tokens,
                                ));
                            }
                        }
                    }
                    Rule::binary_int => {
                        let stripped_int = &stripped_int[2..];
                        match i64::from_str_radix(stripped_int, 2) {
                            Ok(int) => int,
                            Err(_) => {
                                return Err((
                                    SpannedError::spanned(
                                        Error::InvalidBinary,
                                        int_token.as_span(),
                                    ),
                                    output_tokens,
                                ));
                            }
                        }
                    }
                    _ => unreachable!(),
                };
                let mult = if detect_negative(&output_tokens) {
                    // Remove the negative sign from the generated tokens
                    output_tokens.remove(output_tokens.len() - 1);
                    -1
                } else {
                    1
                };
                let tok = Token::Integer(int * mult);
                SpannedToken::new(tok, int_token.as_span())
            }
            Rule::float => {
                let stripped_float = token.as_str().replace('_', "");
                let float = match f64::from_str(&stripped_float) {
                    Ok(i) => i,
                    Err(_) => {
                        return Err((
                            SpannedError::spanned(Error::InvalidFloat, token.as_span()),
                            output_tokens,
                        ));
                    }
                };
                let mult = if detect_negative(&output_tokens) {
                    // Remove the negative sign from the generated tokens
                    output_tokens.remove(output_tokens.len() - 1);
                    -1.
                } else {
                    1.
                };
                let tok = Token::Float(float * mult);
                SpannedToken::new(tok, token.as_span())
            }
            Rule::keyword => {
                let tok = match token.as_str() {
                    "let" => Token::Let,
                    "func" => Token::Func,
                    "struct" => Token::Struct,
                    "enum" => Token::Enum,
                    "type" => Token::Type,
                    _ => {
                        return Err((
                            SpannedError::spanned(Error::UnknownKeyword, token.as_span()),
                            output_tokens,
                        ));
                    }
                };
                SpannedToken::new(tok, token.as_span())
            }
            Rule::glyph => {
                use Token::*;
                let tok = match token.as_str() {
                    "(" => LParen,
                    ")" => RParen,
                    "[" => LBracket,
                    "]" => RBracket,
                    "{" => LBrace,
                    "}" => RBrace,
                    "<" => LAngleBracket,
                    ">" => RAngleBracket,
                    "->" => Arrow,
                    "::" => DoubleColon,
                    ":" => Colon,
                    ";" => Semicolon,
                    "," => Comma,
                    "=" => Equal,
                    "?" => Question,
                    "!" => Bang,
                    "~" => Tilde,
                    "@" => At,
                    "#" => Hash,
                    "$" => Dollar,
                    "%" => Percent,
                    "^" => Carrot,
                    "&" => Ampersand,
                    "*" => Star,
                    "+" => Plus,
                    "-" => Minus,
                    "/" => Slash,
                    "\\" => BackSlash,
                    "|" => Pipe,
                    "_" => Underscore,

                    _ => {
                        return Err((
                            SpannedError::spanned(Error::UnknownGlyph, token.as_span()),
                            output_tokens,
                        ));
                    }
                };
                SpannedToken::new(tok, token.as_span())
            }
            Rule::EOI => break,

            Rule::line_comment => continue,

            // Impossible (silent rules)
            Rule::token
            | Rule::ident_char
            | Rule::ident_start
            | Rule::digit
            | Rule::double_quote_string
            | Rule::raw_string
            | Rule::raw_string_interior
            | Rule::str_inner
            | Rule::str_char
            | Rule::char_inner
            | Rule::number
            | Rule::hex_digit
            | Rule::hex_int
            | Rule::binary_int
            | Rule::decimal_int
            | Rule::token_list
            | Rule::WHITESPACE
            | Rule::COMMENT => unreachable!(),
        };
        output_tokens.push(tok);
    }
    Ok(output_tokens)
}

/// Decide if a set of tokens is a negative number
///
/// For example, the tokens Integer, Minus, Integer is subtraction, and
/// does not include a negative number
/// However the set of  tokens Minus, Integer is a single negative number
///
/// This function first checks if the previous token is a Minus token.
/// If it is, then we check the token before that, if it is numeric, we assume
/// it is a binary op, not a unary op, otherwise, it is most likely a unary op
fn detect_negative(prev_tokens: &[SpannedToken]) -> bool {
    let mut negative = false;
    // Is it previous token a negative sign?
    if let Some(i) = prev_tokens.len().checked_sub(1) {
        if let Some(SpannedToken(Token::Minus, _)) = prev_tokens.get(i) {
            negative = true;
            // Is the number before the negative sign a number?
            // If it is, we can assume that it is subtraction
            if let Some(i) = prev_tokens.len().checked_sub(2) {
                if let Some(token) = prev_tokens.get(i) {
                    match token {
                        SpannedToken(Token::Integer(_), _) | SpannedToken(Token::Float(_), _) => {
                            // It is subtaction, do not negate
                            negative = false;
                        }
                        _ => {}
                    }
                }
            }
        }
    }
    negative
}

// pub fn token_printer(tokens: &[Token]) -> String {
//     let mut output = String::new();

//     for token in tokens {
//         use Token::*;
//         let str = match token {
//             Ident(ident) => ident.to_string(),
//             StringLit(str) => format!("r#\"{}#\"", str.to_string()),
//             Integer(i) => i.to_string(),
//             Float(f) => f.to_string(),

//             Operator(op) => op.to_string(),

//             Let => "let".into(),
//             Func => "func".into(),
//             Struct => "struct".into(),
//             Enum => "enum".into(),
//             Type => "type".into(),

//             // Glyphs
//             LParen => "(".into(),
//             RParen => ")".into(),
//             LBracket => "[".into(),
//             RBracket => "]".into(),
//             LBrace => "{".into(),
//             RBrace => "}".into(),
//             LAngleBracket => "<".into(),
//             RAngleBracket => ">".into(),

//             Arrow => "->".into(),
//             DoubleColon => "::".into(),
//             Colon => ":".into(),
//             Semicolon => ";".into(),
//             Comma => ",".into(),
//             Equal => "=".into(),
//             Question => "?".into(),
//             Bang => "!".into(),
//             Tilde => "~".into(),
//             At => "@".into(),
//             Hash => "#".into(),
//             Dollar => "$".into(),
//             Percent => "%".into(),
//             Carrot => "^".into(),
//             Ampersand => "&".into(),
//             Star => "*".into(),
//             Plus => "+".into(),
//             Minus => "-".into(),
//             Slash => "/".into(),
//             BackSlash => "\\".into(),
//             Pipe => "|".into(),
//             Underscore => "_".into(),
//         };
//         output.push_str(&str);
//         output.push(' ');
//     }
//     output
// }

#[cfg(test)]
mod tests {
    const SAMPLE: &'static str = include_str!("../../../sample.sill");

    #[test]
    fn file() {
        assert!(super::tokenize(sample).is_ok());
    }
}
