pub struct Error;

#[derive(Debug, Clone)]
pub struct Program(pub Vec<Expression>);

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    String(String),
    Char(char),
    Int(i64),
    Float(f64),
}

#[derive(Debug, Clone)]
pub enum Expression {
    Literal(Literal),
    Ident(String),
    FunctionDef {
        name: String,
        params: Vec<Expression>,
        body: Vec<Expression>,
    },
    FunctionCall {
        ident: String,
        args: Vec<Expression>,
    },
}

// pub struct Program(Vec<>)
