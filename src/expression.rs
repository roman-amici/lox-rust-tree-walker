use super::token::Token;

pub struct Assign {
    pub token: Token,
    pub value: Box<Expr>,
}

pub struct Unary {
    pub right: Box<Expr>,
    pub operator: Token,
}

pub struct Binary {
    pub left: Box<Expr>,
    pub right: Box<Expr>,
    pub operator: Token,
}

pub struct Grouping {
    pub inner: Box<Expr>,
}

pub struct Call {
    pub callee: Box<Expr>,
    pub arguments: Vec<Expr>,
    pub opening_paren: Token,
}

pub enum Literal {
    Number(f64),
    StringLiteral(String),
    True,
    False,
    Nil,
}

pub enum Expr {
    Assign(Assign),
    Unary(Unary),
    Binary(Binary),
    Logical(Binary),
    Grouping(Grouping),
    Variable(Token),
    Literal(Literal),
    Call(Call),
}
