use super::expression::Expr;
use super::token::Token;
use std::rc::Rc;

pub struct VarStatement {
    pub name: Token,
    pub initializer: Option<Expr>,
}

pub struct IfStatement {
    pub condition: Expr,
    pub then_branch: Box<Statement>,
    pub else_branch: Option<Box<Statement>>,
}

pub struct WhileStatement {
    pub condition: Expr,
    pub body: Box<Statement>,
}

pub struct FnStatement {
    pub name: Token,
    pub params: Vec<Token>,
    pub body: Vec<Statement>,
}

pub struct ReturnStatement {
    pub token: Token,
    pub value: Option<Expr>,
}

pub enum Statement {
    Expression(Expr),
    Print(Expr),
    Var(VarStatement),
    Block(Vec<Statement>),
    If(IfStatement),
    While(WhileStatement),
    Function(Rc<FnStatement>), //We need this so that we can take ownership later in the parser
    Return(ReturnStatement),
}
