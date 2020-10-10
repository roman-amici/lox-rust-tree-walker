use super::expression::*;

pub struct RPNVisitor {
    string_builder: Vec<String>,
}

impl RPNVisitor {
    pub fn new() -> RPNVisitor {
        RPNVisitor {
            string_builder: vec![],
        }
    }

    pub fn format(&mut self, root: &Expr) -> String {
        self.dispatch_expr(root);

        self.string_builder.join(" ")
    }

    fn lparen(&mut self) {
        self.string_builder.push(String::from("("));
    }

    fn rparen(&mut self) {
        self.string_builder.push(String::from(")"));
    }

    fn visit_unary(&mut self, expr: &Unary) {
        self.lparen();
        self.string_builder.push(expr.operator.lexeme.clone());
        self.dispatch_expr(expr.right.as_ref());
        self.rparen();
    }

    fn visit_binary(&mut self, expr: &Binary) {
        self.lparen();
        self.string_builder.push(expr.operator.lexeme.clone());
        self.dispatch_expr(expr.left.as_ref());
        self.dispatch_expr(expr.right.as_ref());
        self.rparen();
    }

    fn visit_grouping(&mut self, expr: &Grouping) {
        self.lparen();
        self.dispatch_expr(expr.inner.as_ref());
        self.rparen();
    }

    fn dispatch_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Unary(unary) => self.visit_unary(unary),
            Expr::Binary(binary) => self.visit_binary(binary),
            Expr::Grouping(grouping) => self.visit_grouping(grouping),
            Expr::Literal(inner_literal) => match inner_literal {
                Literal::Number(n) => self.string_builder.push(n.to_string()),
                Literal::StringLiteral(s) => self.string_builder.push(s.clone()),
                Literal::True => self.string_builder.push(String::from("true")),
                Literal::False => self.string_builder.push(String::from("false")),
                Literal::Nil => self.string_builder.push(String::from("nil")),
            },
            _ => unimplemented!(),
        }
    }
}
