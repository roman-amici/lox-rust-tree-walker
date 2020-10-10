use super::expression::*;
use super::statement::*;
use super::token::{Token, TokenType};
use std::rc::Rc;

pub struct ParserError {
    token: Token,
    message: String,
}

impl ParserError {
    pub fn to_string(&self) -> String {
        format!(
            "{} at {}: {}",
            self.token.line, self.token.lexeme, self.message
        )
    }
}

pub struct RecursiveDescentParser {
    tokens: Vec<Token>,
    current: usize,
}

impl RecursiveDescentParser {
    pub fn new(tokens: Vec<Token>) -> RecursiveDescentParser {
        RecursiveDescentParser { tokens, current: 0 }
    }

    pub fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    pub fn try_consume(
        &mut self,
        token_type: TokenType,
        err_message: &str,
    ) -> Result<Token, ParserError> {
        if self.is_at_end() {
            Err(ParserError {
                token: self.previous().clone(),
                message: String::from(err_message),
            })
        } else {
            let token = self.peek().clone();
            if token.token_type == token_type {
                self.advance();
                Ok(token)
            } else {
                self.advance();
                Err(ParserError {
                    token: token,
                    message: String::from(err_message),
                })
            }
        }
    }

    pub fn check_type(&mut self, token_type: &TokenType) -> bool {
        if self.current < self.tokens.len() && self.peek().token_type == *token_type {
            true
        } else {
            false
        }
    }

    pub fn is_at_end(&self) -> bool {
        self.current >= self.tokens.len() || self.peek().token_type == TokenType::EOF
    }

    pub fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }

    pub fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
            self.previous()
        } else {
            self.previous()
        }
    }

    pub fn match_token(&mut self, tokens_to_match: &[TokenType]) -> bool {
        for type_to_check in tokens_to_match.iter() {
            if self.check_type(type_to_check) {
                self.advance();
                return true;
            }
        }

        false
    }

    pub fn primary(&mut self) -> Result<Expr, ParserError> {
        let token = self.peek();
        let expr = match token.token_type {
            TokenType::False => Ok(Expr::Literal(Literal::False)),
            TokenType::True => Ok(Expr::Literal(Literal::True)),
            TokenType::Nil => Ok(Expr::Literal(Literal::Nil)),
            TokenType::NumberToken => {
                let number: f64 = token.literal.as_ref().unwrap().parse().unwrap();
                Ok(Expr::Literal(Literal::Number(number)))
            }
            TokenType::StringToken => Ok(Expr::Literal(Literal::StringLiteral(
                token.literal.as_ref().unwrap().clone(),
            ))),
            TokenType::LeftParen => {
                self.advance();
                let expr = self.expression()?;
                self.try_consume(TokenType::RightParen, "Expected )")?;
                return Ok(expr); //Don't advance
            }
            TokenType::Identifier => Ok(Expr::Variable(token.clone())),
            _ => Err(ParserError {
                token: token.clone(),
                message: String::from("Unexpected Token"),
            }),
        };
        self.advance();
        expr
    }

    pub fn finish_call(&mut self, callee: Expr) -> Result<Expr, ParserError> {
        let mut arguments = vec![];

        if !self.check_type(&TokenType::RightParen) {
            arguments.push(self.expression()?); //No do while construct in Rust
            while self.match_token(&[TokenType::Comma]) {
                arguments.push(self.expression()?);
            }
        }

        let opening_paren =
            self.try_consume(TokenType::RightParen, "Expected ')' after arguments.")?;

        Ok(Expr::Call(Call {
            callee: Box::new(callee),
            arguments: arguments,
            opening_paren,
        }))
    }

    pub fn call(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.primary()?;

        loop {
            if self.match_token(&[TokenType::LeftParen]) {
                expr = self.finish_call(expr)?;
            } else {
                break;
            }
        }

        Ok(expr)
    }

    pub fn unary(&mut self) -> Result<Expr, ParserError> {
        if self.match_token(&[TokenType::Bang, TokenType::Minus]) {
            let operator = self.previous().clone();
            let right = self.unary()?;
            Ok(Expr::Unary(Unary {
                right: Box::new(right),
                operator: operator,
            }))
        } else {
            self.call()
        }
    }

    pub fn multiplication(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.unary()?;

        while self.match_token(&[TokenType::Slash, TokenType::Star]) {
            let operator = self.previous().clone();
            let right = self.unary()?;
            expr = Expr::Binary(Binary {
                left: Box::new(expr),
                right: Box::new(right),
                operator: operator,
            });
        }

        Ok(expr)
    }

    pub fn addition(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.multiplication()?;

        while self.match_token(&[TokenType::Minus, TokenType::Plus]) {
            let operator = self.previous().clone();
            let right = self.multiplication()?;
            expr = Expr::Binary(Binary {
                left: Box::new(expr),
                right: Box::new(right),
                operator: operator,
            })
        }

        Ok(expr)
    }

    pub fn comparison(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.addition()?;

        let to_match = [
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ];
        while self.match_token(&to_match) {
            let operator = self.previous().clone();
            let right = self.addition()?;
            expr = Expr::Binary(Binary {
                left: Box::new(expr),
                right: Box::new(right),
                operator: operator,
            });
        }

        Ok(expr)
    }

    pub fn equality(&mut self) -> Result<Expr, ParserError> {
        //Ensures we match an equality operator or anything of *higher* precedence
        let mut expr = self.comparison()?;

        while self.match_token(&[TokenType::BangEqual, TokenType::EqualEqual]) {
            let operator = self.previous().clone();
            let right = self.comparison()?;

            expr = Expr::Binary(Binary {
                left: Box::new(expr),
                right: Box::new(right),
                operator: operator,
            });
        }
        Ok(expr)
    }

    pub fn and(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.equality()?;

        while self.match_token(&[TokenType::And]) {
            let operator = self.previous().clone();
            let right = self.equality()?;
            expr = Expr::Logical(Binary {
                left: Box::new(expr),
                right: Box::new(right),
                operator: operator,
            });
        }

        Ok(expr)
    }

    pub fn or(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.and()?;

        while self.match_token(&[TokenType::Or]) {
            let operator = self.previous().clone();
            let right = self.and()?;
            expr = Expr::Logical(Binary {
                left: Box::new(expr),
                right: Box::new(right),
                operator: operator,
            });
        }

        Ok(expr)
    }

    pub fn assignment(&mut self) -> Result<Expr, ParserError> {
        let expr = self.or()?;

        if self.match_token(&[TokenType::Equal]) {
            let equals = self.previous().clone();
            let value = self.assignment()?;

            if let Expr::Variable(var_token) = expr {
                Ok(Expr::Assign(Assign {
                    token: var_token.clone(),
                    value: Box::new(value),
                }))
            } else {
                Err(ParserError {
                    token: equals,
                    message: String::from("Invalid assignment target."),
                })
            }
        } else {
            Ok(expr)
        }
    }

    //A Rule for each precedence level
    pub fn expression(&mut self) -> Result<Expr, ParserError> {
        self.assignment()
    }

    pub fn print_statement(&mut self) -> Result<Statement, ParserError> {
        let expr = self.expression()?;
        self.try_consume(TokenType::Semicolon, "Expected ; after expression")?;

        Ok(Statement::Print(expr))
    }

    pub fn expression_statement(&mut self) -> Result<Statement, ParserError> {
        let expr = self.expression()?;

        self.try_consume(TokenType::Semicolon, "Expected ; after expression")?;
        Ok(Statement::Expression(expr))
    }

    pub fn block_statement(&mut self) -> Result<Statement, ParserError> {
        let mut inner_statements: Vec<Statement> = vec![];

        while !self.is_at_end() && self.peek().token_type != TokenType::RightBrace {
            let decl = self.declaration()?;
            inner_statements.push(decl);
        }

        self.try_consume(TokenType::RightBrace, "Expected '}' at end of block")?;

        Ok(Statement::Block(inner_statements))
    }

    pub fn if_statement(&mut self) -> Result<Statement, ParserError> {
        self.try_consume(TokenType::LeftParen, "Expected '(' after 'if'.")?;
        let condition = self.expression()?;
        self.try_consume(TokenType::RightParen, "Expected ')' after 'if' condition")?;

        let then_branch = Box::new(self.statement()?);
        let else_branch = if self.match_token(&[TokenType::Else]) {
            Some(Box::new(self.statement()?))
        } else {
            None
        };

        Ok(Statement::If(IfStatement {
            condition,
            then_branch,
            else_branch,
        }))
    }

    pub fn while_statement(&mut self) -> Result<Statement, ParserError> {
        self.try_consume(TokenType::LeftParen, "Expected '(' after 'while' ")?;
        let condition = self.expression()?;
        self.try_consume(TokenType::RightParen, "Expected ')' after condition")?;
        let body = self.statement()?;

        Ok(Statement::While(WhileStatement {
            condition,
            body: Box::new(body),
        }))
    }

    pub fn for_statement(&mut self) -> Result<Statement, ParserError> {
        self.try_consume(TokenType::LeftParen, "Expected '(' after 'for'.")?;
        let initializer = if self.match_token(&[TokenType::Semicolon]) {
            None
        } else if self.match_token(&[TokenType::Var]) {
            Some(self.var_declaration()?)
        } else {
            Some(self.expression_statement()?)
        };

        let condition = if !self.check_type(&TokenType::Semicolon) {
            Some(self.expression()?)
        } else {
            None
        };
        self.try_consume(TokenType::Semicolon, "Expected ';' after loop condition")?;

        let increment = if !self.check_type(&TokenType::RightParen) {
            Some(self.expression()?)
        } else {
            None
        };
        self.try_consume(TokenType::RightParen, "Expected ')' after for clause.")?;

        let body = self.statement()?;

        //We desugar in the opposite order that we parsed in
        let body = if let Some(increment_expr) = increment {
            Statement::Block(vec![body, Statement::Expression(increment_expr)])
        } else {
            body
        };

        let condition = if let Some(cond_expr) = condition {
            cond_expr
        } else {
            Expr::Literal(Literal::True)
        };

        let body = Statement::While(WhileStatement {
            condition,
            body: Box::new(body),
        });

        let body = if let Some(init_stmt) = initializer {
            Statement::Block(vec![init_stmt, body])
        } else {
            body
        };

        Ok(body)
    }

    fn return_statement(&mut self) -> Result<Statement, ParserError> {
        let keyword = self.previous().clone();

        let value = if !self.check_type(&TokenType::Semicolon) {
            Some(self.expression()?)
        } else {
            None
        };

        self.try_consume(TokenType::Semicolon, "Expected ';' after return value")?;

        Ok(Statement::Return(ReturnStatement {
            token: keyword,
            value,
        }))
    }

    pub fn statement(&mut self) -> Result<Statement, ParserError> {
        if self.match_token(&[TokenType::For]) {
            self.for_statement()
        } else if self.match_token(&[TokenType::If]) {
            self.if_statement()
        } else if self.match_token(&[TokenType::Print]) {
            self.print_statement()
        } else if self.match_token(&[TokenType::Return]) {
            self.return_statement()
        } else if self.match_token(&[TokenType::While]) {
            self.while_statement()
        } else if self.match_token(&[TokenType::LeftBrace]) {
            self.block_statement()
        } else {
            self.expression_statement()
        }
    }

    pub fn synchronize(&mut self) {
        while !self.is_at_end() {
            if self.previous().token_type == TokenType::Semicolon {
                return;
            }
            match self.peek().token_type {
                TokenType::Class
                | TokenType::Fun
                | TokenType::Var
                | TokenType::For
                | TokenType::If
                | TokenType::While
                | TokenType::Print
                | TokenType::Return => {
                    return;
                }
                _ => {
                    self.advance();
                }
            }
        }
    }

    pub fn var_declaration(&mut self) -> Result<Statement, ParserError> {
        let token = self.try_consume(TokenType::Identifier, "Expected an identifier")?;

        let initializer = if self.match_token(&[TokenType::Equal]) {
            Some(self.expression()?)
        } else {
            None
        };

        self.try_consume(
            TokenType::Semicolon,
            "Expected ';' after a variable declaration",
        )?;

        Ok(Statement::Var(VarStatement {
            name: token.clone(),
            initializer,
        }))
    }

    fn function(&mut self) -> Result<Statement, ParserError> {
        let name = self.try_consume(TokenType::Identifier, "Expected function/method name.")?;

        self.try_consume(
            TokenType::LeftParen,
            "Expected '(' after function/method name.",
        )?;

        let mut params: Vec<Token> = vec![];
        if !self.check_type(&TokenType::RightParen) {
            params.push(self.try_consume(TokenType::Identifier, "Expected parameter name.")?);
            while self.match_token(&[TokenType::Comma]) {
                params.push(self.try_consume(TokenType::Identifier, "Expected parameter name.")?);
            }
        }

        self.try_consume(TokenType::RightParen, "Expected ')' after parameters")?;
        self.try_consume(TokenType::LeftBrace, "Expected '{' before function body")?;

        let body = self.block_statement()?;

        if let Statement::Block(body) = body {
            Ok(Statement::Function(Rc::new(FnStatement {
                name,
                params,
                body,
            })))
        } else {
            panic!("block_statement did not return Block Statement.")
        }
    }

    pub fn declaration(&mut self) -> Result<Statement, ParserError> {
        if self.match_token(&[TokenType::Fun]) {
            self.function()
        } else if self.match_token(&[TokenType::Var]) {
            self.var_declaration()
        } else {
            self.statement()
        }
    }

    pub fn parse(&mut self) -> Vec<Statement> {
        let mut statements: Vec<Statement> = vec![];

        while !self.is_at_end() {
            match self.declaration() {
                Ok(stmt) => statements.push(stmt),
                Err(error) => {
                    println!("{}", error.to_string());
                    self.synchronize();
                }
            }
        }
        statements
    }
}

pub fn parse_tokens(tokens: Vec<Token>) -> Vec<Statement> {
    let mut parser = RecursiveDescentParser::new(tokens);

    parser.parse()
}
