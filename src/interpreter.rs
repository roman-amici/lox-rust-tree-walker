use super::environment::Environment;
use super::expression::*;
use super::statement::*;
use super::token::{Token, TokenType};
use std::cell::RefCell;
use std::collections::HashMap;
use std::mem::swap;
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};

pub enum Unwind {
    RTE(RuntimeError),
    Return(Value),
}

impl From<RuntimeError> for Unwind {
    fn from(rte: RuntimeError) -> Self {
        Unwind::RTE(rte)
    }
}

pub struct RuntimeError {
    pub token: Token,
    pub message: String,
}

impl RuntimeError {
    pub fn to_string(&self) -> String {
        format!("{}: Type Error: {}", self.token.line, self.message)
    }

    pub fn uncallable(token: &Token) -> RuntimeError {
        RuntimeError {
            token: token.clone(),
            message: String::from("Can only call functions and classes."),
        }
    }

    pub fn arity_error(token: &Token, n_args: usize, arity: usize) -> RuntimeError {
        RuntimeError {
            token: token.clone(),
            message: format!("Expected {} arguments but got {}.", n_args, arity),
        }
    }
}

pub trait Callable {
    fn arity(&self) -> usize;

    fn call(
        &self,
        interpreter: &mut Interpreter,
        arguments: Vec<Value>,
    ) -> Result<Value, RuntimeError>;

    fn to_string(&self) -> String;
}

pub struct NativeFunctionTime {}

impl Callable for NativeFunctionTime {
    fn arity(&self) -> usize {
        0
    }

    fn call(
        &self,
        _interpreter: &mut Interpreter,
        _arguments: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let now = SystemTime::now();

        if let Ok(time_unix) = now.duration_since(UNIX_EPOCH) {
            Ok(Value::Number(time_unix.as_secs() as f64))
        } else {
            //Todo, turn into runtime error
            panic!();
        }
    }

    fn to_string(&self) -> String {
        String::from("<function global>")
    }
}

struct LoxFunction {
    statement: Rc<FnStatement>,
    closure: Vec<u64>,
}

impl Callable for LoxFunction {
    fn arity(&self) -> usize {
        self.statement.params.len()
    }

    fn call(
        &self,
        interpreter: &mut Interpreter,
        arguments: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let mut function_environment = Environment::new();

        let mut arguments = arguments; //Take ownership of the values using drain
        for (param_idx, param) in arguments.drain(..).enumerate() {
            let addr = interpreter.add_to_heap(param);
            function_environment.add(&self.statement.params[param_idx], addr);
        }

        //Set up the current environments on the interpreter while caching the old one.
        let mut prev_environments = vec![];
        swap(
            &mut interpreter.current_environments,
            &mut prev_environments,
        );

        interpreter.current_environments.push(function_environment);

        let result = interpreter.block_statement_try(&self.statement.body);
        interpreter.current_environments = prev_environments;

        match result {
            Ok(value) => Ok(value),
            Err(Unwind::Return(value)) => Ok(value),
            Err(Unwind::RTE(rte)) => Err(rte),
        }
    }

    fn to_string(&self) -> String {
        format!("<fn {}>", self.statement.name.lexeme)
    }
}

#[derive(Clone)]
pub enum Value {
    Number(f64),
    StringValue(String),
    Bool(bool),
    Pointer(u64),
    Callable(Rc<dyn Callable>),
    Nil,
}

pub struct Interpreter {
    heap: HashMap<u64, Value>,
    heap_addr: u64,
    global_environment: Environment,
    current_environments: Vec<Environment>,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        let mut intp = Interpreter {
            heap: HashMap::new(),
            heap_addr: 0,
            global_environment: Environment::new(),
            current_environments: vec![],
        };

        intp.setup_global_environment();
        intp
    }

    pub fn next_addr(&mut self) -> u64 {
        let addr = self.heap_addr;
        self.heap_addr += 1;
        addr
    }

    pub fn add_to_heap(&mut self, value: Value) -> u64 {
        let addr = self.next_addr();
        self.heap.insert(addr, value);
        addr
    }

    pub fn setup_global_environment(&mut self) {
        let addr = self.add_to_heap(Value::Callable(Rc::new(NativeFunctionTime {})));
        self.global_environment.declare("time", addr);
    }

    fn to_truthy(&self, v: &Value) -> bool {
        match v {
            Value::Bool(b) => *b,
            Value::Nil => false,
            _ => true,
        }
    }

    fn get_variable(&self, token: &Token) -> Result<Value, RuntimeError> {
        for environment in self.current_environments.iter().rev() {
            if let Some(ptr) = environment.get(token) {
                if let Some(value) = self.heap.get(&ptr) {
                    return Ok(value.clone());
                }
            }
        }
        //Bottom out at the global environment
        if let Some(ptr) = self.global_environment.get(token) {
            if let Some(value) = self.heap.get(&ptr) {
                return Ok(value.clone());
            }
        }

        Err(RuntimeError {
            token: token.clone(),
            message: format!("{} is undefined", token.lexeme),
        })
    }

    fn assign_variable(&mut self, token: &Token, value: Value) -> Result<Value, RuntimeError> {
        let ptr = self.add_to_heap(value.clone());
        for environment in self.current_environments.iter_mut().rev() {
            if let Some(ptr) = environment.assign(token, ptr) {
                return Ok(value);
            }
        }
        //Bottom out at the global environment
        if let Some(ptr) = self.global_environment.assign(token, ptr) {
            Ok(value)
        } else {
            Err(RuntimeError {
                token: token.clone(),
                message: format!("Assignment to undefined variable {}", token.lexeme),
            })
        }
    }

    fn visit_logical(&mut self, b: &Binary) -> Result<Value, RuntimeError> {
        let left = self.dispatch_expr(b.left.as_ref())?;

        if b.operator.token_type == TokenType::Or {
            if self.to_truthy(&left) {
                return Ok(left);
            }
        } else {
            //Token type And
            if !self.to_truthy(&left) {
                return Ok(left);
            }
        }

        return self.dispatch_expr(b.right.as_ref());
    }

    fn visit_unary(&mut self, u: &Unary) -> Result<Value, RuntimeError> {
        let val = self.dispatch_expr(u.right.as_ref())?;
        match u.operator.token_type {
            TokenType::Minus => {
                if let Value::Number(n) = val {
                    Ok(Value::Number(-n))
                } else {
                    Err(RuntimeError {
                        token: u.operator.clone(),
                        message: String::from("Operand must be a number"),
                    })
                }
            }
            TokenType::Bang => {
                let b = self.to_truthy(&val);
                Ok(Value::Bool(!b))
            }
            _ => unimplemented!(),
        }
    }

    fn number_type_error(&self, token: &Token) -> Result<Value, RuntimeError> {
        Err(RuntimeError {
            token: token.clone(),
            message: String::from("Both operands must be numbers"),
        })
    }

    fn is_equal(&self, left: Value, right: Value) -> Result<Value, RuntimeError> {
        match (left, right) {
            (Value::Number(n1), Value::Number(n2)) => Ok(Value::Bool(n1 == n2)),
            (Value::StringValue(s1), Value::StringValue(s2)) => Ok(Value::Bool(s1 == s2)),
            (Value::Bool(b1), Value::Bool(b2)) => Ok(Value::Bool(b1 == b2)),
            (Value::Nil, Value::Nil) => Ok(Value::Bool(true)),
            _ => Ok(Value::Bool(false)), //I think this is how lox works...
        }
    }

    fn visit_binary(&mut self, binary: &Binary) -> Result<Value, RuntimeError> {
        let left = self.dispatch_expr(binary.left.as_ref())?;
        let right = self.dispatch_expr(binary.right.as_ref())?;

        match binary.operator.token_type {
            TokenType::Minus => match (left, right) {
                (Value::Number(n1), Value::Number(n2)) => Ok(Value::Number(n1 - n2)),
                _ => self.number_type_error(&binary.operator),
            },
            TokenType::Plus => match (left, right) {
                (Value::StringValue(s1), Value::StringValue(s2)) => {
                    Ok(Value::StringValue(format!("{}{}", s1, s2)))
                }
                (Value::Number(n1), Value::Number(n2)) => Ok(Value::Number(n1 + n2)),
                _ => Err(RuntimeError {
                    token: binary.operator.clone(),
                    message: String::from("Both operands must be either strings or numbers"),
                }),
            },
            TokenType::Slash => match (left, right) {
                (Value::Number(n1), Value::Number(n2)) => Ok(Value::Number(n1 / n2)),
                _ => self.number_type_error(&binary.operator),
            },
            TokenType::Star => match (left, right) {
                (Value::Number(n1), Value::Number(n2)) => Ok(Value::Number(n1 * n2)),
                _ => self.number_type_error(&binary.operator),
            },
            TokenType::Greater => match (left, right) {
                (Value::Number(n1), Value::Number(n2)) => Ok(Value::Bool(n1 > n2)),
                _ => self.number_type_error(&binary.operator),
            },
            TokenType::GreaterEqual => match (left, right) {
                (Value::Number(n1), Value::Number(n2)) => Ok(Value::Bool(n1 >= n2)),
                _ => self.number_type_error(&binary.operator),
            },
            TokenType::Less => match (left, right) {
                (Value::Number(n1), Value::Number(n2)) => Ok(Value::Bool(n1 < n2)),
                _ => self.number_type_error(&binary.operator),
            },
            TokenType::LessEqual => match (left, right) {
                (Value::Number(n1), Value::Number(n2)) => Ok(Value::Bool(n1 <= n2)),
                _ => self.number_type_error(&binary.operator),
            },
            TokenType::EqualEqual => self.is_equal(left, right),
            TokenType::BangEqual => {
                let equality_check = self.is_equal(left, right);
                if let Ok(Value::Bool(b)) = equality_check {
                    Ok(Value::Bool(!b))
                } else {
                    equality_check
                }
            }
            _ => unimplemented!(),
        }
    }

    fn visit_assign(&mut self, assign_expr: &Assign) -> Result<Value, RuntimeError> {
        let value = self.dispatch_expr(assign_expr.value.as_ref())?;
        self.assign_variable(&assign_expr.token, value)
    }

    fn try_call(
        &mut self,
        callee: Value,
        arguments: Vec<Value>,
        token: &Token,
    ) -> Result<Value, RuntimeError> {
        match callee {
            Value::Callable(callable_ref) => {
                if callable_ref.arity() != arguments.len() {
                    Err(RuntimeError::arity_error(
                        token,
                        arguments.len(),
                        callable_ref.arity(),
                    ))
                } else {
                    callable_ref.call(self, arguments)
                }
            }
            _ => Err(RuntimeError::uncallable(token)),
        }
    }

    fn visit_call(&mut self, call_expr: &Call) -> Result<Value, RuntimeError> {
        let callee = self.dispatch_expr(&call_expr.callee)?;

        let mut argument_values: Vec<Value> = vec![];
        for argument in call_expr.arguments.iter() {
            argument_values.push(self.dispatch_expr(argument)?);
        }

        self.try_call(callee, argument_values, &call_expr.opening_paren)
    }

    fn dispatch_expr(&mut self, expr: &Expr) -> Result<Value, RuntimeError> {
        match expr {
            Expr::Unary(unary) => self.visit_unary(unary),
            Expr::Binary(binary) => self.visit_binary(binary),
            Expr::Logical(binary) => self.visit_logical(binary),
            Expr::Grouping(grouping) => self.dispatch_expr(&grouping.inner),
            Expr::Literal(inner_literal) => match inner_literal {
                Literal::Number(n) => Ok(Value::Number(*n)),
                Literal::StringLiteral(s) => Ok(Value::StringValue(s.clone())),
                Literal::True => Ok(Value::Bool(true)),
                Literal::False => Ok(Value::Bool(false)),
                Literal::Nil => Ok(Value::Nil),
            },
            Expr::Variable(token) => self.get_variable(token),
            Expr::Assign(assign_expr) => self.visit_assign(&assign_expr),
            Expr::Call(call_expr) => self.visit_call(&call_expr),
            _ => unimplemented!(),
        }
    }

    fn print_value(&self, value: &Value) {
        match value {
            Value::Number(n) => println!("{}", n),
            Value::Bool(b) => println!("{}", b),
            Value::StringValue(s) => println!("{}", s),
            Value::Nil => println!("nil"),
            _ => panic!(),
        }
    }

    fn print_statement(&mut self, expr: &Expr) -> Result<Value, Unwind> {
        let val = self.dispatch_expr(expr)?;
        self.print_value(&val);
        Ok(Value::Nil)
    }

    fn expression_statement(&mut self, expr: &Expr) -> Result<Value, Unwind> {
        self.dispatch_expr(expr)?;
        Ok(Value::Nil)
    }

    fn var_statement(&mut self, var: &VarStatement) -> Result<Value, Unwind> {
        let value = if let Some(expr) = &var.initializer {
            self.dispatch_expr(expr)?
        } else {
            Value::Nil
        };
        let addr = self.add_to_heap(value);
        if let Some(environment) = self.current_environments.last_mut() {
            environment.add(&var.name, addr);
        } else {
            self.global_environment.add(&var.name, addr);
        }
        Ok(Value::Nil)
    }

    fn block_statement_try(&mut self, statements: &Vec<Statement>) -> Result<Value, Unwind> {
        //Like a try, catch with the calling function serving as the "finally" to cleanup environments
        for statement in statements.iter() {
            self.dispatch_statement(statement)?;
        }

        Ok(Value::Nil)
    }

    fn block_statement(&mut self, statements: &Vec<Statement>) -> Result<Value, Unwind> {
        self.current_environments.push(Environment::new());
        let result = self.block_statement_try(statements);
        self.current_environments.pop();
        result
    }

    fn if_statement(&mut self, if_statement: &IfStatement) -> Result<Value, Unwind> {
        let value = self.dispatch_expr(&if_statement.condition)?;

        if self.to_truthy(&value) {
            self.dispatch_statement(if_statement.then_branch.as_ref())
        } else if let Some(else_stmt) = &if_statement.else_branch {
            self.dispatch_statement(else_stmt.as_ref())
        } else {
            Ok(Value::Nil)
        }
    }

    fn while_statement(&mut self, while_stmt: &WhileStatement) -> Result<Value, Unwind> {
        loop {
            //Use a loop to deal with difficulties of borrowing.
            let value = self.dispatch_expr(&while_stmt.condition)?;
            if self.to_truthy(&value) {
                self.dispatch_statement(while_stmt.body.as_ref())?;
            } else {
                break;
            }
        }

        Ok(Value::Nil)
    }

    fn function_statement(&mut self, fn_stmt: &Rc<FnStatement>) -> Result<Value, Unwind> {
        let function = LoxFunction {
            statement: fn_stmt.clone(),
            closure: Vec::new(),
        };

        let addr = self.add_to_heap(Value::Callable(Rc::new(function)));
        if let Some(environment) = self.current_environments.last_mut() {
            environment.add(&fn_stmt.name, addr);
        } else {
            self.global_environment.add(&fn_stmt.name, addr);
        }

        Ok(Value::Nil)
    }

    fn return_statement(&mut self, return_stmt: &ReturnStatement) -> Result<Value, Unwind> {
        let value = if let Some(expr) = &return_stmt.value {
            self.dispatch_expr(expr)?
        } else {
            Value::Nil
        };

        Err(Unwind::Return(value)) //This ensures we short-circut returns
    }

    fn dispatch_statement(&mut self, stmt: &Statement) -> Result<Value, Unwind> {
        match stmt {
            Statement::Expression(expr) => self.expression_statement(expr),
            Statement::Print(expr) => self.print_statement(expr),
            Statement::Var(stmt) => self.var_statement(stmt),
            Statement::Block(statements) => self.block_statement(statements),
            Statement::If(if_stmt) => self.if_statement(if_stmt),
            Statement::While(while_stmt) => self.while_statement(while_stmt),
            Statement::Function(fn_stmt) => self.function_statement(fn_stmt),
            Statement::Return(return_stmt) => self.return_statement(return_stmt),
        }
    }

    pub fn interpret(&mut self, statement: &Statement) {
        match self.dispatch_statement(statement) {
            Ok(_) => (),
            Err(err) => match err {
                Unwind::Return(_) => panic!("Uncaught return value"),
                Unwind::RTE(rte) => println!("{}", rte.to_string()),
            },
        }
    }
}
