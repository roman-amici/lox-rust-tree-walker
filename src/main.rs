use std::env;
use std::error;
use std::fs;
use std::io;
use std::io::Write;
use std::process;

mod environment;
mod expression;
mod interpreter;
mod parser;
mod scanner;
mod statement;
mod token;
mod visitor;

fn main() {
    let args: Vec<String> = env::args().collect();
    match args.len() {
        1 => run_prompt(),
        2 => run_file(&args[1]).unwrap(),
        _ => println!("Usage: [script]"),
    }
    process::exit(64);
}

fn run_file(filename: &str) -> Result<(), Box<dyn error::Error + 'static>> {
    let mut interpreter = interpreter::Interpreter::new();
    let file_contents = fs::read_to_string(filename)?;
    run(&file_contents, &mut interpreter);
    Ok(())
}

fn run_prompt() {
    let mut interpreter = interpreter::Interpreter::new();
    let mut buffer = String::new();
    loop {
        print!("> ");
        io::stdout().flush().unwrap();
        if let Ok(_) = io::stdin().read_line(&mut buffer) {
            run(&buffer, &mut interpreter);
        } else {
            return;
        }
        buffer.clear();
    }
}

fn run(source: &String, interpreter: &mut interpreter::Interpreter) {
    let tokens = scanner::scan_tokens(source).unwrap();
    let statements = parser::parse_tokens(tokens);

    for statement in statements.iter() {
        interpreter.interpret(statement);
    }
}
