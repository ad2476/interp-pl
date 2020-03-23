use std::fmt;
use std::fs::File;
use std::io::prelude::*;
use std::io::{self, Write};
use std::path::{Path, PathBuf};

#[allow(dead_code)]
mod interp;

enum RunError {
    IoError(PathBuf, std::io::Error),
    EvalError(String),
}

impl From<interp::Error> for RunError {
    fn from(other: interp::Error) -> Self {
        RunError::EvalError(other.to_string())
    }
}

impl fmt::Display for RunError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            RunError::IoError(fname, io_err) => {
                write!(f, "Couldn't process {}: {}.", fname.display(), io_err)
            }
            RunError::EvalError(e) => write!(f, "{}", e),
        }
    }
}

fn help() {
    println!("Usage: interp [script]");
}

fn eval(source: String) -> Result<(), RunError> {
    let mut scanner = interp::Scanner::new();
    let tokens = scanner.scan(&source).map_err(|errors| {
        RunError::EvalError(
            errors
                .iter()
                .fold(String::from(""), |acc, e| acc + &e.to_string() + "\n"),
        )
    })?;
    println!("{:?}", tokens);

    let ast = interp::Parser::parse(&tokens)?;
    println!("{:#?}", ast);

    Ok(())
}

fn run_file(file_path: &Path) -> Result<(), RunError> {
    let err_closure = |e| RunError::IoError(file_path.to_path_buf(), e);
    let mut file = File::open(file_path).map_err(err_closure)?;

    let mut contents = String::new();
    file.read_to_string(&mut contents).map_err(err_closure)?;

    eval(contents)
}

fn run_repl() -> Result<(), RunError> {
    println!("REPL start. Enter EOF (CTRL-D) to exit.");

    loop {
        print!("> ");
        io::stdout().flush().unwrap();

        let mut input = String::new();
        match io::stdin().read_line(&mut input) {
            Ok(0) => return Ok(()),
            Err(e) => return Err(RunError::IoError(PathBuf::from("REPL"), e)),
            _ => (),
        }

        match eval(input) {
            Err(e) => println!("{}", e),
            _ => {}
        };
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();

    match args.len() {
        1 => {
            if let Err(e) = run_repl() {
                println!("{}", e)
            }
        }
        2 => {
            let script_name = &args[1];
            match &script_name[..] {
                "--help" => help(),
                _ => {
                    if let Err(e) = run_file(Path::new(&script_name)) {
                        println!("{}", e)
                    }
                }
            }
        }
        _ => {
            help();
        }
    }
}
