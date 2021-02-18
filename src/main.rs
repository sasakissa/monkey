mod lexer;
mod repl;
mod token;
use std::io;

pub use lexer::Lexer;
pub use token::Token;
fn main() {
    println!("Hello! This is the Monkey programming language!");
    println!("Feel free to type in commends!");
    repl::start(&mut io::stdin(), &mut io::stdout());
}
