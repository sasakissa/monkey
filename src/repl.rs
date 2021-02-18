use crate::Lexer;
use crate::Token;
use std::io::{Read, Write};
const PROMPT: &str = ">> ";
pub fn start(reader: &mut Read, writer: &mut Write) {
    let mut scanner = std::io::BufReader::new(reader);
    eprint!("{}", PROMPT);
    loop {
        let mut buf = String::new();
        scanner.read_to_string(&mut buf);
        let mut lexer = Lexer::from(buf.as_ref());
        loop {
            let token = lexer.next_token();
            if token == Token::EOF {
                break;
            }
            eprint!("{:?}\n", token);
        }
        eprint!("{}", PROMPT);
    }
}
