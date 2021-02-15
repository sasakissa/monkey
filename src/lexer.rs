use std::str::Chars;

use crate::token::Token;

pub struct Lexer<'a> {
    input: Chars<'a>,
    cur: char,
    peek: char,
}
impl<'a> From<&'a str> for Lexer<'a> {
    fn from(input: &'a str) -> Self {
        let mut lexer = Self {
            input: input.chars(),
            cur: '\u{0}',
            peek: '\u{0}',
        };
        lexer.read_char();
        lexer.read_char();
        lexer
    }
}

impl<'a> Lexer<'a> {
    // 1文字読み進める
    fn read_char(&mut self) -> char {
        let c = self.cur;
        self.cur = self.peek;
        self.peek = self.input.next().unwrap_or('\u{0}');
        c
    }
    // 次のトークンを返す
    fn next_token(&mut self) -> Token {
        let c = self.read_char();
        let token = match c {
            '=' => Token::ASSIGN,
            ';' => Token::SEMICOLON,
            '(' => Token::LPAREN,
            ')' => Token::RPAREN,
            ',' => Token::COMMA,
            '+' => Token::PLUS,
            '{' => Token::LBRRACE,
            '}' => Token::RBRACE,
            '\u{0}' => Token::EOF,
            _ => Token::EOF,
        };
        token
    }
}
#[cfg(test)]
mod tests {
    use crate::token::Token;

    use super::Lexer;

    #[test]
    fn test_next_token() {
        let input = "=+(){},;";
        let tests = vec![
            Token::ASSIGN,
            Token::PLUS,
            Token::LPAREN,
            Token::RPAREN,
            Token::LBRRACE,
            Token::RBRACE,
            Token::COMMA,
            Token::SEMICOLON,
            Token::EOF,
        ];
        let mut lexer = Lexer::from(input);
        for (_, &test) in tests.iter().enumerate() {
            let token = lexer.next_token();
            println!("{:?}", token);
            assert_eq!(token, test);
        }
    }
}
