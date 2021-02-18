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

    // 空白文字を読み飛ばす
    fn skip_whitespace(&mut self) {
        while self.cur == ' ' || self.cur == '\t' || self.cur == '\n' || self.cur == '\r' {
            self.read_char();
        }
    }

    fn read_identifier(&mut self) -> String {
        let mut res = String::new();
        while is_letter(self.cur) {
            let c = self.read_char();
            res.push(c);
        }

        return res;
    }
    // 次のトークンを返す
    fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        let token = match self.cur {
            '=' => Token::ASSIGN,
            ';' => Token::SEMICOLON,
            '(' => Token::LPAREN,
            ')' => Token::RPAREN,
            ',' => Token::COMMA,
            '+' => Token::PLUS,
            '{' => Token::LBRRACE,
            '}' => Token::RBRACE,
            '\u{0}' => Token::EOF,
            _ => {
                if is_letter(self.cur) {
                    let literal = self.read_identifier();
                    let token = lookup_ident(&literal);
                    token
                } else {
                    Token::Illegal
                }
            }
        };
        token
    }
}

fn is_letter(c: char) -> bool {
    return 'a' <= c && c <= 'z' || 'A' <= c && c <= 'Z' || c == '_';
}
fn keywords(keyword: &String) -> Option<Token> {
    match keyword.as_ref() {
        "fn" => Some(Token::FUNCTION),
        "let" => Some(Token::LET),
        _ => None,
    }
}
fn lookup_ident(ident: &String) -> Token {
    if let Some(token) = keywords(ident) {
        token
    } else {
        Token::IDENT(ident.clone())
    }
}
#[cfg(test)]
mod tests {
    use crate::token::Token;

    use super::Lexer;

    #[test]
    fn test_next_token() {
        let input = "let five = 5;
        let ten = 10;
        let add = fn(x, y) {
         x + y;
        };
        let result = add(five, ten);";
        let tests = vec![
            Token::LET,
            Token::IDENT("five".to_string()),
            Token::ASSIGN,
            Token::INT(5),
            Token::SEMICOLON,
            Token::LET,
            Token::IDENT("ten".to_string()),
            Token::ASSIGN,
            Token::INT(10),
            Token::LET,
            Token::IDENT("add".to_string()),
            Token::ASSIGN,
            Token::FUNCTION,
            Token::LPAREN,
            Token::IDENT("x".to_string()),
            Token::COMMA,
            Token::IDENT("y".to_string()),
            Token::RPAREN,
            Token::LBRRACE,
            Token::IDENT("x".to_string()),
            Token::PLUS,
            Token::IDENT("y".to_string()),
            Token::SEMICOLON,
            Token::RBRACE,
            Token::SEMICOLON,
            Token::LET,
            Token::IDENT("result".to_string()),
            Token::ASSIGN,
            Token::IDENT("five".to_string()),
            Token::COMMA,
            Token::IDENT("ten".to_string()),
            Token::RPAREN,
            Token::SEMICOLON,
            Token::EOF,
        ];
        let mut lexer = Lexer::from(input);
        for (_, test) in tests.into_iter().enumerate() {
            let token = lexer.next_token();
            println!("{:?}", token);
            assert_eq!(token, test);
        }
    }
}
