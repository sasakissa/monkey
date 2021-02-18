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

    // 次の文字を読む
    fn peek_char(&self) -> char {
        self.peek
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
    // 現在位置のliterarlを読み込んでintを返す
    fn read_number(&mut self) -> i32 {
        let mut res = String::new();
        while self.cur.is_ascii_digit() {
            let c = self.read_char();
            res += c.to_string().as_ref();
        }
        res.parse::<i32>().unwrap()
    }
    // 次のトークンを返すｓ
    fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        println!("{}", self.cur);
        let token = match self.cur {
            '=' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    self.read_char();
                    Token::EQ
                } else {
                    self.read_char();
                    Token::ASSIGN
                }
            }
            '+' => {
                self.read_char();
                Token::PLUS
            }
            '-' => {
                self.read_char();
                Token::MINUS
            }
            '!' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    self.read_char();
                    Token::NOT_EQ
                } else {
                    self.read_char();
                    Token::BANG
                }
            }
            '*' => {
                self.read_char();
                Token::ASTERISK
            }
            '<' => {
                self.read_char();
                Token::LT
            }
            '>' => {
                self.read_char();
                Token::GT
            }
            ';' => {
                self.read_char();
                Token::SEMICOLON
            }
            '(' => {
                self.read_char();
                Token::LPAREN
            }
            ')' => {
                self.read_char();
                Token::RPAREN
            }
            ',' => {
                self.read_char();
                Token::COMMA
            }
            '/' => {
                self.read_char();
                Token::SLASH
            }
            '{' => {
                self.read_char();
                Token::LBRRACE
            }
            '}' => {
                self.read_char();
                Token::RBRACE
            }
            '\u{0}' => Token::EOF,
            _ => {
                if is_letter(self.cur) {
                    let literal = self.read_identifier();
                    let token = lookup_ident(&literal);
                    return token;
                } else if self.cur.is_ascii_digit() {
                    let num = self.read_number();
                    let token = Token::INT(num);
                    return token;
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
        "true" => Some(Token::TRUE),
        "false" => Some(Token::FALSE),
        "if" => Some(Token::IF),
        "else" => Some(Token::ELSE),
        "return" => Some(Token::RETURN),
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
        let result = add(five, ten);
        !-/*5;
        5 < 10 > 5;
        if (5 < 10) {
            return true;
           } else {
            return false;
           }
        10 == 10;
        10 != 9;
        ";
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
            Token::SEMICOLON,
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
            Token::IDENT("add".to_string()),
            Token::LPAREN,
            Token::IDENT("five".to_string()),
            Token::COMMA,
            Token::IDENT("ten".to_string()),
            Token::RPAREN,
            Token::SEMICOLON,
            Token::BANG,
            Token::MINUS,
            Token::SLASH,
            Token::ASTERISK,
            Token::INT(5),
            Token::SEMICOLON,
            Token::INT(5),
            Token::LT,
            Token::INT(10),
            Token::GT,
            Token::INT(5),
            Token::SEMICOLON,
            Token::IF,
            Token::LPAREN,
            Token::INT(5),
            Token::LT,
            Token::INT(10),
            Token::RPAREN,
            Token::LBRRACE,
            Token::RETURN,
            Token::TRUE,
            Token::SEMICOLON,
            Token::RBRACE,
            Token::ELSE,
            Token::LBRRACE,
            Token::RETURN,
            Token::FALSE,
            Token::SEMICOLON,
            Token::RBRACE,
            Token::INT(10),
            Token::EQ,
            Token::INT(10),
            Token::SEMICOLON,
            Token::INT(10),
            Token::NOT_EQ,
            Token::INT(9),
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
