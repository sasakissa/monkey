use std::ops::Add;

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub enum Token {
    Illegal,
    EOF,
    // 識別子・リテラル
    IDENT(String),
    INT(i32),

    // 演算子
    ASSIGN,
    PLUS,
    MINUS,
    BANG,
    ASTERISK,
    SLASH,
    LT,
    GT,
    EQ,
    NOT_EQ,

    // デリミタ
    COMMA,
    SEMICOLON,

    LPAREN,
    RPAREN,
    LBRRACE,
    RBRACE,

    // キーワード
    FUNCTION,
    LET,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN,
}

impl Token {
    pub fn literal(&self) -> String {
        let literal = match self {
            Token::IDENT(i) => i,
            Token::INT(i) => "int",
            Token::ASSIGN => "=",
            Token::PLUS => "+",
            Token::MINUS => "-",
            Token::BANG => "!",
            Token::ASTERISK => "*",
            Token::SLASH => "/",
            Token::LT => ">",
            Token::GT => "<",
            Token::EQ => "==",
            Token::NOT_EQ => "!=",
            Token::COMMA => ",",
            Token::SEMICOLON => ";",

            Token::LPAREN => "(",
            Token::RPAREN => ")",
            Token::LBRRACE => "{",
            Token::RBRACE => "}",
            Token::FUNCTION => "fn",
            Token::LET => "let",
            Token::TRUE => "true",
            Token::FALSE => "false",
            Token::IF => "if",
            Token::ELSE => "else",
            Token::RETURN => "return",
            _ => "",
        };
        return literal.to_string();
    }
}
