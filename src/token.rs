#[derive(Debug, PartialEq, Clone)]
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
