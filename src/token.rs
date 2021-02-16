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
}
