#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Token {
    Illegal,
    EOF,
    // 識別子・リテラル
    IDENT,
    INT,

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
