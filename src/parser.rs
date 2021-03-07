use std::{collections::HashMap, error::Error};

use crate::ast::Node;
use crate::{Expression, Lexer, Program, Statement, Token};

const LOWEST: u8 = 1;
const EQUALS: u8 = 2;
const LESSGREATER: u8 = 3;
const SUM: u8 = 4;
const PRODUCT: u8 = 5;
const PREFIX: u8 = 6;
const CALL: u8 = 7;

fn precedences(token: &Token) -> u8 {
    match token {
        Token::EQ => EQUALS,
        Token::NOT_EQ => EQUALS,
        Token::LT => LESSGREATER,
        Token::GT => LESSGREATER,
        Token::PLUS => SUM,
        Token::MINUS => SUM,
        Token::SLASH => PRODUCT,
        Token::ASTERISK => PRODUCT,
        _ => LOWEST,
    }
}

#[derive(Debug)]
struct Parser<'a> {
    l: Lexer<'a>,
    cur_token: Token,
    peek_token: Token,
    errors: Vec<String>,
}

impl<'a> Parser<'a> {
    fn new(l: Lexer<'a>) -> Self {
        let mut p = Self {
            l,
            cur_token: Token::EOF,
            peek_token: Token::EOF,
            errors: vec![],
        };
        p.next_token();
        p.next_token();
        return p;
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.l.next_token();
    }

    fn parse_program(&mut self) -> Option<Program> {
        let mut statements = vec![];
        while self.cur_token != Token::EOF {
            let statement = self.parse_statement();
            if let Some(statement) = statement {
                statements.push(statement);
            }
            self.next_token();
        }
        return Some(Program { statements });
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.cur_token {
            Token::LET => {
                return self.parse_let_statement();
            }
            Token::RETURN => {
                return self.parse_return_statement();
            }
            _ => self.parse_expression_statement(),
        }
    }

    // let文をパースする
    fn parse_let_statement(&mut self) -> Option<Statement> {
        if let Token::IDENT(name) = &self.peek_token {
            let ident = Expression::Identifiler(name.clone());
            self.next_token();
            if !self.expect_peek(&Token::ASSIGN) {
                return None;
            }
            // TODO: セミコロンに遭遇するまで式を読み飛ばしている
            while !self.cur_token_is(Token::SEMICOLON) {
                self.next_token();
            }
            let value = Expression::String("".to_string());
            return Some(Statement::Let {
                ident,
                value: value,
            });
        } else {
            return None;
        }
    }

    // returnb文をパースする
    fn parse_return_statement(&mut self) -> Option<Statement> {
        self.next_token();
        // TODO: セミコロンに遭遇するまで式を読み飛ばしてしまっている
        while !self.cur_token_is(Token::SEMICOLON) {
            self.next_token();
        }
        return Some(Statement::Return {
            value: Expression::String("".to_string()),
        });
    }

    fn cur_token_is(&self, expected_token: Token) -> bool {
        return self.cur_token == expected_token;
    }

    fn peek_token_is(&self, expected_token: &Token) -> bool {
        return self.peek_token == *expected_token;
    }

    fn expect_peek(&mut self, expected_token: &Token) -> bool {
        if self.peek_token_is(expected_token) {
            self.next_token();
            return true;
        } else {
            self.peek_error(expected_token);
            return false;
        }
    }

    // 式文を評価します
    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let expression = self.parse_expression(LOWEST).unwrap();
        let statement = Statement::Expression { value: expression };
        if self.peek_token_is(&Token::SEMICOLON) {
            self.next_token();
        }
        return Some(statement);
    }

    // 式を評価します
    fn parse_expression(&mut self, precedence: u8) -> Option<Expression> {
        let c_token = &self.cur_token.clone();
        let mut left_exp = match c_token {
            Token::IDENT(ident) => self.parse_identifier(),
            Token::INT(i) => self.parse_integer_literarl(),
            Token::BANG => self.parse_prefix_expression(),
            Token::MINUS => self.parse_prefix_expression(),
            _ => {
                self.no_prefix_parse_fn_error(c_token);
                None
            }
        }?;

        while !self.peek_token_is(&Token::SEMICOLON) && precedence < self.peek_precedence() {
            match &self.peek_token {
                Token::PLUS
                | Token::MINUS
                | Token::SLASH
                | Token::ASTERISK
                | Token::EQ
                | Token::NOT_EQ
                | Token::LT
                | Token::GT => {
                    self.next_token();
                    left_exp = self.parse_infix_expression(left_exp).unwrap();
                }
                _ => return Some(left_exp),
            }
        }
        return Some(left_exp);
    }

    fn parse_identifier(&self) -> Option<Expression> {
        let c_token = &self.cur_token;
        if let Token::IDENT(ident) = c_token {
            return Some(Expression::Identifiler(ident.clone()));
        } else {
            return None;
        }
    }

    fn parse_integer_literarl(&self) -> Option<Expression> {
        let c_token = &self.cur_token;
        if let Token::INT(i) = c_token {
            Some(Expression::Integer(*i))
        } else {
            None
        }
    }

    fn parse_prefix_expression(&mut self) -> Option<Expression> {
        let c_token = &self.cur_token.clone();

        self.next_token();
        let right = self.parse_expression(PREFIX).unwrap();
        let prefix_expression = Expression::Prefix {
            operator: c_token.literal(),
            right: Box::new(right),
        };

        return Some(prefix_expression);
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Option<Expression> {
        let c_token = &self.cur_token.clone();
        let precedence = self.cur_precedence();
        self.next_token();
        let right = self.parse_expression(precedence).unwrap();
        Some(Expression::Infix {
            left: Box::new(left),
            operator: c_token.literal(),
            right: Box::new(right),
        })
    }

    fn errors(&self) -> Vec<String> {
        self.errors.clone()
    }

    fn peek_error(&mut self, expected_token: &Token) {
        let msg = format!(
            "expected next token to be {:?}, got {:?} instead, ",
            expected_token, self.peek_token
        );
        self.errors.push(msg);
    }

    fn peek_precedence(&self) -> u8 {
        let precedence = precedences(&self.peek_token);
        return precedence;
    }

    fn cur_precedence(&self) -> u8 {
        let precedence = precedences(&self.cur_token);
        return precedence;
    }

    fn no_prefix_parse_fn_error(&mut self, token: &Token) {
        let msg = format!("no prefix parse function for {:?} found.", token);
        self.errors.push(msg);
    }
}

#[cfg(test)]
mod tests {
    use core::panic;

    use crate::{Expression, Lexer, Node, Statement};

    use super::Parser;

    #[test]
    fn test_let_statements() {
        let input = "let x = 5;
        let y = 10;
        let foobar = 838383;
        ";
        let lexer = Lexer::from(input);
        let mut p = Parser::new(lexer);
        let program = p
            .parse_program()
            .unwrap_or_else(|| panic!("parse_program() returned nul"));
        check_parse_errors(&p);
        if program.statements.len() != 3 {
            panic!(&format!(
                "program.statements does not contain 3 statements. got={}",
                program.statements.len()
            ))
        }
        let expected = vec!["x", "y", "foobar"];
        for i in 0..expected.len() {
            let test = expected[i];
            let stmt = program.statements[i].clone();
            if !test_let_statement(stmt, test.to_string()) {
                return;
            }
        }
    }

    #[test]
    fn test_return_statements() {
        let input = "
        return 5;
        return 10;
        return 993322;
        ";
        let lexer = Lexer::from(input);
        let mut parser = Parser::new(lexer);

        let program = parser
            .parse_program()
            .unwrap_or_else(|| panic!("parse_program retuned nil"));
        check_parse_errors(&parser);

        if program.statements.len() != 3 {
            panic!(&format!(
                "program.statements does not contain 3 statemetns. got {}",
                program.statements.len()
            ));
        }
        let expected = vec![""];
        for i in 0..expected.len() {
            let mut statement = program.statements[i].clone();
            if let Statement::Return { value: v } = statement {
            } else {
                panic!(&format!(
                    "return statement token type is not 'return', got {}",
                    statement.token_literal()
                ))
            }
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar";
        let lexer = Lexer::from(input);
        let mut parser = Parser::new(lexer);
        let program = parser
            .parse_program()
            .unwrap_or_else(|| panic!("parseer prgram error;"));
        check_parse_errors(&mut parser);

        if program.statements.len() != 1 {
            panic!(
                "program has not enough statements. got {}",
                program.statements.len()
            );
        }
        let statement = program.statements[0].clone();
        if let Statement::Expression { value: v } = statement {
            if let Expression::Identifiler(ident) = v {
                if ident != "foobar" {
                    panic!("ident is not {}, got {}", "foobar", ident);
                }
            } else {
                panic!("exp not Identifiler got {:?}", v);
            }
        } else {
            panic!("exp not Identifiler got {:?}", statement);
        }
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "5;";
        let lexer = Lexer::from(input);
        let mut parser = Parser::new(lexer);
        let program = parser
            .parse_program()
            .unwrap_or_else(|| panic!("parse prgram error"));
        check_parse_errors(&parser);

        if program.statements.len() != 1 {
            panic!(
                "program has not enough statements. got {}",
                program.statements.len()
            )
        }
        let statement = program.statements[0].clone();
        if let Statement::Expression { value } = statement {
            if let Expression::Integer(i) = value {
                if i == 5 {
                } else {
                    panic!("literarl value not {}, got {}", 5, i);
                }
            } else {
                panic!("exp not IntergerExpression. got {:?}", value);
            }
        } else {
            panic!("statement not expression statement. got {:?}", statement);
        }
    }

    #[test]
    fn test_parsing_prefix_expression() {
        let prefix_tests = vec![("!5", "!", 5), ("-15", "-", 15)];
        for &(input, exp_operator, exp_integer_value) in prefix_tests.iter() {
            let lexer = Lexer::from(input);
            let mut parser = Parser::new(lexer);
            let program = parser
                .parse_program()
                .unwrap_or_else(|| panic!("failed to parse program"));
            check_parse_errors(&parser);
            if program.statements.len() != 1 {
                panic!(
                    "program.statements doesn nto contain {} statements. got={}",
                    1,
                    program.statements.len()
                )
            }

            let statement = program.statements[0].clone();
            if let Statement::Expression { value: expression } = statement {
                if let Expression::Prefix { operator, right } = expression {
                    // operatorとintegerのあさーと
                    if operator != exp_operator {
                        panic!("operator is not {}. got={}", exp_operator, operator);
                    }
                    if !test_integar_litral(*right, exp_integer_value) {
                        return;
                    }
                } else {
                    panic!(
                        "ExpressionStatement is not PrefixExpression. got={:?}",
                        expression
                    );
                }
            } else {
                panic!("statement is not Expression. got={:?}", statement);
            }
        }
    }

    #[test]
    fn test_parsing_infix_expressions() {
        let inputs = vec![
            ("5 + 5", 5, "+", 5),
            ("5 - 5", 5, "-", 5),
            ("5 * 5", 5, "*", 5),
            ("5 / 5", 5, "/", 5),
            ("5 > 5", 5, ">", 5),
            ("5 < 5", 5, "<", 5),
            ("5 == 5", 5, "==", 5),
            ("5 != 5", 5, "!=", 5),
        ];
        for &(input, exp_left, exp_operator, exp_right) in inputs.iter() {
            let lexer = Lexer::from(input);
            let mut parser = Parser::new(lexer);
            let program = parser
                .parse_program()
                .unwrap_or_else(|| panic!("Failed to parser program"));

            if program.statements.len() != 1 {
                panic!(
                    "program.statements does not contain {} statements. got={}",
                    1,
                    program.statements.len()
                );
            }

            let statement = program.statements[0].clone();
            if let Statement::Expression { value } = statement {
                if let Expression::Infix {
                    left,
                    operator,
                    right,
                } = value
                {
                    if !test_integar_litral(*left, exp_left) {
                        return;
                    }
                    if operator != exp_operator {
                        panic!(
                            "expression operator is not {}. got={}",
                            exp_operator, operator
                        )
                    }
                    if test_integar_litral(*right, exp_right) {
                        return;
                    }
                } else {
                    panic!(
                        "expression statement is not Expression::Infix. got={:?}",
                        value
                    );
                }
            } else {
                panic!(
                    "statement is not Statement::Expression. got={:?}",
                    statement
                );
            }
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        let tests = vec![
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
        ];

        for &(input, expected) in tests.iter() {
            let lexer = Lexer::from(input);
            let mut parser = Parser::new(lexer);
            let program = parser
                .parse_program()
                .unwrap_or_else(|| panic!("Failed to parse program."));
            check_parse_errors(&parser);
            let actual = program.string();
            assert_eq!(actual, expected);
        }
    }

    fn check_parse_errors(parser: &Parser) {
        let errors = parser.errors();
        if errors.len() == 0 {
            return;
        }
        eprintln!("parser has {} errors", errors.len());
        for msg in errors.iter() {
            eprintln!("parser error: {}", msg);
        }
        panic!();
    }

    fn test_let_statement(s: Statement, name: String) -> bool {
        if let Statement::Let { ident: i, value: v } = s {
            if let Expression::Identifiler(s_name) = i {
                assert_eq!(s_name, name);
                return true;
            } else {
                eprint!("let statement name value is not {}", name);
                return false;
            }
        } else {
            eprint!("s.Token is not  'let'.");
            return false;
        }
    }

    fn test_integar_litral(expression: Expression, exp_int: i32) -> bool {
        if let Expression::Integer(i) = expression {
            if i != exp_int {
                panic!("int value is not {}. got={}", exp_int, i);
            }
        } else {
            panic!(
                "expression is not Expression::Integer. got={:?}",
                expression
            );
        }
        return true;
    }
}
