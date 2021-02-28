use std::error::Error;

use crate::{Expression, Lexer, Program, Statement, Token};

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
            _ => None,
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
}

#[cfg(test)]
mod tests {
    use core::panic;

    use crate::{Expression, Lexer, Statement};

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
            let statement = program.statements[i].clone();
            if let Statement::Return { value: v } = statement {
            } else {
                panic!(&format!(
                    "return statement token type is not 'return', got {}",
                    statement.token_literal()
                ))
            }
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
}
