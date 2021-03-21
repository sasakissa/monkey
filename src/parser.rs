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
            Token::TRUE | Token::FALSE => self.parse_boolean(),
            Token::LPAREN => self.parse_grouped_expression(),
            Token::IF => self.parse_if_expression(),
            Token::FUNCTION => self.parse_function(),
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

    fn parse_boolean(&self) -> Option<Expression> {
        let c_token = &self.cur_token;
        let res = match c_token {
            Token::TRUE => Some(Expression::Boolean(true)),
            Token::FALSE => Some(Expression::Boolean(false)),
            _ => None,
        };
        return res;
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

    fn parse_grouped_expression(&mut self) -> Option<Expression> {
        self.next_token();
        let expression = self.parse_expression(LOWEST);
        if !self.expect_peek(&Token::RPAREN) {
            return None;
        }
        return expression;
    }

    fn parse_if_expression(&mut self) -> Option<Expression> {
        if !self.expect_peek(&Token::LPAREN) {
            return None;
        }
        self.next_token();
        let condition = self.parse_expression(LOWEST)?;
        if !self.expect_peek(&Token::RPAREN) {
            return None;
        }
        if !self.expect_peek(&Token::LBRRACE) {
            return None;
        }

        let consequence = self.parse_block_statement()?;

        let mut alternative: Option<Box<Statement>> = None;
        if self.peek_token_is(&Token::ELSE) {
            self.next_token();
            if !self.expect_peek(&Token::LBRRACE) {
                return None;
            }
            alternative = Some(Box::new(self.parse_block_statement()?));
        }
        return Some(Expression::If {
            condition: Box::new(condition),
            consequence: Box::new(consequence),
            alternative: alternative,
        });
    }

    fn parse_block_statement(&mut self) -> Option<Statement> {
        let mut statements = vec![];
        self.next_token();

        while !self.cur_token_is(Token::RBRACE) && !self.cur_token_is(Token::EOF) {
            let statement = self.parse_statement()?;
            statements.push(statement);
            self.next_token();
        }
        return Some(Statement::Block(statements));
    }

    fn parse_function(&mut self) -> Option<Expression> {
        if !self.expect_peek(&Token::LPAREN) {
            return None;
        }
        let parameters = self.parse_function_parameters().expect("parse error");
        if !self.expect_peek(&Token::LBRRACE) {
            return None;
        }
        let body = self.parse_block_statement().expect("parse error");
        return Some(Expression::Function {
            parameters,
            body: Box::new(body),
        });
    }

    fn parse_function_parameters(&mut self) -> Option<Vec<Expression>> {
        let mut identifiers = vec![];
        if self.peek_token_is(&Token::RPAREN) {
            self.next_token();
            return Some(identifiers);
        }

        self.next_token();
        let ident = self.parse_identifier().expect("parse error");
        identifiers.push(ident);

        while self.peek_token_is(&Token::COMMA) {
            self.next_token();
            self.next_token();
            let ident = self.parse_identifier().expect("parse error");
            identifiers.push(ident);
        }

        if !self.expect_peek(&Token::RPAREN) {
            return None;
        }

        return Some(identifiers);
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
    use std::os::macos::raw::stat;

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
    fn test_boolean_expression() {
        let test = vec![("true;", true), ("false;", false)];
        for &(input, exp_bool) in test.iter() {
            let lexer = Lexer::from(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program().expect("failed to parse program");
            check_parse_errors(&parser);

            if program.statements.len() != 1 {
                panic!(
                    "prgram.statements hos not enoufh statement. got={} ",
                    program.statements.len()
                );
            }
            let statement = program.statements[0].clone();
            if let Statement::Expression { value: expression } = statement {
                if let Expression::Boolean(actual) = expression {
                    if actual != exp_bool {
                        panic!("bool is not {}. got={}", exp_bool, actual);
                    }
                } else {
                    panic!(
                        "expression is not Expression::Boolean. got={:?}",
                        expression
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
            // todo: 真偽値への拡張
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
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
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

    #[test]
    fn test_if_expression() {
        let input = "if (x < y) { x }";
        let lexer = Lexer::from(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().expect("Failed to parse program");
        check_parse_errors(&parser);

        if program.statements.len() != 1 {
            panic!(
                "program.statements does not contain {} statements. got={}",
                1,
                program.statements.len()
            );
        }
        let statement = program.statements[0].clone();
        if let Statement::Expression { value } = statement {
            if let Expression::If {
                condition,
                consequence,
                alternative,
            } = value
            {
                if !test_infix_expression(
                    *condition,
                    "x".to_string(),
                    "<".to_string(),
                    "y".to_string(),
                ) {
                    return;
                }
                if let Statement::Block(cons_statements) = *consequence {
                    if cons_statements.len() != 1 {
                        panic!(
                            "consequene is not 1 statements. got ={}",
                            cons_statements.len()
                        );
                    }
                    if let Statement::Expression { value } = cons_statements[0].clone() {
                        if !test_identifier(value, "x".to_string()) {
                            return;
                        }
                    } else {
                        panic!(
                            "consequence statement is not Statement::Expression. got={:?}",
                            cons_statements[0].clone()
                        );
                    }
                } else {
                    panic!(
                        "conssequence is not Statement::Block. got={:?}",
                        consequence
                    );
                }
                if let Some(alternative) = alternative {
                    panic!("expression alternative was not None. got={:?}", alternative);
                }
            } else {
                panic!("expression is not Expression::If. got={:?}", value);
            }
        } else {
            panic!(
                "statement is not Statement::Expression. got={:?}",
                statement
            );
        }
    }

    #[test]
    fn test_if_else_expression() {
        let input = "if (x < y) { x } else { y }";
        let lexer = Lexer::from(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().expect("Failed to parse program");
        check_parse_errors(&parser);

        if program.statements.len() != 1 {
            panic!(
                "program.statements does not contain {} statements. got={}",
                1,
                program.statements.len()
            );
        }
        let statement = program.statements[0].clone();
        if let Statement::Expression { value } = statement {
            if let Expression::If {
                condition,
                consequence,
                alternative,
            } = value
            {
                if !test_infix_expression(
                    *condition,
                    "x".to_string(),
                    "<".to_string(),
                    "y".to_string(),
                ) {
                    return;
                }
                if let Statement::Block(cons_statements) = *consequence {
                    if cons_statements.len() != 1 {
                        panic!(
                            "consequene is not 1 statements. got ={}",
                            cons_statements.len()
                        );
                    }
                    if let Statement::Expression { value } = cons_statements[0].clone() {
                        if !test_identifier(value, "x".to_string()) {
                            return;
                        }
                    } else {
                        panic!(
                            "consequence statement is not Statement::Expression. got={:?}",
                            cons_statements[0].clone()
                        );
                    }
                } else {
                    panic!(
                        "conssequence is not Statement::Block. got={:?}",
                        consequence
                    );
                }
                if let Some(alternative) = alternative {
                    if let Statement::Block(alt_statements) = *alternative {
                        if alt_statements.len() != 1 {
                            panic!(
                                "alternative is not 1 statement. got={}",
                                alt_statements.len()
                            );
                        }
                        if let Statement::Expression { value } = alt_statements[0].clone() {
                            if !test_identifier(value, "y".to_string()) {
                                return;
                            }
                        } else {
                            panic!(
                                "alternative statement is not Statement::Expression. got={:?}",
                                alt_statements[0].clone()
                            )
                        }
                    } else {
                        panic!(
                            "alternative statement is not Statement::Block. got={:?}",
                            alternative
                        );
                    }
                } else {
                    panic!("expression alternative was None.");
                }
            } else {
                panic!("expression is not Expression::If. got={:?}", value);
            }
        } else {
            panic!(
                "statement is not Statement::Expression. got={:?}",
                statement
            );
        }
    }

    #[test]
    fn test_function_litaral_parsing() {
        let input = "fn(x, y) { x + y; }";
        let lexer = Lexer::from(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().expect("Failed to parse program");
        check_parse_errors(&parser);

        if program.statements.len() != 1 {
            panic!(
                "program statements does not contain {} statements. got={}",
                1,
                program.statements.len()
            );
        }
        let statement = program.statements[0].clone();
        if let Statement::Expression { value } = statement {
            if let Expression::Function { parameters, body } = value {
                if parameters.len() != 2 {
                    panic!(
                        "function literal parameters wrong. want 2. got={}",
                        parameters.len()
                    );
                }
                // parameterのアサート
                let x = parameters[0].clone();
                if let Expression::Identifiler(param) = x {
                    if param != "x" {
                        panic!("function param is not {}. got={}", "x", param);
                    }
                } else {
                    panic!("function parameter is not Identifier.got={:?}", x);
                }
                let y = parameters[1].clone();
                if let Expression::Identifiler(param) = y {
                    if param != "y" {
                        panic!("function param is not {}. got={}", "y", param);
                    }
                } else {
                    panic!("function parameter is not Identifier.got={:?}", y);
                }

                // bodyのアサート
                if let Statement::Block(body) = *body {
                    if body.len() != 1 {
                        panic!(
                            "function body statements has not 1 statements. got={}",
                            body.len()
                        );
                    }
                    if let Statement::Expression { value } = body[0].clone() {
                        test_infix_expression(
                            value,
                            "x".to_string(),
                            "+".to_string(),
                            "y".to_string(),
                        );
                    }
                } else {
                    panic!("function body should hava Statement::Body. got={:?}", body);
                }
            } else {
                panic!(
                    "program statemnts[0] is noExpression::Function. got={:?}",
                    value
                );
            }
        } else {
            panic!(
                "program statemnts[0] is not Statement::Expression. got={:?}",
                statement
            );
        }
    }

    #[test]
    fn test_function_parameter_parsing() {
        let tests = vec![
            ("fn() {};", vec![]),
            ("fn(x) {};", vec!["x"]),
            ("fn(x, y, z) {};", vec!["x", "y", "z"]),
        ];

        for (input, expected_params) in tests.into_iter() {
            let lexer = Lexer::from(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program().expect("Failed to parse program");
            check_parse_errors(&parser);

            let statement = program.statements[0].clone();
            if let Statement::Expression { value } = statement {
                if let Expression::Function { parameters, body } = value {
                    if parameters.len() != expected_params.len() {
                        panic!(
                            "length parameters wrong. want {}, got={}",
                            expected_params.len(),
                            parameters.len()
                        );
                    }
                    for (i, &exp_ident) in expected_params.iter().enumerate() {
                        if let Expression::Identifiler(ident) = parameters[i].clone() {
                            if ident != exp_ident {
                                panic!("parameter {} is not {}", ident, exp_ident);
                            }
                        }
                    }
                } else {
                    panic!("expresison is not Expression::Function. got={:?}", value);
                }
            } else {
                panic!(
                    "statement is not Statement::Expression. got={:?}",
                    statement
                );
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

    fn test_identifier(expression: Expression, value: String) -> bool {
        if let Expression::Identifiler(ident) = expression {
            if ident != value {
                panic!("exp identifiler is not {}. got={}", value, ident);
            }
        } else {
            panic!("exp is not Expression::Identifier. got={:?}", expression);
        }
        return true;
    }

    fn test_infix_expression(
        expression: Expression,
        exp_left: String,
        exp_operator: String,
        exp_right: String,
    ) -> bool {
        if let Expression::Infix {
            left,
            operator,
            right,
        } = expression
        {
            let left_string = left.string();
            let right_string = right.string();
            if left_string == exp_left && operator == exp_operator && right_string == exp_right {
                return true;
            } else {
                panic!(
                    "expected: {} {} {}. got={} {} {}",
                    exp_left, exp_operator, exp_right, left_string, operator, right_string
                );
            }
        } else {
            panic!("exprssion is not Expression::Infix. got={:?}", expression);
        }
    }
}
