#[derive(Clone, Debug)]
pub enum Statement {
    Let {
        ident: Expression,
        value: Expression,
    },
    Return {
        value: Expression,
    },
}
impl Statement {
    pub fn token_literal(&self) -> String {
        format!("{:?}", self)
    }
}
#[derive(Clone, Debug)]
pub enum Expression {
    Identifiler(String),
    String(String),
    Integer(i32),
}

pub struct Program {
    pub statements: Vec<Statement>,
}
impl Program {
    fn token_literal(&self) -> String {
        if self.statements.len() > 0 {
            self.statements[0].token_literal()
        } else {
            "".to_string()
        }
    }
}
