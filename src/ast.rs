#[derive(Clone, Debug)]
pub enum Statement {
    Let {
        ident: Expression,
        value: Expression,
    },
}
impl Statement {
    fn token_litarel(&self) -> String {
        "".to_string()
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
            self.statements[0].token_litarel()
        } else {
            "".to_string()
        }
    }
}
