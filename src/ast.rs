use std::ops::Add;

#[derive(Clone, Debug)]
pub enum Statement
where
    Statement: Node,
{
    Let {
        ident: Expression,
        value: Expression,
    },
    Return {
        value: Expression,
    },
    Expression {
        value: Expression,
    },
    Block(Vec<Statement>),
}

impl Node for Statement {
    fn token_literal(&mut self) -> String {
        format!("{:?}", self)
    }

    fn string(&self) -> String {
        match self {
            Statement::Let {
                ident: Expression::Identifiler(i),
                value: Expression::String(v),
            } => format!("let {} = {}", i, v,),
            Statement::Return { value: e } => match e {
                Expression::String(v) => format!("return {}", v),
                Expression::Integer(v) => format!("return {}", v),
                _ => format!("return"),
            },
            Statement::Expression { value } => value.string(),
            _ => format!(""),
        }
    }
}
#[derive(Clone, Debug)]
pub enum Expression {
    Identifiler(String),
    String(String),
    Integer(i32),
    Prefix {
        operator: String,
        right: Box<Expression>,
    },
    Infix {
        left: Box<Expression>,
        operator: String,
        right: Box<Expression>,
    },
    Boolean(bool),
    If {
        condition: Box<Expression>,
        // Statements::Blockをもつ
        consequence: Box<Statement>,
        alternative: Option<Box<Statement>>,
    },
    Function {
        parameters: Vec<Expression>, // Expression::Identifierのリスト
        body: Box<Statement>,        // Statement::Block
    },
}

impl Expression {
    pub fn string(&self) -> String {
        match self {
            Expression::String(v) => format!("{}", v),
            Expression::Integer(v) => format!("{}", v),
            Expression::Identifiler(v) => format!("{}", v),
            Expression::Prefix { operator, right } => format!("({}{})", operator, right.string()),
            Expression::Infix {
                left,
                operator,
                right,
            } => format!("({} {} {})", left.string(), operator, right.string()),
            Expression::Boolean(b) => format!("{}", b),
            Expression::If {
                condition,
                consequence,
                alternative,
            } => {
                if let Some(alt) = alternative {
                    format!(
                        "if {} {} eles {}",
                        condition.string(),
                        consequence.string(),
                        alt.string()
                    )
                } else {
                    format!("if {} {}", condition.string(), consequence.string())
                }
            }
            Expression::Function { parameters, body } => {
                format!("fn ({:?}) {}", parameters, body.string())
            }
        }
    }
}

pub struct Program
where
    Program: Node,
{
    pub statements: Vec<Statement>,
}
impl Node for Program {
    fn token_literal(&mut self) -> String {
        if self.statements.len() > 0 {
            self.statements[0].token_literal()
        } else {
            "".to_string()
        }
    }

    fn string(&self) -> String {
        let mut buf = String::new();
        for i in 0..self.statements.len() {
            buf.push_str(self.statements[i].string().as_ref());
        }
        buf
    }
}

pub trait Node {
    fn token_literal(&mut self) -> String;

    fn string(&self) -> String;
}

#[cfg(test)]
mod tests {
    use crate::ast::Node;
    use crate::Expression;
    use crate::Program;
    use crate::Statement;

    #[test]
    fn test_string() {
        let mut program = Program {
            statements: vec![Statement::Let {
                ident: Expression::Identifiler("myVar".to_string()),
                value: Expression::String("anotherVar".to_string()),
            }],
        };
        if program.string() != "let myVar = anotherVar" {
            panic!("program.String() wrong. get={}", program.string())
        }
    }
}
