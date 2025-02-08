use std::str::Chars;

#[derive(Debug)]
pub struct Token {
    pub text: String,
    pub kind: TokenKind,
}

impl Token {
    pub fn new(text: &str, kind: TokenKind) -> Self {
        Token {
            text: text.to_string(),
            kind,
        }
    }
}

#[derive(Debug)]
pub enum TokenKind {
    Integer,
    Boolean,
    String,
    Operator,
    Keyword,
    Whitespace,
    Identifier,
}

pub struct Simplexer<'a> {
    expr: Chars<'a>,
    buffer: Option<char>,
}

impl<'a> Simplexer<'a> {
    fn new(expr: &'a str) -> Self {
        Simplexer {
            expr: expr.chars(),
            buffer: None,
        }
    }

    fn peek(&mut self) -> Option<char> {
        if self.buffer.is_none() {
            self.buffer = self.expr.next();
        }
        self.buffer
    }

    fn consume(&mut self) -> Option<char> {
        let current = self.buffer.take();
        current.or_else(|| self.expr.next())
    }

    fn consume_while<F>(&mut self, condition: F) -> String
    where
        F: Fn(char) -> bool,
    {
        let mut result = String::new();
        while let Some(c) = self.peek() {
            if condition(c) {
                result.push(c);
                self.consume();
            } else {
                break;
            }
        }
        result
    }

    fn is_operator(c: char) -> bool {
        "+-*/%()=".contains(c)
    }

    fn is_keyword(s: &str) -> bool {
        matches!(s, "if" | "else" | "for" | "while" | "return" | "func" | "break")
    }

    fn is_boolean(s: &str) -> bool {
        matches!(s, "true" | "false")
    }
}

impl<'a> Iterator for Simplexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(c) = self.peek() {
            return if c.is_whitespace() {
                let text = self.consume_while(|ch| ch.is_whitespace());
                Some(Token::new(&text, TokenKind::Whitespace))
            } else if c.is_digit(10) {
                let text = self.consume_while(|ch| ch.is_digit(10));
                Some(Token::new(&text, TokenKind::Integer))
            } else if c == '"' {
                self.consume(); // Consume the opening quote
                let text = self.consume_while(|ch| ch != '"');
                self.consume(); // Consume the closing quote
                Some(Token::new(&format!("\"{}\"", text), TokenKind::String))
            } else if Self::is_operator(c) {
                let text = self.consume().unwrap().to_string();
                Some(Token::new(&text, TokenKind::Operator))
            } else if c.is_alphabetic() || c == '_' || c == '$' {
                let text = self.consume_while(|ch| ch.is_alphanumeric() || ch == '_' || ch == '$');
                if Self::is_keyword(&text) {
                    Some(Token::new(&text, TokenKind::Keyword))
                } else if Self::is_boolean(&text) {
                    Some(Token::new(&text, TokenKind::Boolean))
                } else {
                    Some(Token::new(&text, TokenKind::Identifier))
                }
            } else {
                self.consume(); // Unknown character, just consume it
                None
            };
        }

        None
    }
}

fn main() {
    let input = r#"if (x + 42 == "true") return else"#;
    let lexer = Simplexer::new(input);

    for token in lexer {
        println!("{:?}", token);
    }
}