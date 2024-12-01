use crate::token::*;
use marsc_span::CodeSpan;

pub struct Lexer<'a> {
    input: &'a str,
    cur_pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            cur_pos: 0,
        }
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = vec![];

        while let Some(token) = self.next_token() {
            tokens.push(token);
        }

        tokens
    }

    pub fn next_token(&mut self) -> Option<Token> {
        if self.cur_pos == self.input.len() {
            let eof_char = '\0';
            self.cur_pos += 1;
            return Some(Token::new(
                TokenKind::Eof,
                CodeSpan::new(0, 0, eof_char.to_string()),
            ));
        }
        let c = self.current_char();
        c
            .map(|c| {
            let start = self.cur_pos;
            let mut kind = TokenKind::Bad;
            if Self::is_number_start(&c) {
                let number: i64 = self.consume_number();
                kind = TokenKind::Int64(number);
            } else {
                kind = self.consume_punctuation();
            }

            let end = self.cur_pos;
            let literal = self.input[start..end].to_string();
            let span = CodeSpan::new(start, end, literal);
            Token::new(kind, span)
        })
    }

    fn consume_punctuation(&mut self) -> TokenKind {
        let c = self.consume().unwrap();
        match c {
            '+' => TokenKind::Plus,
            '-' => TokenKind::Minus,
            '*' => TokenKind::Star,
            '/' => TokenKind::Slash,
            '(' => TokenKind::LeftBracket,
            ')' => TokenKind::RightBracket,
            '=' => TokenKind::Equals,

            _ => TokenKind::Bad,
        }
    }

    fn lex_potential_double_char_operator(
        &mut self,
        expected: char,
        one_char_kind: TokenKind,
        double_char_kind: TokenKind,
    ) -> TokenKind {
        if let Some(next) = self.current_char() {
            if next == expected {
                self.consume();
                double_char_kind
            } else {
                one_char_kind
            }
        } else {
            one_char_kind
        }
    }

    fn is_number_start(c: &char) -> bool {
        c.is_digit(10)
    }

    fn is_identifier_start(c: &char) -> bool {
        c.is_alphabetic() || c == &'_'
    }

    fn is_whitespace(c: &char) -> bool {
        c.is_whitespace()
    }

    fn current_char(&self) -> Option<char> {
        self.input.chars().nth(self.cur_pos)
    }

    fn consume(&mut self) -> Option<char> {
        if self.cur_pos >= self.input.len() {
            return None;
        }
        let c = self.current_char();
        self.cur_pos += 1;

        c
    }

    fn consume_identifier(&mut self) -> String {
        let mut identifier = String::new();
        while let Some(c) = self.current_char() {
            if Self::is_identifier_start(&c) {
                self.consume().unwrap();
                identifier.push(c);
            } else {
                break;
            }
        }
        identifier
    }

    fn consume_number(&mut self) -> i64 {
        let mut number: i64 = 0;
        while let Some(c) = self.current_char() {
            if c.is_digit(10) {
                self.consume().unwrap();
                number = number * 10 + c.to_digit(10).unwrap() as i64;
            } else {
                break;
            }
        }
        number
    }
}


#[cfg(test)]
mod tests {
    use super::*;


    #[test]
    fn test1() {
        let inp = "1 + 2";
        let mut lex = Lexer::new(inp);
        let tokens = lex.tokenize();
        println!("{:?}", tokens);
        // assert_eq!(tokens, vec![
        //     Token::new(TokenKind::Int64(1), CodeSpan::new(0, 1, String::from("1"))),
        //     Token::new(TokenKind::Plus, CodeSpan::new(2, 3, String::from("+"))),
        //     Token::new(TokenKind::Int64(2), CodeSpan::new(4, 5, String::from("2"))),
        // ]);
    }
}