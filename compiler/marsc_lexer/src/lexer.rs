use crate::token::*;
use marsc_span::CodeSpan;

pub struct Lexer<'a> {
    input: &'a str,
    cur_pos: usize,
}

enum Number {
    // smaller
    Int32(i32),
    Int64(i64),
    UInt32(u32),
    UInt64(u64),
    Float32(f32),
    Float64(f64),
    // larger
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self { input, cur_pos: 0 }
    }

    // only one public method for lexer struct (except new)
    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = vec![];

        loop {
            let token = self.next_token();
            if token.is_none() { continue }
            let token = token.unwrap();
            if token.kind == TokenKind::Eof {
                break;
            }
            tokens.push(token);
        }

        tokens
    }

    // this func checks char under cur_pos and matches it with possible options
    // if it finds good variant, it returns whole token (string literal) and moves
    // cur_pos pointer to the end of word
    // func must ignore ' ' and '\t', but if it met in string (string interpolation)
    // it must return original string ("hello world")
    fn next_token(&mut self) -> Option<Token> {
        if self.cur_pos >= self.input.len() {
            return None;
        }

        let cur_char = self.consume_char();
        let mut offset = 1;
        let token = match cur_char {
            c if c.is_digit(10) => {
                // todo
                // not working, need to implement func and impl From<Token> for Number
                // self.consume_digit().unwrap().into()
                // tmp

                match self.consume_digit().unwrap() {
                    Number::Int32(x) => {
                        let word = x.to_string();
                        offset = word.len();
                        Some(Token::new(
                            TokenKind::Int32(x),
                            CodeSpan::new(self.cur_pos, self.cur_pos + word.len(), word)
                        ))
                    }
                    _ => todo!()
                }
            }

            '+' => {
                // check +=
                offset = 1;
                Some(Token::new(
                    TokenKind::Plus,
                    CodeSpan::new(self.cur_pos, self.cur_pos + 1, "+".into())
                ))
            }

            '-' => {
                // check -=
                offset = 1;
                Some(Token::new(
                    TokenKind::Minus,
                    CodeSpan::new(self.cur_pos, self.cur_pos + 1, "-".into())
                ))
            }

            '*' => {
                // check *=
                offset = 1;
                Some(Token::new(
                    TokenKind::Star,
                    CodeSpan::new(self.cur_pos, self.cur_pos + 1, "*".into())
                ))
            }

            '/' => {
                // check /=
                // check // (comment) ??
                offset = 1;
                Some(Token::new(
                    TokenKind::Slash,
                    CodeSpan::new(self.cur_pos, self.cur_pos + 1, "/".into())
                ))
            }

            '=' => {
                // check == (equals)
                offset = 1;
                Some(Token::new(
                    TokenKind::Assignment,
                    CodeSpan::new(self.cur_pos, self.cur_pos + 1, "=".into())
                ))
            }

            '(' => {
                offset = 1;
                Some(Token::new(
                    TokenKind::LeftBracket,
                    CodeSpan::new(self.cur_pos, self.cur_pos + 1, "(".into())
                ))
            }

            ')' => {
                offset = 1;
                Some(Token::new(
                    TokenKind::RightBracket,
                    CodeSpan::new(self.cur_pos, self.cur_pos + 1, ")".into())
                ))
            }

            ' ' | '\t' => {
                offset = 1;
                None
            }

            '\n' => {
                offset = 1;
                Some(Token::new(
                    TokenKind::NewLine,
                    CodeSpan::new(self.cur_pos, self.cur_pos + 1, "\n".into())
                ))
            }

            // ??
            '\0' => {
                offset = 1;
                Some(Token::new(
                    TokenKind::Eof,
                    CodeSpan::new(self.cur_pos, self.cur_pos + 1, "\0".into())
                ))
            }

            _ => {
                let word = self.consume_word();
                offset = word.len();
                Some(Token::new(
                    TokenKind::Bad,
                    CodeSpan::new(self.cur_pos, self.cur_pos + word.len(), word)
                ))
            }
        };

        self.cur_pos += offset;
        token
    }

    fn consume_char(&mut self) -> char {
        self.input.chars().nth(self.cur_pos).unwrap();
    }

    fn consume_word(&mut self) -> String {
        let mut cnt = self.cur_pos;
        let breakers = [' ', '\t', '\n', '\0'];
        while self.input.chars().nth(cnt).is_some() &&
            !breakers.contains(&self.input.chars().nth(cnt).unwrap()) {
            cnt += 1;
        }

        let out = self.input[self.cur_pos..cnt].to_owned();
        out
    }

    fn consume_digit(&mut self) -> Option<Number> {
        let digit = self.consume_word().trim().parse::<i32>().unwrap();
        Some(Number::Int32(digit))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test1() {
        let inp = "1 + 2\0";
        let mut lex = Lexer::new(inp);
        let tokens = lex.tokenize();
        println!("{:?}", tokens);
        assert_eq!(tokens, vec![
            Token::new(TokenKind::Int32(1), CodeSpan::new(0, 1, String::from("1"))),
            Token::new(TokenKind::Plus, CodeSpan::new(2, 3, String::from("+"))),
            Token::new(TokenKind::Int32(2), CodeSpan::new(4, 5, String::from("2"))),
        ]);
    }
}
