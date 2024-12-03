use crate::token::*;
use m_span::CodeSpan;

pub struct Lexer<'a> {
    input: &'a str,
    cur_pos: usize,
}

#[derive(Debug, PartialEq)]
pub struct LexError {
    span: CodeSpan,
}

impl LexError {
    pub fn take_span(self) -> CodeSpan {
        self.span
    }
}

enum Number {
    Int64(i64),
    Float64(f64),
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self { input, cur_pos: 0 }
    }

    // only one public method for lexer struct (except new)
    pub fn tokenize(&mut self) -> Result<Vec<Token>, Vec<LexError>> {
        let mut tokens = vec![];
        let mut errors = vec![];

        loop {
            // Nones are tabs and whitespaces (do not ignore them in strings!!)
            let Some(token) = self.next_token() else { continue };
            match token {
                t if t.kind == TokenKind::Bad => errors.push(LexError { span: t.span }),
                t if t.kind == TokenKind::Eof => break,
                _ => tokens.push(token),
            }
        }

        if !errors.is_empty() {
            return Err(errors);
        }

        tokens.push(Token::new(
            TokenKind::Eof,
            CodeSpan::new(self.input.len(), self.input.len() + 1, "\0".into())
        ));

        Ok(tokens)
    }

    // this func checks char under cur_pos and matches it with possible options
    // if it finds good variant, it returns whole token (string literal) and moves
    // cur_pos pointer to the end of word
    // func must ignore ' ' and '\t', but if it met in string (string interpolation)
    // it must return original string ("hello world")
    fn next_token(&mut self) -> Option<Token> {
        if self.cur_pos >= self.input.len() {
            return Some(Token::new(
                TokenKind::Eof,
                CodeSpan::new(self.input.len(), self.input.len() + 1, "\0".into()),
            ));
        }

        let cur_char = self.peek();
        let mut offset = 1;
        let token = match cur_char {
            c if c.is_numeric() => {
                match self.consume_digit() {
                    Ok(Number::Int64(x)) => {
                        let word = self.consume_word();
                        offset = word.len();
                        Some(Token::new(
                            TokenKind::Int64(x),
                            CodeSpan::new(self.cur_pos, self.cur_pos + word.len(), word),
                        ))
                    }

                    Ok(Number::Float64(x)) => {
                        let word = self.consume_word();
                        offset = word.len();
                        Some(Token::new(
                            TokenKind::Float64(x),
                            CodeSpan::new(self.cur_pos, self.cur_pos + word.len(), word),
                        ))
                    }

                    Err(word) => {
                        offset = word.len();
                        Some(Token::new(
                            TokenKind::Bad,
                            CodeSpan::new(self.cur_pos, self.cur_pos + word.len(), word),
                        ))
                    }
                }
            }

            '+' => {
                Some(Token::new(
                    TokenKind::Plus,
                    CodeSpan::new(self.cur_pos, self.cur_pos + 1, "+".into()),
                ))
            }

            '-' => {
                Some(Token::new(
                    TokenKind::Minus,
                    CodeSpan::new(self.cur_pos, self.cur_pos + 1, "-".into()),
                ))
            }

            '*' => {
                Some(Token::new(
                    TokenKind::Star,
                    CodeSpan::new(self.cur_pos, self.cur_pos + 1, "*".into()),
                ))
            }

            '/' => {
                Some(Token::new(
                    TokenKind::Slash,
                    CodeSpan::new(self.cur_pos, self.cur_pos + 1, "/".into()),
                ))
            }

            '=' => {
                // check == (equals)
                Some(Token::new(
                    TokenKind::Equal,
                    CodeSpan::new(self.cur_pos, self.cur_pos + 1, "=".into()),
                ))
            }

            '(' => Some(Token::new(
                TokenKind::LeftParen,
                CodeSpan::new(self.cur_pos, self.cur_pos + 1, "(".into()),
            )),

            ')' => Some(Token::new(
                TokenKind::RightParen,
                CodeSpan::new(self.cur_pos, self.cur_pos + 1, ")".into()),
            )),

            ' ' | '\t' => None,

            '\n' => Some(Token::new(
                TokenKind::NewLine,
                CodeSpan::new(self.cur_pos, self.cur_pos + 1, "\n".into()),
            )),

            // '\0' => {
            //     Some(Token::new(
            //         TokenKind::Eof,
            //         CodeSpan::new(self.input.len(), self.input.len() + 1, "\0".into()),
            //     ))
            // }

            // TODO: more tokens !!!

            _ => {
                let word = self.consume_word();
                offset = word.len();
                Some(Token::new(
                    TokenKind::Bad,
                    CodeSpan::new(self.cur_pos, self.cur_pos + word.len(), word),
                ))
            }
        };

        self.cur_pos += offset;
        token
    }

    fn peek(&mut self) -> char {
        self.input.chars().nth(self.cur_pos).unwrap()
    }

    fn consume_word(&mut self) -> String {
        let mut cnt = self.cur_pos;
        let breakers = [' ', '\t', '\n', '\0'];
        while self.input.chars().nth(cnt).is_some()
            && !breakers.contains(&self.input.chars().nth(cnt).unwrap())
        {
            cnt += 1;
        }

        let out = self.input[self.cur_pos..cnt].to_owned();
        out
    }

    fn consume_digit(&mut self) -> Result<Number, String> {
        let binding = self.consume_word();
        let lit = binding.trim();

        if let Ok(x) = lit.parse::<i64>() {
            Ok(Number::Int64(x))
        } else if let Ok(x) = lit.parse::<f64>() {
            Ok(Number::Float64(x))
        } else {
            Err(lit.to_owned())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple_test_1() {
        let inp = "1 + 2";
        let mut lex = Lexer::new(inp);
        let tokens = lex.tokenize().unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::new(TokenKind::Int64(1), CodeSpan::new(0, 1, String::from("1"))),
                Token::new(TokenKind::Plus, CodeSpan::new(2, 3, String::from("+"))),
                Token::new(TokenKind::Int64(2), CodeSpan::new(4, 5, String::from("2"))),
                Token::new(TokenKind::Eof, CodeSpan::new(5, 6, String::from("\0"))),
            ]
        );
    }

    #[test]
    fn simple_test_2() {
        let inp = "1 # 2";
        let mut lex = Lexer::new(inp);
        let tokens = lex.tokenize();
        assert_eq!(
            tokens.err().unwrap(),
            vec![
                LexError { span: CodeSpan::new(2, 3, String::from("#")) },
            ]
        );
    }

    #[test]
    fn simple_test_3() {
        let inp = "1 + 2.0";
        let mut lex = Lexer::new(inp);
        let tokens = lex.tokenize().unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::new(TokenKind::Int64(1), CodeSpan::new(0, 1, String::from("1"))),
                Token::new(TokenKind::Plus, CodeSpan::new(2, 3, String::from("+"))),
                Token::new(TokenKind::Float64(2.0), CodeSpan::new(4, 7, String::from("2.0"))),
                Token::new(TokenKind::Eof, CodeSpan::new(7, 8, String::from("\0"))),
            ]
        );
    }
}
