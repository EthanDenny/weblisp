const MISMATCHED_PAREN_ERROR: &str = "Closing parenthesis without opening parenthesis";
const UNCLOSED_PAREN_ERROR: &str = "Unclosed parenthesis";

#[derive(PartialEq, Debug)]
pub enum TokenKind {
    LeftParen,
    RightParen,
    Integer(i64),
    End,
    Error(String),
    Unknown,
}

impl TokenKind {
    fn error(msg: &str) -> TokenKind {
        TokenKind::Error(msg.to_string())
    }
}

#[derive(PartialEq, Debug)]
pub struct Token {
    kind: TokenKind,
}

impl Token {
    fn new(kind: TokenKind) -> Token {
        Token { kind }
    }
}

pub fn parse(text: String) -> Vec<Token> {
    let mut chars = text.chars().peekable();
    let mut tokens = Vec::new();
    let mut paren_level = 0;

    while let Some(c) = chars.next() {
        let kind = match c {
            '(' => {
                paren_level += 1;
                Some(TokenKind::LeftParen)
            }
            ')' => {
                if paren_level > 0 {
                    paren_level -= 1;
                    Some(TokenKind::RightParen)
                } else {
                    Some(TokenKind::error(MISMATCHED_PAREN_ERROR))
                }
            }
            _ if c.is_digit(10) => {
                let mut num = String::from(c);

                while let Some(&p) = chars.peek() {
                    if p.is_digit(10) {
                        num.push(p);
                        chars.next();
                    } else {
                        break;
                    }
                }

                let parsed = num.parse::<i64>();

                match parsed {
                    Ok(v) => Some(TokenKind::Integer(v)),

                    // TODO: Figure out a way to test this (shoould theoretically never trigger)
                    Err(_) => Some(TokenKind::Error(format!(
                        "Tried to parse integer \"{}\", but something went wrong",
                        num,
                    ))),
                }
            }
            ' ' => None,
            _ => Some(TokenKind::Unknown),
        };

        if let Some(kind) = kind {
            if let TokenKind::Error(_) = kind {
                return vec![Token::new(kind)];
            }

            tokens.push(Token::new(kind));
        }
    }

    if paren_level != 0 {
        return vec![Token::new(TokenKind::error(UNCLOSED_PAREN_ERROR))];
    }

    tokens.push(Token::new(TokenKind::End));

    tokens
}

pub fn compare_token_vecs(a: &Vec<Token>, b: &Vec<Token>) -> bool {
    for (ta, tb) in a.iter().zip(b.iter()) {
        if ta.kind != tb.kind {
            return false;
        }
    }

    true
}

pub fn test_parsing(text: &str, target: Vec<TokenKind>) {
    let output = parse(text.to_string());

    for (t, k) in output.into_iter().zip(target.into_iter()) {
        if t.kind != k {
            assert!(false);
        }
    }

    assert!(true);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty() {
        test_parsing("", vec![TokenKind::End]);
    }

    #[test]
    fn parens() {
        test_parsing(
            "()",
            vec![TokenKind::LeftParen, TokenKind::RightParen, TokenKind::End],
        );

        test_parsing(
            "(( ( )) )",
            vec![
                TokenKind::LeftParen,
                TokenKind::LeftParen,
                TokenKind::LeftParen,
                TokenKind::RightParen,
                TokenKind::RightParen,
                TokenKind::RightParen,
                TokenKind::End,
            ],
        );

        test_parsing("(()", vec![TokenKind::error(UNCLOSED_PAREN_ERROR)]);

        test_parsing("() )", vec![TokenKind::error(MISMATCHED_PAREN_ERROR)]);
    }

    #[test]
    fn numbers() {
        test_parsing("1", vec![TokenKind::Integer(1), TokenKind::End]);

        test_parsing(
            "1 2",
            vec![TokenKind::Integer(1), TokenKind::Integer(2), TokenKind::End],
        );

        test_parsing("12345", vec![TokenKind::Integer(12345), TokenKind::End]);

        test_parsing(
            "12y45",
            vec![
                TokenKind::Integer(12),
                TokenKind::Unknown,
                TokenKind::Integer(45),
                TokenKind::End,
            ],
        );

        test_parsing(
            "(1 2 3)",
            vec![
                TokenKind::LeftParen,
                TokenKind::Integer(1),
                TokenKind::Integer(2),
                TokenKind::Integer(3),
                TokenKind::RightParen,
                TokenKind::End,
            ],
        );

        test_parsing(
            "(1 2 (3 4 5))",
            vec![
                TokenKind::LeftParen,
                TokenKind::Integer(1),
                TokenKind::Integer(2),
                TokenKind::LeftParen,
                TokenKind::Integer(3),
                TokenKind::Integer(4),
                TokenKind::Integer(5),
                TokenKind::RightParen,
                TokenKind::RightParen,
                TokenKind::End,
            ],
        );
    }
}
