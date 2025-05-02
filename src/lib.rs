const MISMATCHED_PAREN_ERROR: &str = "Closing parenthesis without opening parenthesis";
const UNCLOSED_PAREN_ERROR: &str = "Unclosed parenthesis";

#[derive(PartialEq, Debug)]
pub enum TokenKind {
    LeftParen,
    RightParen,
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

pub fn test_parsing(text: &str, target: Vec<Token>) {
    let output = parse(text.to_string());
    assert!(compare_token_vecs(&output, &target));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty() {
        test_parsing("", vec![Token::new(TokenKind::End)]);
    }

    #[test]
    fn parens() {
        test_parsing(
            "()",
            vec![
                Token::new(TokenKind::LeftParen),
                Token::new(TokenKind::RightParen),
                Token::new(TokenKind::End),
            ],
        );

        test_parsing(
            "(( ( )) )",
            vec![
                Token::new(TokenKind::LeftParen),
                Token::new(TokenKind::LeftParen),
                Token::new(TokenKind::LeftParen),
                Token::new(TokenKind::RightParen),
                Token::new(TokenKind::RightParen),
                Token::new(TokenKind::RightParen),
                Token::new(TokenKind::End),
            ],
        );

        test_parsing(
            "(()",
            vec![Token::new(TokenKind::error(UNCLOSED_PAREN_ERROR))],
        );

        test_parsing(
            "() )",
            vec![Token::new(TokenKind::error(MISMATCHED_PAREN_ERROR))],
        );
    }
}
