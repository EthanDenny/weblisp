// Error strings

const MISMATCHED_PAREN_ERROR: &str = "Closing parenthesis without opening parenthesis";
const UNCLOSED_PAREN_ERROR: &str = "Unclosed parenthesis";

// Parsing types and functions

#[derive(PartialEq, Debug)]
pub enum TokenKind {
    LeftParen,
    RightParen,
    Integer(i64),
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

    tokens
}

pub fn parse_str(text: &str) -> Vec<Token> {
    parse(text.to_string())
}

// Tree types and functions

#[derive(PartialEq, Debug)]
pub enum Element {
    List(Vec<Element>),
    Integer(i64),
}

pub fn construct_list(tokens: &mut std::vec::IntoIter<Token>) -> Element {
    let mut children = Vec::new();

    while let Some(token) = tokens.next() {
        match token.kind {
            TokenKind::Integer(v) => children.push(Element::Integer(v)),
            TokenKind::LeftParen => children.push(construct_list(tokens)),
            TokenKind::RightParen => break,
            _ => panic!("{token:?} cannot be constructed on"),
        }
    }

    Element::List(children)
}

pub fn construct(tokens: Vec<Token>) -> Element {
    construct_list(&mut tokens.into_iter())
}

// Tests

#[cfg(test)]
mod tests {
    use super::*;

    // Parser tests

    fn test_parsing(text: &str, target: Vec<TokenKind>) {
        let output = parse_str(text);

        for (t, k) in output.into_iter().zip(target.into_iter()) {
            if t.kind != k {
                assert!(false);
            }
        }

        assert!(true);
    }

    #[test]
    fn empty() {
        test_parsing("", vec![]);
    }

    #[test]
    fn parens() {
        test_parsing("()", vec![TokenKind::LeftParen, TokenKind::RightParen]);

        test_parsing(
            "(( ( )) )",
            vec![
                TokenKind::LeftParen,
                TokenKind::LeftParen,
                TokenKind::LeftParen,
                TokenKind::RightParen,
                TokenKind::RightParen,
                TokenKind::RightParen,
            ],
        );

        test_parsing("(()", vec![TokenKind::error(UNCLOSED_PAREN_ERROR)]);

        test_parsing("() )", vec![TokenKind::error(MISMATCHED_PAREN_ERROR)]);
    }

    #[test]
    fn numbers() {
        test_parsing("1", vec![TokenKind::Integer(1)]);

        test_parsing("1 2", vec![TokenKind::Integer(1), TokenKind::Integer(2)]);

        test_parsing("12345", vec![TokenKind::Integer(12345)]);

        test_parsing(
            "12y45",
            vec![
                TokenKind::Integer(12),
                TokenKind::Unknown,
                TokenKind::Integer(45),
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
            ],
        );
    }

    // Tree tests

    fn test_trees(text: &str, target: Element) {
        let tokens = parse_str(text);
        let tree = construct(tokens);

        // Wrap in a list to mimic the global list produced during construction
        let wrapped_target = Element::List(vec![target]);

        assert_eq!(tree, wrapped_target)
    }

    #[test]
    pub fn lists() {
        test_trees("()", Element::List(vec![]));

        test_trees("(1)", Element::List(vec![Element::Integer(1)]));

        test_trees(
            "(1 2 3)",
            Element::List(vec![
                Element::Integer(1),
                Element::Integer(2),
                Element::Integer(3),
            ]),
        );

        test_trees(
            "(1 2 (3 4 5))",
            Element::List(vec![
                Element::Integer(1),
                Element::Integer(2),
                Element::List(vec![
                    Element::Integer(3),
                    Element::Integer(4),
                    Element::Integer(5),
                ]),
            ]),
        );
    }
}
