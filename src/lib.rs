// Error strings

const MISMATCHED_PAREN_ERROR: &str = "Closing parenthesis without opening parenthesis";
const UNCLOSED_PAREN_ERROR: &str = "Unclosed parenthesis";
const DOUBLE_QUOTE_ERROR: &str = "Double quoting is not allowed";

// Parsing types and functions

#[derive(PartialEq, Debug)]
pub enum TokenKind {
    LeftParen,
    RightParen,
    Atom(String),
    Quote(String),
    Integer(i64),
    Error(String),
}

impl TokenKind {
    pub fn atom(inner: &str) -> TokenKind {
        TokenKind::Atom(inner.to_string())
    }

    pub fn quote(inner: &str) -> TokenKind {
        TokenKind::Quote(inner.to_string())
    }

    pub fn error(msg: &str) -> TokenKind {
        TokenKind::Error(msg.to_string())
    }
}

#[derive(PartialEq, Debug)]
pub struct Token {
    kind: TokenKind,
}

impl Token {
    pub fn new(kind: TokenKind) -> Token {
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
            _ if c.is_digit(10) => {
                let mut num = String::from(c);
                let mut bad_char = None;

                while let Some(&p) = chars.peek() {
                    match p {
                        _ if p.is_digit(10) => {
                            num.push(p);
                            chars.next();
                        }
                        '(' | ')' => break,
                        _ if p.is_whitespace() => break,
                        _ => {
                            bad_char = Some(p);
                            break;
                        }
                    }
                }

                if let Some(c) = bad_char {
                    Some(TokenKind::Error(format!(
                        "'{c}' cannot be part of an integer",
                    )))
                } else {
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
            }
            _ => {
                let is_quote = c == '\'';

                let mut inner = if is_quote {
                    String::new()
                } else {
                    String::from(c)
                };

                if is_quote && chars.peek() == Some(&'\'') {
                    Some(TokenKind::error(DOUBLE_QUOTE_ERROR))
                } else {
                    while let Some(&p) = chars.peek() {
                        match p {
                            '(' | ')' => break,
                            _ if p.is_whitespace() => break,
                            _ => {
                                inner.push(p);
                                chars.next();
                            }
                        }
                    }

                    if is_quote {
                        Some(TokenKind::Quote(inner))
                    } else {
                        Some(TokenKind::Atom(inner))
                    }
                }
            }
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

#[derive(PartialEq, Debug, Clone)]
pub enum Element {
    List(Vec<Element>),
    Integer(i64),
    Atom(String),
    Quote(String),
}

impl Element {
    pub fn atom(inner: &str) -> Element {
        Element::Atom(inner.to_string())
    }

    pub fn quote(inner: &str) -> Element {
        Element::Quote(inner.to_string())
    }
}

pub fn construct_list(tokens: &mut std::vec::IntoIter<Token>) -> Element {
    let mut children = Vec::new();

    while let Some(token) = tokens.next() {
        match token.kind {
            TokenKind::Atom(inner) => children.push(Element::Atom(inner)),
            TokenKind::Quote(inner) => children.push(Element::Quote(inner)),
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

// Eval

pub fn eval_function(f: Element, args: Vec<Element>) -> Element {
    match f {
        Element::Atom(func) => match func.as_str() {
            "cons" => Element::List(args.into_iter().map(|c| eval(c)).collect()),
            "quote" => {
                if args.len() == 1 {
                    let first = &args[0];
                    match first {
                        Element::Quote(inner) | Element::Atom(inner) => {
                            Element::Quote(inner.clone())
                        }
                        _ => panic!("Cannot quote {first:?}"),
                    }
                } else {
                    Element::List(
                        args.into_iter()
                            .map(|arg| match arg {
                                Element::Quote(inner) | Element::Atom(inner) => {
                                    Element::Quote(inner)
                                }
                                _ => panic!("Cannot quote {arg:?}"),
                            })
                            .collect(),
                    )
                }
            }
            _ => todo!(), // Add lookup when variables are added
        },
        _ => panic!("Cannot evaluate non-atom function: \"{f:?}\""),
    }
}

pub fn eval(e: Element) -> Element {
    match e {
        Element::List(children) => {
            let mut children_iter = children.into_iter().peekable();
            let first = children_iter.next();

            if let Some(first) = first {
                match first {
                    Element::Atom(_) => eval_function(first, children_iter.collect()),
                    Element::List(_) => {
                        let first_evaluated = eval(first);

                        if let Element::Atom(_) = first_evaluated {
                            eval_function(first_evaluated, children_iter.collect())
                        } else {
                            Element::List(
                                [
                                    vec![first_evaluated],
                                    children_iter.map(|c| eval(c)).collect(),
                                ]
                                .concat(),
                            )
                        }
                    }
                    _ => Element::List(
                        [vec![eval(first)], children_iter.map(|c| eval(c)).collect()].concat(),
                    ),
                }
            } else {
                Element::List(vec![])
            }
        }
        Element::Quote(inner) => Element::Atom(inner),
        _ => e,
    }
}

// Tests

#[cfg(test)]
mod tests {
    use super::*;

    // Parser tests

    fn test_parsing(text: &str, target: Vec<TokenKind>) {
        let output = parse_str(text);

        for (t, k) in output.into_iter().zip(target.into_iter()) {
            assert_eq!(t.kind, k);
        }
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
            vec![TokenKind::error("'y' cannot be part of an integer")],
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

    #[test]
    fn atoms_and_quotes() {
        test_parsing("a", vec![TokenKind::atom("a")]);

        test_parsing(
            "a '+ _",
            vec![
                TokenKind::atom("a"),
                TokenKind::quote("+"),
                TokenKind::atom("_"),
            ],
        );

        test_parsing("abc", vec![TokenKind::atom("abc")]);

        test_parsing("'ab1cd", vec![TokenKind::quote("ab1cd")]);

        test_parsing(
            "(a '+ '_)",
            vec![
                TokenKind::LeftParen,
                TokenKind::atom("a"),
                TokenKind::quote("+"),
                TokenKind::quote("_"),
                TokenKind::RightParen,
            ],
        );

        test_parsing(
            "(a '+ (c d e))",
            vec![
                TokenKind::LeftParen,
                TokenKind::atom("a"),
                TokenKind::quote("+"),
                TokenKind::LeftParen,
                TokenKind::atom("c"),
                TokenKind::atom("d"),
                TokenKind::atom("e"),
                TokenKind::RightParen,
                TokenKind::RightParen,
            ],
        );
    }

    // Construction tests

    fn test_construction(text: &str, target: Element) {
        let tokens = parse_str(text);
        let tree = construct(tokens);

        // Wrap in a list to mimic the global list produced during construction
        let wrapped_target = Element::List(vec![target]);

        assert_eq!(tree, wrapped_target)
    }

    #[test]
    fn construction() {
        test_construction("()", Element::List(vec![]));

        test_construction("(1)", Element::List(vec![Element::Integer(1)]));

        test_construction(
            "(1 2 3)",
            Element::List(vec![
                Element::Integer(1),
                Element::Integer(2),
                Element::Integer(3),
            ]),
        );

        test_construction(
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

        test_construction(
            "(+ (+ 1 2) (+ 3 4))",
            Element::List(vec![
                Element::atom("+"),
                Element::List(vec![
                    Element::atom("+"),
                    Element::Integer(1),
                    Element::Integer(2),
                ]),
                Element::List(vec![
                    Element::atom("+"),
                    Element::Integer(3),
                    Element::Integer(4),
                ]),
            ]),
        );

        test_construction(
            "((head ('print '+ '-)) 1 2 3)",
            Element::List(vec![
                Element::List(vec![
                    Element::atom("head"),
                    Element::List(vec![
                        Element::quote("print"),
                        Element::quote("+"),
                        Element::quote("-"),
                    ]),
                ]),
                Element::Integer(1),
                Element::Integer(2),
                Element::Integer(3),
            ]),
        );
    }

    // Eval tests

    fn test_eval(text: &str, target: Element) {
        let tokens = parse_str(text);
        let tree = construct(tokens);
        let result = eval(tree);

        assert_eq!(result, target)
    }

    #[test]
    fn basic_evals() {
        test_eval("", Element::List(vec![]));

        test_eval(
            "1 (2 3)",
            Element::List(vec![
                Element::Integer(1),
                Element::List(vec![Element::Integer(2), Element::Integer(3)]),
            ]),
        );
    }

    #[test]
    fn cons() {
        test_eval(
            "(cons 3) (cons (3 2) ('print 1))",
            Element::List(vec![
                Element::List(vec![Element::Integer(3)]),
                Element::List(vec![
                    Element::List(vec![Element::Integer(3), Element::Integer(2)]),
                    Element::List(vec![Element::atom("print"), Element::Integer(1)]),
                ]),
            ]),
        );
    }

    #[test]
    fn quote() {
        test_eval("(quote a)", Element::List(vec![Element::quote("a")]));
        test_eval(
            "(quote a 'b c)",
            Element::List(vec![Element::List(vec![
                Element::quote("a"),
                Element::quote("b"),
                Element::quote("c"),
            ])]),
        );
    }
}
