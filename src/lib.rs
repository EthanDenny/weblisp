use std::rc::Rc;

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
pub enum NodeValue {
    List(Rc<Node>),
    Integer(i64),
    Atom(String),
    Quote(String),
    Nil,
}

impl NodeValue {
    pub fn atom(value: &str) -> NodeValue {
        NodeValue::Atom(value.to_string())
    }

    pub fn quote(value: &str) -> NodeValue {
        NodeValue::Quote(value.to_string())
    }

    pub fn list(elems: &[NodeValue]) -> NodeValue {
        let node = Node::list(elems);

        match node.value {
            NodeValue::Nil => NodeValue::Nil,
            _ => NodeValue::List(Rc::new(node)),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Node {
    value: NodeValue,
    next: Option<Rc<Node>>,
}

impl Node {
    fn new(value: NodeValue, next: Option<Rc<Node>>) -> Node {
        Node { value, next }
    }

    fn leaf(value: NodeValue) -> Node {
        Node::new(value, None)
    }

    fn nil() -> Node {
        Node::leaf(NodeValue::Nil)
    }

    fn list(elems: &[NodeValue]) -> Node {
        if elems.len() > 1 {
            Node::new(elems[0].clone(), Some(Rc::new(Node::list(&elems[1..]))))
        } else if elems.len() == 1 {
            Node::new(elems[0].clone(), None)
        } else {
            Node::nil()
        }
    }
}

pub fn construct_value(kind: TokenKind) -> NodeValue {
    match kind {
        TokenKind::Atom(inner) => NodeValue::Atom(inner),
        TokenKind::Quote(inner) => NodeValue::Quote(inner),
        TokenKind::Integer(inner) => NodeValue::Integer(inner),
        _ => unreachable!(),
    }
}

pub fn construct_list(tokens: &mut std::vec::IntoIter<Token>) -> Vec<NodeValue> {
    let mut nodes = Vec::new();

    while let Some(t) = tokens.next() {
        let node = match t.kind {
            TokenKind::LeftParen => NodeValue::list(&construct_list(tokens)),
            TokenKind::RightParen => break,
            TokenKind::Error(msg) => panic!("{}", msg),
            kind => construct_value(kind),
        };

        nodes.push(node);
    }

    nodes
}

pub fn construct(tokens: Vec<Token>) -> Vec<Node> {
    let mut tok_iter = tokens.into_iter();
    let mut nodes = Vec::new();

    while let Some(t) = tok_iter.next() {
        let node = match t.kind {
            TokenKind::LeftParen => Node::list(&construct_list(&mut tok_iter)),
            TokenKind::RightParen => unreachable!(),
            TokenKind::Error(msg) => panic!("{}", msg),
            kind => Node::new(construct_value(kind), None),
        };

        nodes.push(node);
    }

    nodes
}

// Eval functions

pub fn eval_arg(arg: Option<NodeValue>) -> Option<NodeValue> {
    match arg {
        Some(NodeValue::List(ptr)) => Some(eval(ptr.as_ref().clone()).value),
        _ => arg,
    }
}

pub fn eval_fn(func_name: String, args: Option<Rc<Node>>) -> Node {
    let func_str = func_name.as_str();

    match func_str {
        "+" | "-" | "*" | "/" => {
            let mut arg0 = None;
            let mut arg1 = None;

            if let Some(args) = args {
                let args = args.as_ref();
                arg0 = Some(args.value.clone());
                if let Some(args) = &args.next {
                    let args = args.as_ref();
                    arg1 = Some(args.value.clone());
                }
            }

            let eval0 = eval_arg(arg0);
            let eval1 = eval_arg(arg1);

            if let (Some(NodeValue::Integer(v0)), Some(NodeValue::Integer(v1))) = (eval0, eval1) {
                let result = match func_str {
                    "+" => NodeValue::Integer(v0 + v1),
                    "-" => NodeValue::Integer(v0 - v1),
                    "*" => NodeValue::Integer(v0 * v1),
                    "/" => NodeValue::Integer(v0 / v1),
                    _ => unreachable!(),
                };
                Node::leaf(result)
            } else {
                panic!("Expected two integer arguments")
            }
        }
        _ => unimplemented!(),
    }
}

pub fn eval(mut expr: Node) -> Node {
    match &expr.value {
        NodeValue::Atom(func_name) => eval_fn(func_name.clone(), expr.next.clone()),
        NodeValue::List(ptr) => eval(ptr.as_ref().clone()),
        NodeValue::Quote(inner) => {
            expr.value = NodeValue::Atom(inner.clone());
            expr
        }
        _ => expr,
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

    fn test_construction(text: &str, target: Vec<Node>) {
        let tokens = parse_str(text);
        let block = construct(tokens);
        assert_eq!(block, target)
    }

    #[test]
    fn construction() {
        test_construction("", vec![]);

        test_construction(
            "1 2 3",
            vec![
                Node::new(NodeValue::Integer(1), None),
                Node::new(NodeValue::Integer(2), None),
                Node::new(NodeValue::Integer(3), None),
            ],
        );

        test_construction("()", vec![Node::nil()]);

        test_construction("(1)", vec![Node::list(&[NodeValue::Integer(1)])]);

        test_construction(
            "(1 2 3)",
            vec![Node::list(&[
                NodeValue::Integer(1),
                NodeValue::Integer(2),
                NodeValue::Integer(3),
            ])],
        );

        test_construction(
            "(1) (2) (3)",
            vec![
                Node::list(&[NodeValue::Integer(1)]),
                Node::list(&[NodeValue::Integer(2)]),
                Node::list(&[NodeValue::Integer(3)]),
            ],
        );

        test_construction(
            "(1 2 (3 4 5))",
            vec![Node::list(&[
                NodeValue::Integer(1),
                NodeValue::Integer(2),
                NodeValue::list(&[
                    NodeValue::Integer(3),
                    NodeValue::Integer(4),
                    NodeValue::Integer(5),
                ]),
            ])],
        );
    }

    // Eval tests

    fn test_eval(text: &str, target: Vec<Node>) {
        let tokens = parse_str(text);
        let block = construct(tokens);
        let result: Vec<Node> = block.into_iter().map(|expr| eval(expr)).collect();

        assert_eq!(result, target)
    }

    #[test]
    fn basic_evals() {
        test_eval("", vec![]);

        test_eval(
            "1 2 3",
            vec![
                Node::leaf(NodeValue::Integer(1)),
                Node::leaf(NodeValue::Integer(2)),
                Node::leaf(NodeValue::Integer(3)),
            ],
        );

        test_eval("()", vec![Node::nil()]);

        test_eval("(1)", vec![Node::list(&[NodeValue::Integer(1)])]);

        test_eval(
            "(1 2 3)",
            vec![Node::list(&[
                NodeValue::Integer(1),
                NodeValue::Integer(2),
                NodeValue::Integer(3),
            ])],
        );

        test_eval(
            "(1) (2) (3)",
            vec![
                Node::list(&[NodeValue::Integer(1)]),
                Node::list(&[NodeValue::Integer(2)]),
                Node::list(&[NodeValue::Integer(3)]),
            ],
        );

        test_eval(
            "(1 2 (3 4 5))",
            vec![Node::list(&[
                NodeValue::Integer(1),
                NodeValue::Integer(2),
                NodeValue::list(&[
                    NodeValue::Integer(3),
                    NodeValue::Integer(4),
                    NodeValue::Integer(5),
                ]),
            ])],
        );
    }

    #[test]
    fn simple_arithmetic() {
        test_eval("(+ 2 2)", vec![Node::leaf(NodeValue::Integer(4))]);
        test_eval("(- 2 2)", vec![Node::leaf(NodeValue::Integer(0))]);
        test_eval("(* 2 2)", vec![Node::leaf(NodeValue::Integer(4))]);
        test_eval("(/ 2 2)", vec![Node::leaf(NodeValue::Integer(1))]);
    }

    #[test]
    fn chained_arithmetic() {
        test_eval("(+ 2 (- 2 2))", vec![Node::leaf(NodeValue::Integer(2))]);
        test_eval("(- (* 2 2) 2)", vec![Node::leaf(NodeValue::Integer(2))]);
        test_eval("(* 2 (/ 2 2))", vec![Node::leaf(NodeValue::Integer(2))]);
        test_eval("(/ 2 (* 2 2))", vec![Node::leaf(NodeValue::Integer(0))]);
    }
}
