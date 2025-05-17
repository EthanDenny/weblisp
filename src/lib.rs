use std::{collections::HashMap, fmt, rc::Rc};

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
            ' ' | '\n' => None,
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

impl fmt::Display for NodeValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NodeValue::Integer(n) => write!(f, "{}", n),
            NodeValue::Atom(s) => write!(f, "{}", s),
            NodeValue::Quote(s) => write!(f, "'{}", s),
            NodeValue::Nil => write!(f, "nil"),
            NodeValue::List(node) => {
                write!(f, "(")?;
                let mut current = Some(Rc::clone(node));
                let mut first = true;
                while let Some(rc_node) = current {
                    if !first {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", rc_node.value)?;
                    current = rc_node.next.clone();
                    first = false;
                }
                write!(f, ")")
            }
        }
    }
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

// Eval types functions

#[derive(Debug, Clone)]
pub struct FuncDef {
    args: Vec<String>,
    body: NodeValue,
}

#[derive(Debug, Clone)]
pub enum Var {
    Value(NodeValue),
    Func(FuncDef),
    Empty,
}

#[derive(Debug, Clone)]
pub struct Scope {
    vars: HashMap<String, Var>,
}

impl Scope {
    pub fn new() -> Scope {
        Scope {
            vars: HashMap::new(),
        }
    }

    pub fn set(&mut self, name: String, value: NodeValue) {
        self.vars.insert(name, Var::Value(value));
    }

    pub fn def(&mut self, name: String, args: Vec<String>, body: NodeValue) {
        self.vars.insert(name, Var::Func(FuncDef { args, body }));
    }

    pub fn get(&mut self, name: String) -> Var {
        self.vars.get(&name).cloned().unwrap_or(Var::Empty)
    }

    pub fn make_child(&mut self) -> Scope {
        self.clone()
    }
}

pub fn extract_args(arg_head: Option<Rc<Node>>) -> Vec<NodeValue> {
    if let Some(args) = arg_head {
        let args_ref = args.as_ref();

        let head_value = args_ref.value.clone();
        let head_vec = vec![head_value];

        let tail_vec = extract_args(args_ref.next.clone());

        if tail_vec.len() > 0 {
            [head_vec, tail_vec].concat()
        } else {
            head_vec
        }
    } else {
        vec![]
    }
}

pub fn eval_macro(macro_name: &str, args_list: Option<Rc<Node>>, scope: &mut Scope) -> Node {
    let args = extract_args(args_list);

    match macro_name {
        "let" => {
            if let (Some(NodeValue::Atom(var_name)), Some(var_value_raw)) =
                (args.get(0), args.get(1))
            {
                let var_value = eval_value(var_value_raw.clone(), scope);

                scope.set(var_name.clone(), var_value.clone());

                Node::leaf(var_value)
            } else {
                panic!("Cannot set variable, incorrect argument types")
            }
        }
        "def" => {
            if let (
                Some(NodeValue::Atom(func_name)),
                Some(NodeValue::List(func_args)),
                Some(NodeValue::List(func_body)),
            ) = (args.get(0), args.get(1), args.get(2))
            {
                let func_arg_names = extract_args(Some(func_args.clone()))
                    .into_iter()
                    .map(|arg| {
                        if let NodeValue::Atom(arg_name) = arg {
                            arg_name
                        } else {
                            panic!("Expected an atom for func arg")
                        }
                    })
                    .collect();

                scope.def(func_name.clone(), func_arg_names, func_body.value.clone())
            } else {
                panic!("Cannot declare function, incorrect argument types")
            }

            Node::nil()
        }
        _ => unreachable!(),
    }
}

pub fn eval_builtin(func_name: String, args_list: Option<Rc<Node>>, scope: &mut Scope) -> Node {
    let func_str = func_name.as_str();

    // Escape to special-case macro land if necessary
    // Using if-let here because it will be nice to add future macros
    if let "let" | "def" = func_str {
        return eval_macro(func_str, args_list, scope);
    }

    // Back to our regularly scheduled programming

    let args = extract_args(args_list);

    match func_str {
        "if" => {
            if args.len() == 3 {
                let predicate = eval_value(args[0].clone(), scope);

                let branch = match predicate {
                    NodeValue::Nil | NodeValue::Integer(0) => &args[2],
                    _ => &args[1],
                };

                let result = eval_value(branch.clone(), scope);

                Node::leaf(result)
            } else {
                panic!("Expected three arguments")
            }
        }
        "+" | "*" | "/" | "<" | ">" | "<=" | ">=" => {
            if args.len() == 2 {
                if let (NodeValue::Integer(v0), NodeValue::Integer(v1)) = (
                    eval_value(args[0].clone(), scope),
                    eval_value(args[1].clone(), scope),
                ) {
                    let result = match func_str {
                        "+" => NodeValue::Integer(v0 + v1),
                        "*" => NodeValue::Integer(v0 * v1),
                        "/" => NodeValue::Integer(v0 / v1),
                        "<" => NodeValue::Integer(if v0 < v1 { 1 } else { 0 }),
                        ">" => NodeValue::Integer(if v0 > v1 { 1 } else { 0 }),
                        "<=" => NodeValue::Integer(if v0 <= v1 { 1 } else { 0 }),
                        ">=" => NodeValue::Integer(if v0 >= v1 { 1 } else { 0 }),
                        _ => unreachable!(),
                    };

                    Node::leaf(result)
                } else {
                    panic!("Expected two integer arguments")
                }
            } else {
                panic!("Expected two arguments")
            }
        }
        "=" => {
            if args.len() == 2 {
                let (v0, v1) = (
                    eval_value(args[0].clone(), scope),
                    eval_value(args[1].clone(), scope),
                );

                Node::leaf(NodeValue::Integer(if v0 == v1 { 1 } else { 0 }))
            } else {
                panic!("Expected two arguments")
            }
        }
        "-" => match args.len() {
            1 => {
                if let NodeValue::Integer(v0) = eval_value(args[0].clone(), scope) {
                    Node::leaf(NodeValue::Integer(-v0))
                } else {
                    panic!("Expected one integer argument")
                }
            }
            2 => {
                if let (NodeValue::Integer(v0), NodeValue::Integer(v1)) = (
                    eval_value(args[0].clone(), scope),
                    eval_value(args[1].clone(), scope),
                ) {
                    Node::leaf(NodeValue::Integer(v0 - v1))
                } else {
                    panic!("Expected two integer arguments")
                }
            }
            _ => panic!("Expected one or two arguments"),
        },
        _ => panic!("'{}' is not a builtin", func_name),
    }
}

pub fn eval_fn(func_def: FuncDef, args_list: Option<Rc<Node>>, scope: &mut Scope) -> Node {
    let arg_names = func_def.args;
    let body = func_def.body;
    let args = extract_args(args_list);

    let mut child_scope = scope.make_child();

    for (name, value) in arg_names.into_iter().zip(args.into_iter()) {
        child_scope.set(name, eval_value(value, scope))
    }

    eval(Node::leaf(body), &mut child_scope)
}

pub fn eval_value(value: NodeValue, scope: &mut Scope) -> NodeValue {
    eval(Node::leaf(value), scope).value
}

pub fn eval(mut expr: Node, scope: &mut Scope) -> Node {
    match &expr.value {
        NodeValue::Atom(atom) => match scope.get(atom.clone()) {
            Var::Value(value) => Node::leaf(value),
            Var::Func(func_def) => eval_fn(func_def, expr.next.clone(), scope),
            Var::Empty => eval_builtin(atom.clone(), expr.next.clone(), scope),
        },
        NodeValue::List(ptr) => eval(ptr.as_ref().clone(), scope),
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

        let mut scope = Scope::new();
        let result: Vec<Node> = block
            .into_iter()
            .map(|expr| eval(expr, &mut scope))
            .collect();

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

    #[test]
    fn negation() {
        test_eval("(- 2)", vec![Node::leaf(NodeValue::Integer(-2))]);
        test_eval("(- 0)", vec![Node::leaf(NodeValue::Integer(0))]);
        test_eval("(+ 3 (- 2))", vec![Node::leaf(NodeValue::Integer(1))]);
    }

    #[test]
    fn assignment() {
        test_eval(
            "(let x 2) (+ x 2)",
            vec![
                Node::leaf(NodeValue::Integer(2)),
                Node::leaf(NodeValue::Integer(4)),
            ],
        );

        test_eval(
            "(let x 3) (+ x x)",
            vec![
                Node::leaf(NodeValue::Integer(3)),
                Node::leaf(NodeValue::Integer(6)),
            ],
        );

        test_eval(
            "(let x 2) (+ (/ 2 2) x)",
            vec![
                Node::leaf(NodeValue::Integer(2)),
                Node::leaf(NodeValue::Integer(3)),
            ],
        );
    }

    #[test]
    fn func_def() {
        test_eval(
            "(def square (x) ((* x x))) (square 3)",
            vec![Node::nil(), Node::leaf(NodeValue::Integer(9))],
        );
    }

    #[test]
    fn if_else() {
        test_eval("(if 1 1 2)", vec![Node::leaf(NodeValue::Integer(1))]);
        test_eval("(if (+ 1 2) 1 2)", vec![Node::leaf(NodeValue::Integer(1))]);

        test_eval("(if 0 1 2)", vec![Node::leaf(NodeValue::Integer(2))]);
        test_eval("(if () 1 2)", vec![Node::leaf(NodeValue::Integer(2))]);
    }

    #[test]
    fn recursion() {
        test_eval(
            "
            (def fib (n) (
                (if (< n 2)
                    n
                    (+ (fib (- n 1)) (fib (- n 2)))
                )
            ))

            (fib 0)
            (fib 1)
            (fib 2)
            (fib 3)
            (fib 4)
            (fib 5)
            ",
            vec![
                Node::nil(),
                Node::leaf(NodeValue::Integer(0)),
                Node::leaf(NodeValue::Integer(1)),
                Node::leaf(NodeValue::Integer(1)),
                Node::leaf(NodeValue::Integer(2)),
                Node::leaf(NodeValue::Integer(3)),
                Node::leaf(NodeValue::Integer(5)),
            ],
        );

        test_eval(
            "
            (def pow (x y) (
                (if (= y 0)
                    1
                    (* x (pow x (- y 1)))
                )
            ))

            (pow 2 0)
            (pow 3 1)
            (pow 3 2)
            (pow 2 3)
            ",
            vec![
                Node::nil(),
                Node::leaf(NodeValue::Integer(1)),
                Node::leaf(NodeValue::Integer(3)),
                Node::leaf(NodeValue::Integer(9)),
                Node::leaf(NodeValue::Integer(8)),
            ],
        )
    }
}
