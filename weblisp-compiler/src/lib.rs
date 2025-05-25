use std::{collections::HashMap, fmt};
use wasm_bindgen::prelude::*;

// Error strings

const MISMATCHED_PAREN_ERROR: &str = "Closing parenthesis without opening parenthesis";
const UNCLOSED_PAREN_ERROR: &str = "Unclosed parenthesis";

// Parsing types and functions

#[derive(PartialEq, Debug)]
pub enum TokenKind {
    LeftParen,
    RightParen,
    Atom(String),
    Number(f64),
    String(String),
    Error(String),
}

impl TokenKind {
    pub fn atom(inner: &str) -> TokenKind {
        TokenKind::Atom(inner.to_string())
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
                TokenKind::LeftParen
            }
            ')' => {
                if paren_level > 0 {
                    paren_level -= 1;
                    TokenKind::RightParen
                } else {
                    TokenKind::error(MISMATCHED_PAREN_ERROR)
                }
            }
            '"' => {
                let mut str = String::new();

                while let Some(p) = chars.next() {
                    if p != '"' {
                        str.push(p);
                    } else {
                        break;
                    }
                }

                TokenKind::String(str)
            }
            _ if c.is_digit(10) => {
                let mut num = String::from(c);
                let mut bad_char = None;

                while let Some(&p) = chars.peek() {
                    let mut push_p = || {
                        num.push(p);
                        chars.next();
                    };

                    match p {
                        _ if p.is_digit(10) => push_p(),
                        '.' => push_p(),
                        '(' | ')' => break,
                        _ if p.is_whitespace() => break,
                        _ => {
                            bad_char = Some(p);
                            break;
                        }
                    }
                }

                if let Some(c) = bad_char {
                    TokenKind::Error(format!("'{c}' cannot be part of an number",))
                } else {
                    let parsed = num.parse::<f64>();

                    match parsed {
                        Ok(v) => TokenKind::Number(v),

                        // TODO: Figure out a way to test this (shoould theoretically never trigger)
                        Err(_) => TokenKind::Error(format!(
                            "Tried to parse number \"{}\", but something went wrong",
                            num,
                        )),
                    }
                }
            }
            _ if c.is_whitespace() => continue,
            _ => {
                if let ('/', Some(&'/')) = (c, chars.peek()) {
                    chars.next();

                    while let Some(p) = chars.next() {
                        if p == '\n' {
                            break;
                        }
                    }

                    continue;
                } else {
                    let mut inner = String::from(c);

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

                    TokenKind::Atom(inner)
                }
            }
        };

        if let TokenKind::Error(_) = kind {
            return vec![Token::new(kind)];
        }

        tokens.push(Token::new(kind));
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
    List(Box<Node>),
    Number(f64),
    Atom(String),
    Lambda(FuncDef),
    Builtin(String),
    String(String),
    Nil,
}

impl fmt::Display for NodeValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NodeValue::Number(n) => write!(f, "{}", n),
            NodeValue::Atom(s) => write!(f, "{}", s),
            NodeValue::Builtin(s) => write!(f, "{}", s),
            NodeValue::Nil => write!(f, "nil"),
            NodeValue::Lambda(_) => write!(f, "lambda"),
            NodeValue::String(s) => write!(f, "{}", s),
            NodeValue::List(node) => write!(f, "({})", node),
        }
    }
}

impl NodeValue {
    pub fn atom(value: &str) -> NodeValue {
        NodeValue::Atom(value.to_string())
    }

    pub fn string(value: &str) -> NodeValue {
        NodeValue::String(value.to_string())
    }

    pub fn list(elems: &[NodeValue]) -> NodeValue {
        let node = Node::list(elems);

        match node.value {
            NodeValue::Nil => NodeValue::Nil,
            _ => NodeValue::List(Box::new(node)),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Node {
    value: NodeValue,
    next: Option<Box<Node>>,
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut current = Some(Box::new(self.clone()));
        let mut first = true;
        while let Some(ptr) = current {
            if !first {
                write!(f, " ")?;
            }
            write!(f, "{}", ptr.value)?;
            current = ptr.next.clone();
            first = false;
        }
        Ok(())
    }
}

impl Node {
    fn new(value: NodeValue, next: Node) -> Node {
        Node {
            value,
            next: Some(next.into()),
        }
    }

    fn leaf(value: NodeValue) -> Node {
        Node { value, next: None }
    }

    fn nil() -> Node {
        Node::leaf(NodeValue::Nil)
    }

    fn list(elems: &[NodeValue]) -> Node {
        if elems.len() > 1 {
            Node::new(elems[0].clone(), Node::list(&elems[1..]))
        } else if elems.len() == 1 {
            Node::leaf(elems[0].clone())
        } else {
            Node::nil()
        }
    }
}

pub fn construct_value(kind: TokenKind) -> NodeValue {
    match kind {
        TokenKind::Atom(inner) => NodeValue::Atom(inner),
        TokenKind::Number(inner) => NodeValue::Number(inner),
        TokenKind::String(inner) => NodeValue::String(inner),
        TokenKind::LeftParen | TokenKind::RightParen | TokenKind::Error(_) => unreachable!(),
    }
}

pub fn construct_list(tokens: &mut std::vec::IntoIter<Token>) -> Result<Vec<NodeValue>, String> {
    let mut nodes = Vec::new();

    while let Some(t) = tokens.next() {
        let node = match t.kind {
            TokenKind::LeftParen => NodeValue::list(&construct_list(tokens)?),
            TokenKind::RightParen => break,
            TokenKind::Error(msg) => return Err(msg),
            kind => construct_value(kind),
        };

        nodes.push(node);
    }

    Ok(nodes)
}

pub fn construct(tokens: Vec<Token>) -> Result<Vec<Node>, String> {
    let mut tok_iter = tokens.into_iter();
    let mut nodes = Vec::new();

    while let Some(t) = tok_iter.next() {
        let node = match t.kind {
            TokenKind::LeftParen => Node::list(&construct_list(&mut tok_iter)?),
            TokenKind::RightParen => unreachable!(),
            TokenKind::Error(msg) => panic!("{}", msg),
            kind => Node::leaf(construct_value(kind)),
        };

        nodes.push(node);
    }

    Ok(nodes)
}

// Eval types functions

#[derive(Debug, Clone, PartialEq)]
pub struct FuncDef {
    args: Vec<String>,
    body: Vec<NodeValue>,
}

#[derive(Debug, Clone)]
pub struct Scope {
    stdout: String,
    vars: HashMap<String, NodeValue>,
}

impl Scope {
    pub fn new() -> Scope {
        Scope {
            stdout: String::new(),
            vars: HashMap::new(),
        }
    }

    pub fn set(&mut self, name: String, value: NodeValue) {
        self.vars.insert(name, value);
    }

    pub fn def(&mut self, name: String, args: Vec<String>, body: Vec<NodeValue>) {
        self.vars
            .insert(name, NodeValue::Lambda(FuncDef { args, body }));
    }

    pub fn get(&mut self, name: String) -> Result<NodeValue, String> {
        match self.vars.get(&name).cloned() {
            Some(value) => Ok(value),
            None => Err(format!("Unrecognized symbol: {}", name)),
        }
    }

    pub fn make_child(&mut self) -> Scope {
        self.clone()
    }
}

pub fn extract_args(arg_head: Option<Box<Node>>) -> Vec<NodeValue> {
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

pub fn eval_macro(
    macro_name: &str,
    args_list: Option<Box<Node>>,
    scope: &mut Scope,
) -> Result<Node, String> {
    let args = extract_args(args_list);

    let node = match macro_name {
        "let" => {
            if let (Some(NodeValue::Atom(var_name)), Some(var_value_raw)) =
                (args.get(0), args.get(1))
            {
                let var_value = eval_value(var_value_raw.clone(), scope)?;

                scope.set(var_name.clone(), var_value.clone());

                Node::leaf(var_value)
            } else {
                return Err("Cannot set variable, incorrect argument types".to_string());
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
                            panic!("Expected an atom for func arg");
                        }
                    })
                    .collect();

                let func_body_exprs = extract_args(Some(func_body.clone()));

                scope.def(func_name.clone(), func_arg_names, func_body_exprs)
            } else {
                return Err("Cannot declare function, incorrect argument types".to_string());
            }

            Node::nil()
        }
        _ => unreachable!(),
    };

    Ok(node)
}

pub fn is_builtin(atom: &str) -> bool {
    match atom {
        "if" | "+" | "*" | "/" | "<" | ">" | "<=" | ">=" | "=" | "-" | "let" | "def" | "cons"
        | "print" | "println" => true,
        _ => false,
    }
}

pub fn eval_builtin(
    func_name: String,
    args_list: Option<Box<Node>>,
    scope: &mut Scope,
) -> Result<Node, String> {
    let func_str = func_name.as_str();

    // Escape to special-case macro land if necessary
    // Using if-let here because it will be nice to add future macros
    if let "let" | "def" = func_str {
        return eval_macro(func_str, args_list, scope);
    }

    // Back to our regularly scheduled programming

    let args = extract_args(args_list);

    let node = match func_str {
        "print" | "println" => {
            for arg in args {
                let value = eval_value(arg, scope)?;

                if scope.stdout.len() > 0 && scope.stdout.chars().last() != Some('\n') {
                    scope.stdout += " "
                }

                scope.stdout += format!("{}", value).as_str();
            }

            if func_str == "println" {
                scope.stdout += "\n";
            }

            Node::nil()
        }
        "cons" => {
            if args.len() == 2 {
                let head = eval_value(args[0].clone(), scope)?;
                let tail = eval_value(args[1].clone(), scope)?;
                Node::new(head, Node::leaf(tail))
            } else {
                return Err("Expected two arguments".to_string());
            }
        }
        "if" => {
            if args.len() == 3 {
                let predicate = eval_value(args[0].clone(), scope)?;

                let branch = match predicate {
                    NodeValue::Nil | NodeValue::Number(0.0) => &args[2],
                    _ => &args[1],
                };

                let result = eval_value(branch.clone(), scope)?;

                Node::leaf(result)
            } else {
                return Err("Expected three arguments".to_string());
            }
        }
        "+" | "*" | "/" | "<" | ">" | "<=" | ">=" => {
            if args.len() == 2 {
                if let (NodeValue::Number(v0), NodeValue::Number(v1)) = (
                    eval_value(args[0].clone(), scope)?,
                    eval_value(args[1].clone(), scope)?,
                ) {
                    let result = match func_str {
                        "+" => NodeValue::Number(v0 + v1),
                        "*" => NodeValue::Number(v0 * v1),
                        "/" => NodeValue::Number(v0 / v1),
                        "<" => NodeValue::Number(if v0 < v1 { 1.0 } else { 0.0 }),
                        ">" => NodeValue::Number(if v0 > v1 { 1.0 } else { 0.0 }),
                        "<=" => NodeValue::Number(if v0 <= v1 { 1.0 } else { 0.0 }),
                        ">=" => NodeValue::Number(if v0 >= v1 { 1.0 } else { 0.0 }),
                        _ => unreachable!(),
                    };

                    Node::leaf(result)
                } else {
                    return Err("Expected two number arguments".to_string());
                }
            } else {
                return Err("Expected two arguments".to_string());
            }
        }
        "=" => {
            if args.len() == 2 {
                let (v0, v1) = (
                    eval_value(args[0].clone(), scope),
                    eval_value(args[1].clone(), scope),
                );

                Node::leaf(NodeValue::Number(if v0 == v1 { 1.0 } else { 0.0 }))
            } else {
                return Err("Expected two arguments".to_string());
            }
        }
        "-" => match args.len() {
            1 => {
                if let NodeValue::Number(v0) = eval_value(args[0].clone(), scope)? {
                    Node::leaf(NodeValue::Number(-v0))
                } else {
                    return Err("Expected one number argument".to_string());
                }
            }
            2 => {
                if let (NodeValue::Number(v0), NodeValue::Number(v1)) = (
                    eval_value(args[0].clone(), scope)?,
                    eval_value(args[1].clone(), scope)?,
                ) {
                    Node::leaf(NodeValue::Number(v0 - v1))
                } else {
                    return Err("Expected two number arguments".to_string());
                }
            }
            _ => return Err("Expected one or two arguments".to_string()),
        },
        _ => return Err(format!("'{}' is not a builtin", func_name)),
    };

    Ok(node)
}

pub fn eval_fn(
    func_def: FuncDef,
    args_list: Option<Box<Node>>,
    scope: &mut Scope,
) -> Result<Node, String> {
    let arg_names = func_def.args;
    let body = func_def.body;
    let args = extract_args(args_list);

    let mut child_scope = scope.make_child();

    for (name, value) in arg_names.into_iter().zip(args.into_iter()) {
        child_scope.set(name, eval_value(value, scope)?)
    }

    let result: Vec<Result<NodeValue, String>> = body
        .into_iter()
        .map(|expr| eval_value(expr, &mut child_scope))
        .collect();

    if let Some(return_value) = result.last() {
        eval(Node::leaf(return_value.clone()?), &mut child_scope)
    } else {
        Ok(Node::nil())
    }
}

pub fn eval_value(value: NodeValue, scope: &mut Scope) -> Result<NodeValue, String> {
    Ok(match value {
        NodeValue::Atom(atom) => {
            if is_builtin(&atom) {
                NodeValue::Builtin(atom)
            } else {
                scope.get(atom)?
            }
        }
        NodeValue::List(ptr) => {
            let result_node = eval(ptr.as_ref().clone(), scope)?;
            match result_node.next {
                Some(_) => NodeValue::List(result_node.into()),
                None => result_node.value,
            }
        }
        _ => value,
    })
}

pub fn eval(node: Node, scope: &mut Scope) -> Result<Node, String> {
    let value = eval_value(node.value, scope)?;

    Ok(match node.next {
        Some(ptr) => match value {
            NodeValue::Builtin(func_name) => eval_builtin(func_name, Some(ptr), scope)?,
            NodeValue::Lambda(func_def) => eval_fn(func_def, Some(ptr), scope)?,
            _ => Node::new(value, eval(*ptr, scope)?),
        },
        None => Node::leaf(value),
    })
}

// WASM

#[wasm_bindgen]
pub fn wasm_eval(text: String) -> String {
    let inner_eval = || -> Result<String, String> {
        let tokens = parse(text);
        let block = construct(tokens)?;
        let mut scope = Scope::new();

        for expr in block {
            eval(expr, &mut scope)?;
        }

        Ok(scope.stdout)
    };

    match inner_eval() {
        Ok(stdout) => stdout,
        Err(msg) => msg,
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
        test_parsing("1", vec![TokenKind::Number(1.0)]);

        test_parsing("1 2", vec![TokenKind::Number(1.0), TokenKind::Number(2.0)]);

        test_parsing("12345", vec![TokenKind::Number(12345.0)]);

        test_parsing(
            "12y45",
            vec![TokenKind::error("'y' cannot be part of an number")],
        );

        test_parsing(
            "(1 2 3)",
            vec![
                TokenKind::LeftParen,
                TokenKind::Number(1.0),
                TokenKind::Number(2.0),
                TokenKind::Number(3.0),
                TokenKind::RightParen,
            ],
        );

        test_parsing(
            "(1 2 (3 4 5))",
            vec![
                TokenKind::LeftParen,
                TokenKind::Number(1.0),
                TokenKind::Number(2.0),
                TokenKind::LeftParen,
                TokenKind::Number(3.0),
                TokenKind::Number(4.0),
                TokenKind::Number(5.0),
                TokenKind::RightParen,
                TokenKind::RightParen,
            ],
        );
    }

    #[test]
    fn atoms() {
        test_parsing("a", vec![TokenKind::atom("a")]);

        test_parsing("abc", vec![TokenKind::atom("abc")]);

        test_parsing(
            "(a + _)",
            vec![
                TokenKind::LeftParen,
                TokenKind::atom("a"),
                TokenKind::atom("+"),
                TokenKind::atom("_"),
                TokenKind::RightParen,
            ],
        );

        test_parsing(
            "(a + (c d e))",
            vec![
                TokenKind::LeftParen,
                TokenKind::atom("a"),
                TokenKind::atom("+"),
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
        let block =
            construct(tokens).unwrap_or_else(|_| panic!("Got an error while testing construction"));
        assert_eq!(block, target)
    }

    #[test]
    fn construction() {
        test_construction("", vec![]);

        test_construction(
            "1 2 3",
            vec![
                Node::leaf(NodeValue::Number(1.0)),
                Node::leaf(NodeValue::Number(2.0)),
                Node::leaf(NodeValue::Number(3.0)),
            ],
        );

        test_construction("()", vec![Node::nil()]);

        test_construction("(1)", vec![Node::list(&[NodeValue::Number(1.0)])]);

        test_construction(
            "(0.1 2.1 3.)",
            vec![Node::list(&[
                NodeValue::Number(0.1),
                NodeValue::Number(2.1),
                NodeValue::Number(3.0),
            ])],
        );

        test_construction(
            "(1) (2) (3)",
            vec![
                Node::list(&[NodeValue::Number(1.0)]),
                Node::list(&[NodeValue::Number(2.0)]),
                Node::list(&[NodeValue::Number(3.0)]),
            ],
        );

        test_construction(
            "(1 2 (3 4 5))",
            vec![Node::list(&[
                NodeValue::Number(1.0),
                NodeValue::Number(2.0),
                NodeValue::list(&[
                    NodeValue::Number(3.0),
                    NodeValue::Number(4.0),
                    NodeValue::Number(5.0),
                ]),
            ])],
        );
    }

    // Eval tests

    fn test_eval(text: &str, target: Vec<Node>) {
        let tokens = parse_str(text);
        let block =
            construct(tokens).unwrap_or_else(|_| panic!("Got an error while testing evals"));

        let mut scope = Scope::new();
        let result: Vec<Node> = block
            .into_iter()
            .map(|expr| {
                eval(expr, &mut scope)
                    .unwrap_or_else(|_| panic!("Got an error while testing evals"))
            })
            .collect();

        assert_eq!(result, target)
    }

    #[test]
    fn basic_evals() {
        test_eval("", vec![]);

        test_eval(
            "1 2 3",
            vec![
                Node::leaf(NodeValue::Number(1.0)),
                Node::leaf(NodeValue::Number(2.0)),
                Node::leaf(NodeValue::Number(3.0)),
            ],
        );

        test_eval("()", vec![Node::nil()]);

        test_eval("(1)", vec![Node::list(&[NodeValue::Number(1.0)])]);

        test_eval(
            "(1 2 3)",
            vec![Node::list(&[
                NodeValue::Number(1.0),
                NodeValue::Number(2.0),
                NodeValue::Number(3.0),
            ])],
        );

        test_eval(
            "(1) (2) (3)",
            vec![
                Node::list(&[NodeValue::Number(1.0)]),
                Node::list(&[NodeValue::Number(2.0)]),
                Node::list(&[NodeValue::Number(3.0)]),
            ],
        );

        test_eval(
            "(1 2 (3 4 5))",
            vec![Node::list(&[
                NodeValue::Number(1.0),
                NodeValue::Number(2.0),
                NodeValue::list(&[
                    NodeValue::Number(3.0),
                    NodeValue::Number(4.0),
                    NodeValue::Number(5.0),
                ]),
            ])],
        );
    }

    #[test]
    fn complex_lists() {
        test_eval(
            "((3 4 5) 6 7)",
            vec![Node::list(&[
                NodeValue::list(&[
                    NodeValue::Number(3.0),
                    NodeValue::Number(4.0),
                    NodeValue::Number(5.0),
                ]),
                NodeValue::Number(6.0),
                NodeValue::Number(7.0),
            ])],
        );

        test_eval(
            "(cons ((3 4 5) 6) (cons 7 8))",
            vec![Node::list(&[
                NodeValue::list(&[
                    NodeValue::list(&[
                        NodeValue::Number(3.0),
                        NodeValue::Number(4.0),
                        NodeValue::Number(5.0),
                    ]),
                    NodeValue::Number(6.0),
                ]),
                NodeValue::list(&[NodeValue::Number(7.0), NodeValue::Number(8.0)]),
            ])],
        );
    }

    #[test]
    fn simple_arithmetic() {
        test_eval("(+ 2 2)", vec![Node::leaf(NodeValue::Number(4.0))]);
        test_eval("(- 2 2)", vec![Node::leaf(NodeValue::Number(0.0))]);
        test_eval("(* 2 2)", vec![Node::leaf(NodeValue::Number(4.0))]);
        test_eval("(/ 2 2)", vec![Node::leaf(NodeValue::Number(1.0))]);
    }

    #[test]
    fn chained_arithmetic() {
        test_eval("(+ 2 (- 2 2))", vec![Node::leaf(NodeValue::Number(2.0))]);
        test_eval("(- (* 2 2) 2)", vec![Node::leaf(NodeValue::Number(2.0))]);
        test_eval("(* 2 (/ 2 2))", vec![Node::leaf(NodeValue::Number(2.0))]);
        test_eval("(/ 2 (* 2 2))", vec![Node::leaf(NodeValue::Number(0.5))]);
    }

    #[test]
    fn negation() {
        test_eval("(- 2)", vec![Node::leaf(NodeValue::Number(-2.0))]);
        test_eval("(- 0)", vec![Node::leaf(NodeValue::Number(0.0))]);
        test_eval("(+ 3 (- 2))", vec![Node::leaf(NodeValue::Number(1.0))]);
    }

    #[test]
    fn assignment() {
        test_eval(
            "(let x 2) (+ x 2)",
            vec![
                Node::leaf(NodeValue::Number(2.0)),
                Node::leaf(NodeValue::Number(4.0)),
            ],
        );

        test_eval(
            "(let x 3) (+ x x)",
            vec![
                Node::leaf(NodeValue::Number(3.0)),
                Node::leaf(NodeValue::Number(6.0)),
            ],
        );

        test_eval(
            "(let x 2) (+ (/ 2 2) x)",
            vec![
                Node::leaf(NodeValue::Number(2.0)),
                Node::leaf(NodeValue::Number(3.0)),
            ],
        );
    }

    #[test]
    fn func_def() {
        test_eval(
            "(def square (x) ((* x x))) (square 3)",
            vec![Node::nil(), Node::leaf(NodeValue::Number(9.0))],
        );
    }

    #[test]
    fn if_else() {
        test_eval("(if 1 1 2)", vec![Node::leaf(NodeValue::Number(1.0))]);
        test_eval("(if (+ 1 2) 1 2)", vec![Node::leaf(NodeValue::Number(1.0))]);

        test_eval("(if 0 1 2)", vec![Node::leaf(NodeValue::Number(2.0))]);
        test_eval("(if () 1 2)", vec![Node::leaf(NodeValue::Number(2.0))]);
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
                Node::leaf(NodeValue::Number(0.0)),
                Node::leaf(NodeValue::Number(1.0)),
                Node::leaf(NodeValue::Number(1.0)),
                Node::leaf(NodeValue::Number(2.0)),
                Node::leaf(NodeValue::Number(3.0)),
                Node::leaf(NodeValue::Number(5.0)),
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
                Node::leaf(NodeValue::Number(1.0)),
                Node::leaf(NodeValue::Number(3.0)),
                Node::leaf(NodeValue::Number(9.0)),
                Node::leaf(NodeValue::Number(8.0)),
            ],
        )
    }

    #[test]
    fn assignment_in_function() {
        test_eval(
            "
            (def sqrt (x) (
                (def inner_sqrt (x n) (
                    (let n_squared (* n n))
                    (if (= n_squared x)
                        n
                        (if (> n_squared x)
                            (- n 1)
                            (inner_sqrt x (+ n 1))
                        )
                    )
                ))
                (inner_sqrt x 0)
            ))

            (sqrt 0)
            (sqrt 1)
            (sqrt 4)
            (sqrt 10)
            ",
            vec![
                Node::nil(),
                Node::leaf(NodeValue::Number(0.0)),
                Node::leaf(NodeValue::Number(1.0)),
                Node::leaf(NodeValue::Number(2.0)),
                Node::leaf(NodeValue::Number(3.0)),
            ],
        )
    }

    #[test]
    fn strings() {
        test_eval(
            "\"Hello, World\"",
            vec![Node::leaf(NodeValue::string("Hello, World"))],
        )
    }
}
