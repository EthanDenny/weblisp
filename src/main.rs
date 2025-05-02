use mylisplib::*;

fn format_element(root: Element) -> String {
    match root {
        Element::Quote(inner) | Element::Atom(inner) => format!("{inner}"),
        Element::Integer(v) => format!("{v}"),
        Element::List(children) => {
            format!(
                "({})",
                children
                    .into_iter()
                    .map(|c| format_element(c))
                    .collect::<Vec<String>>()
                    .join(" ")
            )
        }
    }
}

fn format_tree(root: Element) -> String {
    if let Element::List(children) = root {
        children
            .into_iter()
            .map(|c| format_element(c))
            .collect::<Vec<String>>()
            .join("")
    } else {
        format_element(root)
    }
}

fn execute(text: &str) {
    let tokens = parse_str(text);
    let tree = construct(tokens);
    let result = eval(tree);

    println!("FINAL TREE:");
    println!("{}", format_tree(result));
}

fn main() {
    execute("(cons 1 2 (cons 3 2))\n(quote a 'b)\n(print 1 2)");
}
