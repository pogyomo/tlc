use tlc_lexer::Lexer;
use tlc_parser::Parser;

fn main() {
    let path = std::env::args().nth(1).unwrap();
    let s = std::fs::read_to_string(path).unwrap();

    let lexer = Lexer::new(s.as_str());
    let parser = Parser::new(lexer).unwrap();

    match parser.parse() {
        Ok(ast) => println!("{:#?}", ast),
        Err(e) => {
            println!("{:#?}", e);
            println!("{}", &s[e.span.start..e.span.end]);
        }
    }
}
