use rcc::lexer::Lexer;
use rcc::ast::CProgram;
use rcc::parser::Parse;
use rcc::lexer::LexemeFeed;

fn main() {
    let input = r#"
int main() {
    sqrt();
    func(a, b, c, &a);
}
"#;
    let mut lexer = Lexer::new();

    let lexemes = lexer.lex(input);
    println!("{:?}", lexemes);
    let mut feed = LexemeFeed::from_iter(Box::new(lexemes.into_iter()));
    let prog = CProgram::parse(&mut feed);
}

