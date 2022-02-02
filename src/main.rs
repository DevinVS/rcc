use rcc::lexer::Lexer;
use rcc::ast::CProgram;
use rcc::parser::Parse;

fn main() {
    let input = r#"int main(int argc, char ** argv) {}"#;

    let mut lexer = Lexer::new();

    let lexemes = lexer.lex(input);
    println!("Lexemes:\n{lexemes:#?}");
    let prog = CProgram::parse(&mut lexemes.into_iter().peekable());
    println!("Parse Tree:\n{prog:#?}");
}
