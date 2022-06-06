use std::fs::read_to_string;
use rcc::lexer::{Lexer, LexemeFeed};
use rcc::parser::Parse;
use rcc::ast::CProgram;

#[test]
fn parse_hello_world() {
    let path = "./tests/parser_files/hello-world-preprocessed.c";
    let contents = read_to_string(path).unwrap();

    let mut lexer = Lexer::new();
    let lexemes = lexer.lex(&contents);
    let iter = Box::new(lexemes.into_iter());
    let mut pump = LexemeFeed::from_iter(iter);

    let _parse_tree = CProgram::parse(&mut pump).unwrap();
}
