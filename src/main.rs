use std::fs::read_to_string;

use rcc::lexer::Lexer;
use rcc::ast::CProgram;
use rcc::parser::Parse;
use rcc::lexer::LexemeFeed;

fn main() {
    let input = read_to_string("./test.c").unwrap();
    let mut lexer = Lexer::new();

    let lexemes = lexer.lex(&input);
    let mut feed = LexemeFeed::from_iter(Box::new(lexemes.into_iter()));
    let prog = CProgram::parse(&mut feed).unwrap();
}

