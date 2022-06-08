use std::fs::read_to_string;

use rcc::lexer::Lexer;
use rcc::ast::CProgram;
use rcc::parser::Parse;

fn main() {
    let input = read_to_string("./test.c").unwrap();
    let mut lexer = Lexer::new();

    let lexemes = lexer.lex(&input);
    let (prog, t) = CProgram::parse(&lexemes).unwrap();

    println!("{} vs {}", lexemes.len(), t);


    println!("{:?}", prog);
}

