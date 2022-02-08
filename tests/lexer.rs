use rcc::lexer::Lexer;
use rcc::lexeme::Lexeme;
use std::fs::read_to_string;

#[test]
fn lex_identifiers() {
    let input = read_to_string("./tests/lexer_files/identifiers.txt").unwrap();
    let mut lexer = Lexer::new();
    let lexemes = lexer.lex(&input);

    assert_eq!(vec![
        Lexeme::Identifier("hello".into()),
        Lexeme::Identifier("_testvar".into()),
        Lexeme::Identifier("hel09302".into()),
        Lexeme::Identifier("test".into()),
    ], lexemes);
}

#[test]
fn lex_typenames() {
    let input = read_to_string("./tests/lexer_files/typename.txt").unwrap();
    let mut lexer = Lexer::new();
    let lexemes = lexer.lex(&input);

    assert_eq!(vec![
        Lexeme::Typedef,
        Lexeme::Identifier("ident".into()),
        Lexeme::TypeName("type_name".into()),
        Lexeme::Semicolon,
        Lexeme::Identifier("ident".into())
    ], lexemes);
}

#[test]
fn lex_keywords() {
    let input = read_to_string("./tests/lexer_files/keywords.txt").unwrap();
    let mut lexer = Lexer::new();
    let lexemes = lexer.lex(&input);

    assert_eq!(vec![
        Lexeme::Auto,
        Lexeme::Break,
        Lexeme::Case,
        Lexeme::Char,
        Lexeme::Const,
        Lexeme::Default,
        Lexeme::Do,
        Lexeme::Double,
        Lexeme::Else,
        Lexeme::Enum,
        Lexeme::Extern,
        Lexeme::Float,
        Lexeme::For,
        Lexeme::Goto,
        Lexeme::If,
        Lexeme::Inline,
        Lexeme::Int,
        Lexeme::Long,
        Lexeme::Register,
        Lexeme::Restrict,
        Lexeme::Return,
        Lexeme::Short,
        Lexeme::Signed,
        Lexeme::Sizeof,
        Lexeme::Static,
        Lexeme::Struct,
        Lexeme::Switch,
        Lexeme::Typedef,
        Lexeme::Union,
        Lexeme::Unsigned,
        Lexeme::Void,
        Lexeme::Volatile,
        Lexeme::While,
        Lexeme::Alignas,
        Lexeme::Alignof,
        Lexeme::Atomic,
        Lexeme::Bool,
        Lexeme::Complex,
        Lexeme::Generic,
        Lexeme::Imaginary,
        Lexeme::Noreturn,
        Lexeme::StaticAssert,
        Lexeme::ThreadLocal
    ], lexemes);
}

#[test]
fn lex_operators() {
    let input = read_to_string("./tests/lexer_files/operators.txt").unwrap();
    let mut lexer = Lexer::new();
    let lexemes = lexer.lex(&input);

    assert_eq!(vec![
        Lexeme::Add,
        Lexeme::AddAssign,
        Lexeme::Sub,
        Lexeme::SubAssign,
        Lexeme::MulOrPointer,
        Lexeme::MulAssign,
        Lexeme::Div,
        Lexeme::DivAssign,
        Lexeme::Mod,
        Lexeme::ModAssign,
        Lexeme::Increment,
        Lexeme::Decrement,
        Lexeme::Equal,
        Lexeme::NotEqual,
        Lexeme::LessThan,
        Lexeme::LessThanEqual,
        Lexeme::GreaterThan,
        Lexeme::GreaterThanEqual,
        Lexeme::And,
        Lexeme::Or,
        Lexeme::Not,
        Lexeme::BitwiseAndReference,
        Lexeme::BitwiseAndAssign,
        Lexeme::BitwiseOr,
        Lexeme::BitwiseOrAssign,
        Lexeme::BitwiseXor,
        Lexeme::BitwiseXorAssign,
        Lexeme::BitwiseNot,
        Lexeme::BitwiseNotAssign,
        Lexeme::LeftShift,
        Lexeme::LeftShiftAssign,
        Lexeme::RightShift,
        Lexeme::RightShiftAssign,
        Lexeme::Assign,
        Lexeme::Question,
        Lexeme::Colon,
        Lexeme::Dot,
        Lexeme::Arrow
    ], lexemes)
}

#[test]
fn lex_separators() {
    let input = read_to_string("./tests/lexer_files/separators.txt").unwrap();
    let mut lexer = Lexer::new();
    let lexemes = lexer.lex(&input);

    assert_eq!(vec![
        Lexeme::Comma,
        Lexeme::Semicolon,
        Lexeme::LeftParen,
        Lexeme::RightParen,
        Lexeme::LeftBracket,
        Lexeme::RightBracket,
        Lexeme::LeftBrace,
        Lexeme::RightBrace
    ], lexemes);
}

#[test]
fn lex_literals() {
    let input = read_to_string("./tests/lexer_files/literals.txt").unwrap();
    let mut lexer = Lexer::new();
    let lexemes = lexer.lex(&input);

    assert_eq!(vec![
        Lexeme::CharLiteral('a'),
        Lexeme::CharLiteral('\n'),
        Lexeme::CharLiteral('\t'),
        Lexeme::CharLiteral(char::from_u32(8).unwrap()),
        Lexeme::CharLiteral('\r'),
        Lexeme::CharLiteral(char::from_u32(7).unwrap()),
        Lexeme::CharLiteral('\''),
        Lexeme::CharLiteral('\"'),
        Lexeme::CharLiteral('?'),
        Lexeme::CharLiteral('\\'),
        Lexeme::CharLiteral(char::from_u32(12).unwrap()),
        Lexeme::CharLiteral(char::from_u32(11).unwrap()),
        Lexeme::CharLiteral('\0'),
        Lexeme::CharLiteral(char::from_u32(072).unwrap()),
        Lexeme::CharLiteral(char::from_u32(0x33).unwrap()),
        Lexeme::StringLiteral("hello world".into()),
        Lexeme::StringLiteral("hello */ /*  */ // weird string".into()),
        Lexeme::IntLiteral(5),
        Lexeme::IntLiteral(43),
        Lexeme::IntLiteral(0x43),
        Lexeme::IntLiteral(0645),
        Lexeme::IntLiteral(43),
        Lexeme::IntLiteral(9999),
        Lexeme::IntLiteral(2343),
        Lexeme::IntLiteral(858),
        Lexeme::FloatLiteral(2.0),
        Lexeme::FloatLiteral(4.3092),
        Lexeme::FloatLiteral(5.0),
        Lexeme::FloatLiteral(5.0),
        Lexeme::FloatLiteral(4.3)
    ], lexemes);
}
