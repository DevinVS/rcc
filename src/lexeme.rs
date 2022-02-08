use std::collections::HashMap;
use lazy_static::lazy_static;

lazy_static! {
    pub static ref KEYWORDS: HashMap<&'static str, Lexeme> = {
        let mut m = HashMap::new();

        m.insert("auto", Lexeme::Auto);
        m.insert("break", Lexeme::Break);
        m.insert("case", Lexeme::Case);
        m.insert("char", Lexeme::Char);
        m.insert("const", Lexeme::Const);
        m.insert("continue", Lexeme::Continue);
        m.insert("default", Lexeme::Default);
        m.insert("do", Lexeme::Do);
        m.insert("double", Lexeme::Double);
        m.insert("else", Lexeme::Else);
        m.insert("enum", Lexeme::Enum);
        m.insert("extern", Lexeme::Extern);
        m.insert("float", Lexeme::Float);
        m.insert("for", Lexeme::For);
        m.insert("goto", Lexeme::Goto);
        m.insert("if", Lexeme::If);
        m.insert("inline", Lexeme::Inline);
        m.insert("int", Lexeme::Int);
        m.insert("long", Lexeme::Long);
        m.insert("register", Lexeme::Register);
        m.insert("restrict", Lexeme::Restrict);
        m.insert("return", Lexeme::Return);
        m.insert("short", Lexeme::Short);
        m.insert("signed", Lexeme::Signed);
        m.insert("sizeof", Lexeme::Sizeof);
        m.insert("static", Lexeme::Static);
        m.insert("struct", Lexeme::Struct);
        m.insert("switch", Lexeme::Switch);
        m.insert("typedef", Lexeme::Typedef);
        m.insert("union", Lexeme::Union);
        m.insert("unsigned", Lexeme::Unsigned);
        m.insert("void", Lexeme::Void);
        m.insert("volatile", Lexeme::Volatile);
        m.insert("while", Lexeme::While);
        m.insert("_Alignas", Lexeme::Alignas);
        m.insert("_Alignof", Lexeme::Alignof);
        m.insert("_Atomic", Lexeme::Atomic);
        m.insert("_Bool", Lexeme::Bool);
        m.insert("_Complex", Lexeme::Complex);
        m.insert("_Generic", Lexeme::Generic);
        m.insert("_Imaginary", Lexeme::Imaginary);
        m.insert("_Noreturn", Lexeme::Noreturn);
        m.insert("_Static_assert", Lexeme::StaticAssert);
        m.insert("_Thread_local", Lexeme::ThreadLocal);

        m
    };
}

lazy_static! {
    pub static ref OPERATORS: HashMap<&'static str, Lexeme> = {
        let mut m = HashMap::new();

        m.insert("+", Lexeme::Add);
        m.insert("+=", Lexeme::AddAssign);
        m.insert("-", Lexeme::Sub);
        m.insert("-=", Lexeme::SubAssign);
        m.insert("*", Lexeme::MulOrPointer);
        m.insert("*=", Lexeme::MulAssign);
        m.insert("/", Lexeme::Div);
        m.insert("/=", Lexeme::DivAssign);
        m.insert("%", Lexeme::Mod);
        m.insert("%=", Lexeme::ModAssign);
        m.insert("++", Lexeme::Increment);
        m.insert("--", Lexeme::Decrement);
        m.insert("=", Lexeme::Assign);
        m.insert("==", Lexeme::Equal);
        m.insert("!", Lexeme::Not);
        m.insert("!=", Lexeme::NotEqual);
        m.insert("<", Lexeme::LessThan);
        m.insert("<=", Lexeme::LessThanEqual);
        m.insert("<<", Lexeme::LeftShift);
        m.insert("<<=", Lexeme::LeftShiftAssign);
        m.insert(">", Lexeme::GreaterThan);
        m.insert(">=", Lexeme::GreaterThanEqual);
        m.insert(">>", Lexeme::RightShift);
        m.insert(">>=", Lexeme::RightShiftAssign);
        m.insert(":", Lexeme::Colon);
        m.insert("?", Lexeme::Question);
        m.insert("&", Lexeme::BitwiseAndReference);
        m.insert("&&", Lexeme::And);
        m.insert("&=", Lexeme::BitwiseAndAssign);
        m.insert("|", Lexeme::BitwiseOr);
        m.insert("||", Lexeme::Or);
        m.insert("|=", Lexeme::BitwiseOrAssign);
        m.insert("^", Lexeme::BitwiseXor);
        m.insert("^=", Lexeme::BitwiseXorAssign);
        m.insert("~", Lexeme::BitwiseNot);
        m.insert("~=", Lexeme::BitwiseNotAssign);
        m.insert(".", Lexeme::Dot);
        m.insert("->", Lexeme::Arrow);

        m
    };
}

lazy_static! {
    pub static ref SEPARATORS: HashMap<char, Lexeme> = {
        let mut m = HashMap::new();

        m.insert(',', Lexeme::Comma);
        m.insert(';', Lexeme::Semicolon);
        m.insert('(', Lexeme::LeftParen);
        m.insert(')', Lexeme::RightParen);
        m.insert('[', Lexeme::LeftBracket);
        m.insert(']', Lexeme::RightBracket);
        m.insert('{', Lexeme::LeftBrace);
        m.insert('}', Lexeme::RightBrace);

        m
    };
}


#[derive(Debug, PartialEq, Clone)]
pub enum Lexeme {
    // Identifiers
    Identifier(String),
    TypeName(String),

    // Separators
    Comma,
    Semicolon,
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,


    // Keywords
    Auto,
    Break,
    Case,
    Char,
    Const,
    Continue,
    Default,
    Do,
    Double,
    Else,
    Enum,
    Extern,
    Float,
    For,
    Goto,
    If,
    Inline,
    Int,
    Long,
    Register,
    Restrict,
    Return,
    Short,
    Signed,
    Sizeof,
    Static,
    Struct,
    Switch,
    Typedef,
    Union,
    Unsigned,
    Void,
    Volatile,
    While,
    Alignas,
    Alignof,
    Atomic,
    Bool,
    Complex,
    Generic,
    Imaginary,
    Noreturn,
    StaticAssert,
    ThreadLocal,

    // Operators
    Add,
    AddAssign,
    Sub,
    SubAssign,
    MulOrPointer,
    MulAssign,
    Div,
    DivAssign,
    Mod,
    ModAssign,
    Increment,
    Decrement,
    Equal,
    NotEqual,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    And,
    Or,
    Not,
    BitwiseAndReference,
    BitwiseAndAssign,
    BitwiseOr,
    BitwiseOrAssign,
    BitwiseXor,
    BitwiseXorAssign,
    BitwiseNot,
    BitwiseNotAssign,
    LeftShift,
    LeftShiftAssign,
    RightShift,
    RightShiftAssign,
    Assign,
    Question,
    Colon,
    Dot,
    Arrow,

    // Literals
    IntLiteral(u32),
    LongLiteral(u64),
    FloatLiteral(f32),
    DoubleLiteral(f64),
    StringLiteral(String),
    BooleanLiteral(bool),
    CharLiteral(char)
}
