use crate::lexeme::{
    Lexeme,
    KEYWORDS,
    OPERATORS,
    SEPARATORS
};

use std::iter::Peekable;

use std::collections::HashSet;

pub struct Lexer {
    single_line_comment: bool,
    multi_line_comment: bool,
    in_string: bool,
    in_type_def: bool,
    type_names: HashSet<String>
}

impl Lexer {
    pub fn new() -> Lexer {
        Lexer {
            single_line_comment: false,
            multi_line_comment: false,
            in_string: false,
            in_type_def: false,
            type_names: HashSet::new()
        }
    }

    pub fn lex(&mut self, input: &str) -> Vec<Lexeme> {
        let mut lexemes = Vec::new();
        let mut chars = input.chars().peekable();

        let mut stack = String::new();

        while let Some(c) = chars.next() {
            let next = chars.peek();

            // If we are in a single line comment skip all
            // characters until we reach a newline
            if self.single_line_comment {
                if c=='\n' {
                    self.single_line_comment = false;
                }
                continue;
            }

            // If we are in a multi line comment skip all
            // characters until we reach */
            if self.multi_line_comment {
                if c=='*' && next.is_some() && *next.unwrap()=='/' {
                    chars.next();
                    self.multi_line_comment = false;
                }
                continue;
            }

            // If we are in a string skip all characters until "
            // while also transforming escape sequences
            if self.in_string {
                match c {
                    '"' => {
                        self.push_lexeme(&mut lexemes, &mut stack);
                        self.in_string = false;
                    }
                    // Escape Characters!!!
                    // Doesn't yet support octal or hex numbers
                    '\\' => {
                        let new_c = match next {
                            Some('a') => char::from_u32(7).unwrap(),
                            Some('b') => char::from_u32(8).unwrap(),
                            Some('f') => char::from_u32(12).unwrap(),
                            Some('n') => '\n',
                            Some('r') => '\r',
                            Some('t') => '\t',
                            Some('v') => char::from_u32(11).unwrap(),
                            Some('\\') => '\\',
                            Some('"') => '"',
                            Some('?') => '?',
                            Some('0') => char::from_u32(0).unwrap(),
                            _ => panic!("Invalid escape sequence")
                        };

                        stack.push(new_c);
                        chars.next();
                    },
                    _ => stack.push(c)
                }

                continue;
            }

            match c {
                // // means enter a single line comment
                '/' if next.is_some() && *next.unwrap()=='/' => {
                    self.push_lexeme(&mut lexemes, &mut stack);
                    self.single_line_comment = true;
                }

                // /* means we eneter a multi line comment
                '/' if next.is_some() && *next.unwrap()=='*' => {
                    self.push_lexeme(&mut lexemes, &mut stack);
                    self.multi_line_comment = true;
                }
                // String literal
                '"' => {
                    self.push_lexeme(&mut lexemes, &mut stack);
                    self.in_string = true;
                }
                // Char literal
                '\'' => {
                    self.push_lexeme(&mut lexemes, &mut stack);

                    if next.is_none() {
                        panic!("Invalid char literal");
                    }

                    let val = *next.unwrap();

                    // Consume val
                    chars.next();

                    let end = chars.next();

                    lexemes.push(Lexeme::CharLiteral(val));
                }

                // Separators
                ',' | ';' | '(' | ')' | '[' | ']' | '{' | '}' => {
                    self.push_lexeme(&mut lexemes, &mut stack);
                    if self.in_type_def && c ==';' {
                        if let Some(Lexeme::Identifier(n)) = lexemes.pop() {
                            lexemes.push(Lexeme::TypeName(n.clone()));
                            self.type_names.insert(n);
                            self.in_type_def = false;
                        }
                    }
                    lexemes.push(SEPARATORS.get(&c).unwrap().clone());
                }

                // Operators
                '+' | '-' | '=' | '<' | '>' | '&' | '|'  => {
                    self.push_lexeme(&mut lexemes, &mut stack);
                    let input = if let Some(next) = next {
                        if *next==c {
                            chars.next().unwrap();
                            if (c=='<' || c=='>') && chars.peek().is_some() && *chars.peek().unwrap() == '=' {
                                chars.next().unwrap();
                                format!("{}{}=", c, c)
                            } else {
                                format!("{}{}", c, c)
                            }
                        } else if *next=='=' {
                            chars.next().unwrap();
                            format!("{}=", c)
                        } else if c=='-' && *next=='>' {
                            chars.next().unwrap();
                            String::from("->")
                        } else {
                            c.to_string()
                        }
                    } else {
                        c.to_string()
                    };

                    lexemes.push(OPERATORS.get(input.as_str()).unwrap().clone());
                }

                '*' | '/' | '%' | '!' | '^' | '~' => {
                    self.push_lexeme(&mut lexemes, &mut stack);
                    let input = if let Some(next) = next {
                        if *next=='=' {
                            chars.next().unwrap();
                            format!("{}=", c)
                        } else {
                            c.to_string()
                        }
                    } else {
                        c.to_string()
                    };

                    lexemes.push(OPERATORS.get(input.as_str()).unwrap().clone());
                }
                ':' | '?' | '.' => {
                    self.push_lexeme(&mut lexemes, &mut stack);
                    lexemes.push(OPERATORS.get(c.to_string().as_str()).unwrap().clone());
                }

                // A space or newline will make us push the lexeme that we have, but is otherwise ignored
                ' ' | '\n' => self.push_lexeme(&mut lexemes, &mut stack),
                // By default just add the character to the stack
                _ => {
                    stack.push(c);
                }
            }
        }

        // End of stream push remaining stack
        self.push_lexeme(&mut lexemes, &mut stack);

        lexemes
    }

    fn push_lexeme(&mut self, lexemes: &mut Vec<Lexeme>, stack: &mut String) {
        if !stack.is_empty() {
            if self.in_string {
                lexemes.push(Lexeme::StringLiteral(stack.clone()));
            } else if let Some(keyword) = KEYWORDS.get(stack.as_str()) {
                if *keyword==Lexeme::Typedef {
                    self.in_type_def = true;
                }
                lexemes.push(keyword.clone());

            } else if let Ok(i) = str::parse::<u32>(&stack) {
                lexemes.push(Lexeme::IntLiteral(i))
            } else if let Ok(f) = str::parse::<f32>(&stack) {
                lexemes.push(Lexeme::FloatLiteral(f));
            } else if self.type_names.contains(stack) {
                lexemes.push(Lexeme::TypeName(stack.clone()));
            } else {
                lexemes.push(Lexeme::Identifier(stack.clone()));
            }
        }

        *stack = String::new();
    }
}

pub struct LexemeFeed {
    inner: Peekable<Box<dyn Iterator<Item=Lexeme>>>
}

impl LexemeFeed {
    pub fn from_iter(i: Box<dyn Iterator<Item=Lexeme>>) -> LexemeFeed {
        LexemeFeed {
            inner: i.peekable()
        }
    }

    pub fn next(&mut self) -> Option<Lexeme> {
        self.inner.next()
    }

    pub fn peek(&mut self) -> Option<&Lexeme> {
        self.inner.peek()
    }

    pub fn test(&mut self, l: Lexeme) -> bool {
        if Some(&l) == self.peek() {
            self.next();
            true
        } else {
            false
        }
    }

    pub fn consume(&mut self, l: Lexeme) -> Result<(), String> {
        if Some(l.clone()) == self.next() {
            Ok(())
        } else {
            Err(format!("Expected: {:?}", l))
        }
    }
}

