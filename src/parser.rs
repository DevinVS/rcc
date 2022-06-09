use crate::lexeme::Lexeme;
use crate::ast::*;
use std::fmt::Debug;
use std::any::type_name;

pub trait Parse {
    /// Parse a given expression. The result indicates whether the expression
    /// parsed correctly, whereas the option indicates whether the expression
    /// matched its pattern
    fn parse(l: &[Lexeme]) -> Option<(Box<Self>, usize)>where Self: Sized;
    fn log(l: &[Lexeme]) {
        //println!("{}: {:?}", type_name::<Self>(), &l.get(0));
    }
}

// Parse lists of objects
impl<T: Debug + Parse> Parse for Vec<Box<T>> {
    fn parse(l: &[Lexeme]) -> Option<(Box<Self>, usize)> {
		Self::log(l);

        let mut items = vec![];
        let mut tokens = 0;

        while let Some((i, t)) = T::parse(&l[tokens..]) {
            tokens += t;
            items.push(i);
        }

        if items.len() == 0 {
            None
        } else {
            Some((Box::new(items), tokens))
        }
    }
}

// Parse lexeme separated lists of items
impl<T: Debug + Parse, const SEP: usize> Parse for SVec<Box<T>, SEP> {
    fn parse(l: &[Lexeme]) -> Option<(Box<Self>, usize)> {
		Self::log(l);
        let mut items = vec![];
        let mut tokens = 0;

        let (first, t) = T::parse(l)?;
        tokens += t;
        items.push(first);

        while l.get(tokens)? == &(SVec::<Box<T>, SEP>::lexeme()) {
            let (i, t) = T::parse(&l[tokens..]).unwrap();
            tokens += t;
            items.push(i);
        }

        Some((Box::new(SVec{ items }), tokens))
    }
}


// Parse the start of a right recursive expression
impl<T: Parse + Debug, S: Parse + Debug> Parse for StartExpression<S, T> {
    fn parse(l: &[Lexeme]) -> Option<(Box<Self>, usize)>where Self: Sized {
		Self::log(l);
        let mut tokens = 0;
        let (expr, t) = S::parse(&l[tokens..])?;
        tokens += t;

        let next = T::parse(&l[tokens..])
            .map(|(a,t)| {
                tokens += t;
                a
            });

        Some((Box::new(Self { expr, next }), tokens))
    }
}

impl Parse for Identifier {
    fn parse(l: &[Lexeme]) -> Option<(Box<Self>, usize)> {
		Self::log(l);
        if let Some(Lexeme::Identifier(val)) = l.get(0) {
            let val = val.clone();
            Some((Box::new(Identifier(val)), 1))
        } else {
            None
        }
    }
}

impl Parse for ExternalDeclaration {
    fn parse(l: &[Lexeme]) -> Option<(Box<Self>, usize)> {
        let mut tokens = 0;

        if let Some((func_def, t)) = FunctionDefinition::parse(&l[tokens..]) {
            tokens += t;

            return Some((Box::new(Self::FunctionDefinition(func_def)), tokens));
        }

        if let Some((decl, t)) = Declaration::parse(&l[tokens..]) {
            tokens += t;

            return Some((Box::new(Self::Declaration(decl)), tokens));
        }

        None
    }
}

impl Parse for FunctionDefinition {
    fn parse(l: &[Lexeme]) -> Option<(Box<Self>, usize)>where Self: Sized {
		Self::log(l);
        let mut tokens = 0;

        let declaration_specifiers = DeclarationSpecifiers::parse(&l[tokens..])
            .map(|(a, t)| {
                tokens += t;
                a
            });

        let (declarator, t) = Declarator::parse(&l[tokens..])?;
        tokens += t;

        let declaration_list = DeclarationList::parse(&l[tokens..])
            .map(|(a, t)| {
                tokens += t;
                a
            });

        let (compound, t) = CompoundStatement::parse(&l[tokens..])?;
        tokens += t;

        Some((Box::new(FunctionDefinition {
            declaration_specifiers,
            declarator,
            declaration_list,
            compound
        }), tokens))
    }
}

impl Parse for Declaration {
    fn parse(l: &[Lexeme]) -> Option<(Box<Self>, usize)>where Self: Sized {
		Self::log(l);
        let mut tokens = 0;

        let (specs, t) = DeclarationSpecifiers::parse(&l[tokens..])?;
        tokens += t;

        let init_list = InitDeclaratorList::parse(&l[tokens..])
            .map(|(a,t)| {
                tokens += t;
                a
            });

        if l.get(tokens)? != &Lexeme::Semicolon {
            None
        } else {
            tokens += 1;
            Some((Box::new(Declaration {
                specs,
                init_list
            }), tokens))
        }
    }
}

impl Parse for DeclarationSpecifier {
    fn parse(l: &[Lexeme]) -> Option<(Box<Self>, usize)>where Self: Sized {
		Self::log(l);
        let mut tokens = 0;

        if let Some((storage_class, t)) = StorageClassSpecifier::parse(&l[tokens..]) {
            tokens += t;
            return Some((Box::new(DeclarationSpecifier::StorageClassSpecifier(storage_class)), tokens));
        }

        if let Some((type_specifier, t)) = TypeSpecifier::parse(&l[tokens..]) {
            tokens += t;
            return Some((Box::new(DeclarationSpecifier::TypeSpecifier(type_specifier)), tokens));
        }

        if let Some((type_qualifier, t)) = TypeQualifier::parse(&l[tokens..]) {
            tokens += t;
            return Some((Box::new(DeclarationSpecifier::TypeQualifier(type_qualifier)), tokens));
        }

        None
    }
}

impl Parse for StorageClassSpecifier {
    fn parse(l: &[Lexeme]) -> Option<(Box<Self>, usize)>where Self: Sized {
		Self::log(l);
        Some((Box::new(match l.get(0)? {
            Lexeme::Auto => Self::Auto,
            Lexeme::Register => Self::Register,
            Lexeme::Static => Self::Static,
            Lexeme::Extern => Self::Extern,
            Lexeme::Typedef => Self::Typedef,
            _ => return None
        }), 1))
    }
}

impl Parse for TypeSpecifier {
    fn parse(l: &[Lexeme]) -> Option<(Box<Self>, usize)>where Self: Sized {
		Self::log(l);
        if let Some((sou_spec, t)) = StructOrUnionSpecifier::parse(&l) {
            return Some((Box::new(Self::StructOrUnion(sou_spec)), t));
        }

        if let Some((enum_spec, t)) = EnumSpecifier::parse(&l) {
            return Some((Box::new(Self::Enum(enum_spec)), t));
        }

        if let Some((typedef_name, t)) = TypedefName::parse(&l) {
            return Some((Box::new(Self::Typedef(typedef_name)), t));
        }

        Some((Box::new(match l.get(0)? {
            Lexeme::Void => Self::Void,
            Lexeme::Char => Self::Char,
            Lexeme::Short => Self::Short,
            Lexeme::Int => Self::Int,
            Lexeme::Long => Self::Long,
            Lexeme::Float => Self::Float,
            Lexeme::Double => Self::Double,
            Lexeme::Signed => Self::Signed,
            Lexeme::Unsigned => Self::Signed,
            _ => return None
        }), 1))
    }
}

impl Parse for TypeQualifier {
    fn parse(l: &[Lexeme]) -> Option<(Box<Self>, usize)>where Self: Sized {
		Self::log(l);
        Some((Box::new(match l.get(0)? {
            Lexeme::Const => Self::Const,
            Lexeme::Volatile => Self::Volatile,
            _ => return None
        }), 1))
    }
}

impl Parse for StructOrUnionSpecifier {
    fn parse(l: &[Lexeme]) -> Option<(Box<Self>, usize)>where Self: Sized {
		Self::log(l);
        let mut tokens = 0;
        let (sou, t) = StructOrUnion::parse(&l[tokens..])?;
        tokens += t;

        let identifier = Identifier::parse(&l[tokens..])
            .map(|(a, t)| {
                tokens += t;
                a
            });

        if l.get(tokens)? == &Lexeme::LeftBrace {
            tokens += 1;

            if let Some((list, t)) = StructDeclarationList::parse(&l[tokens..]) {
                tokens += t;

                if l.get(tokens)? == &Lexeme::RightBrace {
                    tokens += 1;
                    return Some((Box::new(Self::List(sou, identifier, list)), tokens));
                }

                tokens -= t;
            }

            tokens -= 1;
        }

        if let Some(identifier) = identifier {
            return Some((Box::new(Self::Val(sou, identifier)), tokens));
        }

        None
    }
}

impl Parse for StructOrUnion {
    fn parse(l: &[Lexeme]) -> Option<(Box<Self>, usize)>where Self: Sized {
		Self::log(l);
        Some((Box::new(match l.get(0)? {
            Lexeme::Struct => Self::Struct,
            Lexeme::Union => Self::Union,
            _ => return None
        }), 1))
    }
}

impl Parse for InitDeclarator {
    fn parse(l: &[Lexeme]) -> Option<(Box<Self>, usize)>where Self: Sized {
		Self::log(l);
        let mut tokens = 0;

        let (declarator, t) = Declarator::parse(&l[tokens..])?;
        tokens += t;

        if l.get(tokens)? == &Lexeme::Equal {
            tokens += 1;

            if let Some((initializer, t)) = Initializer::parse(&l[tokens..]) {
                tokens += t;

                return Some((Box::new(Self::Assign(declarator, initializer)), tokens));
            }

            tokens -= 1;
        }

        Some((Box::new(Self::Declarator(declarator)), tokens))
    }
}

impl Parse for StructDeclaration {
    fn parse(l: &[Lexeme]) -> Option<(Box<Self>, usize)>where Self: Sized {
		Self::log(l);
        let mut tokens = 0;

        let (spec_qualifier_list, t) = SpecifierQualifierList::parse(&l[tokens..])?;
        tokens += t;

        let (decl_list, t) = StructDeclaratorList::parse(&l[tokens..])?;
        tokens += t;

        if l.get(tokens)? == &Lexeme::Semicolon {
            tokens += 1;

            Some((Box::new(Self {
                spec_qualifier_list,
                decl_list
            }), tokens))
        } else {
            None
        }
    }
}

impl Parse for SpecifierOrQualifier {
    fn parse(l: &[Lexeme]) -> Option<(Box<Self>, usize)>where Self: Sized {
		Self::log(l);
        let mut tokens = 0;

        if let Some((type_spec, t)) = TypeSpecifier::parse(&l[tokens..]) {
            tokens += t;
            return Some((Box::new(Self::Specifier(type_spec)), tokens))
        }

        if let Some((type_qual, t)) = TypeQualifier::parse(&l[tokens..]) {
            tokens += t;
            return Some((Box::new(Self::Qualifier(type_qual)), tokens));
        }

        None
    }
}

impl Parse for StructDeclarator {
    fn parse(l: &[Lexeme]) -> Option<(Box<Self>, usize)>where Self: Sized {
		Self::log(l);
        let mut tokens = 0;
        let declarator = Declarator::parse(&l[tokens..])
            .map(|(a,t)| {
                tokens += t;
                a
            });

        if l.get(tokens)? == &Lexeme::Colon {
            tokens += 1;

            if let Some((const_expr, t)) = ConstantExpression::parse(&l[tokens..]) {
                tokens += t;

                return Some((Box::new(Self::Const(declarator, const_expr)), tokens));
            }

            tokens -= 1;
        }

        if let Some(decl) = declarator {
            return Some((Box::new(Self::Decl(decl)), tokens));
        }

        None
    }
}

impl Parse for EnumSpecifier {
    fn parse(l: &[Lexeme]) -> Option<(Box<Self>, usize)>where Self: Sized {
		Self::log(l);
        let mut tokens = 0;

        if l.get(tokens)? != &Lexeme::Enum {
            return None;
        }
        tokens += 1;

        let ident = Identifier::parse(&l[tokens..])
            .map(|(a, t)| {
                tokens += t;
                a
            });

        if l.get(tokens)? == &Lexeme::LeftBrace {
            tokens += 1;

            if let Some((enum_list, t)) = EnumeratorList::parse(&l[tokens..]) {
                tokens += t;

                if l.get(tokens)? == &Lexeme::RightBrace {
                    tokens += 1;
                    return Some((Box::new(Self::List(ident, enum_list)), tokens));
                }

                tokens -= t;
            }

            tokens -= 1;
        }

        if let Some(ident) = ident {
            return Some((Box::new(Self::Identifier(ident)), tokens));
        }

        None
    }
}

impl Parse for Enumerator {
    fn parse(l: &[Lexeme]) -> Option<(Box<Self>, usize)>where Self: Sized {
		Self::log(l);
        let mut tokens = 0;

        let (ident, t) = Identifier::parse(&l[tokens..])?;
        tokens += t;

        if l.get(tokens)? == &Lexeme::Equal {
            tokens += 1;

            if let Some((const_expr, t)) = ConstantExpression::parse(&l[tokens..]) {
                tokens += t;
                return Some((Box::new(Self::ConstantExpression(ident, const_expr)), tokens));
            }

            tokens -= 1;
        }

        Some((Box::new(Self::Identifier(ident)), tokens))
    }
}

impl Parse for Declarator {
    fn parse(l: &[Lexeme]) -> Option<(Box<Self>, usize)>where Self: Sized {
		Self::log(l);
        let mut tokens = 0;

        let pointer = Pointer::parse(&l[tokens..])
            .map(|(a, t)| {
                tokens += t;
                a
            });

        let (decl, t) = DirectDeclarator::parse(&l[tokens..])?;
        tokens += t;

        Some((Box::new(Self {
            pointer,
            decl
        }), tokens))
    }
}

impl Parse for DirectDeclarator {
    fn parse(l: &[Lexeme]) -> Option<(Box<Self>, usize)>where Self: Sized {
		Self::log(l);
        let mut tokens = 0;

        if let Some((ident, t)) = Identifier::parse(&l[tokens..]) {
            tokens += t;

            let end = DirectDeclaratorEnd::parse(&l[tokens..])
                .map(|(a, t)| {
                    tokens += t;
                    a
                });

            return Some((Box::new(DirectDeclarator::Identifier(ident, end)), tokens));
        }

        if let Some((typedef_name, t)) = TypedefName::parse(&l[tokens..]) {
            tokens += t;

            let end = DirectDeclaratorEnd::parse(&l[tokens..])
                .map(|(a, t)| {
                    tokens += t;
                    a
                });

            return Some((Box::new(DirectDeclarator::TypedefName(typedef_name, end)), tokens));
        }

        if l.get(tokens)? == &Lexeme::LeftParen {
            tokens += 1;

            if let Some((decl, t)) = Declarator::parse(&l[tokens..]) {
                tokens += t;

                if l.get(tokens)? == &Lexeme::RightParen {
                    tokens += 1;

                    let end = DirectDeclaratorEnd::parse(&l[tokens..])
                        .map(|(a, t)| {
                            tokens += t;
                            a
                        });

                    return Some((Box::new(Self::Declarator(decl, end)), tokens));
                }
            }
        }

        None
    }
}

impl Parse for DirectDeclaratorEnd {
    fn parse(l: &[Lexeme]) -> Option<(Box<Self>, usize)>where Self: Sized {
		Self::log(l);
        let mut tokens = 0;

        if l.get(tokens)? == &Lexeme::LeftBracket {
            tokens += 1;

            let const_expr = ConstantExpression::parse(&l[tokens..])
                .map(|(a, t)| {
                    tokens += t;
                    a
                });

            let end = DirectDeclaratorEnd::parse(&l[tokens..])
                .map(|(a, t)| {
                    tokens += t;
                    a
                });

            return Some((Box::new(Self::ConstantExpression(const_expr, end)), tokens));
        }

        if l.get(tokens)? == &Lexeme::LeftParen {
            tokens += 1;

            if let Some((param_list, t)) = ParameterTypeList::parse(&l[tokens..]) {
                tokens += t;

                if l.get(tokens)? == &Lexeme::RightParen {
                    tokens += 1;

                    let end = DirectDeclaratorEnd::parse(&l[tokens..])
                        .map(|(a, t)| {
                            tokens += t;
                            a
                        });

                    return Some((Box::new(Self::ParameterTypeList(param_list, end)), tokens));
                }

                tokens -= t;
            }

            let ident_list = IdentifierList::parse(&l[tokens..])
                .map(|(a, t)| {
                    tokens += t;
                    a
                });

            if l.get(tokens)? == &Lexeme::RightParen {
                tokens += 1;

                let end = DirectDeclaratorEnd::parse(&l[tokens..])
                    .map(|(a, t)| {
                        tokens += t;
                        a
                    });

                return Some((Box::new(Self::IdentifierList(ident_list, end)), tokens));
            }
        }

        None
    }
}

impl Parse for Pointer {
    fn parse(l: &[Lexeme]) -> Option<(Box<Self>, usize)>where Self: Sized {
		Self::log(l);
        let mut tokens = 0;

        if l.get(tokens)? == &Lexeme::MulOrPointer {
            tokens += 1;
            let list = TypeQualifierList::parse(&l[tokens..])
                .map(|(a, t)| {
                    tokens += t;
                    a
                });

            if let Some((pointer, t)) = Pointer::parse(&l[tokens..]) {
                tokens += t;
                return Some((Box::new(Pointer::List(list, pointer)), tokens));
            } else {
                return Some((Box::new(Self::Pointer(list)), tokens));
            }
        }

        None
    }
}

impl Parse for ParameterTypeList {
    fn parse(l: &[Lexeme]) -> Option<(Box<Self>, usize)>where Self: Sized {
		Self::log(l);
        let mut tokens = 0;
        let (qual, t) = ParameterList::parse(&l[tokens..])?;
        tokens += t;

        if l.get(tokens)? == &Lexeme::Comma {
            tokens += 1;

            if l.get(tokens)? == &Lexeme::Dot {
                tokens += 1;

                if l.get(tokens)? == &Lexeme::Dot {
                    tokens += 1;

                    if l.get(tokens)? == &Lexeme::Dot {
                        tokens += 1;

                        return Some((Box::new(Self::VarArgs(qual)), tokens));
                    }

                    tokens -= 1;
                }

                tokens -= 1;
            }

            tokens -= 1;
        }

        Some((Box::new(Self::Regular(qual)), tokens))
    }
}

impl Parse for ParameterDeclaration {
    fn parse(l: &[Lexeme]) -> Option<(Box<Self>, usize)>where Self: Sized {
		Self::log(l);
        let mut tokens = 0;
        let (decl_specs, t) = DeclarationSpecifiers::parse(&l[tokens..])?;
        tokens += t;

        if let Some((decl, t)) = Declarator::parse(&l[tokens..]) {
            tokens += t;
            return Some((Box::new(Self::Declaration(decl_specs, decl)), tokens));
        }

        let abstract_decl = AbstractDeclarator::parse(&l[tokens..])
            .map(|(a,t)| {
                tokens += t;
                a
            });

        return Some((Box::new(Self::AbstractDeclarator(decl_specs, abstract_decl)), tokens));
    }
}

impl Parse for Initializer {
    fn parse(l: &[Lexeme]) -> Option<(Box<Self>, usize)>where Self: Sized {
		Self::log(l);
        let mut tokens = 0;

        if let Some((assn_expr, t)) = AssignmentExpression::parse(&l[tokens..]) {
            tokens += t;
            return Some((Box::new(Self::Assign(assn_expr)), tokens));
        }

        if l.get(tokens)? == &Lexeme::LeftBrace {
            tokens += 1;

            let (init_list, t) = InitializerList::parse(&l[tokens..])?;
            tokens += t;

            if l.get(tokens)? == &Lexeme::Comma { tokens += 1; }

            if l.get(tokens)? == &Lexeme::RightBrace {
                tokens += 1;
                return Some((Box::new(Self::List(init_list)), tokens));
            }
        }

        None
    }
}

impl Parse for TypeName {
    fn parse(l: &[Lexeme]) -> Option<(Box<Self>, usize)>where Self: Sized {
		Self::log(l);
        let mut tokens = 0;
        let (list, t) = SpecifierQualifierList::parse(&l[tokens..])?;
        tokens += t;

        let declarator = AbstractDeclarator::parse(&l[tokens..])
            .map(|(a, t)| {
                tokens += t;
                a
            });

        Some((Box::new(Self {
            list,
            declarator
        }), tokens))
    }
}

impl Parse for AbstractDeclarator {
    fn parse(l: &[Lexeme]) -> Option<(Box<Self>, usize)>where Self: Sized {
		Self::log(l);
        let mut tokens = 0;

        let pointer = Pointer::parse(&l[tokens..])
            .map(|(a, t)| {
                tokens += t;
                a
            });

        if let Some((dad, t)) = DirectAbstractDeclarator::parse(&l[tokens..]) {
            tokens += t;
            return Some((Box::new(Self::Declarator(pointer, dad)), tokens));
        }

        if let Some(pointer) = pointer {
            return Some((Box::new(Self::Pointer(pointer)), tokens));
        }

        None
    }
}

impl Parse for DirectAbstractDeclarator {
    fn parse(l: &[Lexeme]) -> Option<(Box<Self>, usize)>where Self: Sized {
		Self::log(l);
        let mut tokens = 0;

        if l.get(tokens)? == &Lexeme::LeftBracket {
            tokens += 1;
            let const_expr = ConstantExpression::parse(&l[tokens..])
                .map(|(a, t)| {
                    tokens += t;
                    a
                });

            if l.get(tokens)? == &Lexeme::RightBracket {
                tokens += 1;

                let end = DirectAbstractDeclaratorEnd::parse(&l[tokens..])
                    .map(|(a, t)| {
                        tokens += t;
                        a
                    });

                return Some((Box::new(Self::Const(const_expr, end)), tokens));
            }
        } else if l.get(tokens)? == &Lexeme::LeftParen {
            tokens += 1;

            if let Some((decl, t)) = AbstractDeclarator::parse(&l[tokens..]) {
                tokens += t;

                if l.get(tokens)? == &Lexeme::RightParen {
                    tokens += 1;
                    let end = DirectAbstractDeclaratorEnd::parse(&l[tokens..])
                        .map(|(a,t)| {
                            tokens += t;
                            a
                        });

                    return Some((Box::new(Self::Decl(decl, end)), tokens));
                }

                tokens -= t;
            }

            let list = ParameterTypeList::parse(&l[tokens..])
                .map(|(a,t)| {
                    tokens += t;
                    a
                });

            if l.get(tokens)? == &Lexeme::RightParen {
                tokens += 1;

                let end = DirectAbstractDeclaratorEnd::parse(&l[tokens..])
                    .map(|(a,t)| {
                        tokens += t;
                        a
                    });

                return Some((Box::new(Self::Param(list, end)), tokens));
            }
        }

        None
    }
}

impl Parse for DirectAbstractDeclaratorEnd {
    fn parse(l: &[Lexeme]) -> Option<(Box<Self>, usize)>where Self: Sized {
		Self::log(l);
        let mut tokens = 0;

        if l.get(tokens)? == &Lexeme::LeftBracket {
            tokens += 1;
            let const_expr = ConstantExpression::parse(&l[tokens..])
                .map(|(a, t)| {
                    tokens += t;
                    a
                });

            if l.get(tokens)? == &Lexeme::RightBracket {
                tokens += 1;

                let end = DirectAbstractDeclaratorEnd::parse(&l[tokens..])
                    .map(|(a, t)| {
                        tokens += t;
                        a
                    });

                return Some((Box::new(Self::Const(const_expr, end)), tokens));
            }
        } else if l.get(tokens)? == &Lexeme::LeftParen {
            tokens += 1;
            let list = ParameterTypeList::parse(&l[tokens..])
                .map(|(a,t)| {
                    tokens += t;
                    a
                });

            if l.get(tokens)? == &Lexeme::RightParen {
                tokens += 1;

                let end = DirectAbstractDeclaratorEnd::parse(&l[tokens..])
                    .map(|(a,t)| {
                        tokens += t;
                        a
                    });

                return Some((Box::new(Self::Param(list, end)), tokens));
            }
        }

        None
    }
}

impl Parse for TypedefName {
    fn parse(l: &[Lexeme]) -> Option<(Box<Self>, usize)>where Self: Sized {
		Self::log(l);
        if let Some(Lexeme::TypeName(val)) = l.get(0) {
            let val = val.clone();
            Some((Box::new(TypedefName{ name: val }), 1))
        } else {
            None
        }
    }
}

impl Parse for Statement {
    fn parse(l: &[Lexeme]) -> Option<(Box<Self>, usize)>where Self: Sized {
		Self::log(l);
        if let Some((labeled, t)) = LabeledStatement::parse(l) {
            return Some((Box::new(Self::Labeled(labeled)), t));
        }

        if let Some((expression, t)) = ExpressionStatement::parse(l) {
            return Some((Box::new(Self::Expression(expression)), t));
        }

        if let Some((compound, t)) = CompoundStatement::parse(l) {
            return Some((Box::new(Self::Compound(compound)), t));
        }

        if let Some((selection, t)) = SelectionStatement::parse(l) {
            return Some((Box::new(Self::Selection(selection)), t));
        }

        if let Some((iteration, t)) = IterationStatement::parse(l) {
            return Some((Box::new(Self::Iteration(iteration)), t));
        }

        if let Some((jump, t)) = JumpStatement::parse(l) {
            return Some((Box::new(Self::Jump(jump)), t));
        }

        None
    }
}

impl Parse for LabeledStatement {
    fn parse(l: &[Lexeme]) -> Option<(Box<Self>, usize)>where Self: Sized {
		Self::log(l);
        let mut tokens = 0;
        if l.get(tokens)? == &Lexeme::Case {
            tokens += 1;

            let (expr, t) = ConstantExpression::parse(&l[tokens..])?;
            tokens += t;

            if l.get(tokens)? != &Lexeme::Colon {
                return None;
            }
            tokens += 1;

            let (stmt, t) = Statement::parse(&l[tokens..])?;
            tokens += t;

            Some((Box::new(Self::Case(expr, stmt)), tokens))
        } else if l.get(tokens)? == &Lexeme::Default {
            tokens += 1;

            if l.get(tokens)? == &Lexeme::Colon {
                return None;
            }
            tokens += 1;

            let (stmt, t) = Statement::parse(&l[tokens..])?;
            tokens += t;

            Some((Box::new(Self::Default(stmt)), tokens))
        } else {
            let (identifier, t) = Identifier::parse(&l[tokens..])?;
            tokens += t;

            if l.get(tokens)? == &Lexeme::Colon {
                return None;
            }
            tokens += 1;

            let (stmt, t) = Statement::parse(&l[tokens..])?;
            tokens += t;

            Some((Box::new(Self::Map(identifier, stmt)), tokens))
        }
    }
}

impl Parse for ExpressionStatement {
    fn parse(l: &[Lexeme]) -> Option<(Box<Self>, usize)>where Self: Sized {
		Self::log(l);
        let mut tokens = 0;
        let expression = Expression::parse(&l[tokens..])
            .map(|(a, t)| {
                tokens += t;
                a
            });

        if l.get(tokens)? == &Lexeme::Semicolon {
            Some((Box::new(Self {
                expression
            }), tokens))
        } else {
            None
        }
    }
}

impl Parse for CompoundStatement {
    fn parse(l: &[Lexeme]) -> Option<(Box<Self>, usize)>where Self: Sized {
		Self::log(l);
        let mut tokens = 0;

        if l.get(tokens)? == &Lexeme::LeftBrace {
            tokens += 1;

            let declarations = DeclarationList::parse(&l[tokens..])
                .map(|(a, t)| {
                    tokens += t;
                    a
                });

            let statements = StatementList::parse(&l[tokens..])
                .map(|(a, t)| {
                    tokens += t;
                    a
                });

            if l.get(tokens)? == &Lexeme::RightBrace {
                tokens += 1;
                return Some((Box::new(Self {
                    declarations,
                    statements
                }), tokens));
            }
        }

        None
    }
}

impl Parse for SelectionStatement {
    fn parse(l: &[Lexeme]) -> Option<(Box<Self>, usize)>where Self: Sized {
		Self::log(l);
        let mut tokens = 0;

        if l.get(tokens)? == &Lexeme::If {
            tokens += 1;

            if l.get(tokens)? == &Lexeme::LeftParen {
                tokens += 1;

                if let Some((expr, t)) = Expression::parse(&l[tokens..]) {
                    tokens += t;

                    if l.get(tokens)? == &Lexeme::RightParen {
                        tokens += 1;

                        if let Some((stmt, t)) = Statement::parse(&l[tokens..]) {
                            tokens += t;

                            if l.get(tokens)? == &Lexeme::Else {
                                tokens += 1;

                                if let Some((else_stmt, t)) = Statement::parse(&l[tokens..]) {
                                    tokens += t;
                                    return Some((Box::new(Self::IfElse(expr, stmt, else_stmt)), tokens));
                                }

                                tokens -= 1;
                            }

                            return Some((Box::new(Self::If(expr, stmt)), tokens));
                        }
                    }
                }
            }

            return None;
        }

        if l.get(tokens)? == &Lexeme::Switch {
            if l.get(tokens)? == &Lexeme::LeftParen {
                tokens += 1;

                if let Some((expr, t)) = Expression::parse(&l[tokens..]) {
                    tokens += t;

                    if l.get(tokens)? == &Lexeme::RightParen {
                        tokens += 1;

                        if let Some((stmt, t)) = Statement::parse(&l[tokens..]) {
                            tokens += t;
                            return Some((Box::new(Self::Switch(expr, stmt)), tokens));
                        }
                    }
                }
            }
        }

        None
    }
}

impl Parse for IterationStatement {
    fn parse(l: &[Lexeme]) -> Option<(Box<Self>, usize)>where Self: Sized {
		Self::log(l);
        let mut tokens = 0;
        if l.get(tokens)? == &Lexeme::While {
            tokens += 1;

            if l.get(tokens)? == &Lexeme::LeftParen {
                tokens += 1;

                if let Some((expr, t)) = Expression::parse(&l[tokens..]) {
                    tokens += t;

                    if l.get(tokens)? == &Lexeme::RightParen {
                        tokens += 1;

                        if let Some((stmt, t)) = Statement::parse(&l[tokens..]) {
                            tokens += t;

                            return Some((Box::new(Self::While(expr, stmt)), tokens));
                        }
                    }
                }
            }

            return None;
        }

        if l.get(tokens)? == &Lexeme::Do {
            tokens += 1;

            if let Some((stmt, t)) = Statement::parse(&l[tokens..]) {
                tokens += t;

                if l.get(tokens)? == &Lexeme::While {
                    tokens += 1;

                    if l.get(tokens)? == &Lexeme::LeftParen {
                        tokens += 1;

                        if let Some((expr, t)) = Expression::parse(&l[tokens..]) {
                            tokens += t;

                            return Some((Box::new(Self::DoWhile(stmt, expr)), tokens));
                        }
                    }
                }
            }

            return None;
        }

        if l.get(tokens)? == &Lexeme::For {
            tokens += 1;

            if l.get(tokens)? != &Lexeme::LeftParen { return None; }
            tokens += 1;

            let expr1 = Expression::parse(&l[tokens..])
                .map(|(a, t)| {
                    tokens += t;
                    a
                });

            if l.get(tokens)? != &Lexeme::Semicolon { return None; }
            tokens += 1;

            let expr2 = Expression::parse(&l[tokens..])
                .map(|(a,t)| {
                    tokens += t;
                    a
                });

            if l.get(tokens)? != &Lexeme::Semicolon { return None; }
            tokens += 1;

            let expr3 = Expression::parse(&l[tokens..])
                .map(|(a, t)| {
                    tokens += t;
                    a
                });

            if l.get(tokens)? != &Lexeme::LeftParen { return None; }
            tokens += 1;

            let (stmt, t) = Statement::parse(&l[tokens..])?;
            tokens += t;

            return Some((Box::new(Self::For(expr1, expr2, expr3, stmt)), tokens));
        }

        None
    }
}

impl Parse for JumpStatement {
    fn parse(l: &[Lexeme]) -> Option<(Box<Self>, usize)>where Self: Sized {
		Self::log(l);
        let mut tokens = 1;
        match l.get(tokens)? {
            Lexeme::Goto => {
                if let Some((identifier, t)) = Identifier::parse(&l[tokens..]) {
                    tokens += t;

                    if l.get(tokens)? == &Lexeme::Semicolon {
                        tokens += 1;

                        return Some((Box::new(Self::Goto(identifier)), tokens));
                    }
                }
                None
            },
            Lexeme::Continue => {
                if l.get(tokens)? == &Lexeme::Semicolon {
                    tokens += 1;

                    return Some((Box::new(Self::Continue), tokens));
                }

                None
            },
            Lexeme::Break => {
                if l.get(tokens)? == &Lexeme::Semicolon {
                    tokens += 1;

                    return Some((Box::new(Self::Break), tokens));
                }

                None
            },
            Lexeme::Return => {
                let expr = Expression::parse(&l[tokens..])
                    .map(|(a,t)| {
                        tokens += t;
                        a
                    });

                if l.get(tokens)? == &Lexeme::Semicolon {
                    tokens += 1;
                    return Some((Box::new(Self::Return(expr)), tokens));
                }

                None
            },
            _ => return None
        }
    }
}

impl Parse for AssignmentExpression {
    fn parse(l: &[Lexeme]) -> Option<(Box<Self>, usize)>where Self: Sized {
		Self::log(l);
        let mut tokens = 0;

        if let Some((cond, t)) = ConditionalExpression::parse(&l[tokens..]) {
            tokens += t;
            return Some((Box::new(Self::Conditional(cond)), tokens));
        }

        if let Some((unary, t)) = UnaryExpression::parse(&l[tokens..]) {
            tokens += t;

            if let Some((assn_op, t)) = AssignmentOperator::parse(&l[tokens..]) {
                tokens += t;

                if let Some((assn, t)) = AssignmentExpression::parse(&l[tokens..]) {
                    tokens += t;

                    return Some((Box::new(Self::Assign(unary, assn_op, assn)), tokens));
                }
            }
        }

        None
    }
}

impl Parse for AssignmentOperator {
    fn parse(l: &[Lexeme]) -> Option<(Box<Self>, usize)>where Self: Sized {
		Self::log(l);
        Some((Box::new(match l.get(0)? {
            Lexeme::Equal => Self::Assign,
            Lexeme::MulAssign => Self::MulAssign,
            Lexeme::DivAssign => Self::DivAssign,
            Lexeme::ModAssign => Self::ModAssign,
            Lexeme::AddAssign => Self::AddAssign,
            Lexeme::SubAssign => Self::SubAssign,
            Lexeme::LeftShiftAssign => Self::LeftShiftAssign,
            Lexeme::RightShiftAssign => Self::RightShiftAssign,
            Lexeme::BitwiseAndAssign => Self::AndAssign,
            Lexeme::BitwiseOrAssign => Self::OrAssign,
            Lexeme::BitwiseXorAssign => Self::XorAssign,
            Lexeme::BitwiseNotAssign => Self::NotAssign,
            _ => return None
        }), 1))
    }
}

impl Parse for ConditionalExpression {
    fn parse(l: &[Lexeme]) -> Option<(Box<Self>, usize)>where Self: Sized {
		Self::log(l);
        let mut tokens = 0;

        let (or, t) = LogicalOrExpression::parse(&l[tokens..])?;
        tokens += t;

        if l.get(tokens)? == &Lexeme::Question {
            tokens += 1;

            if let Some((expr, t)) = Expression::parse(&l[tokens..]) {
                tokens += t;

                if let Some((cond_expr, t)) = ConditionalExpression::parse(&l[tokens..]) {
                    tokens += t;
                    return Some((Box::new(Self::Ternary(or, expr, cond_expr)), tokens));
                }

                tokens -= t;
            }

            tokens -= 1;
        }

        Some((Box::new(Self::Or(or)), tokens))
    }
}

impl Parse for ConstantExpression {
    fn parse(l: &[Lexeme]) -> Option<(Box<Self>, usize)>where Self: Sized {
		Self::log(l);
        let (expr, t) = ConditionalExpression::parse(l)?;
        Some((Box::new(Self(expr)), t))
    }
}


impl Parse for EqualityExpressionEnd {
    fn parse(l: &[Lexeme]) -> Option<(Box<Self>, usize)>where Self: Sized {
		Self::log(l);
        let mut tokens = 0;
        let op = l.get(tokens)?;
        tokens += 1;

        let (expr, t) = RelationalExpression::parse(&l[tokens..])?;
        tokens += t;
        let end = EqualityExpressionEnd::parse(&l[tokens..])
            .map(|(a,t)| {
                tokens += t;
                a
            });

        Some((Box::new(match op {
            Lexeme::Equal => Self::Equal(expr, end),
            Lexeme::NotEqual => Self::NotEqual(expr, end),
            _ => return None
        }), tokens))
    }
}

impl Parse for RelationalExpressionEnd {
    fn parse(l: &[Lexeme]) -> Option<(Box<Self>, usize)>where Self: Sized {
		Self::log(l);
        let mut tokens = 0;
        let op = l.get(tokens)?;
        tokens += 1;

        let (expr, t) = ShiftExpression::parse(&l[tokens..])?;
        tokens += t;
        let end = RelationalExpressionEnd::parse(&l[tokens..])
            .map(|(a,t)| {
                tokens += t;
                a
            });

        Some((Box::new(match op {
            Lexeme::LessThan => Self::LT(expr, end),
            Lexeme::GreaterThan => Self::GT(expr, end),
            Lexeme::LessThanEqual => Self::LTE(expr, end),
            Lexeme::GreaterThanEqual => Self::GTE(expr, end),
            _ => return None
        }), tokens))
    }
}

impl Parse for ShiftExpressionEnd {
    fn parse(l: &[Lexeme]) -> Option<(Box<Self>, usize)>where Self: Sized {
		Self::log(l);
        let mut tokens = 0;
        let op = l.get(tokens)?;
        tokens += 1;

        let (expr, t) = AdditiveExpression::parse(&l[tokens..])?;
        tokens += t;

        let end = ShiftExpressionEnd::parse(&l[tokens..])
            .map(|(a,t)| {
                tokens += t;
                a
            });

        Some((Box::new(match op {
            Lexeme::LeftShift => Self::LS(expr, end),
            Lexeme::RightShift => Self::RS(expr, end),
            _ => return None
        }), tokens))
    }
}

impl Parse for AdditiveExpressionEnd {
    fn parse(l: &[Lexeme]) -> Option<(Box<Self>, usize)>where Self: Sized {
		Self::log(l);
        let mut tokens = 0;
        let op = l.get(tokens)?;
        tokens += 1;

        let (expr, t) = MultiplicativeExpression::parse(&l[tokens..])?;
        tokens += t;

        let end = AdditiveExpressionEnd::parse(&l[tokens..])
            .map(|(a,t)| {
                tokens += t;
                a
            });

        Some((Box::new(match op {
            Lexeme::Add => Self::Add(expr, end),
            Lexeme::Sub => Self::Sub(expr, end),
            _ => return None
        }), tokens))
    }
}

impl Parse for MultiplicativeExpressionEnd {
    fn parse(l: &[Lexeme]) -> Option<(Box<Self>, usize)>where Self: Sized {
		Self::log(l);
        let mut tokens = 0;
        let op = l.get(tokens)?;
        tokens += 1;

        let (expr, t) = CastExpression::parse(&l[tokens..])?;
        tokens += t;

        let end = MultiplicativeExpressionEnd::parse(&l[tokens..])
            .map(|(a,t)| {
                tokens += t;
                a
            });

        Some((Box::new(match op {
            Lexeme::MulOrPointer => Self::Mul(expr, end),
            Lexeme::Div => Self::Div(expr, end),
            Lexeme::Mod => Self::Mod(expr, end),
            _ => return None
        }), tokens))
    }
}

impl Parse for CastExpression {
    fn parse(l: &[Lexeme]) -> Option<(Box<Self>, usize)>where Self: Sized {
		Self::log(l);
        let mut tokens = 0;
        if let Some((expr, t)) = UnaryExpression::parse(&l[tokens..]) {
            tokens += t;
            return Some((Box::new(Self::Unary(expr)), tokens));
        }

        if l.get(tokens)? != &Lexeme::LeftParen { return None; }
        tokens += 1;

        let (type_name, t) = TypeName::parse(&l[tokens..])?;
        tokens += t;

        if l.get(tokens)? != &Lexeme::RightParen { return None; }
        tokens += 1;

        let (expr, t) = CastExpression::parse(&l[tokens..])?;
        tokens += t;

        Some((Box::new(Self::Cast(type_name, expr)), tokens))
    }
}

impl Parse for UnaryExpression {
    fn parse(l: &[Lexeme]) -> Option<(Box<Self>, usize)>where Self: Sized {
		Self::log(l);
        let mut tokens = 0;

        if let Some((expr, t)) = PostfixExpression::parse(&l[tokens..]) {
            tokens += t;
            return Some((Box::new(Self::Postfix(expr)), tokens));
        }

        if let Some((op, t)) = UnaryOperator::parse(&l[tokens..]) {
            tokens += t;

            if let Some((cast, t)) = CastExpression::parse(&l[tokens..]) {
                tokens += t;
                return Some((Box::new(Self::Cast(op, cast)), tokens));
            }

            tokens -= t;
        }

        match l.get(tokens)? {
            Lexeme::Increment => {
                tokens += 1;

                let (expr, t) = UnaryExpression::parse(&l[tokens..])?;
                tokens += t;

                Some((Box::new(Self::Increment(expr)), tokens))
            },
            Lexeme::Decrement => {
                tokens += 1;

                let (expr, t) = UnaryExpression::parse(&l[tokens..])?;
                tokens += t;

                Some((Box::new(Self::Decrement(expr)), tokens))

            },
            Lexeme::Sizeof => {
                tokens += 1;
                if let Some((expr, t)) = UnaryExpression::parse(&l[tokens..]) {
                    tokens += t;
                    Some((Box::new(Self::Sizeof(expr)), tokens))
                } else if l.get(tokens)? == &Lexeme::LeftParen {
                    tokens += 1;

                    let (type_name, t) = TypeName::parse(&l[tokens..])?;
                    tokens += t;

                    if l.get(tokens)? != &Lexeme::RightParen {
                        return None;
                    }
                    tokens += 1;

                    Some((Box::new(Self::SizeofType(type_name)), tokens))
                } else {
                    None
                }
            }
            _ => None
        }
    }
}

impl Parse for UnaryOperator {
    fn parse(l: &[Lexeme]) -> Option<(Box<Self>, usize)>where Self: Sized {
		Self::log(l);
        Some((Box::new(match l.get(0)? {
            Lexeme::BitwiseAndReference => Self::Ref,
            Lexeme::MulOrPointer => Self::Deref,
            Lexeme::Add => Self::Pos,
            Lexeme::Sub => Self::Neg,
            Lexeme::BitwiseNot => Self::Inv,
            Lexeme::Not => Self::Not,
            _ => return None
        }), 1))
    }
}

impl Parse for PostfixExpression {
    fn parse(l: &[Lexeme]) -> Option<(Box<Self>, usize)>where Self: Sized {
		Self::log(l);
        let mut tokens = 0;
        let (primary, t) = PrimaryExpression::parse(&l[tokens..])?;
        tokens += t;

        let mut suffixes = Vec::new();

        while let Some((postfix, t)) = PostfixExpressionEnd::parse(&l[tokens..]) {
            tokens += t;
            suffixes.push(postfix);
        }

        Some((Box::new(Self {
            primary,
            suffixes: Box::new(suffixes)
        }), tokens))
    }
}

impl Parse for PostfixExpressionEnd {
    fn parse(l: &[Lexeme]) -> Option<(Box<Self>, usize)>where Self: Sized {
		Self::log(l);
        let mut tokens = 0;
        let op = l.get(tokens)?;
        tokens += 1;

        Some(match op {
            Lexeme::Increment => {
                (Box::new(Self::Increment), tokens)
            }
            Lexeme::Decrement => {
                (Box::new(Self::Decrement), tokens)
            }
            Lexeme::LeftBracket => {
                let (expr, t) = Expression::parse(&l[tokens..])?;
                tokens += t;

                if l.get(tokens)? != &Lexeme::RightBracket { return None; }
                tokens += 1;

                (Box::new(Self::Index(expr)), tokens)
            }
            Lexeme::LeftParen => {
                let list = ArgumentExpressionList::parse(&l[tokens..])
                    .map(|(a,t)| {
                        tokens += t;
                        a
                    });

                if l.get(tokens)? != &Lexeme::RightParen { return None; }

                (Box::new(Self::Call(list)), tokens)
            }
            Lexeme::Dot => {
                let (ident, t) = Identifier::parse(&l[tokens..])?;
                tokens += t;
                (Box::new(Self::Dot(ident)), tokens)
            },
            Lexeme::Arrow => {
                let (ident, t) = Identifier::parse(&l[tokens..])?;
                tokens += t;
                (Box::new(Self::Deref(ident)), tokens)
            },
            _ => return None
        })
    }
}

impl Parse for PrimaryExpression {
    fn parse(l: &[Lexeme]) -> Option<(Box<Self>, usize)>where Self: Sized {
		Self::log(l);
        let mut tokens = 0;

        if let Some((ident, t)) = Identifier::parse(&l[tokens..]) {
            tokens += t;
            return Some((Box::new(Self::Identifier(ident)), tokens));
        }

        if let Some((c, t)) = Constant::parse(&l[tokens..]) {
            tokens += t;
            return Some((Box::new(Self::Constant(c)), tokens));
        }

        if let Lexeme::StringLiteral(s) = &l.get(tokens)? {
            tokens += 1;
            return Some((Box::new(Self::String(s.clone())), tokens));
        }

        if l.get(tokens)? != &Lexeme::LeftParen { return None; }
        tokens += 1;

        let (expr, t) = Expression::parse(&l[tokens..])?;
        tokens += t;

        if l.get(tokens)? != &Lexeme::RightParen { return None; }
        tokens += 1;

        Some((Box::new(Self::Expression(expr)), tokens))
    }
}

impl Parse for Constant {
    fn parse(l: &[Lexeme]) -> Option<(Box<Self>, usize)>where Self: Sized {
		Self::log(l);
        Some((Box::new(match l.get(0)? {
            Lexeme::IntLiteral(i) => Self::Int(*i),
            Lexeme::CharLiteral(c) => Self::Char(*c),
            Lexeme::FloatLiteral(f) => Self::Float(*f),
            _ => return None
        }), 1))
    }
}
