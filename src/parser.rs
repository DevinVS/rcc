use std::iter::Peekable;
use crate::lexeme::Lexeme;
use crate::ast::*;
use std::fmt::Debug;

pub trait Parse {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> where Self: Sized;
}

impl<T: Debug + Parse> Parse for Vec<T> {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {
        let mut items = vec![];

        while let Some(i) = T::parse(l) {
            items.push(i);
        }

        if items.len() == 0 {
            None
        } else {
            Some(items)
        }
    }
}

impl<T: Parse> Parse for CSVec<T> {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {
        let mut items = vec![];

        let first = T::parse(l);
        if first.is_none() {
            return None;
        }

        items.push(first.unwrap());

        while Some(&Lexeme::Comma) == l.peek() {
            l.next().unwrap();
            items.push(T::parse(l).unwrap());
        }

        Some(CSVec{ items })
    }
}

impl Parse for Identifier {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {
        if let Some(Lexeme::Identifier(val)) = l.peek() {
            let val = val.clone();
            l.next().unwrap();
            Some(Identifier(val))
        } else {
            None
        }
    }
}

impl Parse for ExternalDeclaration {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {
        if let Some(fd) = FunctionDefinition::parse(l) {
            Some(ExternalDeclaration::FunctionDefinition(fd))
        } else if let Some(decl) = Declaration::parse(l) {
            Some(ExternalDeclaration::Declaration(decl))
        } else {
            None
        }
    }
}

impl Parse for FunctionDefinition {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {
        let decl_spec = DeclarationSpecifiers::parse(l);
        let declarator = Declarator::parse(l);

        if declarator.is_none() {
            return None;
        }

        let decl_list = DeclarationList::parse(l);
        let compound = CompoundStatement::parse(l);

        if compound.is_none() {
            return None;
        }

        Some(FunctionDefinition {
            declaration_specifiers: decl_spec,
            declarator: declarator.unwrap(),
            declaration_list: decl_list,
            compound: compound.unwrap()
        })
    }
}

impl Parse for Declaration {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {
        let specs = DeclarationSpecifiers::parse(l);

        if specs.is_none() {
            return None;
        }

        let init_list = InitDeclaratorList::parse(l);

        assert_eq!(Lexeme::Semicolon, l.next().unwrap());

        Some(Declaration {
            specs: specs.unwrap(),
            init_list
        })
    }
}

impl Parse for DeclarationSpecifier {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {
        if let Some(scs) = StorageClassSpecifier::parse(l) {
            Some(Self::StorageClassSpecifier(scs))
        } else if let Some(ts) = TypeSpecifier::parse(l) {
            Some(Self::TypeSpecifier(ts))
        } else if let Some(tq) = TypeQualifier::parse(l) {
            Some(Self::TypeQualifier(tq))
        } else {
            None
        }
    }
}

impl Parse for StorageClassSpecifier {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {
        let res = match l.peek() {
            Some(Lexeme::Auto) => Some(Self::Auto),
            Some(Lexeme::Register) => Some(Self::Register),
            Some(Lexeme::Static) => Some(Self::Static),
            Some(Lexeme::Extern) => Some(Self::Extern),
            Some(Lexeme::Typedef) => Some(Self::Typedef),
            _ => None
        };

        if res.is_some() {l.next().unwrap();}
        res
    }
}

impl Parse for TypeSpecifier {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {
        let res = match l.peek() {
            Some(Lexeme::Void) => Some(Self::Void),
            Some(Lexeme::Char) => Some(Self::Char),
            Some(Lexeme::Short) => Some(Self::Short),
            Some(Lexeme::Int) => Some(Self::Int),
            Some(Lexeme::Long) => Some(Self::Long),
            Some(Lexeme::Float) => Some(Self::Float),
            Some(Lexeme::Double) => Some(Self::Double),
            Some(Lexeme::Signed) => Some(Self::Signed),
            Some(Lexeme::Unsigned) => Some(Self::Unsigned),
            _ => None
        };

        let res = if res.is_none() {
            if let Some(sou) = StructOrUnionSpecifier::parse(l) {
                Some(Self::StructOrUnion(sou))
            } else if let Some(es) = EnumSpecifier::parse(l) {
                Some(Self::Enum(es))
            } else if let Some(tn) = TypedefName::parse(l) {
                Some(Self::Typedef(tn))
            } else {
                None
            }
        } else {
            l.next().unwrap();
            res
        };

        res
    }
}

impl Parse for TypeQualifier {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {
        let res = match l.peek() {
            Some(Lexeme::Const) => Some(Self::Const),
            Some(Lexeme::Volatile) => Some(Self::Volatile),
            _ => None
        };

        if res.is_some() {l.next().unwrap();}
        res
    }
}

impl Parse for StructOrUnionSpecifier {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {
        let sou = StructOrUnion::parse(l);

        if sou.is_none() {
            return None;
        }

        let identifier = Identifier::parse(l);
        let sdl = StructDeclarationList::parse(l);

        if sdl.is_some() {
            Some(Self::List(sou.unwrap(), identifier, sdl.unwrap()))
        } else if identifier.is_some() {
            Some(Self::Val(sou.unwrap(), identifier.unwrap()))
        } else {
            None
        }
    }
}

impl Parse for StructOrUnion {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {
        let res = match l.peek() {
            Some(Lexeme::Struct) => Some(Self::Struct),
            Some(Lexeme::Union) => Some(Self::Union),
            _ => None
        };

        if res.is_some() { l.next().unwrap(); }
        res
    }
}

impl Parse for InitDeclarator {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {
        let decl = Declarator::parse(l);

        if decl.is_none() {
            return None;
        }

        if Some(&Lexeme::Equal) == l.peek() {
            let init = Initializer::parse(l).unwrap();
            Some(Self::Assign(decl.unwrap(), init))
        } else {
            Some(Self::Declarator(decl.unwrap()))
        }
    }
}

impl Parse for StructDeclaration {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {
        let qual = SpecifierQualifierList::parse(l);

        if qual.is_none() {
            return None;
        }

        let sdl = StructDeclarationList::parse(l).unwrap();
        assert_eq!(Some(Lexeme::Semicolon), l.next());

        Some(Self{
            spec_qualifier_list: qual.unwrap(),
            decl_list: sdl
        })
    }
}

impl Parse for SpecifierOrQualifier {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {
        if let Some(ts) = TypeSpecifier::parse(l) {
            Some(Self::Specifier(ts))
        } else if let Some(tq) = TypeQualifier::parse(l) {
            Some(Self::Qualifier(tq))
        } else {
            None
        }
    }
}

impl Parse for StructDeclarator {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {
        let declarator = Declarator::parse(l);

        if Some(&Lexeme::Colon) == l.peek() {
            let expr = ConstantExpression::parse(l).unwrap();
            Some(Self::Const(declarator, expr))
        } else {
            Some(Self::Decl(declarator.unwrap()))
        }
    }
}

impl Parse for EnumSpecifier {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {
        if Some(&Lexeme::Enum) != l.peek() {
            return None;
        }
        l.next().unwrap();

        let identifier = Identifier::parse(l);

        if Some(&Lexeme::LeftBrace) == l.peek() {
            l.next().unwrap();
            let enum_list = EnumeratorList::parse(l).unwrap();
            assert_eq!(Some(Lexeme::RightBrace), l.next());

            Some(Self::List(identifier, enum_list))
        } else {
            Some(Self::Identifier(identifier.unwrap()))
        }
    }
}


impl Parse for Enumerator {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {
        let identifier = Identifier::parse(l);

        if identifier.is_none() {
            return None;
        }

        if Some(&Lexeme::Assign) == l.peek() {
            l.next().unwrap();
            let expr = ConstantExpression::parse(l).unwrap();
            Some(Self::ConstantExpression(identifier.unwrap(), expr))
        } else {
            Some(Self::Identifier(identifier.unwrap()))
        }
    }
}


impl Parse for Declarator {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {
        let pointer = Pointer::parse(l);
        let decl = DirectDeclarator::parse(l);

        if pointer.is_none() && decl.is_none() {
            return None;
        }

        Some(Self { pointer, decl: decl.unwrap() })
    }
}

impl Parse for DirectDeclarator {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {
        if let Some(ident) = Identifier::parse(l) {
            let dd = DirectDeclaratorEnd::parse(l);
            Some(DirectDeclarator::Identifier(ident, dd))
        } else if Some(&Lexeme::LeftParen) == l.peek() {
            let decl = Declarator::parse(l).unwrap();
            assert_eq!(Some(Lexeme::RightParen), l.next());
            let dd = DirectDeclaratorEnd::parse(l);

            Some(DirectDeclarator::Declarator(Box::new(decl), dd))
        } else {
            None
        }
    }
}

impl Parse for DirectDeclaratorEnd {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {
        if Some(&Lexeme::LeftBracket) == l.peek() {
            l.next().unwrap();
            let const_expr = ConstantExpression::parse(l);
            assert_eq!(Some(Lexeme::RightBracket), l.next());
            let dd = DirectDeclaratorEnd::parse(l).map(|e| Box::new(e));
            Some(Self::ConstantExpression(const_expr, dd))
        } else if Some(&Lexeme::LeftParen) == l.peek() {
            l.next().unwrap();
            if let Some(pl) = ParameterTypeList::parse(l) {
                assert_eq!(Some(Lexeme::RightParen), l.next());
                let dd = DirectDeclaratorEnd::parse(l).map(|e| Box::new(e));
                Some(Self::ParameterTypeList(pl, dd))
            } else {
                let idents = IdentifierList::parse(l);
                assert_eq!(Some(Lexeme::RightParen), l.next());
                let dd = DirectDeclaratorEnd::parse(l).map(|e| Box::new(e));
                Some(Self::IdentifierList(idents, dd))
            }
        } else {
            None
        }
    }
}

impl Parse for Pointer {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {
        if Some(&Lexeme::MulOrPointer) != l.peek() {
            return None;
        }
        l.next().unwrap();

        let tql = TypeQualifierList::parse(l);

        if let Some(pointer) = Pointer::parse(l) {
            Some(Pointer::List(tql, Box::new(pointer)))
        } else {
            Some(Pointer::Pointer(tql))
        }
    }
}

impl Parse for ParameterTypeList {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {
        let param_list = ParameterList::parse(l);

        if param_list.is_none() {
            return None;
        }

        if Some(&Lexeme::Comma) == l.peek() {
            l.next().unwrap();
            assert_eq!(Some(Lexeme::Dot), l.next());
            assert_eq!(Some(Lexeme::Dot), l.next());
            assert_eq!(Some(Lexeme::Dot), l.next());

            Some(Self::VarArgs(param_list.unwrap()))
        } else {
            Some(Self::Regular(param_list.unwrap()))
        }
    }
}


impl Parse for ParameterDeclaration {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {
        let decl_specs = DeclarationSpecifiers::parse(l);

        if decl_specs.is_none() {
            return None;
        }

        if let Some(decl) = Declarator::parse(l) {
            Some(ParameterDeclaration::Declaration(decl_specs.unwrap(), decl))
        } else {
            let decls = AbstractDeclarator::parse(l);
            Some(ParameterDeclaration::AbstractDeclarator(decl_specs.unwrap(), decls))
        }
    }
}

impl Parse for Initializer {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {
        if let Some(expr) = AssignmentExpression::parse(l) {
            Some(Self::Assign(expr))
        } else if Some(&Lexeme::LeftBrace) == l.peek() {
            l.next().unwrap();

            let init_list = InitializerList::parse(l).unwrap();

            if Some(&Lexeme::Comma) == l.peek() {
                l.next().unwrap();
            }

            assert_eq!(Some(Lexeme::RightBrace), l.next());

            Some(Self::List(init_list))
        } else {
            None
        }
    }
}

impl Parse for TypeName {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {
        let sql = SpecifierQualifierList::parse(l);
        if sql.is_none() { return None; }

        let decl = AbstractDeclarator::parse(l);

        Some(Self{ list: sql.unwrap(), declarator: decl.map(|e| Box::new(e)) })
    }
}

impl Parse for AbstractDeclarator {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {
        let pointer = Pointer::parse(l);
        let dad = DirectAbstractDeclarator::parse(l);

        if dad.is_some() {
            Some(Self::Declarator(pointer, dad.unwrap()))
        } else if pointer.is_some() {
            Some(Self::Pointer(pointer.unwrap()))
        } else {
            None
        }
    }
}

impl Parse for DirectAbstractDeclarator {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {

        if Some(&Lexeme::LeftParen) == l.peek() {
            l.next().unwrap();
            if let Some(ad) = AbstractDeclarator::parse(l) {
                assert_eq!(Some(Lexeme::RightParen), l.next());
                let dad = DirectAbstractDeclaratorEnd::parse(l);
                Some(Self::Decl(Box::new(ad), dad))
            } else {
                let params = ParameterTypeList::parse(l);
                assert_eq!(Some(Lexeme::RightParen), l.next());
                let dad = DirectAbstractDeclaratorEnd::parse(l);
                Some(Self::Param(params, dad))
            }
        } else if Some(&Lexeme::LeftBracket) == l.peek() {
            l.next().unwrap();
            let expr = ConstantExpression::parse(l);
            assert_eq!(Some(Lexeme::RightBracket), l.next());
            let dad = DirectAbstractDeclaratorEnd::parse(l);
            Some(Self::Const(expr, dad))
        } else {
            None
        }
    }
}

impl Parse for DirectAbstractDeclaratorEnd {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {
        if Some(&Lexeme::LeftParen) == l.peek() {
            l.next().unwrap();
            let params = ParameterTypeList::parse(l);
            assert_eq!(Some(Lexeme::RightParen), l.next());
            let dad = DirectAbstractDeclaratorEnd::parse(l).map(Box::new);
            Some(Self::Param(params, dad))
        } else if Some(&Lexeme::LeftBracket) == l.peek() {
            l.next().unwrap();
            let expr = ConstantExpression::parse(l);
            assert_eq!(Some(Lexeme::RightBracket), l.next());
            let dad = DirectAbstractDeclaratorEnd::parse(l).map(Box::new);
            Some(Self::Const(expr, dad))
        } else {
            None
        }
    }
}

impl Parse for TypedefName {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {
        if let Some(Lexeme::TypeName(n)) = l.peek() {
            let n = n.clone();
            l.next().unwrap();
            Some(Self{ name: n})
        } else {
            None
        }
    }
}

impl Parse for Statement {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {
        if let Some(ls) = LabeledStatement::parse(l) {
            Some(Self::Labeled(Box::new(ls)))
        } else if let Some(es) = ExpressionStatement::parse(l) {
            Some(Self::Expression(Box::new(es)))
        } else if let Some(cs) = CompoundStatement::parse(l) {
            Some(Self::Compound(Box::new(cs)))
        } else if let Some(ss) = SelectionStatement::parse(l) {
            Some(Self::Selection(Box::new(ss)))
        } else if let Some(is) = IterationStatement::parse(l) {
            Some(Self::Iteration(Box::new(is)))
        } else if let Some(js) = JumpStatement::parse(l) {
            Some(Self::Jump(Box::new(js)))
        } else {
            None
        }
    }
}

impl Parse for LabeledStatement {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {
        if let Some(ident) = Identifier::parse(l) {
            assert_eq!(Some(Lexeme::Colon), l.next());

            let statement = Statement::parse(l).unwrap();
            Some(Self::Map(ident, statement))
        } else if Some(&Lexeme::Case) == l.peek() {
            l.next().unwrap();
            let expr = ConstantExpression::parse(l).unwrap();
            assert_eq!(Some(Lexeme::Colon), l.next());

            let statement = Statement::parse(l).unwrap();
            Some(Self::Case(expr, statement))
        } else if Some(&Lexeme::Default) == l.peek() {
            l.next().unwrap();
            assert_eq!(Some(Lexeme::Colon), l.next());

            let statement = Statement::parse(l).unwrap();
            Some(Self::Default(statement))
        } else {
            None
        }
    }
}

impl Parse for ExpressionStatement {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {
        let expr = Expression::parse(l);

        if expr.is_some() {
            Some(Self { expression: expr })
        } else if Some(&Lexeme::Semicolon) == l.peek() {
            l.next().unwrap();
            Some(Self { expression: expr })
        } else {
            None
        }
    }
}

impl Parse for CompoundStatement {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {
        if Some(&Lexeme::LeftBrace) == l.peek() {
            l.next().unwrap();

            let decl_list = DeclarationList::parse(l);
            let statement_list = StatementList::parse(l);

            assert_eq!(Some(Lexeme::RightBrace), l.next());

            Some(Self { declarations: decl_list, statements: statement_list })
        } else {
            None
        }
    }
}

impl Parse for SelectionStatement {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {
        if Some(&Lexeme::If) == l.peek() {
            l.next().unwrap();
            assert_eq!(Some(Lexeme::LeftParen), l.next());

            let expr = Expression::parse(l).unwrap();
            assert_eq!(Some(Lexeme::RightParen), l.next());

            let stmt = Statement::parse(l).unwrap();

            if Some(&Lexeme::Else) == l.peek() {
                l.next().unwrap();
                let else_stmt = Statement::parse(l).unwrap();
                Some(Self::IfElse(expr, stmt, else_stmt))
            } else {
                Some(Self::If(expr, stmt))
            }
        } else if Some(&Lexeme::Switch) == l.peek() {
            l.next().unwrap();
            assert_eq!(Some(Lexeme::LeftParen), l.next());

            let expr = Expression::parse(l).unwrap();

            assert_eq!(Some(Lexeme::RightParen), l.next());

            let stmt = Statement::parse(l).unwrap();

            Some(Self::Switch(expr, stmt))
        } else {
            None
        }
    }
}


impl Parse for IterationStatement {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {
        if Some(&Lexeme::While) == l.peek() {
            l.next().unwrap();
            assert_eq!(Some(Lexeme::LeftParen), l.next());
            let expr = Expression::parse(l).unwrap();
            assert_eq!(Some(Lexeme::RightParen), l.next());
            let stmt = Statement::parse(l).unwrap();

            Some(Self::While(expr, stmt))
        } else if Some(&Lexeme::Do) == l.peek() {
            l.next().unwrap();
            let stmt = Statement::parse(l).unwrap();
            assert_eq!(Some(Lexeme::While), l.next());
            assert_eq!(Some(Lexeme::LeftParen), l.next());
            let expr = Expression::parse(l).unwrap();
            assert_eq!(Some(Lexeme::RightParen), l.next());
            assert_eq!(Some(Lexeme::Semicolon), l.next());

            Some(Self::DoWhile(stmt, expr))
        } else if Some(&Lexeme::For) == l.peek() {
            l.next().unwrap();
            assert_eq!(Some(Lexeme::LeftParen), l.next());

            let expr1 = Expression::parse(l);
            assert_eq!(Some(Lexeme::Semicolon), l.next());
            let expr2 = Expression::parse(l);
            assert_eq!(Some(Lexeme::Semicolon), l.next());
            let expr3 = Expression::parse(l);

            assert_eq!(Some(Lexeme::RightParen), l.next());
            let stmt = Statement::parse(l).unwrap();

            Some(Self::For(expr1, expr2, expr3, stmt))
        } else {
            None
        }
    }
}

impl Parse for JumpStatement {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {
        match l.peek() {
            Some(Lexeme::Goto) => {
                l.next().unwrap();
                let ident = Identifier::parse(l).unwrap();
                assert_eq!(Some(Lexeme::Semicolon), l.next());

                Some(Self::Goto(ident))
            }
            Some(Lexeme::Continue) => {
                l.next().unwrap();
                assert_eq!(Some(Lexeme::Semicolon), l.next());

                Some(Self::Continue)
            }
            Some(Lexeme::Break) => {
                l.next().unwrap();
                assert_eq!(Some(Lexeme::Semicolon), l.next());

                Some(Self::Break)
            }
            Some(Lexeme::Return) => {
                l.next().unwrap();
                let expr = Expression::parse(l);
                assert_eq!(Some(Lexeme::Semicolon), l.next());
                Some(Self::Return(expr))
            }
            _ => None
        }
    }
}

impl Parse for AssignmentExpression {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {
        if let Some(ce) = ConditionalExpression::parse(l) {
            Some(Self::Conditional(ce))
        } else if let Some(ue) = UnaryExpression::parse(l) {
            let ao = AssignmentOperator::parse(l).unwrap();
            let ae = Box::new(AssignmentExpression::parse(l).unwrap());

            Some(Self::Assign(Box::new(ue), ao, ae))
        } else {
            None
        }
    }
}

impl Parse for AssignmentOperator {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {
        let res = match l.peek() {
            Some(Lexeme::Assign) => Some(Self::Assign),
            Some(Lexeme::MulAssign) => Some(Self::MulAssign),
            Some(Lexeme::DivAssign) => Some(Self::DivAssign),
            Some(Lexeme::ModAssign) => Some(Self::ModAssign),
            Some(Lexeme::AddAssign) => Some(Self::AddAssign),
            Some(Lexeme::SubAssign) => Some(Self::SubAssign),
            Some(Lexeme::LeftShiftAssign) => Some(Self::LeftShiftAssign),
            Some(Lexeme::RightShiftAssign) => Some(Self::RightShiftAssign),
            Some(Lexeme::LogicalAndAssign) => Some(Self::AndAssign),
            Some(Lexeme::LogicalOrAssign) => Some(Self::OrAssign),
            Some(Lexeme::LogicalXorAssign) => Some(Self::XorAssign),
            Some(Lexeme::LogicalNotAssign) => Some(Self::NotAssign),
            _ => None
        };

        if res.is_some() { l.next().unwrap(); }
        res
    }
}

impl Parse for ConditionalExpression {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {
        let loe = LogicalOrExpression::parse(l);

        if loe.is_none() {
            return None;
        }

        if Some(&Lexeme::Question) == l.peek() {
            l.next().unwrap();

            let expr = Expression::parse(l).unwrap();
            assert_eq!(Some(Lexeme::Colon), l.next());
            let cond_expr = Box::new(ConditionalExpression::parse(l).unwrap());

            Some(Self::Ternary(loe.unwrap(), expr, cond_expr))
        } else {
            Some(Self::Or(loe.unwrap()))
        }
    }
}

impl Parse for ConstantExpression {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {
        let expr = ConditionalExpression::parse(l);

        expr.map(|e| Self(e))
    }
}

impl Parse for LogicalOrExpression {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {
        if let Some(expr) = LogicalAndExpression::parse(l) {
            let next = LogicalOrExpressionEnd::parse(l);
            Some(Self { expr, next })
        } else {
            None
        }
    }
}

impl Parse for LogicalOrExpressionEnd {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {
        if Some(&Lexeme::Or) == l.peek() {
            l.next().unwrap();
            let expr = LogicalAndExpression::parse(l).unwrap();
            let next = LogicalOrExpressionEnd::parse(l).map(Box::new);
            Some(Self{expr, next})
        } else {
            None
        }
    }
}

impl Parse for LogicalAndExpression {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {
        if let Some(expr) = InclusiveOrExpression::parse(l) {
            let next = LogicalAndExpressionEnd::parse(l);
            Some(Self{next, expr})
        } else {
            None
        }
    }
}

impl Parse for LogicalAndExpressionEnd {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {
        if Some(&Lexeme::And) == l.peek() {
            l.next().unwrap();
            let expr = InclusiveOrExpression::parse(l).unwrap();
            let next = LogicalAndExpressionEnd::parse(l).map(Box::new);
            Some(Self{expr, next})
        } else {
            None
        }
    }
}

impl Parse for InclusiveOrExpression {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {
        if let Some(expr) = ExclusiveOrExpression::parse(l) {
            let next = InclusiveOrExpressionEnd::parse(l);
            Some(Self{expr, next})
        } else {
            None
        }
    }
}

impl Parse for InclusiveOrExpressionEnd {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {
        if Some(&Lexeme::LogicalOr) == l.peek() {
            l.next().unwrap();
            let expr = ExclusiveOrExpression::parse(l).unwrap();
            let next = InclusiveOrExpressionEnd::parse(l).map(Box::new);
            Some(Self{expr,next})
        } else {
            None
        }
    }
}

impl Parse for ExclusiveOrExpression {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {
        if let Some(expr) = AndExpression::parse(l) {
            let next = ExclusiveOrExpressionEnd::parse(l);
            Some(Self{expr, next})
        } else {
            None
        }
    }
}

impl Parse for ExclusiveOrExpressionEnd {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {
        if Some(&Lexeme::LogicalXor) == l.peek() {
            l.next().unwrap();
            let expr = AndExpression::parse(l).unwrap();
            let next = ExclusiveOrExpressionEnd::parse(l).map(Box::new);
            Some(Self{expr, next})
        } else {
            None
        }
    }
}

impl Parse for AndExpression {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {
        if let Some(expr) = EqualityExpression::parse(l) {
            let next = AndExpressionEnd::parse(l);
            Some(Self{ expr, next })
        } else {
            None
        }
    }
}

impl Parse for AndExpressionEnd {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {
        if Some(&Lexeme::LogicalAndReference) == l.peek() {
            l.peek().unwrap();
            let expr = EqualityExpression::parse(l).unwrap();
            let next = AndExpressionEnd::parse(l).map(Box::new);
            Some(Self{expr, next})
        } else {
            None
        }
    }
}

impl Parse for EqualityExpression {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {
        if let Some(expr) = RelationalExpression::parse(l) {
            let next = EqualityExpressionEnd::parse(l);
            Some(Self{expr, next})
        } else {
            None
        }
    }
}

impl Parse for EqualityExpressionEnd {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {
        if Some(&Lexeme::Equal) == l.peek() {
            l.next().unwrap();
            let expr = RelationalExpression::parse(l).unwrap();
            let next = EqualityExpressionEnd::parse(l).map(Box::new);
            Some(Self::Equal(expr, next))
        } else if Some(&Lexeme::Sub) == l.peek() {
            l.next().unwrap();
            let expr = RelationalExpression::parse(l).unwrap();
            let next = EqualityExpressionEnd::parse(l).map(Box::new);
            Some(Self::NotEqual(expr, next))
        } else {
            None
        }
    }
}

impl Parse for RelationalExpression {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {
        if let Some(expr) = ShiftExpression::parse(l) {
            let next = RelationalExpressionEnd::parse(l);
            Some(Self{next, expr})
        } else {
            None
        }
    }
}

impl Parse for RelationalExpressionEnd {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {
        if Some(&Lexeme::LessThan) == l.peek() {
            l.next().unwrap();
            let expr = ShiftExpression::parse(l).unwrap();
            let next = RelationalExpressionEnd::parse(l).map(Box::new);
            Some(Self::LT(expr, next))
        } else if Some(&Lexeme::GreaterThan) == l.peek() {
            l.next().unwrap();
            let expr = ShiftExpression::parse(l).unwrap();
            let next = RelationalExpressionEnd::parse(l).map(Box::new);
            Some(Self::GT(expr, next))
        } else if Some(&Lexeme::LessThanEqual) == l.peek() {
            l.next().unwrap();
            let expr = ShiftExpression::parse(l).unwrap();
            let next = RelationalExpressionEnd::parse(l).map(Box::new);
            Some(Self::LTE(expr, next))
        } else if Some(&Lexeme::GreaterThanEqual) == l.peek() {
            l.next().unwrap();
            let expr = ShiftExpression::parse(l).unwrap();
            let next = RelationalExpressionEnd::parse(l).map(Box::new);
            Some(Self::GTE(expr, next))
        } else {
            None
        }
    }
}

impl Parse for ShiftExpression {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {
        if let Some(expr) = AdditiveExpression::parse(l) {
            let next = ShiftExpressionEnd::parse(l);
            Some(Self{expr, next})
        } else {
            None
        }
    }
}

impl Parse for ShiftExpressionEnd {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {
        if Some(&Lexeme::LeftShift) == l.peek() {
            l.next().unwrap();
            let expr = AdditiveExpression::parse(l).unwrap();
            let next = ShiftExpressionEnd::parse(l).map(Box::new);
            Some(Self::LS(expr, next))
        } else if Some(&Lexeme::RightShift) == l.peek() {
            l.next().unwrap();
            let expr = AdditiveExpression::parse(l).unwrap();
            let next = ShiftExpressionEnd::parse(l).map(Box::new);
            Some(Self::RS(expr, next))
        } else {
            None
        }
    }
}

impl Parse for AdditiveExpression {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {
        if let Some(expr) = MultiplicativeExpression::parse(l) {
            let next = AdditiveExpressionEnd::parse(l);
            Some(Self{expr, next})
        } else {
            None
        }
    }
}

impl Parse for AdditiveExpressionEnd {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {
        if Some(&Lexeme::Add) == l.peek() {
            l.next().unwrap();
            let expr = MultiplicativeExpression::parse(l).unwrap();
            let next = AdditiveExpressionEnd::parse(l).map(Box::new);
            Some(Self::Add(expr, next))
        } else if Some(&Lexeme::Sub) == l.peek() {
            l.next().unwrap();
            let expr = MultiplicativeExpression::parse(l).unwrap();
            let next = AdditiveExpressionEnd::parse(l).map(Box::new);
            Some(Self::Sub(expr, next))
        } else {
            None
        }
    }
}

impl Parse for MultiplicativeExpression {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {
        if let Some(expr) = CastExpression::parse(l) {
            let next = MultiplicativeExpressionEnd::parse(l);
            Some(Self{expr, next})
        } else {
            None
        }
    }
}

impl Parse for MultiplicativeExpressionEnd {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {
        if Some(&Lexeme::MulOrPointer) == l.peek() {
            l.next().unwrap();
            let expr = CastExpression::parse(l).unwrap();
            let next = MultiplicativeExpressionEnd::parse(l).map(Box::new);
            Some(Self::Mul(expr, next))
        } else if Some(&Lexeme::Div) == l.peek() {
            l.next().unwrap();
            let expr = CastExpression::parse(l).unwrap();
            let next = MultiplicativeExpressionEnd::parse(l).map(Box::new);
            Some(Self::Div(expr, next))
        } else if Some(&Lexeme::Mod) == l.peek() {
            l.next().unwrap();
            let expr = CastExpression::parse(l).unwrap();
            let next = MultiplicativeExpressionEnd::parse(l).map(Box::new);
            Some(Self::Mod(expr, next))
        } else {
            None
        }
    }
}

impl Parse for CastExpression {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {
        if let Some(unary_expr) = UnaryExpression::parse(l) {
            Some(Self::Unary(Box::new(unary_expr)))
        } else if Some(&Lexeme::LeftParen) == l.peek() {
            l.next().unwrap();
            let type_name = TypeName::parse(l).unwrap();
            assert_eq!(Some(Lexeme::RightParen), l.next());
            let expr = Box::new(CastExpression::parse(l).unwrap());

            Some(Self::Cast(type_name, expr))
        } else {
            None
        }
    }
}

impl Parse for UnaryExpression {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {
        if let Some(postfix) = PostfixExpression::parse(l) {
            Some(Self::Postfix(postfix))
        } else if Some(&Lexeme::Increment) == l.peek() {
            l.next().unwrap();
            let expr = Box::new(UnaryExpression::parse(l).unwrap());
            Some(Self::Increment(expr))
        } else if Some(&Lexeme::Decrement) == l.peek() {
            l.next().unwrap();
            let expr = Box::new(UnaryExpression::parse(l).unwrap());
            Some(Self::Decrement(expr))
        } else if let Some(unary_op) = UnaryOperator::parse(l) {
            let cast_expr = CastExpression::parse(l).unwrap();
            Some(Self::Cast(unary_op, cast_expr))
        } else if Some(&Lexeme::Sizeof) == l.peek() {
            l.next().unwrap();

            if let Some(unary_expr) = UnaryExpression::parse(l) {
                Some(Self::Sizeof(Box::new(unary_expr)))
            } else if Some(&Lexeme::LeftParen) == l.peek() {
                l.next().unwrap();
                let type_name = TypeName::parse(l).unwrap();
                assert_eq!(Some(Lexeme::RightParen), l.next());

                Some(Self::SizeofType(type_name))
            } else {
                panic!("")
            }
        } else {
            None
        }
    }
}

impl Parse for UnaryOperator {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {
        let res = match l.peek() {
            Some(Lexeme::LogicalAndReference) => Some(Self::Ref),
            Some(Lexeme::MulOrPointer) => Some(Self::Deref),
            Some(Lexeme::Add) => Some(Self::Pos),
            Some(Lexeme::Sub) => Some(Self::Neg),
            Some(Lexeme::LogicalNot) => Some(Self::Inv),
            Some(Lexeme::Not) => Some(Self::Not),
            _ => None
        };

        if res.is_some() { l.next().unwrap(); }
        res
    }
}

impl Parse for PostfixExpression {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {
        if let Some(prim_expr) = PrimaryExpression::parse(l) {
            let postfix = PostfixExpressionEnd::parse(l);
            Some(Self::Primary(prim_expr, postfix))
        } else {
            None
        }
    }
}

impl Parse for PostfixExpressionEnd {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {
        match l.next() {
            Some(Lexeme::LeftBracket) => {
                let expr = Expression::parse(l).unwrap();
                assert_eq!(Some(Lexeme::RightBracket), l.next());
                let postfix = PostfixExpressionEnd::parse(l).map(|e| Box::new(e));
                Some(Self::Index(expr, postfix))
            }
            Some(Lexeme::LeftParen) => {
                let args = ArgumentExpressionList::parse(l);
                assert_eq!(Some(Lexeme::RightParen), l.next());
                let postfix = PostfixExpressionEnd::parse(l).map(|e| Box::new(e));
                Some(Self::Call(args, postfix))
            }
            Some(Lexeme::Dot) => {
                let ident = Identifier::parse(l).unwrap();
                let postfix = PostfixExpressionEnd::parse(l).map(|e| Box::new(e));
                Some(Self::Dot(ident, postfix))
            }
            Some(Lexeme::Arrow) => {
                let ident = Identifier::parse(l).unwrap();
                let postfix = PostfixExpressionEnd::parse(l).map(|e| Box::new(e));
                Some(Self::Deref(ident, postfix))
            }
            Some(Lexeme::Increment) => {
                let postfix = PostfixExpressionEnd::parse(l).map(|e| Box::new(e));
                Some(Self::Increment(postfix))
            }
            Some(Lexeme::Decrement) => {
                let postfix = PostfixExpressionEnd::parse(l).map(|e| Box::new(e));
                Some(Self::Decrement(postfix))
            }
            _ => panic!("")
        }
    }
}

impl Parse for PrimaryExpression {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {
        if let Some(ident) = Identifier::parse(l) {
            Some(Self::Identifier(ident))
        } else if let Some(c) = Constant::parse(l) {
            Some(Self::Constant(c))
        } else if let Some(Lexeme::StringLiteral(s)) = l.peek() {
            let s = s.clone();
            l.next().unwrap();
            Some(Self::String(s))
        } else if Some(&Lexeme::LeftParen) == l.peek() {
            l.next().unwrap();
            let expr = Expression::parse(l).unwrap();
            assert_eq!(Some(Lexeme::RightParen), l.next());
            Some(Self::Expression(expr))
        } else {
            None
        }
    }
}

impl Parse for Constant {
    fn parse(l: &mut Peekable<impl Iterator<Item=Lexeme>>) -> Option<Self> {
        let res = match l.peek() {
            Some(Lexeme::IntLiteral(a)) => Some(Self::Int(*a)),
            Some(Lexeme::CharLiteral(c)) => Some(Self::Char(*c)),
            Some(Lexeme::FloatLiteral(f)) => Some(Self::Float(*f)),
            _ => None
        };

        if res.is_some() { l.next().unwrap(); }
        res
    }
}
