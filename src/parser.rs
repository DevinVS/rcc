use crate::lexeme::Lexeme;
use crate::ast::*;
use std::fmt::Debug;
use std::error::Error;
use std::any::type_name;
use crate::lexer::LexemeFeed;

pub trait Parse {
    /// Parse a given expression. The result indicates whether the expression
    /// parsed correctly, whereas the option indicates whether the expression
    /// matched its pattern
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>> where Self: Sized;
    fn log(l: &mut LexemeFeed) {
        println!("{}: {:?}", type_name::<Self>(), l.peek());
    }
}

// Parse lists of objects
impl<T: Debug + Parse> Parse for Vec<Box<T>> {
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>> {
		Self::log(l);
        let mut items = vec![];

        while let Some(i) = T::parse(l)? {
            items.push(i);
        }

        if items.len() == 0 {
            Ok(None)
        } else {
            Ok(Some(Box::new(items)))
        }
    }
}

// Parse comma separated lists of objects
impl<T: Debug + Parse> Parse for CSVec<Box<T>> {
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>> {
		Self::log(l);
        let mut items = vec![];

        let first = T::parse(l)?;
        if first.is_none() {
            return Ok(None);
        }

        items.push(first.unwrap());

        while l.test(Lexeme::Comma) {
            items.push(T::parse(l)?.expect("Expected another item in list"));
        }

        Ok(Some(Box::new(CSVec{ items })))
    }
}

impl Parse for Identifier {
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>> {
		Self::log(l);
        if let Some(Lexeme::Identifier(val)) = l.peek() {
            let val = val.clone();
            l.next().unwrap();
            Ok(Some(Box::new(Identifier(val))))
        } else {
            Ok(None)
        }
    }
}

impl Parse for ExternalDeclaration {
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>> {
		Self::log(l);
        let decl_specs = DeclarationSpecifiers::parse(l)?;

        if let Some(declarator) = Declarator::parse(l)? {
            // FunctionDefinition
            let decl_list = DeclarationList::parse(l)?;
            let compound  = CompoundStatement::parse(l)?.unwrap();
            Ok(Some(Box::new(ExternalDeclaration::FunctionDefinition(
                    Box::new(FunctionDefinition {
                        declaration_specifiers: decl_specs,
                        declarator,
                        compound,
                        declaration_list: decl_list
                    }
            )))))
        } else if decl_specs.is_some() {
            // Declaration
            let init_list = InitDeclaratorList::parse(l)?;
            l.consume(Lexeme::Semicolon)?;
            Ok(Some(Box::new(Self::Declaration(Box::new(Declaration {
                specs: decl_specs.unwrap(),
                init_list
            })))))
        } else {
            Ok(None)
        }
    }
}

impl Parse for Declaration {
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>>
    where Self: Sized {
        if let Some(decl_specs) = DeclarationSpecifiers::parse(l)? {
            let init_list = InitDeclaratorList::parse(l)?;
            l.consume(Lexeme::Semicolon)?;
            Ok(Some(Box::new(Self{specs: decl_specs, init_list})))
        } else {
            Ok(None)
        }
    }
}

impl Parse for DeclarationSpecifier {
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>> {
		Self::log(l);
        let val = if let Some(scs) = StorageClassSpecifier::parse(l)? {
            Some(Self::StorageClassSpecifier(scs))
        } else if let Some(ts) = TypeSpecifier::parse(l)? {
            Some(Self::TypeSpecifier(ts))
        } else if let Some(tq) = TypeQualifier::parse(l)? {
            Some(Self::TypeQualifier(tq))
        } else {
            None
        };

        Ok(val.map(Box::new))
    }
}

impl Parse for StorageClassSpecifier {
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>> {
		Self::log(l);
        let res = match l.peek() {
            Some(Lexeme::Auto) => Ok(Some(Self::Auto)),
            Some(Lexeme::Register) => Ok(Some(Self::Register)),
            Some(Lexeme::Static) => Ok(Some(Self::Static)),
            Some(Lexeme::Extern) => Ok(Some(Self::Extern)),
            Some(Lexeme::Typedef) => Ok(Some(Self::Typedef)),
            _ => Ok(None)
        };

        if res.as_ref().unwrap().is_some() {l.next().unwrap();}
        res.map(|e| e.map(Box::new))
    }
}

impl Parse for TypeSpecifier {
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>> {
		Self::log(l);
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
            if let Some(sou) = StructOrUnionSpecifier::parse(l)? {
                Some(Box::new(Self::StructOrUnion(sou)))
            } else if let Some(es) = EnumSpecifier::parse(l)? {
                Some(Box::new(Self::Enum(es)))
            } else if let Some(tn) = TypedefName::parse(l)? {
                Some(Box::new(Self::Typedef(tn)))
            } else {
                None
            }
        } else {
            l.next().unwrap();
            res.map(Box::new)
        };

        Ok(res)
    }
}

impl Parse for TypeQualifier {
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>> {
		Self::log(l);
        let res = match l.peek() {
            Some(Lexeme::Const) => Ok(Some(Self::Const)),
            Some(Lexeme::Volatile) => Ok(Some(Self::Volatile)),
            _ => Ok(None)
        };

        if res.as_ref().unwrap().is_some() {l.next().unwrap();}
        res.map(|e| e.map(Box::new))
    }
}

impl Parse for StructOrUnionSpecifier {
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>> {
		Self::log(l);
        let sou = StructOrUnion::parse(l)?;

        if sou.is_none() {
            return Ok(None);
        }

        let identifier = Identifier::parse(l)?;
        let sdl = StructDeclarationList::parse(l)?;

        if sdl.is_some() {
            Ok(Some(Box::new(Self::List(sou.unwrap(), identifier, sdl.unwrap()))))
        } else if identifier.is_some() {
            Ok(Some(Box::new(Self::Val(sou.unwrap(), identifier.unwrap()))))
        } else {
            Ok(None)
        }
    }
}

impl Parse for StructOrUnion {
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>> {
		Self::log(l);
        let res = match l.peek() {
            Some(Lexeme::Struct) => Ok(Some(Self::Struct)),
            Some(Lexeme::Union) => Ok(Some(Self::Union)),
            _ => Ok(None)
        };

        if res.as_ref().unwrap().is_some() { l.next().unwrap(); }
        res.map(|e| e.map(Box::new))
    }
}

impl Parse for InitDeclarator {
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>> {
		Self::log(l);
        let decl = Declarator::parse(l)?;

        if decl.is_none() {
            return Ok(None);
        }

        if l.test(Lexeme::Assign) {
            let init = Initializer::parse(l)?.unwrap();
            Ok(Some(Box::new(Self::Assign(decl.unwrap(), init))))
        } else {
            Ok(Some(Box::new(Self::Declarator(decl.unwrap()))))
        }
    }
}

impl Parse for StructDeclaration {
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>> {
		Self::log(l);
        let qual = SpecifierQualifierList::parse(l)?;

        if qual.is_none() {
            return Ok(None);
        }

        let sdl = StructDeclarationList::parse(l)?.unwrap();
        l.consume(Lexeme::Semicolon)?;

        Ok(Some(Box::new(Self{
            spec_qualifier_list: qual.unwrap(),
            decl_list: sdl
        })))
    }
}

impl Parse for SpecifierOrQualifier {
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>> {
		Self::log(l);
        if let Some(ts) = TypeSpecifier::parse(l)? {
            Ok(Some(Box::new(Self::Specifier(ts))))
        } else if let Some(tq) = TypeQualifier::parse(l)? {
            Ok(Some(Box::new(Self::Qualifier(tq))))
        } else {
            Ok(None)
        }
    }
}

impl Parse for StructDeclarator {
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>> {
		Self::log(l);
        let declarator = Declarator::parse(l)?;

        if l.test(Lexeme::Colon) {
            let expr = ConstantExpression::parse(l)?.unwrap();
            Ok(Some(Box::new(Self::Const(declarator, expr))))
        } else {
            Ok(Some(Box::new(Self::Decl(declarator.unwrap()))))
        }
    }
}

impl Parse for EnumSpecifier {
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>> {
		Self::log(l);
        if l.test(Lexeme::Enum) {
            let identifier = Identifier::parse(l)?;

            if l.test(Lexeme::LeftBrace) {
                let enum_list = EnumeratorList::parse(l)?.unwrap();
                l.consume(Lexeme::RightBrace)?;
                Ok(Some(Box::new(Self::List(identifier, enum_list))))
            } else {
                Ok(Some(Box::new(Self::Identifier(identifier.unwrap()))))
            }
        } else {
            Ok(None)
        }
    }
}


impl Parse for Enumerator {
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>> {
		Self::log(l);
        if let Some(identifier) = Identifier::parse(l)? {
            if l.test(Lexeme::Assign) {
                let expr = ConstantExpression::parse(l)?.unwrap();
                Ok(Some(Box::new(Self::ConstantExpression(identifier, expr))))
            } else {
                Ok(Some(Box::new(Self::Identifier(identifier))))
            }
        } else {
            Ok(None)
        }
    }
}


impl Parse for Declarator {
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>> {
		Self::log(l);
        let pointer = Pointer::parse(l)?;
        let decl = DirectDeclarator::parse(l)?;

        if pointer.is_none() && decl.is_none() {
            return Ok(None);
        }

        Ok(Some(Box::new(Self { pointer, decl: decl.unwrap()})))
    }
}

impl Parse for DirectDeclarator {
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>> {
		Self::log(l);
        if let Some(ident) = Identifier::parse(l)? {
            let dd = DirectDeclaratorEnd::parse(l)?;
            Ok(Some(Box::new(DirectDeclarator::Identifier(ident, dd))))
        } else if l.test(Lexeme::LeftParen) {
            let decl = Declarator::parse(l)?.unwrap();
            l.consume(Lexeme::RightParen)?;
            let dd = DirectDeclaratorEnd::parse(l)?;

            Ok(Some(Box::new(DirectDeclarator::Declarator(decl, dd))))
        } else {
            Ok(None)
        }
    }
}

impl Parse for DirectDeclaratorEnd {
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>> {
		Self::log(l);
        if l.test(Lexeme::LeftBracket) {
            let const_expr = ConstantExpression::parse(l)?;
            l.consume(Lexeme::RightBracket)?;
            let dd = DirectDeclaratorEnd::parse(l)?;
            Ok(Some(Box::new(Self::ConstantExpression(const_expr, dd))))
        } else if l.test(Lexeme::LeftParen) {
            if let Some(pl) = ParameterTypeList::parse(l)? {
                l.consume(Lexeme::RightParen)?;
                let dd = DirectDeclaratorEnd::parse(l)?;
                Ok(Some(Box::new(Self::ParameterTypeList(pl, dd))))
            } else {
                let idents = IdentifierList::parse(l)?;
                l.consume(Lexeme::RightParen)?;
                let dd = DirectDeclaratorEnd::parse(l)?;
                Ok(Some(Box::new(Self::IdentifierList(idents, dd))))
            }
        } else {
            Ok(None)
        }
    }
}

impl Parse for Pointer {
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>> {
		Self::log(l);
        if l.test(Lexeme::MulOrPointer) {
            let tql = TypeQualifierList::parse(l)?;

            if let Some(pointer) = Pointer::parse(l)? {
                Ok(Some(Box::new(Pointer::List(tql, pointer))))
            } else {
                Ok(Some(Box::new(Pointer::Pointer(tql))))
            }
        } else {
            Ok(None)
        }
    }
}

impl Parse for ParameterTypeList {
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>> {
		Self::log(l);
        if let Some(param_list) = ParameterList::parse(l)? {
            if l.test(Lexeme::Comma) {
                l.consume(Lexeme::Dot)?;
                l.consume(Lexeme::Dot)?;
                l.consume(Lexeme::Dot)?;

                Ok(Some(Box::new(Self::VarArgs(param_list))))
            } else {
                Ok(Some(Box::new(Self::Regular(param_list))))
            }
        } else {
            Ok(None)
        }

    }
}


impl Parse for ParameterDeclaration {
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>> {
		Self::log(l);
        if let Some(decl_specs) = DeclarationSpecifiers::parse(l)? {
            if let Some(decl) = Declarator::parse(l)? {
                Ok(Some(Box::new(ParameterDeclaration::Declaration(decl_specs, decl))))
            } else {
                let decls = AbstractDeclarator::parse(l)?;
                Ok(Some(Box::new(ParameterDeclaration::AbstractDeclarator(decl_specs, decls))))
            }
        } else {
            Ok(None)
        }
    }
}

impl Parse for Initializer {
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>> {
		Self::log(l);
        if let Some(expr) = AssignmentExpression::parse(l)? {
            Ok(Some(Box::new(Self::Assign(expr))))
        } else if l.test(Lexeme::LeftBrace) {
            let init_list = InitializerList::parse(l)?.unwrap();
            l.test(Lexeme::Comma);
            l.consume(Lexeme::RightBrace)?;

            Ok(Some(Box::new(Self::List(init_list))))
        } else {
            Ok(None)
        }
    }
}

impl Parse for TypeName {
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>> {
		Self::log(l);
        if let Some(sql) = SpecifierQualifierList::parse(l)? {
            let decl = AbstractDeclarator::parse(l)?;
            Ok(Some(Box::new(Self{ list: sql, declarator: decl })))
        } else {
            Ok(None)
        }
    }
}

impl Parse for AbstractDeclarator {
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>> {
		Self::log(l);
        let pointer = Pointer::parse(l)?;
        let dad = DirectAbstractDeclarator::parse(l)?;

        if dad.is_some() {
            Ok(Some(Box::new(Self::Declarator(pointer, dad.unwrap()))))
        } else if pointer.is_some() {
            Ok(Some(Box::new(Self::Pointer(pointer.unwrap()))))
        } else {
            Ok(None)
        }
    }
}

impl Parse for DirectAbstractDeclarator {
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>> {
		Self::log(l);

        if l.test(Lexeme::LeftParen) {
            if let Some(ad) = AbstractDeclarator::parse(l)? {
                l.consume(Lexeme::RightParen)?;
                let dad = DirectAbstractDeclaratorEnd::parse(l)?;
                Ok(Some(Box::new(Self::Decl(ad, dad))))
            } else {
                let params = ParameterTypeList::parse(l)?;
                l.consume(Lexeme::RightParen)?;
                let dad = DirectAbstractDeclaratorEnd::parse(l)?;
                Ok(Some(Box::new(Self::Param(params, dad))))
            }
        } else if l.test(Lexeme::LeftBracket) {
            let expr = ConstantExpression::parse(l)?;
            l.consume(Lexeme::RightBracket)?;
            let dad = DirectAbstractDeclaratorEnd::parse(l)?;
            Ok(Some(Box::new(Self::Const(expr, dad))))
        } else {
            Ok(None)
        }
    }
}

impl Parse for DirectAbstractDeclaratorEnd {
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>> {
		Self::log(l);
        if l.test(Lexeme::LeftParen) {
            let params = ParameterTypeList::parse(l)?;
            l.consume(Lexeme::RightParen)?;
            let dad = DirectAbstractDeclaratorEnd::parse(l)?;
            Ok(Some(Box::new(Self::Param(params, dad))))
        } else if l.test(Lexeme::LeftBracket) {
            let expr = ConstantExpression::parse(l)?;
            l.consume(Lexeme::RightBracket)?;
            let dad = DirectAbstractDeclaratorEnd::parse(l)?;
            Ok(Some(Box::new(Self::Const(expr, dad))))
        } else {
            Ok(None)
        }
    }
}

impl Parse for TypedefName {
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>> {
		Self::log(l);
        if let Some(Lexeme::TypeName(n)) = l.peek() {
            let n = n.clone();
            l.next().unwrap();
            Ok(Some(Box::new(Self{ name: n})))
        } else {
            Ok(None)
        }
    }
}

impl Parse for Statement {
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>> {
		Self::log(l);

        if let Some(es) = ExpressionStatement::parse(l)? {
            Ok(Some(Box::new(Self::Expression(es))))
        } else if let Some(ls) = LabeledStatement::parse(l)? {
            Ok(Some(Box::new(Self::Labeled(ls))))
        } else if let Some(cs) = CompoundStatement::parse(l)? {
            Ok(Some(Box::new(Self::Compound(cs))))
        } else if let Some(ss) = SelectionStatement::parse(l)? {
            Ok(Some(Box::new(Self::Selection(ss))))
        } else if let Some(is) = IterationStatement::parse(l)? {
            Ok(Some(Box::new(Self::Iteration(is))))
        } else if let Some(js) = JumpStatement::parse(l)? {
            Ok(Some(Box::new(Self::Jump(js))))
        } else {
            Ok(None)
        }
    }
}

impl Parse for LabeledStatement {
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>> {
		Self::log(l);
        if let Some(ident) = Identifier::parse(l)? {
            l.consume(Lexeme::Colon)?;

            let statement = Statement::parse(l)?.unwrap();
            Ok(Some(Box::new(Self::Map(ident, statement))))
        } else if l.test(Lexeme::Case) {
            let expr = ConstantExpression::parse(l)?.unwrap();
            l.consume(Lexeme::Colon)?;

            let statement = Statement::parse(l)?.unwrap();
            Ok(Some(Box::new(Self::Case(expr, statement))))
        } else if l.test(Lexeme::Default) {
            l.consume(Lexeme::Colon)?;

            let statement = Statement::parse(l)?.unwrap();
            Ok(Some(Box::new(Self::Default(statement))))
        } else {
            Ok(None)
        }
    }
}

impl Parse for ExpressionStatement {
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>> {
		Self::log(l);
        let expr = Expression::parse(l)?;

        if expr.is_some() {
            Ok(Some(Box::new(Self { expression: expr })))
        } else if l.test(Lexeme::Semicolon) {
            Ok(Some(Box::new(Self { expression: expr })))
        } else {
            Ok(None)
        }
    }
}

impl Parse for CompoundStatement {
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>> {
		Self::log(l);
        if l.test(Lexeme::LeftBrace) {
            let decl_list = DeclarationList::parse(l)?;
            let statement_list = StatementList::parse(l)?;

            l.consume(Lexeme::RightBrace)?;

            Ok(Some(Box::new(Self { declarations: decl_list, statements: statement_list })))
        } else {
            Ok(None)
        }
    }
}

impl Parse for SelectionStatement {
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>> {
		Self::log(l);
        if l.test(Lexeme::If) {
            l.consume(Lexeme::LeftParen)?;
            let expr = Expression::parse(l)?.unwrap();
            l.consume(Lexeme::RightParen)?;

            let stmt = Statement::parse(l)?.unwrap();

            if l.test(Lexeme::Else) {
                let else_stmt = Statement::parse(l)?.unwrap();
                Ok(Some(Box::new(Self::IfElse(expr, stmt, else_stmt))))
            } else {
                Ok(Some(Box::new(Self::If(expr, stmt))))
            }
        } else if l.test(Lexeme::Switch) {
            l.consume(Lexeme::LeftParen)?;

            let expr = Expression::parse(l)?.unwrap();

            l.consume(Lexeme::RightParen)?;

            let stmt = Statement::parse(l)?.unwrap();

            Ok(Some(Box::new(Self::Switch(expr, stmt))))
        } else {
            Ok(None)
        }
    }
}


impl Parse for IterationStatement {
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>> {
		Self::log(l);
        if l.test(Lexeme::While) {
            l.consume(Lexeme::LeftParen)?;
            let expr = Expression::parse(l)?.unwrap();
            l.consume(Lexeme::RightParen)?;
            let stmt = Statement::parse(l)?.unwrap();

            Ok(Some(Box::new(Self::While(expr, stmt))))
        } else if l.test(Lexeme::Do) {
            let stmt = Statement::parse(l)?.unwrap();
            l.consume(Lexeme::While)?;
            l.consume(Lexeme::LeftParen)?;
            let expr = Expression::parse(l)?.unwrap();
            l.consume(Lexeme::RightParen)?;
            l.consume(Lexeme::Semicolon)?;

            Ok(Some(Box::new(Self::DoWhile(stmt, expr))))
        } else if l.test(Lexeme::For) {
            l.consume(Lexeme::LeftParen)?;

            let expr1 = Expression::parse(l)?;
            l.consume(Lexeme::Semicolon)?;
            let expr2 = Expression::parse(l)?;
            l.consume(Lexeme::Semicolon)?;
            let expr3 = Expression::parse(l)?;

            l.consume(Lexeme::RightParen)?;
            let stmt = Statement::parse(l)?.unwrap();

            Ok(Some(Box::new(Self::For(expr1, expr2, expr3, stmt))))
        } else {
            Ok(None)
        }
    }
}

impl Parse for JumpStatement {
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>> {
		Self::log(l);
        if l.test(Lexeme::Goto) {
            let ident = Identifier::parse(l)?.unwrap();
            l.consume(Lexeme::Semicolon)?;
            Ok(Some(Box::new(Self::Goto(ident))))
        } else if l.test(Lexeme::Continue) {
            l.consume(Lexeme::Semicolon)?;
            Ok(Some(Box::new(Self::Continue)))
        } else if l.test(Lexeme::Break) {
            l.consume(Lexeme::Semicolon)?;
            Ok(Some(Box::new(Self::Break)))
        } else if l.test(Lexeme::Return) {
            let expr = Expression::parse(l)?;
            l.consume(Lexeme::Semicolon)?;
            Ok(Some(Box::new(Self::Return(expr))))
        } else {
            Ok(None)
        }
    }
}

impl Parse for AssignmentExpression {
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>> {
		Self::log(l);
        if let Some(ce) = ConditionalExpression::parse(l)? {
            Ok(Some(Box::new(Self::Conditional(ce))))
        } else if let Some(ue) = UnaryExpression::parse(l)? {
            let ao = AssignmentOperator::parse(l)?.unwrap();
            let ae = AssignmentExpression::parse(l)?.unwrap();

            Ok(Some(Box::new(Self::Assign(ue, ao, ae))))
        } else {
            Ok(None)
        }
    }
}

impl Parse for AssignmentOperator {
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>> {
		Self::log(l);
        let res = match l.peek() {
            Some(Lexeme::Assign) => Ok(Some(Self::Assign)),
            Some(Lexeme::MulAssign) => Ok(Some(Self::MulAssign)),
            Some(Lexeme::DivAssign) => Ok(Some(Self::DivAssign)),
            Some(Lexeme::ModAssign) => Ok(Some(Self::ModAssign)),
            Some(Lexeme::AddAssign) => Ok(Some(Self::AddAssign)),
            Some(Lexeme::SubAssign) => Ok(Some(Self::SubAssign)),
            Some(Lexeme::LeftShiftAssign) => Ok(Some(Self::LeftShiftAssign)),
            Some(Lexeme::RightShiftAssign) => Ok(Some(Self::RightShiftAssign)),
            Some(Lexeme::BitwiseAndAssign) => Ok(Some(Self::AndAssign)),
            Some(Lexeme::BitwiseOrAssign) => Ok(Some(Self::OrAssign)),
            Some(Lexeme::BitwiseXorAssign) => Ok(Some(Self::XorAssign)),
            Some(Lexeme::BitwiseNotAssign) => Ok(Some(Self::NotAssign)),
            _ => Ok(None)
        };

        if res.as_ref().unwrap().is_some() { l.next().unwrap(); }
        res.map(|e| e.map(Box::new))
    }
}

impl Parse for ConditionalExpression {
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>> {
		Self::log(l);

        if let Some(loe) = LogicalOrExpression::parse(l)? {
            if l.test(Lexeme::Question) {
                let expr = Expression::parse(l)?.unwrap();
                l.consume(Lexeme::Colon)?;
                let cond_expr = ConditionalExpression::parse(l)?.unwrap();

                Ok(Some(Box::new(Self::Ternary(loe, expr, cond_expr))))
            } else {
                Ok(Some(Box::new(Self::Or(loe))))
            }
        } else {
            Ok(None)
        }
    }
}

impl Parse for ConstantExpression {
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>> {
		Self::log(l);
        let expr = ConditionalExpression::parse(l)?;

        Ok(expr.map(|e| Box::new(Self(e))))
    }
}

impl Parse for LogicalOrExpression {
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>> {
		Self::log(l);
        if let Some(expr) = LogicalAndExpression::parse(l)? {
            let next = LogicalOrExpressionEnd::parse(l)?;
            Ok(Some(Box::new(Self { expr, next })))
        } else {
            Ok(None)
        }
    }
}

impl Parse for LogicalOrExpressionEnd {
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>> {
		Self::log(l);
        if l.test(Lexeme::Or) {
            let expr = LogicalAndExpression::parse(l)?.unwrap();
            let next = LogicalOrExpressionEnd::parse(l)?;
            Ok(Some(Box::new(Self{expr, next})))
        } else {
            Ok(None)
        }
    }
}

impl Parse for LogicalAndExpression {
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>> {
		Self::log(l);
        if let Some(expr) = InclusiveOrExpression::parse(l)? {
            let next = LogicalAndExpressionEnd::parse(l)?;
            Ok(Some(Box::new(Self{next, expr})))
        } else {
            Ok(None)
        }
    }
}

impl Parse for LogicalAndExpressionEnd {
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>> {
		Self::log(l);
        if l.test(Lexeme::And) {
            let expr = InclusiveOrExpression::parse(l)?.unwrap();
            let next = LogicalAndExpressionEnd::parse(l)?;
            Ok(Some(Box::new(Self{expr, next})))
        } else {
            Ok(None)
        }
    }
}

impl Parse for InclusiveOrExpression {
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>> {
		Self::log(l);
        if let Some(expr) = ExclusiveOrExpression::parse(l)? {
            let next = InclusiveOrExpressionEnd::parse(l)?;
            Ok(Some(Box::new(Self{expr, next})))
        } else {
            Ok(None)
        }
    }
}

impl Parse for InclusiveOrExpressionEnd {
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>> {
		Self::log(l);
        if l.test(Lexeme::BitwiseOr) {
            let expr = ExclusiveOrExpression::parse(l)?.unwrap();
            let next = InclusiveOrExpressionEnd::parse(l)?;
            Ok(Some(Box::new(Self{expr,next})))
        } else {
            Ok(None)
        }
    }
}

impl Parse for ExclusiveOrExpression {
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>> {
		Self::log(l);
        if let Some(expr) = AndExpression::parse(l)? {
            let next = ExclusiveOrExpressionEnd::parse(l)?;
            Ok(Some(Box::new(Self{expr, next})))
        } else {
            Ok(None)
        }
    }
}

impl Parse for ExclusiveOrExpressionEnd {
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>> {
		Self::log(l);
        if l.test(Lexeme::BitwiseXor) {
            let expr = AndExpression::parse(l)?.unwrap();
            let next = ExclusiveOrExpressionEnd::parse(l)?;
            Ok(Some(Box::new(Self{expr, next})))
        } else {
            Ok(None)
        }
    }
}

impl Parse for AndExpression {
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>> {
		Self::log(l);
        if let Some(expr) = EqualityExpression::parse(l)? {
            let next = AndExpressionEnd::parse(l)?;
            Ok(Some(Box::new(Self{ expr, next })))
        } else {
            Ok(None)
        }
    }
}

impl Parse for AndExpressionEnd {
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>> {
		Self::log(l);
        if l.test(Lexeme::BitwiseAndReference) {
            let expr = EqualityExpression::parse(l)?.unwrap();
            let next = AndExpressionEnd::parse(l)?;
            Ok(Some(Box::new(Self{expr, next})))
        } else {
            Ok(None)
        }
    }
}

impl Parse for EqualityExpression {
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>> {
		Self::log(l);
        if let Some(expr) = RelationalExpression::parse(l)? {
            let next = EqualityExpressionEnd::parse(l)?;
            Ok(Some(Box::new(Self{expr, next})))
        } else {
            Ok(None)
        }
    }
}

impl Parse for EqualityExpressionEnd {
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>> {
		Self::log(l);
        if l.test(Lexeme::Equal) {
            let expr = RelationalExpression::parse(l)?.unwrap();
            let next = EqualityExpressionEnd::parse(l)?;
            Ok(Some(Box::new(Self::Equal(expr, next))))
        } else if l.test(Lexeme::Sub) {
            let expr = RelationalExpression::parse(l)?.unwrap();
            let next = EqualityExpressionEnd::parse(l)?;
            Ok(Some(Box::new(Self::NotEqual(expr, next))))
        } else {
            Ok(None)
        }
    }
}

impl Parse for RelationalExpression {
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>> {
		Self::log(l);
        if let Some(expr) = ShiftExpression::parse(l)? {
            let next = RelationalExpressionEnd::parse(l)?;
            Ok(Some(Box::new(Self{next, expr})))
        } else {
            Ok(None)
        }
    }
}

impl Parse for RelationalExpressionEnd {
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>> {
		Self::log(l);
        if l.test(Lexeme::LessThan) {
            let expr = ShiftExpression::parse(l)?.unwrap();
            let next = RelationalExpressionEnd::parse(l)?;
            Ok(Some(Box::new(Self::LT(expr, next))))
        } else if l.test(Lexeme::GreaterThan) {
            let expr = ShiftExpression::parse(l)?.unwrap();
            let next = RelationalExpressionEnd::parse(l)?;
            Ok(Some(Box::new(Self::GT(expr, next))))
        } else if l.test(Lexeme::LessThanEqual) {
            let expr = ShiftExpression::parse(l)?.unwrap();
            let next = RelationalExpressionEnd::parse(l)?;
            Ok(Some(Box::new(Self::LTE(expr, next))))
        } else if l.test(Lexeme::GreaterThanEqual) {
            let expr = ShiftExpression::parse(l)?.unwrap();
            let next = RelationalExpressionEnd::parse(l)?;
            Ok(Some(Box::new(Self::GTE(expr, next))))
        } else {
            Ok(None)
        }
    }
}

impl Parse for ShiftExpression {
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>> {
		Self::log(l);
        if let Some(expr) = AdditiveExpression::parse(l)? {
            let next = ShiftExpressionEnd::parse(l)?;
            Ok(Some(Box::new(Self{expr, next})))
        } else {
            Ok(None)
        }
    }
}

impl Parse for ShiftExpressionEnd {
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>> {
		Self::log(l);
        if l.test(Lexeme::LeftShift) {
            let expr = AdditiveExpression::parse(l)?.unwrap();
            let next = ShiftExpressionEnd::parse(l)?;
            Ok(Some(Box::new(Self::LS(expr, next))))
        } else if l.test(Lexeme::RightShift) {
            let expr = AdditiveExpression::parse(l)?.unwrap();
            let next = ShiftExpressionEnd::parse(l)?;
            Ok(Some(Box::new(Self::RS(expr, next))))
        } else {
            Ok(None)
        }
    }
}

impl Parse for AdditiveExpression {
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>> {
		Self::log(l);
        if let Some(expr) = MultiplicativeExpression::parse(l)? {
            let next = AdditiveExpressionEnd::parse(l)?;
            Ok(Some(Box::new(Self{expr, next})))
        } else {
            Ok(None)
        }
    }
}

impl Parse for AdditiveExpressionEnd {
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>> {
		Self::log(l);
        if l.test(Lexeme::Add) {
            let expr = MultiplicativeExpression::parse(l)?.unwrap();
            let next = AdditiveExpressionEnd::parse(l)?;
            Ok(Some(Box::new(Self::Add(expr, next))))
        } else if l.test(Lexeme::Sub) {
            let expr = MultiplicativeExpression::parse(l)?.unwrap();
            let next = AdditiveExpressionEnd::parse(l)?;
            Ok(Some(Box::new(Self::Sub(expr, next))))
        } else {
            Ok(None)
        }
    }
}

impl Parse for MultiplicativeExpression {
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>> {
		Self::log(l);
        if let Some(expr) = CastExpression::parse(l)? {
            let next = MultiplicativeExpressionEnd::parse(l)?;
            Ok(Some(Box::new(Self{expr, next})))
        } else {
            Ok(None)
        }
    }
}

impl Parse for MultiplicativeExpressionEnd {
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>> {
		Self::log(l);
        if l.test(Lexeme::MulOrPointer) {
            let expr = CastExpression::parse(l)?.unwrap();
            let next = MultiplicativeExpressionEnd::parse(l)?;
            Ok(Some(Box::new(Self::Mul(expr, next))))
        } else if l.test(Lexeme::Div) {
            let expr = CastExpression::parse(l)?.unwrap();
            let next = MultiplicativeExpressionEnd::parse(l)?;
            Ok(Some(Box::new(Self::Div(expr, next))))
        } else if l.test(Lexeme::Mod) {
            let expr = CastExpression::parse(l)?.unwrap();
            let next = MultiplicativeExpressionEnd::parse(l)?;
            Ok(Some(Box::new(Self::Mod(expr, next))))
        } else {
            Ok(None)
        }
    }
}

impl Parse for CastExpression {
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>> {
		Self::log(l);
        if l.test(Lexeme::LeftParen) {

            if let Some(type_name) = TypeName::parse(l)? {
                l.consume(Lexeme::RightParen)?;
                let expr = CastExpression::parse(l)?.unwrap();
                Ok(Some(Box::new(Self::Cast(type_name, expr))))
            } else {
                // Stealing some thunder from PrimaryExpression here, but it solves the ambiguity
                let expr = Expression::parse(l)?.unwrap();
                Ok(Some(
                    Box::new(Self::Unary(
                        Box::new(UnaryExpression::Postfix(
                            Box::new(PostfixExpression {
                                primary: Box::new(PrimaryExpression::Expression(expr)),
                                suffixes: Vec::<Box<PostfixExpressionEnd>>::parse(l)?.unwrap_or(Box::new(Vec::new()))
                            })
                        ))
                    )
                )))
            }

        } else if let Some(unary_expr) = UnaryExpression::parse(l)? {
            Ok(Some(Box::new(Self::Unary(unary_expr))))
        } else {
            Ok(None)
        }
    }
}

impl Parse for UnaryExpression {
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>> {
		Self::log(l);
        if let Some(postfix) = PostfixExpression::parse(l)? {
            Ok(Some(Box::new(Self::Postfix(postfix))))
        } else if l.test(Lexeme::Increment) {
            let expr = UnaryExpression::parse(l)?.unwrap();
            Ok(Some(Box::new(Self::Increment(expr))))
        } else if l.test(Lexeme::Decrement) {
            let expr = UnaryExpression::parse(l)?.unwrap();
            Ok(Some(Box::new(Self::Decrement(expr))))
        } else if let Some(unary_op) = UnaryOperator::parse(l)? {
            let cast_expr = CastExpression::parse(l)?.unwrap();
            Ok(Some(Box::new(Self::Cast(unary_op, cast_expr))))
        } else if l.test(Lexeme::Sizeof) {
            if let Some(unary_expr) = UnaryExpression::parse(l)? {
                Ok(Some(Box::new(Self::Sizeof(unary_expr))))
            } else if l.test(Lexeme::LeftParen) {
                let type_name = TypeName::parse(l)?.unwrap();
                l.consume(Lexeme::RightParen)?;

                Ok(Some(Box::new(Self::SizeofType(type_name))))
            } else {
                Err("Expected Left Paren".into())
            }
        } else {
            Ok(None)
        }
    }
}

impl Parse for UnaryOperator {
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>> {
		Self::log(l);
        let res = match l.peek() {
            Some(Lexeme::BitwiseAndReference) => Ok(Some(Self::Ref)),
            Some(Lexeme::MulOrPointer) => Ok(Some(Self::Deref)),
            Some(Lexeme::Add) => Ok(Some(Self::Pos)),
            Some(Lexeme::Sub) => Ok(Some(Self::Neg)),
            Some(Lexeme::BitwiseNot) => Ok(Some(Self::Inv)),
            Some(Lexeme::Not) => Ok(Some(Self::Not)),
            _ => Ok(None)
        };

        if res.as_ref().unwrap().is_some() { l.next().unwrap(); }
        res.map(|e| e.map(Box::new))
    }
}

impl Parse for PostfixExpression {
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>> {
		Self::log(l);

        if let Some(prim_expr) = PrimaryExpression::parse(l)? {
            let suffixes = Vec::<Box<PostfixExpressionEnd>>::parse(l)?.unwrap_or(Box::new(Vec::new()));
            Ok(Some(Box::new(Self {primary: prim_expr, suffixes})))
        } else {
            Ok(None)
        }
    }
}

impl Parse for PostfixExpressionEnd {
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>> {
		Self::log(l);

        let res = if l.test(Lexeme::LeftBracket) {
            let expr = Expression::parse(l)?.unwrap();
            l.consume(Lexeme::RightBracket)?;
            Ok(Some(Self::Index(expr)))
        } else if l.test(Lexeme::LeftParen) {
            let args = ArgumentExpressionList::parse(l)?;
            l.consume(Lexeme::RightParen)?;
            Ok(Some(Self::Call(args)))
        } else if l.test(Lexeme::Dot) {
            let ident = Identifier::parse(l)?.unwrap();
            Ok(Some(Self::Dot(ident)))
        } else if l.test(Lexeme::Arrow) {
            let ident = Identifier::parse(l)?.unwrap();
            Ok(Some(Self::Deref(ident)))
        } else if l.test(Lexeme::Increment) {
            Ok(Some(Self::Increment))
        } else if l.test(Lexeme::Decrement) {
            Ok(Some(Self::Decrement))
        } else {
            Ok(None)
        };

        res.map(|e| e.map(Box::new))
    }
}

impl Parse for PrimaryExpression {
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>> {
		Self::log(l);
        if let Some(ident) = Identifier::parse(l)? {
            Ok(Some(Box::new(Self::Identifier(ident))))
        } else if let Some(c) = Constant::parse(l)? {
            Ok(Some(Box::new(Self::Constant(c))))
        } else if let Some(Lexeme::StringLiteral(s)) = l.peek() {
            let s = s.clone();
            l.next().unwrap();
            Ok(Some(Box::new(Self::String(s))))
        } else if l.test(Lexeme::LeftParen) {
            let expr = Expression::parse(l)?.unwrap();
            l.consume(Lexeme::RightParen)?;
            Ok(Some(Box::new(Self::Expression(expr))))
        } else {
            Ok(None)
        }
    }
}

impl Parse for Constant {
    fn parse(l: &mut LexemeFeed) -> Result<Option<Box<Self>>, Box<dyn Error>> {
		Self::log(l);
        let res = match l.peek() {
            Some(Lexeme::IntLiteral(a)) => Ok(Some(Self::Int(*a))),
            Some(Lexeme::CharLiteral(c)) => Ok(Some(Self::Char(*c))),
            Some(Lexeme::FloatLiteral(f)) => Ok(Some(Self::Float(*f))),
            _ => Ok(None)
        };

        if res.as_ref().unwrap().is_some() { l.next().unwrap(); }
        res.map(|e| e.map(Box::new))
    }
}
