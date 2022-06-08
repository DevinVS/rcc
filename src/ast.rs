use crate::lexeme::Lexeme;

pub type CProgram = Vec<Box<ExternalDeclaration>>;

#[derive(Debug)]
pub struct Identifier(pub String);

#[derive(Debug)]
pub struct SVec<T, const SEP: usize> {
    pub items: Vec<T>
}

#[derive(Debug)]
pub struct StartExpression<S, T> {
    pub expr: Box<S>,
    pub next: Option<Box<T>>,
}

impl<T, const SEP: usize> SVec<T, SEP> {
    pub fn lexeme() -> Lexeme {
        match SEP {
            0 => Lexeme::Comma,
            1 => Lexeme::Or,
            2 => Lexeme::And,
            3 => Lexeme::BitwiseOr,
            4 => Lexeme::BitwiseXor,
            5 => Lexeme::BitwiseAndReference,
            _ => panic!("Invalid Separator")
        }
    }
}

#[derive(Debug)]
pub enum ExternalDeclaration {
    FunctionDefinition(Box<FunctionDefinition>),
    Declaration(Box<Declaration>)
}

#[derive(Debug)]
pub struct FunctionDefinition {
    pub declaration_specifiers: Option<Box<DeclarationSpecifiers>>,
    pub declarator: Box<Declarator>,
    pub declaration_list: Option<Box<DeclarationList>>,
    pub compound: Box<CompoundStatement>
}

#[derive(Debug)]
pub struct Declaration {
    pub specs: Box<DeclarationSpecifiers>,
    pub init_list: Option<Box<InitDeclaratorList>>
}

pub type DeclarationList = Vec<Box<Declaration>>;
pub type DeclarationSpecifiers = Vec<Box<DeclarationSpecifier>>;

#[derive(Debug)]
pub enum DeclarationSpecifier {
    StorageClassSpecifier(Box<StorageClassSpecifier>),
    TypeSpecifier(Box<TypeSpecifier>),
    TypeQualifier(Box<TypeQualifier>)
}

#[derive(Debug)]
pub enum StorageClassSpecifier {
    Auto,
    Register,
    Static,
    Extern,
    Typedef
}

#[derive(Debug)]
pub enum TypeSpecifier {
    Void,
    Char,
    Short,
    Int,
    Long,
    Float,
    Double,
    Signed,
    Unsigned,
    StructOrUnion(Box<StructOrUnionSpecifier>),
    Enum(Box<EnumSpecifier>),
    Typedef(Box<TypedefName>),
}

#[derive(Debug)]
pub enum TypeQualifier {
    Const,
    Volatile
}

#[derive(Debug)]
pub enum StructOrUnionSpecifier {
    Val(Box<StructOrUnion>, Box<Identifier>),
    List(Box<StructOrUnion>, Option<Box<Identifier>>, Box<StructDeclarationList>)
}

#[derive(Debug)]
pub enum StructOrUnion {
    Struct,
    Union
}

pub type StructDeclarationList = Vec<Box<StructDeclaration>>;

pub type InitDeclaratorList = SVec<Box<InitDeclarator>, 0>;

#[derive(Debug)]
pub enum InitDeclarator {
    Declarator(Box<Declarator>),
    Assign(Box<Declarator>, Box<Initializer>)
}

#[derive(Debug)]
pub struct StructDeclaration {
    pub spec_qualifier_list: Box<SpecifierQualifierList>,
    pub decl_list: Box<StructDeclaratorList>
}

pub type SpecifierQualifierList = Vec<Box<SpecifierOrQualifier>>;

#[derive(Debug)]
pub enum SpecifierOrQualifier {
    Specifier(Box<TypeSpecifier>),
    Qualifier(Box<TypeQualifier>)
}

pub type StructDeclaratorList = SVec<Box<StructDeclarator>, 0>;

#[derive(Debug)]
pub enum StructDeclarator {
    Decl(Box<Declarator>),
    Const(Option<Box<Declarator>>, Box<ConstantExpression>)
}

#[derive(Debug)]
pub enum EnumSpecifier {
    List(Option<Box<Identifier>>, Box<EnumeratorList>),
    Identifier(Box<Identifier>)
}

pub type EnumeratorList = SVec<Box<Enumerator>, 0>;

#[derive(Debug)]
pub enum Enumerator {
    Identifier(Box<Identifier>),
    ConstantExpression(Box<Identifier>, Box<ConstantExpression>)
}

#[derive(Debug)]
pub struct Declarator {
    pub pointer: Option<Box<Pointer>>,
    pub decl: Box<DirectDeclarator>
}

#[derive(Debug)]
pub enum DirectDeclarator {
    Identifier(Box<Identifier>, Option<Box<DirectDeclaratorEnd>>),
    TypedefName(Box<TypedefName>, Option<Box<DirectDeclaratorEnd>>),
    Declarator(Box<Declarator>, Option<Box<DirectDeclaratorEnd>>),
}

#[derive(Debug)]
pub enum DirectDeclaratorEnd {
    ConstantExpression(Option<Box<ConstantExpression>>, Option<Box<DirectDeclaratorEnd>>),
    ParameterTypeList(Box<ParameterTypeList>, Option<Box<DirectDeclaratorEnd>>),
    IdentifierList(Option<Box<IdentifierList>>, Option<Box<DirectDeclaratorEnd>>)
}


#[derive(Debug)]
pub enum Pointer {
    Pointer(Option<Box<TypeQualifierList>>),
    List(Option<Box<TypeQualifierList>>, Box<Pointer>)
}

pub type TypeQualifierList = Vec<Box<TypeQualifier>>;

#[derive(Debug)]
pub enum ParameterTypeList {
    Regular(Box<ParameterList>),
    VarArgs(Box<ParameterList>)
}

pub type ParameterList = SVec<Box<ParameterDeclaration>, 0>;

#[derive(Debug)]
pub enum ParameterDeclaration {
    Declaration(Box<DeclarationSpecifiers>, Box<Declarator>),
    AbstractDeclarator(Box<DeclarationSpecifiers>, Option<Box<AbstractDeclarator>>)
}

pub type IdentifierList = SVec<Box<Identifier>, 0>;

#[derive(Debug)]
pub enum Initializer {
    Assign(Box<AssignmentExpression>),
    List(Box<InitializerList>)
}

pub type InitializerList = SVec<Box<Initializer>, 0>;

#[derive(Debug)]
pub struct TypeName {
    pub list: Box<SpecifierQualifierList>,
    pub declarator: Option<Box<AbstractDeclarator>>
}

#[derive(Debug)]
pub enum AbstractDeclarator {
    Pointer(Box<Pointer>),
    Declarator(Option<Box<Pointer>>, Box<DirectAbstractDeclarator>)
}

#[derive(Debug)]
pub enum DirectAbstractDeclarator {
    Decl(Box<AbstractDeclarator>, Option<Box<DirectAbstractDeclaratorEnd>>),
    Const(Option<Box<ConstantExpression>>, Option<Box<DirectAbstractDeclaratorEnd>>),
    Param(Option<Box<ParameterTypeList>>, Option<Box<DirectAbstractDeclaratorEnd>>)
}

#[derive(Debug)]
pub enum DirectAbstractDeclaratorEnd {
    Const(Option<Box<ConstantExpression>>, Option<Box<DirectAbstractDeclaratorEnd>>),
    Param(Option<Box<ParameterTypeList>>, Option<Box<DirectAbstractDeclaratorEnd>>)
}


#[derive(Debug)]
pub struct TypedefName {
    pub name: String
}

#[derive(Debug)]
pub enum Statement {
    Labeled(Box<LabeledStatement>),
    Expression(Box<ExpressionStatement>),
    Compound(Box<CompoundStatement>),
    Selection(Box<SelectionStatement>),
    Iteration(Box<IterationStatement>),
    Jump(Box<JumpStatement>),
}

#[derive(Debug)]
pub enum LabeledStatement {
    Map(Box<Identifier>, Box<Statement>),
    Case(Box<ConstantExpression>, Box<Statement>),
    Default(Box<Statement>)
}

#[derive(Debug)]
pub struct ExpressionStatement {
    pub expression: Option<Box<Expression>>
}

#[derive(Debug)]
pub struct CompoundStatement {
    pub declarations: Option<Box<DeclarationList>>,
    pub statements: Option<Box<StatementList>>
}

pub type StatementList = Vec<Box<Statement>>;

#[derive(Debug)]
pub enum SelectionStatement {
    If(Box<Expression>, Box<Statement>),
    IfElse(Box<Expression>, Box<Statement>, Box<Statement>),
    Switch(Box<Expression>, Box<Statement>)
}

#[derive(Debug)]
pub enum IterationStatement {
    While(Box<Expression>, Box<Statement>),
    DoWhile(Box<Statement>, Box<Expression>),
    For(Option<Box<Expression>>, Option<Box<Expression>>, Option<Box<Expression>>, Box<Statement>)
}

#[derive(Debug)]
pub enum JumpStatement {
    Goto(Box<Identifier>),
    Continue,
    Break,
    Return(Option<Box<Expression>>)
}

pub type Expression = SVec<Box<AssignmentExpression>, 0>;

#[derive(Debug)]
pub enum AssignmentExpression {
    Conditional(Box<ConditionalExpression>),
    Assign(Box<UnaryExpression>, Box<AssignmentOperator>, Box<AssignmentExpression>)
}

#[derive(Debug)]
pub enum AssignmentOperator {
    Assign,
    MulAssign,
    DivAssign,
    ModAssign,
    AddAssign,
    SubAssign,
    LeftShiftAssign,
    RightShiftAssign,
    AndAssign,
    OrAssign,
    XorAssign,
    NotAssign
}

#[derive(Debug)]
pub enum ConditionalExpression {
    Or(Box<LogicalOrExpression>),
    Ternary(Box<LogicalOrExpression>, Box<Expression>, Box<ConditionalExpression>)
}

#[derive(Debug)]
pub struct ConstantExpression(pub Box<ConditionalExpression>);

pub type LogicalOrExpression = SVec<Box<LogicalAndExpression>, 1>;
pub type LogicalAndExpression = SVec<Box<InclusiveOrExpression>, 2>;
pub type InclusiveOrExpression = SVec<Box<ExclusiveOrExpression>, 3>;
pub type ExclusiveOrExpression = SVec<Box<AndExpression>, 4>;
pub type AndExpression = SVec<Box<EqualityExpression>, 5>;

pub type EqualityExpression = StartExpression<RelationalExpression, EqualityExpressionEnd>;

#[derive(Debug)]
pub enum EqualityExpressionEnd {
    Equal(Box<RelationalExpression>, Option<Box<EqualityExpressionEnd>>),
    NotEqual(Box<RelationalExpression>, Option<Box<EqualityExpressionEnd>>)
}

pub type RelationalExpression = StartExpression<ShiftExpression, RelationalExpressionEnd>;

#[derive(Debug)]
pub enum RelationalExpressionEnd {
    LT(Box<ShiftExpression>, Option<Box<RelationalExpressionEnd>>),
    GT(Box<ShiftExpression>, Option<Box<RelationalExpressionEnd>>),
    LTE(Box<ShiftExpression>, Option<Box<RelationalExpressionEnd>>),
    GTE(Box<ShiftExpression>, Option<Box<RelationalExpressionEnd>>)
}

pub type ShiftExpression = StartExpression<AdditiveExpression, ShiftExpressionEnd>;

#[derive(Debug)]
pub enum ShiftExpressionEnd {
    LS(Box<AdditiveExpression>, Option<Box<ShiftExpressionEnd>>),
    RS(Box<AdditiveExpression>, Option<Box<ShiftExpressionEnd>>)
}

pub type AdditiveExpression = StartExpression<MultiplicativeExpression, AdditiveExpressionEnd>;

#[derive(Debug)]
pub enum AdditiveExpressionEnd {
    Add(Box<MultiplicativeExpression>, Option<Box<AdditiveExpressionEnd>>),
    Sub(Box<MultiplicativeExpression>, Option<Box<AdditiveExpressionEnd>>)
}

pub type MultiplicativeExpression = StartExpression<CastExpression, MultiplicativeExpressionEnd>;

#[derive(Debug)]
pub enum MultiplicativeExpressionEnd {
    Mul(Box<CastExpression>, Option<Box<MultiplicativeExpressionEnd>>),
    Div(Box<CastExpression>, Option<Box<MultiplicativeExpressionEnd>>),
    Mod(Box<CastExpression>, Option<Box<MultiplicativeExpressionEnd>>),
}

#[derive(Debug)]
pub enum CastExpression {
    Unary(Box<UnaryExpression>),
    Cast(Box<TypeName>, Box<CastExpression>)
}

#[derive(Debug)]
pub enum UnaryExpression {
    Postfix(Box<PostfixExpression>),
    Increment(Box<UnaryExpression>),
    Decrement(Box<UnaryExpression>),
    Cast(Box<UnaryOperator>, Box<CastExpression>),
    Sizeof(Box<UnaryExpression>),
    SizeofType(Box<TypeName>)
}

#[derive(Debug)]
pub enum UnaryOperator {
    Ref,
    Deref,
    Pos,
    Neg,
    Inv,
    Not
}

#[derive(Debug)]
pub struct PostfixExpression {
    pub primary: Box<PrimaryExpression>,
    pub suffixes: Box<Vec<Box<PostfixExpressionEnd>>>
}

#[derive(Debug)]
pub enum PostfixExpressionEnd {
    Index(Box<Expression>),
    Call(Option<Box<ArgumentExpressionList>>),
    Dot(Box<Identifier>),
    Deref(Box<Identifier>),
    Increment,
    Decrement
}

#[derive(Debug)]
pub enum PrimaryExpression {
    Identifier(Box<Identifier>),
    Constant(Box<Constant>),
    String(String),
    Expression(Box<Expression>)
}

pub type ArgumentExpressionList = SVec<Box<AssignmentExpression>, 0>;

#[derive(Debug)]
pub enum Constant {
    Int(u32),
    Char(char),
    Float(f32),
    Enum(u32)
}
