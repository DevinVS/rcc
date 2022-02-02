pub type CProgram = Vec<ExternalDeclaration>;

#[derive(Debug)]
pub struct Identifier(pub String);

#[derive(Debug)]
pub struct CSVec<T> {
    pub items: Vec<T>
}

#[derive(Debug)]
pub enum ExternalDeclaration {
    FunctionDefinition(FunctionDefinition),
    Declaration(Declaration)
}

#[derive(Debug)]
pub struct FunctionDefinition {
    pub declaration_specifiers: Option<DeclarationSpecifiers>,
    pub declarator: Declarator,
    pub declaration_list: Option<DeclarationList>,
    pub compound: CompoundStatement
}

#[derive(Debug)]
pub struct Declaration {
    pub specs: DeclarationSpecifiers,
    pub init_list: Option<InitDeclaratorList>
}

pub type DeclarationList = Vec<Declaration>;
pub type DeclarationSpecifiers = Vec<DeclarationSpecifier>;

#[derive(Debug)]
pub enum DeclarationSpecifier {
    StorageClassSpecifier(StorageClassSpecifier),
    TypeSpecifier(TypeSpecifier),
    TypeQualifier(TypeQualifier)
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
    StructOrUnion(StructOrUnionSpecifier),
    Enum(EnumSpecifier),
    Typedef(TypedefName),
}

#[derive(Debug)]
pub enum TypeQualifier {
    Const,
    Volatile
}

#[derive(Debug)]
pub enum StructOrUnionSpecifier {
    Val(StructOrUnion, Identifier),
    List(StructOrUnion, Option<Identifier>, StructDeclarationList)
}

#[derive(Debug)]
pub enum StructOrUnion {
    Struct,
    Union
}

pub type StructDeclarationList = Vec<StructDeclaration>;

pub type InitDeclaratorList = CSVec<InitDeclarator>;

#[derive(Debug)]
pub enum InitDeclarator {
    Declarator(Declarator),
    Assign(Declarator, Initializer)
}

#[derive(Debug)]
pub struct StructDeclaration {
    pub spec_qualifier_list: SpecifierQualifierList,
    pub decl_list: StructDeclarationList
}

pub type SpecifierQualifierList = Vec<SpecifierOrQualifier>;

#[derive(Debug)]
pub enum SpecifierOrQualifier {
    Specifier(TypeSpecifier),
    Qualifier(TypeQualifier)
}

pub type StructDeclaratorList = CSVec<StructDeclarator>;

#[derive(Debug)]
pub enum StructDeclarator {
    Decl(Declarator),
    Const(Option<Declarator>, ConstantExpression)
}

#[derive(Debug)]
pub enum EnumSpecifier {
    List(Option<Identifier>, EnumeratorList),
    Identifier(Identifier)
}

pub type EnumeratorList = CSVec<Enumerator>;

#[derive(Debug)]
pub enum Enumerator {
    Identifier(Identifier),
    ConstantExpression(Identifier, ConstantExpression)
}

#[derive(Debug)]
pub struct Declarator {
    pub pointer: Option<Pointer>,
    pub decl: DirectDeclarator
}

#[derive(Debug)]
pub enum DirectDeclarator {
    Identifier(Identifier, Option<DirectDeclaratorEnd>),
    Declarator(Box<Declarator>, Option<DirectDeclaratorEnd>),
}

#[derive(Debug)]
pub enum DirectDeclaratorEnd {
    ConstantExpression(Option<ConstantExpression>, Option<Box<DirectDeclaratorEnd>>),
    ParameterTypeList(ParameterTypeList, Option<Box<DirectDeclaratorEnd>>),
    IdentifierList(Option<IdentifierList>, Option<Box<DirectDeclaratorEnd>>)
}


#[derive(Debug)]
pub enum Pointer {
    Pointer(Option<TypeQualifierList>),
    List(Option<TypeQualifierList>, Box<Pointer>)
}

pub type TypeQualifierList = Vec<TypeQualifier>;

#[derive(Debug)]
pub enum ParameterTypeList {
    Regular(ParameterList),
    VarArgs(ParameterList)
}

pub type ParameterList = CSVec<ParameterDeclaration>;

#[derive(Debug)]
pub enum ParameterDeclaration {
    Declaration(DeclarationSpecifiers, Declarator),
    AbstractDeclarator(DeclarationSpecifiers, Option<AbstractDeclarator>)
}

pub type IdentifierList = CSVec<Identifier>;

#[derive(Debug)]
pub enum Initializer {
    Assign(AssignmentExpression),
    List(InitializerList)
}

pub type InitializerList = CSVec<Initializer>;

#[derive(Debug)]
pub struct TypeName {
    pub list: SpecifierQualifierList,
    pub declarator: Option<Box<AbstractDeclarator>>
}

#[derive(Debug)]
pub enum AbstractDeclarator {
    Pointer(Pointer),
    Declarator(Option<Pointer>, DirectAbstractDeclarator)
}

#[derive(Debug)]
pub enum DirectAbstractDeclarator {
    Decl(Box<AbstractDeclarator>, Option<DirectAbstractDeclaratorEnd>),
    Const(Option<ConstantExpression>, Option<DirectAbstractDeclaratorEnd>),
    Param(Option<ParameterTypeList>, Option<DirectAbstractDeclaratorEnd>)
}

#[derive(Debug)]
pub enum DirectAbstractDeclaratorEnd {
    Const(Option<ConstantExpression>, Option<Box<DirectAbstractDeclaratorEnd>>),
    Param(Option<ParameterTypeList>, Option<Box<DirectAbstractDeclaratorEnd>>)
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
    Map(Identifier, Statement),
    Case(ConstantExpression, Statement),
    Default(Statement)
}

#[derive(Debug)]
pub struct ExpressionStatement {
    pub expression: Option<Expression>
}

#[derive(Debug)]
pub struct CompoundStatement {
    pub declarations: Option<DeclarationList>,
    pub statements: Option<StatementList>
}

pub type StatementList = Vec<Statement>;

#[derive(Debug)]
pub enum SelectionStatement {
    If(Expression, Statement),
    IfElse(Expression, Statement, Statement),
    Switch(Expression, Statement)
}

#[derive(Debug)]
pub enum IterationStatement {
    While(Expression, Statement),
    DoWhile(Statement, Expression),
    For(Option<Expression>, Option<Expression>, Option<Expression>, Statement)
}

#[derive(Debug)]
pub enum JumpStatement {
    Goto(Identifier),
    Continue,
    Break,
    Return(Option<Expression>)
}

pub type Expression = CSVec<AssignmentExpression>;

#[derive(Debug)]
pub enum AssignmentExpression {
    Conditional(ConditionalExpression),
    Assign(Box<UnaryExpression>, AssignmentOperator, Box<AssignmentExpression>)
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
    Or(LogicalOrExpression),
    Ternary(LogicalOrExpression, Expression, Box<ConditionalExpression>)
}

#[derive(Debug)]
pub struct ConstantExpression(pub ConditionalExpression);

#[derive(Debug)]
pub struct LogicalOrExpression {
    pub expr: LogicalAndExpression,
    pub next: Option<LogicalOrExpressionEnd>
}

#[derive(Debug)]
pub struct LogicalOrExpressionEnd {
    pub expr: LogicalAndExpression,
    pub next: Option<Box<LogicalOrExpressionEnd>>
}

#[derive(Debug)]
pub struct LogicalAndExpression {
    pub expr: InclusiveOrExpression,
    pub next: Option<LogicalAndExpressionEnd>
}

#[derive(Debug)]
pub struct LogicalAndExpressionEnd {
    pub expr: InclusiveOrExpression,
    pub next: Option<Box<LogicalAndExpressionEnd>>
}

#[derive(Debug)]
pub struct InclusiveOrExpression {
    pub expr: ExclusiveOrExpression,
    pub next: Option<InclusiveOrExpressionEnd>
}

#[derive(Debug)]
pub struct InclusiveOrExpressionEnd {
    pub expr: ExclusiveOrExpression,
    pub next: Option<Box<InclusiveOrExpressionEnd>>
}

#[derive(Debug)]
pub struct ExclusiveOrExpression {
    pub expr: AndExpression,
    pub next: Option<ExclusiveOrExpressionEnd>
}

#[derive(Debug)]
pub struct ExclusiveOrExpressionEnd {
    pub expr: AndExpression,
    pub next: Option<Box<ExclusiveOrExpressionEnd>>
}

#[derive(Debug)]
pub struct AndExpression {
    pub expr: EqualityExpression,
    pub next: Option<AndExpressionEnd>
}

#[derive(Debug)]
pub struct AndExpressionEnd {
    pub expr: EqualityExpression,
    pub next: Option<Box<AndExpressionEnd>>
}

#[derive(Debug)]
pub struct EqualityExpression {
    pub expr: RelationalExpression,
    pub next: Option<EqualityExpressionEnd>
}

#[derive(Debug)]
pub enum EqualityExpressionEnd {
    Equal(RelationalExpression, Option<Box<EqualityExpressionEnd>>),
    NotEqual(RelationalExpression, Option<Box<EqualityExpressionEnd>>)
}

#[derive(Debug)]
pub struct RelationalExpression {
    pub expr: ShiftExpression,
    pub next: Option<RelationalExpressionEnd>
}

#[derive(Debug)]
pub enum RelationalExpressionEnd {
    LT(ShiftExpression, Option<Box<RelationalExpressionEnd>>),
    GT(ShiftExpression, Option<Box<RelationalExpressionEnd>>),
    LTE(ShiftExpression, Option<Box<RelationalExpressionEnd>>),
    GTE(ShiftExpression, Option<Box<RelationalExpressionEnd>>)
}

#[derive(Debug)]
pub struct ShiftExpression {
    pub expr: AdditiveExpression,
    pub next: Option<ShiftExpressionEnd>
}

#[derive(Debug)]
pub enum ShiftExpressionEnd {
    LS(AdditiveExpression, Option<Box<ShiftExpressionEnd>>),
    RS(AdditiveExpression, Option<Box<ShiftExpressionEnd>>)
}

#[derive(Debug)]
pub struct AdditiveExpression {
    pub expr: MultiplicativeExpression,
    pub next: Option<AdditiveExpressionEnd>
}

#[derive(Debug)]
pub enum AdditiveExpressionEnd {
    Add(MultiplicativeExpression, Option<Box<AdditiveExpressionEnd>>),
    Sub(MultiplicativeExpression, Option<Box<AdditiveExpressionEnd>>)
}

#[derive(Debug)]
pub struct MultiplicativeExpression {
    pub expr: CastExpression,
    pub next: Option<MultiplicativeExpressionEnd>,
}

#[derive(Debug)]
pub enum MultiplicativeExpressionEnd {
    Mul(CastExpression, Option<Box<MultiplicativeExpressionEnd>>),
    Div(CastExpression, Option<Box<MultiplicativeExpressionEnd>>),
    Mod(CastExpression, Option<Box<MultiplicativeExpressionEnd>>),
}

#[derive(Debug)]
pub enum CastExpression {
    Unary(Box<UnaryExpression>),
    Cast(TypeName, Box<CastExpression>)
}

#[derive(Debug)]
pub enum UnaryExpression {
    Postfix(PostfixExpression),
    Increment(Box<UnaryExpression>),
    Decrement(Box<UnaryExpression>),
    Cast(UnaryOperator, CastExpression),
    Sizeof(Box<UnaryExpression>),
    SizeofType(TypeName)
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
pub enum PostfixExpression {
    Primary(PrimaryExpression, Option<PostfixExpressionEnd>),
}

#[derive(Debug)]
pub enum PostfixExpressionEnd {
    Index(Expression, Option<Box<PostfixExpressionEnd>>),
    Call(Option<ArgumentExpressionList>, Option<Box<PostfixExpressionEnd>>),
    Dot(Identifier, Option<Box<PostfixExpressionEnd>>),
    Deref(Identifier, Option<Box<PostfixExpressionEnd>>),
    Increment(Option<Box<PostfixExpressionEnd>>),
    Decrement(Option<Box<PostfixExpressionEnd>>)
}

#[derive(Debug)]
pub enum PrimaryExpression {
    Identifier(Identifier),
    Constant(Constant),
    String(String),
    Expression(Expression)
}


pub type ArgumentExpressionList = CSVec<AssignmentExpression>;

#[derive(Debug)]
pub enum Constant {
    Int(u32),
    Char(char),
    Float(f32),
    Enum(u32)
}
