#lang reader "../private/parameterized-peg.rkt"

(require peg/peg-result
         "../ast.rkt"
         "char-classes.rkt"
         "parse-helpers.rkt");

///
/// Lexical grammar
///

// White Space and Comments

WhiteSpace < TAB / VT / FF / SP / NBSP / ZWNBSP / USP;
LineTerminator < LF / CR / LS / PS;
LineTerminatorSequence < LF / CR LF? / LS / PS;

Comment < MultiLineComment / SingleLineComment;
MultiLineComment < ~'/*' ('*' !'/' / [^*])* ~'*/';
SingleLineComment < ~'//' (!LineTerminator .)*;

__ < (WhiteSpace / LineTerminatorSequence / Comment)+;
_ < __?;
NoLineTerminator < (WhiteSpace / ('/*' ('*' !'/' / !LineTerminator [^*])* '*/') / SingleLineComment)?;

SEMICOLON <
    NoLineTerminator (';' / LineTerminatorSequence (_ ';')?) /
    _ (&'}' / !.);

// Names and Keywords

PrivateIdentifier <- '#' IdentifierName;
IdentifierName <- IdentifierStart IdentifierPart*;
IdentifierStart <- IdentifierStartChar / ~'\\' UnicodeEscapeSequence;
IdentifierPart <- IdentifierPartChar / ~'\\' UnicodeEscapeSequence;
IdentifierStartChar <- UnicodeIDStart / '$' / '_';
IdentifierPartChar <- UnicodeIDContinue / '$' / ZWNJ / ZWJ;
ReservedWord <-
    ('await' / 'break' / 'case' / 'catch' / 'class' / 'const' / 'continue' /
     'debugger' / 'default' / 'delete' / 'do' / 'else' / 'enum' / 'export' / 'extends' /
     'false' / 'finally' / 'for' / 'function' / 'if' / 'import' / 'in' / 'instanceof' /
     'new' / 'null' / 'return' / 'super' / 'switch' / 'this' / 'throw' / 'true' / 'try' /
     'typeof' / 'var' / 'void' / 'while' / 'with' / 'yield')
    !IdentifierPartChar;

// Punctuators

Punctuator <- OptionalChainingPunctuator / OtherPunctuator;
OptionalChainingPunctuator <- '?.' !DecimalDigit;
OtherPunctuator <- '>>>=' /
    '...' / '===' / '!==' / '>>>' / '**=' / '<<=' / '>>=' / '&&=' / '||=' / '??=' /
    '<=' / '>=' / '==' / '!=' / '**' / '++' / '--' / '<<' / '>>' / '&&' / '||' / '??' /
    '+=' / '-=' / '*=' / '%=' / '&=' / '|=' / '^=' / '=>' /
    '{' / '(' / ')' / '[' / ']' / '.' / ';' / ',' / '<' / '>' / '+' / '-' / '*' / '%' /
    '&' / '|' / '^' / '!' / '~' / '?' / ':' / '=';
DivPunctuator <- '/=' / '/';
RightBracePunctuator <- '}';

// Literals

NullLiteral <- 'null' -> (literal:null location);

BooleanLiteral <- t:'true' / 'false' -> (literal:boolean location (if t #t #f));

NumericLiteralSeparator < '_';
NumericLiteral <-
    n:NonDecimalIntegerLiteral{+Sep} BigIntLiteralSuffix? /
    n:LegacyOctalIntegerLiteral /
    n:DecimalBigIntegerLiteral /
    n:DecimalLiteral
    -> (literal:number location n);
DecimalBigIntegerLiteral <-
    n:('0' BigIntLiteralSuffix /
       NonZeroDigit DecimalDigits{+Sep}? BigIntLiteralSuffix /
       NonZeroDigit NumericLiteralSeparator DecimalDigits{+Sep} BigIntLiteralSuffix)
    -> (string->number n 10);
NonDecimalIntegerLiteral{Sep} <- BinaryIntegerLiteral{?Sep} / OctalIntegerLiteral{?Sep} / HexIntegerLiteral{?Sep};
BigIntLiteralSuffix < 'n';
DecimalLiteral <-
    n:(DecimalIntegerLiteral ('.' DecimalDigits{+Sep}?)? ExponentPart{+Sep}? /
       '.' DecimalDigits{+Sep} ExponentPart{+Sep}?)
    -> (string->number n 10);
DecimalIntegerLiteral <-
    NonZeroDigit NumericLiteralSeparator? DecimalDigits{+Sep}? /
    NonOctalDecimalIntegerLiteral /
    '0';
DecimalDigits{Sep} <-
    {~Sep} DecimalDigit+ /
    {+Sep} DecimalDigit+ (NumericLiteralSeparator DecimalDigit+)*;
DecimalDigit <- [0-9];
NonZeroDigit <- [1-9];
ExponentPart{Sep} <- ExponentIndicator SignedInteger{?Sep};
ExponentIndicator <- [eE];
SignedInteger{Sep} <- [+\-]? DecimalDigits{?Sep};
BinaryIntegerLiteral{Sep} <- ('0b' / '0B') n:BinaryDigits{?Sep} -> (string->number n 2);
BinaryDigits{Sep} <-
    {~Sep} BinaryDigit+ /
    {+Sep} BinaryDigit+ (NumericLiteralSeparator BinaryDigit+)*;
BinaryDigit <- [01];
OctalIntegerLiteral{Sep} <- ('0o' / '0O') n:OctalDigits{?Sep} -> (string->number n 8);
OctalDigits{Sep} <-
    {~Sep} OctalDigit+ /
    {+Sep} OctalDigit+ (NumericLiteralSeparator OctalDigit+)*;
LegacyOctalIntegerLiteral <- '0' n:OctalDigit+ !NonOctalDigit -> (string->number n 8);
NonOctalDecimalIntegerLiteral <- n:('0' OctalDigit* NonOctalDigit DecimalDigit*) -> (string->number n 10);
OctalDigit <- [0-7];
NonOctalDigit <- [89];
HexIntegerLiteral{Sep} <- ('0x' / '0X') n:HexDigits{?Sep} -> (string->number n 16);
HexDigits{Sep} <-
    {~Sep} HexDigit+ /
    {+Sep} HexDigit+ (NumericLiteralSeparator HexDigit+)*;
HexDigit <- [0-9a-fA-F];

StringLiteral <-
    '"' s:DoubleStringCharacter* '"' /
    '\'' s:SingleStringCharacter* '\''
    -> (literal:string location (if (string? s) s (apply string-append s)));
DoubleStringCharacter <- !('"' / '\\' / LineTerminator) . / LS / PS / LineContinuation / ~'\\' EscapeSequence;
SingleStringCharacter <- !('\'' / '\\' / LineTerminator) . / LS / PS / LineContinuation / ~'\\' EscapeSequence;
LineContinuation < '\\' LineTerminatorSequence;
EscapeSequence <- CharacterEscapeSequence / '0' !DecimalDigit / LegacyOctalEscapeSequence /
    NonOctalDecimalEscapeSequence / HexEscapeSequence / UnicodeEscapeSequence;
CharacterEscapeSequence <- SingleEscapeCharacter / NonEscapeCharacter;
SingleEscapeCharacter <- c:["'\\bfnrtv]
    -> (case c
         [("b") "\u0008"]
         [("t") "\u0009"]
         [("n") "\u000A"]
         [("v") "\u000B"]
         [("f") "\u000C"]
         [("r") "\u000D"]
         [else c]);
NonEscapeCharacter <- !(EscapeCharacter / LineTerminator) .;
EscapeCharacter <- SingleEscapeCharacter / DecimalDigit / 'x' / 'u';
LegacyOctalEscapeSequence <-
    code:('0' ![89] /
          [1-7] ![0-8] /
          [0-3] [0-7] ![0-7] /
          [4-7] [0-7] /
          [0-3] [0-7] [0-7])
    -> (string (integer->char (string->number code 8)));
NonOctalDecimalEscapeSequence <- [89];
HexEscapeSequence <- 'x' code:(HexDigit HexDigit)
    -> (string (integer->char (string->number code 16)));
UnicodeEscapeSequence <- 'u' code:Hex4Digits / 'u{' code:HexDigit+ '}'
    -> (string (integer->char (string->number code 16)));
Hex4Digits <- HexDigit HexDigit HexDigit HexDigit;

// Regular Expressions

RegularExpressionLiteral <- '/' pattern:RegularExpressionBody '/' flags:RegularExpressionFlags
    -> (literal:regexp location
                       (if (string? pattern) pattern "")
                       (if (string? flags) (string->list flags) '()));
RegularExpressionBody <- RegularExpressionFirstChar RegularExpressionChar*;
RegularExpressionFirstChar <- ![*\\/\[] RegularExpressionNonTerminator /
    RegularExpressionBackslashSequence / RegularExpressionClass;
RegularExpressionChar <- ![\\/\[] RegularExpressionNonTerminator /
    RegularExpressionBackslashSequence / RegularExpressionClass;
RegularExpressionBackslashSequence <- '\\' RegularExpressionNonTerminator;
RegularExpressionNonTerminator <- !LineTerminator .;
RegularExpressionClass <- '[' RegularExpressionClassChar* ']';
RegularExpressionClassChar <- ![\]\\] RegularExpressionNonTerminator / RegularExpressionBackslashSequence;
RegularExpressionFlags <- IdentifierPartChar*;

// TODO: templates

///
/// Expressions
///

IdentifierReference{Yield, Await} <- id:Identifier{?Yield, ?Await}
    -> (expression:reference location id);

BindingIdentifier{Yield, Await} <- Identifier{?Yield, ?Await};

LabelIdentifier{Yield, Await} <- Identifier{?Yield, ?Await};

Identifier{Yield, Await} <-
    !ReservedWord name:IdentifierName /
    {~Yield} name:'yield' /
    {~Await} name:'await'
    -> (identifier location (string->symbol name));

// TODO: TemplateLiteral

PrimaryExpression{Yield, Await} <-
    ThisExpression / IdentifierReference{?Yield, ?Await} /
    LiteralExpression{?Yield, ?Await} / FunctionExpression /
    ClassExpression{?Yield, ?Await} / GeneratorExpression / AsyncFunctionExpression /
    AsyncGeneratorExpression / RegularExpressionLiteral / // TODO: TemplateLiteral
    ParenthesizedExpression{?Yield, ?Await};

ThisExpression <- 'this' -> (expression:this location);

LiteralExpression{Yield, Await} <-
    e:(Literal / ArrayLiteral{?Yield, ?Await} / ObjectLiteral{?Yield, ?Await})
    -> (expression:literal location e);

ParenthesizedExpression{Yield, Await} <- '(' _ e:Expression{+In, ?Yield, ?Await} _ ')' -> e;

Literal <- NullLiteral / BooleanLiteral / NumericLiteral / StringLiteral;

ArrayLiteral{Yield, Await} <-
    elements:(~'[' _ (ElementList{?Yield, ?Await} _ (~',' _ Elision?)? / Elision?) ~']')
    -> (literal:array location elements);

ElementList{Yield, Await} <-
    Elision?
    (AssignmentExpression{+In, ?Yield, ?Await} / SpreadElement{?Yield, ?Await})
    (_ ~',' _ ElementList{?Yield, ?Await})*;

Elision <- x:(',' _)+ -> (elision location (string-length x));

SpreadElement{Yield, Await} <- '...' _ e:AssignmentExpression{+In, ?Yield, ?Await}
    -> (spread location e);

ObjectLiteral{Yield, Await} <- '{' _ properties:(PropertyDefinitionList{?Yield, ?Await} _ (',' _)?)? '}'
    -> (literal:object location properties);

PropertyDefinitionList{Yield, Await} <-
    PropertyDefinition{?Yield, ?Await} (_ ~',' _ PropertyDefinition{?Yield, ?Await})*;

PropertyDefinition{Yield, Await} <-
    PropertyName{?Yield, ?Await} _ ':' _ AssignmentExpression{+In, ?Yield, ?Await} /
    MethodDefinition{?Yield, ?Await} /
    CoverInitializedName{?Yield, ?Await} /
    IdentifierReference{?Yield, ?Await} /
    '...' _ AssignmentExpression{+In, ?Yield, ?Wait};

PropertyName{Yield, Await} <-
    LiteralPropertyName /
    ComputedPropertyName{?Yield, ?Await};

LiteralPropertyName <- IdentifierName / StringLiteral / NumericLiteral;

ComputedPropertyName{Yield, Await} <- '[' _ AssignmentExpression{+In, ?Yield, ?Await} _ ']';

CoverInitializedName{Yield, Await} <- IdentifierReference{?Yield, ?Await} _ Initializer{+In, ?Yield, ?Await};

Initializer{In, Yield, Await} <- ~'=' _ e:AssignmentExpression{?In, ?Yield, ?Await} -> e;

// TODO: TemplateLiteral

MemberExpression{Yield, Await} <-
    base:(PrimaryExpression{?Yield, ?Await} /
          SuperProperty{?Yield, ?Await} /
          MetaProperty /
          MemberExpression_New{?Yield, ?Await})
    chain:(_ PropertyReference{?Yield, ?Await})*
    -> (fold-chain base chain);

PropertyReference{Yield, Await} <-
    '[' _ property:Expression{+In, ?Yield, ?Await} _ ']' /
    '.' _ property:PropertyIdentifier
    -> (let ([loc location])
         (λ (base)
           (expression:member-reference (merge-locations (syntax-element-location base) loc) base property)));

PropertyIdentifier <- n:(IdentifierName / PrivateIdentifier)
    -> (identifier location n);

MemberExpression_New{Yield, Await} <-
    'new' _ constructor:MemberExpression{?Yield, ?Await} _ arguments:Arguments{?Yield, ?Await}
    -> (expression:new location constructor arguments);

SuperProperty{Yield, Await} <-
    'super' _ ('[' _ property:Expression{+In, ?Yield, ?Await} _ ']' /
               '.' _ property:PropertyIdentifier)
    -> (expression:super-reference location property);

MetaProperty <- NewTarget / ImportMeta;

NewTarget <- 'new' _ '.' _ 'target';

ImportMeta <- 'import' _ '.' _ 'meta';

CallExpression{Yield, Await} <-
    base:MemberExpression{?Yield, ?Await}
    chain:(_ CallExpressionArguments{?Yield, ?Await}
             (_ (CallExpressionArguments{?Yield, ?Await} / PropertyReference{?Yield, ?Await}))*)? /
    base:(SuperCall{?Yield, ?Await} / ImportCall{?Yield, ?Await})
    chain:(_ (CallExpressionArguments{?Yield, ?Await} / PropertyReference{?Yield, ?Await}))*
    -> (fold-chain base chain);

CallExpressionArguments{Yield, Await} <- arguments:Arguments{?Yield, ?Await}
    -> (let ([loc location])
         (λ (base) (expression:call (merge-locations (syntax-element-location base) loc) base arguments)));

SuperCall{Yield, Await} <- 'super' _ arguments:Arguments{?Yield, ?Await}
    -> (expression:super-call location arguments);

ImportCall{Yield, Await} <- 'import' _ '(' _ AssignmentExpression{+In, ?Yield, ?Await} _ ')';

Arguments{Yield, Await} <- ~'(' _ (ArgumentList{?Yield, ?Await} _ ~(',' _)?)? ~')';

ArgumentList{Yield, Await} <-
    (AssignmentExpression{+In, ?Yield, ?Await} / SpreadElement{?Yield, ?Await})
    (_ ~',' _ ArgumentList{?Yield, ?Await})*;

OptionalExpression{Yield, Await} <-
    base:CallExpression{?Yield, ?Await} chain:(_ OptionalChain{?Yield, ?Await})*
    -> (fold-chain base chain);

OptionalChain{Yield, Await} <-
    '?.' _ (args:Arguments{?Yield, ?Await} /
            '[' _ property:Expression{+In, ?Yield, ?Await} _ ']' /
            property:PropertyIdentifier) /
    e:CallExpressionArguments{?Yield, ?Await} /
    e:PropertyReference{?Yield, ?Await}
    -> (or e
           (let ([loc location])
             (λ (base)
               ((if args expression:call expression:member-reference)
                (merge-locations (syntax-element-location base) loc)
                (expression:optional base)
                (or args property)))));

NewExpression{Yield, Await} <-
    'new' _ type:(MemberExpression{?Yield, ?Await} / NewExpression{?Yield, ?Await})
    -> (expression:new location type '());

LeftHandSideExpression{Yield, Await} <-
    OptionalExpression{?Yield, ?Await} /
    NewExpression{?Yield, ?Await};

UpdateExpression{Yield, Await} <-
    pre:UpdateOperator _ e:UnaryExpression{?Yield, ?Await} /
    e:LeftHandSideExpression{?Yield, ?Await} (NoLineTerminator post:UpdateOperator)?
    -> (cond
         [pre (expression:unary location pre e)]
         [post (expression:postfix location e post)]
         [else e]);

UpdateOperator <- op:('++' / '--') -> (operator location op);

UnaryExpression{Yield, Await} <-
    op:UnaryOperator _ e:UnaryExpression{?Yield, ?Await} /
    e:UpdateExpression{?Yield, ?Await} /
    {+Await} e:AwaitExpression{?Yield}
    -> (if op (expression:unary location op e) e);

UnaryOperator <- op:('delete' / 'void' / 'typeof' / '+' / '-' / '~' / '!') -> (operator location op);

ExponentiationExpression{Yield, Await} <-
    l:UpdateExpression{?Yield, ?Await} (_ op:ExponentiationOperator _ r:ExponentiationExpression{?Yield, ?Await})? /
    e:UnaryExpression{?Yield, ?Await}
    -> (if op (expression:binary location l op r) (or l e));

ExponentiationOperator <- op:'**' -> (operator location op);

MultiplicativeExpression{Yield, Await} <-
    e:(ExponentiationExpression{?Yield, ?Await}
       (_ MultiplicativeOperator _ ExponentiationExpression{?Yield, ?Await})*)
    -> (associate-left e);

MultiplicativeOperator <- op:[*/%] -> (operator location op);

AdditiveExpression{Yield, Await} <-
    e:(MultiplicativeExpression{?Yield, ?Await}
       (_ AdditiveOperator _ MultiplicativeExpression{?Yield, ?Await})*)
    -> (associate-left e);

AdditiveOperator <- op:[+\-] -> (operator location op);

ShiftExpression{Yield, Await} <-
    e:(AdditiveExpression{?Yield, ?Await}
       (_ ShiftOperator _ AdditiveExpression{?Yield, ?Await})*)
    -> (associate-left e);

ShiftOperator <- op:('<<' / '>>>' / '>>') -> (operator location op);

RelationalExpression{In, Yield, Await} <-
    {~In} e:(ShiftExpression{?Yield, ?Await}
             (_ RelationalOperator{~In} _ ShiftExpression{?Yield, ?Await})*) /
    {+In} e:((PrivateIdentifier _ InOperator _)?
             (ShiftExpression{?Yield, ?Await}
              (_ RelationalOperator{+In} _ ShiftExpression{?Yield, ?Await})*))
    -> (associate-left e);

RelationalOperator{In} <-
    op:('<=' / '>=' / '<' / '>' / 'instanceof') /
    {+In} op:'in'
    -> (operator location op);

InOperator <- op:'in' -> (operator location op);

EqualityExpression{In, Yield, Await} <-
    e:(RelationalExpression{?In, ?Yield, ?Await}
       (_ EqualityOperator _ RelationalExpression{?In, ?Yield, ?Await})*)
    -> (associate-left e);

EqualityOperator <- op:('===' / '!==' / '==' / '!=') -> (operator location op);

BitwiseANDExpression{In, Yield, Await} <-
    e:(EqualityExpression{?In, ?Yield, ?Await}
       (_ BitwiseANDOperator _ EqualityExpression{?In, ?Yield, ?Await})*)
    -> (associate-left e);

BitwiseANDOperator <- op:'&' -> (operator location op);

BitwiseXORExpression{In, Yield, Await} <-
    e:(BitwiseANDExpression{?In, ?Yield, ?Await}
       (_ BitwiseXOROperator _ BitwiseANDExpression{?In, ?Yield, ?Await})*)
    -> (associate-left e);

BitwiseXOROperator <- op:'^' -> (operator location op);

BitwiseORExpression{In, Yield, Await} <-
    e:(BitwiseXORExpression{?In, ?Yield, ?Await}
       (_ BitwiseOROperator _ BitwiseXORExpression{?In, ?Yield, ?Await})*)
    -> (associate-left e);

BitwiseOROperator <- op:'|' -> (operator location op);

LogicalANDExpression{In, Yield, Await} <-
    e:(BitwiseORExpression{?In, ?Yield, ?Await}
       (_ LogicalANDOperator _ BitwiseORExpression{?In, ?Yield, ?Await})*)
    -> (associate-left e);

LogicalANDOperator <- op:'&&' -> (operator location op);

LogicalORExpression{In, Yield, Await} <-
    e:(LogicalANDExpression{?In, ?Yield, ?Await}
       (_ LogicalOROperator _ LogicalANDExpression{?In, ?Yield, ?Await})*)
    -> (associate-left e);

LogicalOROperator <- op:'||' -> (operator location op);

CoalesceExpression{In, Yield, Await} <-
    e:(LogicalORExpression{?In, ?Yield, ?Await}
       (_ CoalesceOperator _ BitwiseORExpression{?In, ?Yield, ?Await})*)
    -> (associate-left e);

CoalesceOperator <- op:'??' -> (operator location op);

ShortCircuitExpression{In, Yield, Await} <-
    CoalesceExpression{?In, ?Yield, ?Await};

ConditionalExpression{In, Yield, Await} <-
    e:ShortCircuitExpression{?In, ?Yield, ?Await}
    (_ '?' _ c:AssignmentExpression{+In, ?Yield, ?Await}
     _ ':' _ a:AssignmentExpression{+In, ?Yield, ?Await})?
    -> (if c (expression:conditional location e c a) e);

AssignmentExpression{In, Yield, Await} <-
    l:ArrowFunction{?In, ?Yield, ?Await} /
    l:ConditionalExpression{?In, ?Yield, ?Await} (_ op:AssignmentOperator _ r:AssignmentExpression{?In, ?Yield, ?Await})? /
    {+Yield} e:YieldExpression{?In, ?Await} /
    l:AsyncArrowFunction{?In, ?Yield, ?Await}
    -> (if r (expression:binary location l op r) l);

AssignmentOperator <-
    op:('=' / '*=' / '/=' / '%=' / '+=' / '-=' /
        '<<=' / '>>=' / '>>>=' / '&=' / '^=' / '|=' /
        '**=' / '&&=' / '||=' / '??=')
    -> (operator location op);

AssignmentPattern{Yield, Await} <-
    ObjectAssignmentPattern{?Yield, ?Await} /
    ArrayAssignmentPattern{?Yield, ?Await};

ObjectAssignmentPattern{Yield, Await} <-
    '{' _ (AssignmentRestProperty{?Yield, ?Await} _)? '}' /
    '{' _ AssignmentPropertyList{?Yield, ?Await} _ (',' _ (AssignmentRestProperty{?Yield, ?Await} _)?)? '}';

ArrayAssignmentPattern{Yield, Await} <-
    '[' _ (Elision _)? (AssignmentRestElement{?Yield, ?Await} _)? ']' /
    '[' _ AssignmentElementList{?Yield, ?Await} _ (',' _ (Elision _)? (AssignmentRestElement{?Yield, ?Await} _)?)? ']';

AssignmentRestProperty{Yield, Await} <- '...' _ DestructuringAssignmentTarget{?Yield, ?Await};

AssignmentPropertyList{Yield, Await} <-
    AssignmentProperty{?Yield, ?Await} (_ ',' _ AssignmentProperty{?Yield, ?Await})*;

AssignmentElementList{Yield, Await} <-
    AssignmentElisionElement{?Yield, ?Await} (_ ',' _ AssignmentElisionElement{?Yield, ?Await})*;

AssignmentElisionElement{Yield, Await} <-
    (Elision _)? AssignmentElement{?Yield, ?Await};

AssignmentProperty{Yield, Await} <-
    PropertyName{?Yield, ?Await} _ ':' _ AssignmentElement{?Yield, ?Await} /
    IdentifierReference{?Yield, ?Await} (_ Initializer{+In, ?Yield, ?Await})?;

AssignmentElement{Yield, Await} <-
    DestructuringAssignmentTarget{?Yield, ?Await} (_ Initializer{+In, ?Yield, ?Await})?;

AssignmentRestElement{Yield, Await} <-
    '...' _ DestructuringAssignmentTarget{?Yield, ?Await};

DestructuringAssignmentTarget{Yield, Await} <-
    LeftHandSideExpression{?Yield, ?Await};

CommaExpression{In, Yield, Await} <-
    l:AssignmentExpression{?In, ?Yield, ?Await} (_ ',' _ r:CommaExpression{?In, ?Yield, ?Await})*
    -> (if r (expression:comma location l r) l);

Expression{In, Yield, Await} <- CommaExpression{?In, ?Yield, ?Await};

//
// Statements
//

Statement{Yield, Await, Return} <-
    BlockStatement{?Yield, ?Await, ?Return} /
    VariableStatement{?Yield, ?Await} /
    EmptyStatement /
    ExpressionStatement{?Yield, ?Await} /
    IfStatement{?Yield, ?Await, ?Return} /
    BreakableStatement{?Yield, ?Await, ?Return} /
    ContinueStatement{?Yield, ?Await} /
    BreakStatement{?Yield, ?Await} /
    {+Return} ReturnStatement{?Yield, ?Await} /
    WithStatement{?Yield, ?Await, ?Return} /
    LabelledStatement{?Yield, ?Await, ?Return} /
    ThrowStatement{?Yield, ?Await} /
    TryStatement{?Yield, ?Await, ?Return} /
    DebuggerStatement;

Declaration{Yield, Await} <-
    HoistableDeclaration{?Yield, ?Await, ~Default} /
    ClassDeclaration{?Yield, ?Await, ~Defualt} /
    LexicalDeclaration{+In, ?Yield, ?Await};

HoistableDeclaration{Yield, Await, Default} <-
    FunctionDeclaration{?Yield, ?Await, ?Default} /
    GeneratorDeclaration{?Yield, ?Await, ?Default} /
    AsyncFunctionDeclaration{?Yield, ?Await, ?Default} /
    AsyncGeneratorDeclaration{?Yield, ?Await, ?Default};

BreakableStatement{Yield, Await, Return} <-
    IterationStatement{?Yield, ?Await, ?Return} /
    SwitchStatement{?Yield, ?Await, ?Return};

BlockStatement{Yield, Await, Return} <-
    body:Block{?Yield, ?Await, ?Return}
    -> (statement:block location body);

Block{Yield, Await, Return} <-
    ~'{' _ (StatementList{?Yield, ?Await, ?Return} _)? ~'}';

StatementList{Yield, Await, Return} <-
    StatementListItem{?Yield, ?Await, ?Return} (_ StatementListItem{?Yield, ?Await, ?Return})*;

StatementListItem{Yield, Await, Return} <-
    Statement{?Yield, ?Await, ?Return} /
    Declaration{?Yield, ?Await};

LexicalDeclaration{In, Yield, Await} <-
    t:('let' / 'const') _ bindings:BindingList{?In, ?Yield, ?Await} SEMICOLON
    -> ((if (equal? "let" t) declaration:let declaration:const)
        location
        bindings);

BindingList{In, Yield, Await} <-
    LexicalBinding{?In, ?Yield, ?Await} (_ ~',' _ LexicalBinding{?In, ?Yield, ?Await})*;

LexicalBinding{In, Yield, Await} <-
    binding:BindingIdentifier{?Yield, ?Await} (_ init:Initializer{?In, ?Yield, ?Await})? /
    binding:BindingPattern{?Yield, ?Await} _ init:Initializer{?In, ?Yield, ?Await}
    -> (variable-declaration location binding init);

VariableStatement{Yield, Await} <-
    'var' _ decls:VariableDeclarationList{+In, ?Yield, ?Await} SEMICOLON
    -> (declaration:var location decls);

VariableDeclarationList{In, Yield, Await} <-
    VariableDeclaration{?In, ?Yield, ?Await} (_ ~',' _ VariableDeclaration{?In, ?Yield, ?Await})*;

VariableDeclaration{In, Yield, Await} <-
    binding:BindingIdentifier{?Yield, ?Await} (_ init:Initializer{?In, ?Yield, ?Await})? /
    binding:BindingPattern{?Yield, ?Await} _ init:Initializer{?In, ?Yield, ?Await}
    -> (variable-declaration location binding init);

BindingPattern{Yield, Await} <-
    ObjectBindingPattern{?Yield, ?Await} /
    ArrayBindingPattern{?Yield, ?Await};

ObjectBindingPattern{Yield, Await} <-
    '{' _ (BindingRestProperty{?Yield, ?Await} _)? '}' /
    '{' _ BindingPropertyList{?Yield, ?Await} _ (',' _ (BindingRestProperty{?Yield, ?Await} _)?)? '}';

ArrayBindingPattern{Yield, Await} <-
    '[' _ Elision? (BindingRestElement{?Yield, ?Await} _)? ']' /
    '[' _ BindingElementList{?Yield, ?Await} _ (',' _ (Elision _)? (BindingRestElement{?Yield, ?Await} _)?)? ']';

BindingRestProperty{Yield, Await} <-
    '...' _ BindingIdentifier{?Yield, ?Await};

BindingPropertyList{Yield, Await} <-
    BindingProperty{?Yield, ?Await} (_ ',' _ BindingProperty{?Yield, ?Await})*;

BindingElementList{Yield, Await} <-
    BindingElisionElement{?Yield, ?Await} (_ ',' _ BindingElisionElement{?Yield, ?Await})*;

BindingElisionElement{Yield, Await} <-
    (Elision _)? BindingElement{?Yield, ?Await};

BindingProperty{Yield, Await} <-
    SingleNameBinding{?Yield, ?Await} /
    PropertyName{?Yield, ?Await} _ ':' _ BindingElement{?Yield, ?Await};

BindingElement{Yield, Await} <-
    SingleNameBinding{?Yield, ?Await} /
    BindingPattern{?Yield, ?Await} (_ Initializer{+In, ?Yield, ?Await})?;

SingleNameBinding{Yield, Await} <-
    BindingIdentifier{?Yield, ?Await} (_ Initializer{+In, ?Yield, ?Await})?;

BindingRestElement{Yield, Await} <-
    '...' _ (BindingIdentifier{?Yield, ?Await} / BindingPattern{?Yield, ?Await});

EmptyStatement <- ';' -> (statement:empty location);

ExpressionStatement{Yield, Await} <-
    !('{' / ('async' NoLineTerminator)? 'function' / 'class' / 'let' _ '[')
    e:Expression{+In, ?Yield, ?Await} SEMICOLON
    -> (statement:expression location e);

IfStatement{Yield, Await, Return} <-
    'if' _ '(' _ p:Expression{+In, ?Yield, ?Await} _ ')'
    _ c:Statement{?Yield, ?Await, ?Return}
    (_ 'else' _ a:Statement{?Yield, ?Await, ?Return})?
    -> (statement:if location p c a);

IterationStatement{Yield, Await, Return} <-
    DoWhileStatement{?Yield, ?Await, ?Return} /
    WhileStatement{?Yield, ?Await, ?Return} /
    ForStatement{?Yield, ?Await, ?Return} /
    ForInOfStatement{?Yield, ?Await, ?Return};

DoWhileStatement{Yield, Await, Return} <-
    'do' _ b:Statement{?Yield, ?Await, ?Return}
    _ 'while' _ '(' _ t:Expression{+In, ?Yield, ?Await} _ ')' SEMICOLON
    -> (statement:do location b t);

WhileStatement{Yield, Await, Return} <-
    'while' _ '(' _ t:Expression{+In, ?Yield, ?Await} _ ')' _ b:Statement{?Yield, ?Await, ?Return}
    -> (statement:while location t b);

ForStatement{Yield, Await, Return} <-
    'for' _ '(' _ ('var' _ vars:VariableDeclarationList{~In, ?Yield, ?Await} _ ';' /
                   init:LexicalDeclaration{~In, ?Yield, ?Await} /
                   !('let' _ '[') init:Expression{~In, ?Yield, ?Await}? _ ';')
                _ (test:Expression{+In, ?Yield, ?Await} _)? ';'
                _ (update:Expression{+In, ?Yield, ?Await} _)? ')'
        _ body:Statement{?Yield, ?Await, ?Return}
    -> (statement:for location
                      (if vars (declaration:var #f vars) init)
                      test
                      update
                      body);

ForInOfStatement{Yield, Await, Return} <-
    'for' _ '(' _ (!('let' _ '[') LeftHandSideExpression{?Yield, ?Await} _ 'in' _ Expression{+In, ?Yield, ?Await} /
                   'var' _ ForBinding{?Yield, ?Await} _ 'in' _ Expression{+In, ?Yield, ?Await} /
                   ForDeclaration{?Yield, ?Await} _ 'in' _ Expression{+In, ?Yield, ?Await} /
                   !('let' / 'async' _ 'of') LeftHandSideExpression{?Yield, ?Await} _ 'of' _ AssignmentExpression{+In, ?Yield, ?Await} /
                   'var' _ ForBinding{?Yield, ?Await} _ 'of' _ AssignmentExpression{+In, ?Yield, ?Await} /
                   ForDeclaration{?Yield, ?Await} _ 'of' AssignmentExpression{+In, ?Yield, ?Await}) _ ')' _
        Statement{?Yield, ?Await, ?Return} /
    {+Await} 'for' _ 'await' _ '(' _ (!'let' LeftHandSideExpression{?Yield, ?Await} /
                                      'var' _ ForBinding{?Yield, ?Await} /
                                      ForDeclaration{?Yield, ?Await}) _ 'of' _
                                   AssignmentExpression{+In, ?Yield, ?Await} _ ')' _
        Statement{?Yield, ?Await, ?Return};

ForDeclaration{Yield, Await} <-
    ('let' / 'const') _ ForBinding{?Yield, ?Await};

ForBinding{Yield, Await} <-
    BindingIdentifier{?Yield, ?Await} /
    BindingPattern{?Yield, ?Await};

ContinueStatement{Yield, Await} <-
    'continue' (NoLineTerminator label:LabelIdentifier{?Yield, ?Await})? SEMICOLON
    -> (statement:continue location label);

BreakStatement{Yield, Await} <-
    'break' (NoLineTerminator label:LabelIdentifier{?Yield, ?Await})? SEMICOLON
    -> (statement:break location label);

ReturnStatement{Yield, Await} <-
    'return' (NoLineTerminator e:Expression{+In, ?Yield, ?Await})? SEMICOLON
    -> (statement:return location e);

WithStatement{Yield, Await, Return} <-
    'with' _ '(' _ e:Expression{+In, ?Yield, ?Await} _ ')' _ body:Statement{?Yield, ?Await, ?Return}
    -> (statement:with location e body);

SwitchStatement{Yield, Await, Return} <-
    'switch' _ '(' _ e:Expression{+In, ?Yield, ?Await} _ ')' _ body:CaseBlock{?Yield, ?Await, ?Return}
    -> (statement:switch location e body);

CaseBlock{Yield, Await, Return} <-
    ~'{' _ CaseClauses{?Yield, ?Await, ?Return}?
    (DefaultClause{?Yield, ?Await, ?Return} _
    (CaseClauses{?Yield, ?Await, ?Return} _)?)? ~'}';

CaseClauses{Yield, Await, Return} <- CaseClause{?Yield, ?Await, ?Return}+;

CaseClause{Yield, Await, Return} <-
    'case' _ e:Expression{+In, ?Yield, ?Await} _ ':' _ body:StatementList{?Yield, ?Await, ?Return}?
    -> (case-clause location e body);

DefaultClause{Yield, Await, Return} <-
    'default' _ ':' _ body:StatementList{?Yield, ?Await, ?Return}?
    -> (default-clause location body);

LabelledStatement{Yield, Await, Return} <-
    label:LabelIdentifier{?Yield, ?Await} _ ':' _ item:LabelledItem{?Yield, ?Await, ?Return}
    -> (statement:label location label item);

LabelledItem{Yield, Await, Return} <-
    Statement{?Yield, ?Await, ?Return} /
    FunctionDeclaration{?Yield, ?Await, ~Default};

ThrowStatement{Yield, Await} <-
    'throw' NoLineTerminator e:Expression{+In, ?Yield, ?Await} SEMICOLON
    -> (statement:throw location e);

TryStatement{Yield, Await, Return} <-
    'try' _ body:Block{?Yield, ?Await, ?Return}
    (_ 'catch' _ ('(' _ catch-id:CatchParameter{?Yield, ?Await} _ ')' _)?
     catch-body:Block{?Yield, ?Await, ?Return}
     _ (finally:Finally{?Yield, ?Await, ?Return})? /
     _ finally:Finally{?Yield, ?Await, ?Return})
    -> (statement:try location body catch-id catch-body finally);

Finally{Yield, Await, Return} <- ~'finally' _ body:Block{?Yield, ?Await, ?Return};

CatchParameter{Yield, Await} <-
    BindingIdentifier{?Yield, ?Await} /
    BindingPattern{?Yield, ?Await};

DebuggerStatement <- 'debugger' SEMICOLON -> (statement:debugger location);

//
// Functions and Classes
//

UniqueFormalParameters{Yield, Await} <- FormalParameters{?Yield, ?Await};

FormalParameters{Yield, Await} <-
    FormalParameterList{?Yield, ?Await} (_ ~',' (_ FunctionRestParameter{?Yield, ?Await})?)? /
    FunctionRestParameter{?Yield, ?Await}?;

FormalParameterList{Yield, Await} <-
    FormalParameter{?Yield, ?Await} (_ ~',' _ FormalParameter{?Yield, ?Await})*;

FunctionRestParameter{Yield, Await} <- BindingRestElement{?Yield, ?Await};

FormalParameter{Yield, Await} <- BindingElement{?Yield, ?Await};

FunctionDeclaration{Yield, Await, Default} <-
    'function' _ name:BindingIdentifier{?Yield, ?Await} _
    '(' _ params:FormalParameters{~Yield, ~Await} _ ')' _
    '{' _ body:FunctionBody{~Yield, ~Await} _ '}' /
    {+Default} 'function' _ '(' _ params:FormalParameters{~Yield, ~Await} _ ')' _
               '{' _ body:FunctionBody{~Yield, ~Await} _ '}'
    -> (declaration:function location (function location name params body));

FunctionExpression <- f:FunctionDeclaration{~Yield, ~Await, +Default}
    -> (expression:function location f);

FunctionBody{Yield, Await} <- FunctionStatementList{?Yield, ?Await};

FunctionStatementList{Yield, Await} <- StatementList{?Yield, ?Await, +Return}?;

ArrowFunction{In, Yield, Await} <-
    params:ArrowParameters{?Yield, ?Await} NoLineTerminator '=>' _ body:ConciseBody{?In}
    -> (function location #f params body);

ArrowParameters{Yield, Await} <-
    BindingIdentifier{?Yield, ?Await} /
    ArrowFormalParameters{?Yield, ?Await};

ArrowFormalParameters{Yield, Await} <- ~'(' _ UniqueFormalParameters{?Yield, ?Await} _ ~')';

ConciseBody{In} <-
    !'{' ExpressionBody{?In, ~Await} /
    ~'{' _ FunctionBody{~Yield, ~Await} _ ~'}';

ExpressionBody{In, Await} <- AssignmentExpression{?In, ~Yield, ?Await};

AsyncArrowFunction{In, Yield, Await} <-
    'async' NoLineTerminator AsyncArrowBindingIdentifier{?Yield} NoLineTerminator '=>' _ AsyncConciseBody{?In} /
    AsyncArrowHead NoLineTerminator '=>' _ AsyncConciseBody{?In};

AsyncConciseBody{In} <-
    !'{' ExpressionBody{?In, +Await} /
    '{' _ AsyncFunctionBody _ '}';

AsyncArrowBindingIdentifier{Yield} <- BindingIdentifier{?Yield, +Await};

AsyncArrowHead <- 'async' NoLineTerminator ArrowFormalParameters{~Yield, +Await};

MethodDefinition{Yield, Await} <-
    ClassElementName{?Yield, ?Await} _ '(' _ UniqueFormalParameters{~Yield, ~Await} _ ')' _ '{' _ FunctionBody{~Yield, ~Await} _ '}' /
    GeneratorMethod{?Yield, ?Await} /
    AsyncMethod{?Yield, ?Await} /
    AsyncGeneratorMethod{?Yield, ?Await} /
    'get' _ ClassElementName{?Yield, ?Await} _ '(' _ ')' _ '{' _ FunctionBody{~Yield, ~Await} _ '}' /
    'set' _ ClassElementName{?Yield, ?Await} _ '(' _ PropertySetParameterList _ ')' _ '{' _ FunctionBody{~Yield, ~Await} _ '}';

PropertySetParameterList <- FormalParameter{~Yield, ~Await};

GeneratorDeclaration{Yield, Await, Default} <-
    'function' _ '*' _ BindingIdentifier{?Yield, ?Await} _ '(' _ FormalParameters{+Yield, ~Await} _ ')' _ '{' _ GeneratorBody _ '}' /
    {+Default} 'function' _ '*' _ '(' _ FormalParameters{+Yield, ~Await} _ ')' _ '{' _ GeneratorBody _ '}';

GeneratorExpression <-
    'function' _ '*' _ (BindingIdentifier{+Yield, ~Await} _)? '(' _ FormalParameters{+Yield, ~Await} _ ')' _ '{' _ GeneratorBody _ '}';

GeneratorMethod{Yield, Await} <-
    '*' _ ClassElementName{?Yield, ?Await} _ '(' _ UniqueFormalParameters{+Yield, ~Await} _ ')' _ '{' _ GeneratorBody _ '}';

GeneratorBody <- FunctionBody{+Yield, ~Await};

YieldExpression{In, Await} <-
    'yield' (NoLineTerminator ('*' _)? AssignmentExpression{?In, +Yield, ?Await})?;

AsyncGeneratorDeclaration{Yield, Await, Default} <-
    'async' NoLineTerminator 'function' _ '*' _ BindingIdentifier{?Yield, ?Await} _ '(' _ FormalParameters{+Yield, +Await} _ ')' _ '{' _ AsyncGeneratorBody _ '}' /
    {+Default} 'async' NoLineTerminator 'function' _ '*' _ '(' _ FormalParameters{+Yield, +Await} _ ')' _ '{' _ AsyncGeneratorBody _ '}';

AsyncGeneratorExpression <-
    'async' NoLineTerminator 'function' _ '*' _ (BindingIdentifier{+Yield, +Await} _)? '(' _ FormalParameters{+Yield, +Await} _ ')' _ '{' _ AsyncGeneratorBody _ '}';

AsyncGeneratorMethod{Yield, Await} <-
    'async' NoLineTerminator '*' _ ClassElementName{?Yield, ?Await} _ '(' _ UniqueFormalParameters{+Yield, +Await} _ ')' _ '{' _ AsyncGeneratorBody _ '}';

AsyncGeneratorBody <- FunctionBody{+Yield, +Await};

AsyncFunctionDeclaration{Yield, Await, Default} <-
    'async' NoLineTerminator 'function' _ BindingIdentifier{?Yield, ?Await} _ '(' _ FormalParameters{~Yield, +Await} _ ')' _ '{' _ AsyncFunctionBody _ '}' /
    {+Default} 'async' NoLineTerminator 'function' _ '(' FormalParameters{~Yield, ~Await} _ ')' _ '{' _ AsyncFunctionBody _ '}';

AsyncFunctionExpression <-
    'async' NoLineTerminator 'function' _ (BindingIdentifier{~Yield, +Await} _)? '(' _ FormalParameters{~Yield, +Await} _ ')' _ '{' _ AsyncFunctionBody _ '}';

AsyncMethod{Yield, Await} <-
    'async' NoLineTerminator ClassElementName{?Yield, ?Await} _ '(' _ UniqueFormalParameters{~Yield, +Await} _ ')' _ '{' _ AsyncFunctionBody _ '}';

AsyncFunctionBody <- FunctionBody{~Yield, +Await};

AwaitExpression{Yield} <- 'await' _ (UpdateExpression{?Yield, +Await} / UnaryExpression{?Yield, +Await});

ClassDeclaration{Yield, Await, Default} <-
    'class' _ BindingIdentifier{?Yield, ?Await} _ ClassTail{?Yield, ?Await} /
    {+Default} 'class' _ ClassTail{?Yield, ?Await};

ClassExpression{Yield, Await} <-
    'class' _ (BindingIdentifier{?Yield, ?Await} _)? ClassTail{?Yield, ?Await};

ClassTail{Yield, Await} <-
    (ClassHeritage{?Yield, ?Await} _)? '{' _ (ClassBody{?Yield, ?Await} _)? '}';

ClassHeritage{Yield, Await} <-
    'extends' _ LeftHandSideExpression{?Yield, ?Await};

ClassBody{Yield, Await} <- ClassElementList{?Yield, ?Await};

ClassElementList{Yield, Await} <-
    ClassElement{?Yield, ?Await} (_ ClassElement{?Yield, ?Await})*;

ClassElement{Yield, Await} <-
    ('static' _)? (MethodDefinition{?Yield, ?Await} / FieldDefinition{?Yield, ?Await} SEMICOLON) /
    ClassStaticBlock /
    SEMICOLON;

FieldDefinition{Yield, Await} <-
    ClassElementName{?Yield, ?Await} (_ Initializer{+In, ?Yield, ?Await})?;

ClassElementName{Yield, Await} <-
    PropertyName{?Yield, ?Await} /
    PrivateIdentifier;

ClassStaticBlock <- 'static' _ '{' _ ClassStaticBlockBody _ '}';

ClassStaticBlockBody <- ClassStaticBlockStatementList;

ClassStaticBlockStatementList <- StatementList{~Yield, +Await, ~Return}?;

//
// Scripts and Modules
//

Script <- _ ScriptBody? _;

ScriptBody <- StatementList{~Yield, ~Await, ~Return};

Module <- _ ModuleBody? _;

ModuleBody <- ModuleItemList;

ModuleItemList <- ModuleItem (_ ModuleItem)*;

ModuleItem <-
    ImportDeclaration /
    ExportDeclaration /
    StatementListItem{~Yield, +Await, ~Return};

ImportDeclaration <-
    'import' _ (ImportClause _ FromClause / ModuleSpecifier) _ SEMICOLON;

ImportClause <-
    ImportedDefaultBinding /
    NameSpaceImport /
    NamedImports /
    ImportedDefaultBinding _ ',' _ (NameSpaceImport / NamedImports);

ImportedDefaultBinding <- ImportedBinding;

NameSpaceImport <- '*' _ 'as' _ ImportedBinding;

NamedImports <- '{' _ (ImportsList _ (',' _)?)? '}';

FromClause <- 'from' _ ModuleSpecifier;

ImportsList <- ImportSpecifier (_ ',' _ ImportSpecifier)*;

ImportSpecifier <- (IdentifierName _ 'as' _ )? ImportedBinding;

ModuleSpecifier <- StringLiteral;

ImportedBinding <- BindingIdentifier{~Yield, +Await};

ExportDeclaration <-
    'export' _ (ExportFromClause _ FromClause SEMICOLON /
                NamedExports SEMICOLON /
                VariableStatement{~Yield, +Await} /
                Declaration{~Yield, +Await} /
                'default' _ (HoistableDeclaration{~Yield, +Await, +Default} /
                             ClassDeclaration{~Yield, +Await, +Default} /
                             !('function' / 'async' NoLineTerminator 'function' / 'class')
                             AssignmentExpression{+In, ~Yield, +Await} SEMICOLON));

ExportFromClause <-
    '*' (_ 'as' _ IdentifierName)? /
    NamedExports;

NamedExports <- '{' _ (ExportsList _ (',' _)?)? '}';

ExportsList <-
    ExportSpecifier (_ ',' _ ExportSpecifier)*;

ExportSpecifier <-
    IdentifierName (_ 'as' _ IdentifierName)?;
