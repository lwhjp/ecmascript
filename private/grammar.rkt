#lang ragg

program: (ws source-element)* ws
source-element: statement | function-declaration

ws: (COMMENT | EOL | WS)*
ws-no-eol: (COMMENT | WS)*
eol-or-semicolon: EOL | ";"

;; Expressions

primary-expression: "this" | identifier | literal | array-literal
    | object-literal | "(" ws expression ws ")"
identifier: IDENTIFIER
literal: "null" | "true" | "false" | numeric | string
numeric: NUMERIC
string: STRING
array-literal: "[" [ws assignment-expression]
                   (ws "," [ws assignment-expression])* ws "]"
object-literal: "{" (ws property-assignment ws ",")*
                    [ws property-assignment] ws "}"
property-assignment: property-name ws ":" ws assignment-expression |
    "get" ws property-name ws "(" ws ")" ws "{" (ws source-element)* ws "}" |
    "set" ws property-name ws "(" ws identifier ws ")" ws "{" (ws source-element)* ws "}"
property-name: identifier | string | numeric
member-expression:
    primary-expression |
    function-expression |
    member-expression ws "[" ws expression ws "]" |
    member-expression ws "." ws identifier |
    "new" ws member-expression ws arguments
new-expression:
    member-expression |
    "new" ws new-expression
call-expression:
    member-expression ws arguments |
    call-expression ws arguments |
    call-expression ws "[" ws expression ws "]" |
    call-expression ws "." ws identifier
arguments: "(" [ ws assignment-expression [ ws "," ws assignment-expression ]* ] ws ")"
left-hand-side-expression: new-expression | call-expression
postfix-expression: left-hand-side-expression ws-no-eol [ "++" | "--" ]
unary-expression: postfix-expression | ("delete" | "void" | "typeof" | "++" | "--" | "+" | "-" | "~" | "!") ws unary-expression
multiplicative-expression: [multiplicative-expression ws ("*" | "/" | "%") ws] unary-expression
additive-expression: [additive-expression ws ("+" | "-") ws] multiplicative-expression
shift-expression: [shift-expression ws ("<<" | ">>" | ">>>") ws] additive-expression
relational-expression: [relational-expression ws ("<" | ">" | "<=" | ">=" | "instanceof" | "in") ws] shift-expression
relational-expression-no-in: [relational-expression ws ("<" | ">" | "<=" | ">=" | "instanceof") ws] shift-expression
equality-expression: [equality-expression ws ("==" | "!=" | "===" | "!==") ws] relational-expression
equality-expression-no-in: [equality-expression-no-in ws ("==" | "!=" | "===" | "!==") ws] relational-expression-no-in
bitwise-and-expression: [bitwise-and-expression ws "&" ws] equality-expression
bitwise-and-expression-no-in: [bitwise-and-expression-no-in ws "&" ws] equality-expression-no-in
bitwise-xor-expression: [bitwise-xor-expression ws "^" ws] bitwise-and-expression
bitwise-xor-expression-no-in: [bitwise-xor-expression-no-in ws "^" ws] bitwise-and-expression-no-in
bitwise-or-expression: [bitwise-or-expression ws "|" ws] bitwise-xor-expression
bitwise-or-expression-no-in: [bitwise-or-expression-no-in ws "|" ws] bitwise-xor-expression-no-in
logical-and-expression: [logical-and-expression ws "&&" ws] bitwise-or-expression
logical-and-expression-no-in: [logical-and-expression-no-in ws "&&" ws] bitwise-or-expression-no-in
logical-or-expression: [logical-or-expression ws "||" ws] logical-and-expression
logical-or-expression-no-in: [logical-or-expression-no-in ws "||" ws] logical-and-expression-no-in
conditional-expression: logical-or-expression [ ws "?" ws assignment-expression ws ":" ws assignment-expression]
conditional-expression-no-in: logical-or-expression-no-in [ ws "?" ws assignment-expression ws ":" ws assignment-expression-no-in]
assignment-expression:
    conditional-expression |
    left-hand-side-expression ws "=" ws assignment-expression |
    left-hand-side-expression ws assignment-operator ws assignment-expression
assignment-expression-no-in:
    conditional-expression-no-in |
    left-hand-side-expression ws "=" ws assignment-expression-no-in |
    left-hand-side-expression ws assignment-operator ws assignment-expression-no-in
assignment-operator: "*=" | "/=" | "%=" | "+=" | "-=" | "<<=" | ">>=" | ">>>=" | "&=" | "^=" | "|="
expression: [expression ws "," ws] assignment-expression
expression-no-in: [expression-no-in ws "," ws] assignment-expression-no-in

;; Statements

statement: block | variable-statement | empty-statement | expression-statement |
    if-statement | iteration-statement | continue-statement |
    break-statement | return-statement | with-statement |
    labelled-statement | switch-statement | throw-statement |
    try-statement | debugger-statement
block: "{" (ws statement)* ws "}"
variable-statement: "var" ws variable-declaration-list ws eol-or-semicolon
variable-declaration-list: variable-declaration (ws "," ws variable-declaration)*
variable-declaration-list-no-in: variable-declaration-no-in (ws "," ws variable-declaration-no-in)*
variable-declaration: identifier [ws "=" ws assignment-expression]
variable-declaration-no-in: identifier [ws "=" ws assignment-expression-no-in]
empty-statement: ws ";"
expression-statement: expression ws eol-or-semicolon
if-statement: "if" ws "(" ws expression ws ")" ws statement [ws "else" ws statement]
iteration-statement:
    "do" ws statement ws "while" ws "(" ws expression ws ")" ws eol-or-semicolon |
    "while" ws "(" ws expression ws ")" ws statement |
    "for" ws "(" ([ws expression-no-in] | ws "var" ws variable-declaration-list-no-in) ws ";"
                 [ws expression] ws ";" [ws expression] ws ")" ws statement |
    "for" ws "(" ws (left-hand-side-expression | "var" ws variable-declaration-no-in)
                 ws "in" ws expression ws ")" ws statement
continue-statement: "continue" [ws-no-eol identifier] ws eol-or-semicolon
break-statement: "break" [ws-no-eol identifier] ws eol-or-semicolon
return-statement: "return" [ws-no-eol expression] ws eol-or-semicolon
with-statement: "with" ws "(" ws expression ws ")" ws statement
switch-statement: "switch" ws "(" ws expression ws ")" case-block
case-block: "{" (ws case-clause)* [ws default-clause (ws case-clause)*] ws "}"
case-clause: "case" ws expression ws ":" (ws statement)*
default-clause: "default" ws ":" (ws statement)*
labelled-statement: identifier ws ":" ws statement
throw-statement: "throw" ws-no-eol expression ws eol-or-semicolon
try-statement: "try" ws block ws (catch | finally | catch ws finally)
catch: "catch" ws "(" ws identifier ws ")" ws block
finally: "finally" ws block
debugger-statement: "debugger" ws eol-or-semicolon

;; Function definition

function-declaration: "function" ws identifier ws "(" [ws formal-parameter-list] ws ")"
    ws "{" (ws source-element)* ws "}"
function-expression: "function" [ws identifier] ws "(" [ws formal-parameter-list] ws ")"
    ws "{" (ws source-element)* ws "}"
formal-parameter-list: identifier (ws "," ws identifier)*
